## =============================================================================
## Scripts/Functions/dsm_functions.R
## Shared survival functions and forward Dynamic Stock Model (DSM) for
## material-intensity forecast scripts (04c, 04d, ...).
##
## Functions exported:
##   weibull_survival(age, mean_life, k)
##   get_survival(ages, mean_life, k)
##   run_forward_dsm(cohorts_2024, target_stock_traj, mean_life, k, ...)
## =============================================================================

# -- Survival functions --------------------------------------------------------

# Weibull scale from mean: mean = lambda * Gamma(1 + 1/k)
weibull_survival <- function(age, mean_life, k) {
  lambda <- mean_life / gamma(1 + 1 / k)
  exp(-(age / lambda)^k)
}

get_survival <- function(ages, mean_life, k) {
  lambda <- mean_life / gamma(1 + 1 / k)
  s <- exp(-(pmax(ages, 0) / lambda)^k)
  s[ages < 0] <- 0
  s
}


# -- Forward DSM: 2025-FORECAST_END for one (region x end_use x scenario) group ----
#
# cohorts_2024      tibble: cohort_year, surviving_stock_Mt (age profile at start_year)
# target_stock_traj tibble: year, target_stock_Mt (annual targets; must cover start+1..end)
# mean_life, k      Weibull parameters
# start_year        simulation anchor year (default 2024)
# end_year          last simulation year (default FORECAST_END, Scripts/00-CommonParameters.R)
# snapshot_years    years to record full cohort age profile
#
# Returns list:
#   $summary   tibble: year, total_stock_Mt, new_additions_Mt, replacement_Mt,
#                      production_Mt, waste_Mt
#   $snapshots tibble: snapshot_year, cohort_year, cohort_age, surviving_stock_Mt

run_forward_dsm <- function(
  cohorts_2024,
  target_stock_traj,
  mean_life,
  k,
  start_year = 2024L,
  end_year = FORECAST_END,
  snapshot_years = c(2024L, SNAPSHOT_YEARS)
) {
  # cohorts_2024 contains cohort_year + surviving_stock_Mt at start_year
  # Age each cohort forward directly — no back-calculation needed
  cohorts <- cohorts_2024 |> dplyr::filter(surviving_stock_Mt > 1e-9) |> dplyr::select(cohort_year, surviving_stock_Mt)

  if (nrow(cohorts) == 0) {
    empty_sum <- tibble::tibble(
      year = integer(),
      total_stock_Mt = double(),
      new_additions_Mt = double(),
      replacement_Mt = double(),
      production_Mt = double(),
      waste_Mt = double()
    )
    empty_snap <- tibble::tibble(
      snapshot_year = integer(),
      cohort_year = integer(),
      cohort_age = integer(),
      surviving_stock_Mt = double()
    )
    return(list(summary = empty_sum, snapshots = empty_snap))
  }

  sim_years <- seq(start_year + 1L, end_year)

  results_list <- vector("list", length(sim_years))
  snapshots_list <- list()

  # Pre-compute survival at base year once per cohort (enables direct-aging formula)
  all_cohorts <- cohorts |>
    dplyr::mutate(
      surv_base = get_survival(start_year - cohort_year, mean_life, k),
      base_stock_Mt = surviving_stock_Mt
    ) |>
    dplyr::select(cohort_year, base_stock_Mt, surv_base)

  # At start_year: surv_t = surv_base, so stock_Mt = base_stock_Mt
  prev_cohort_stocks <- all_cohorts |>
    dplyr::mutate(stock_Mt = dplyr::if_else(surv_base > 0.001, base_stock_Mt, 0)) |>
    dplyr::select(cohort_year, stock_Mt)

  target_lookup <- stats::setNames(target_stock_traj$target_stock_Mt, as.character(target_stock_traj$year))

  # Fix 5: starting condition snapshot at start_year
  if (start_year %in% snapshot_years) {
    snapshots_list[[as.character(start_year)]] <- cohorts |>
      dplyr::mutate(cohort_age = as.integer(start_year - cohort_year), snapshot_year = start_year) |>
      dplyr::select(snapshot_year, cohort_year, cohort_age, surviving_stock_Mt)
  }

  for (t in sim_years) {
    target_t <- target_lookup[as.character(t)]
    if (is.na(target_t)) {
      warning(sprintf("run_forward_dsm: no target for year %d -- skipping", t))
      next
    }

    # Direct-aging: stock(t) = base_stock * surv(age_t) / surv(base_age)
    curr_pre <- all_cohorts |>
      dplyr::mutate(
        age = t - cohort_year,
        surv_t = get_survival(age, mean_life, k),
        stock_Mt = dplyr::if_else(surv_base > 0.001, base_stock_Mt * surv_t / surv_base, 0)
      ) |>
      dplyr::select(cohort_year, age, stock_Mt)

    # Waste = decline per cohort from previous year; floor at 0
    waste_total <- prev_cohort_stocks |>
      dplyr::left_join(curr_pre |> dplyr::select(cohort_year, curr_stock = stock_Mt), by = "cohort_year") |>
      dplyr::mutate(waste = pmax(stock_Mt - tidyr::replace_na(curr_stock, 0), 0)) |>
      dplyr::summarise(waste = sum(waste, na.rm = TRUE)) |>
      dplyr::pull(waste)

    total_surviving <- sum(curr_pre$stock_Mt, na.rm = TRUE)

    # Fix 1: production logic
    production <- pmax(0, target_t - total_surviving)
    replacement <- min(waste_total, production)
    new_additions <- production - replacement

    # BUG FIX: the full production enters the stock as an age-0 cohort.
    # Previously only new_additions was registered, so replacement mass
    # vanished from the stock: reported stock = target - replacement, and
    # subsequent years' waste/production were computed against a stock
    # missing all past replacement cohorts. The new/replacement split is
    # an accounting split of production, not a physical one.
    if (production > 0) {
      all_cohorts <- dplyr::bind_rows(
        all_cohorts,
        tibble::tibble(cohort_year = t, base_stock_Mt = production, surv_base = 1.0)
      )
    }

    # Register new cohort (surv_base = 1 at age 0)
    if (new_additions > 0) {
      all_cohorts <- dplyr::bind_rows(
        all_cohorts,
        tibble::tibble(cohort_year = t, base_stock_Mt = new_additions, surv_base = 1.0)
      )
    }

    # Previous-year stocks for next iteration (include new cohort at age 0)
    prev_cohort_stocks <- if (production > 0) {
      dplyr::bind_rows(
        curr_pre |> dplyr::select(cohort_year, stock_Mt),
        tibble::tibble(cohort_year = t, stock_Mt = production)
      )
    } else {
      curr_pre |> dplyr::select(cohort_year, stock_Mt)
    }

    results_list[[as.character(t)]] <- tibble::tibble(
      year = t,
      total_stock_Mt = total_surviving + production, # was + new_additions
      new_additions_Mt = new_additions,
      replacement_Mt = replacement,
      production_Mt = production,
      waste_Mt = waste_total
    )

    if (t %in% snapshot_years) {
      new_row <- if (production > 0) {
        tibble::tibble(cohort_year = t, cohort_age = 0L, surviving_stock_Mt = production)
      } else {
        tibble::tibble(cohort_year = integer(), cohort_age = integer(), surviving_stock_Mt = double())
      }

      snapshots_list[[as.character(t)]] <- dplyr::bind_rows(
        curr_pre |> dplyr::select(cohort_year, cohort_age = age, surviving_stock_Mt = stock_Mt),
        new_row
      ) |>
        dplyr::mutate(snapshot_year = t)
    }
  }

  list(summary = dplyr::bind_rows(results_list), snapshots = dplyr::bind_rows(snapshots_list))
}


# -- Forward DSM (pure numeric) — used by MC ──────────────────────────────────
#
# Splice method: Nelson's Cumulative Exposure Model (CEM).
# Each legacy cohort (placed before start_year) is relabelled to the equivalent
# age s* on the new sampled Weibull curve where the cumulative fraction-failed
# matches what the cohort actually reached under the historical distribution:
#   S_new(s*) = S_hist(a),  a = start_year − cohort_year
# For Weibull this is closed-form:
#   s* = λ_new · (a / λ_hist)^(k_hist / k_new)
#   where λ = mean_life / Γ(1 + 1/k)
# Effective placement:  O_eff = cohort_stock_2024 / S_new(s*)
# Future stock:         stock(t) = O_eff · S_new((t − start_year) + s*)
#
# (a) CEM preserves stock-level continuity at the 2024→2025 seam by matching
#     fraction-failed rather than chronological age, eliminating the spike that
#     the old back-calculation O = stock/S_new(a) produced for short-lifetime
#     runs (where S_new(a) ≈ 0 for old cohorts → huge O → spike in waste).
# (b) CEM does NOT force the outflow rate to be perfectly continuous: for
#     equal-shape Weibull a residual step of mean_life_hist/mean_life_new
#     remains at the seam. This is the intended physical signal — a shorter
#     sampled lifetime raises the retirement rate — not an artifact.
# (c) When mean_life_new == mean_life_hist and k_new == k_hist, s* = a and CEM
#     reduces exactly to direct aging, reproducing a smooth historical continuation.
#
# Args (all plain numeric vectors / scalars, NO tibbles):
#   cohort_years        integer vector: years of pre-base cohorts (e.g. 1925..2024)
#   cohort_stocks_2024  numeric vector: surviving stock at start_year
#   target_stock        numeric vector length (end_year - start_year):
#                         target stock for years (start_year+1) .. end_year
#   mean_life, k        Weibull params for the SAMPLED (new) distribution
#   mean_life_hist, k_hist  Weibull params for the HISTORICAL distribution
#                           (central values from Lifetimes sheet used in
#                            03_UNEP_stock_flow_model.R to build the age profile)
#   start_year          base year (default 2024)
#   end_year            last year (default FORECAST_END, Scripts/00-CommonParameters.R)
#
# Returns a list of 6 elements: year + 5 numeric vectors of length (end_year - start_year):
#   total_stock, new_additions, replacement, production, waste
#
# No dplyr, no tibbles, no joins. Pre-base cohorts use direct exp() formula
# (non-integer s*); new cohorts use integer-age S_grid lookup.

run_forward_dsm_fast <- function(
  cohort_years,
  cohort_stocks_2024,
  target_stock,
  mean_life,
  k,
  mean_life_hist,
  k_hist,
  start_year = 2024L,
  end_year = FORECAST_END
) {
  n_years <- end_year - start_year
  sim_years <- seq.int(start_year + 1L, end_year)

  total_stock <- numeric(n_years)
  new_additions <- numeric(n_years)
  replacement <- numeric(n_years)
  production <- numeric(n_years)
  waste <- numeric(n_years)

  lambda_new <- mean_life / gamma(1 + 1 / k)
  lambda_hist_val <- mean_life_hist / gamma(1 + 1 / k_hist)

  S_grid <- exp(-(seq.int(0L, n_years) / lambda_new)^k)

  keep <- cohort_stocks_2024 > 1e-9
  cy <- cohort_years[keep]
  cs <- cohort_stocks_2024[keep]
  a <- start_year - cy

  sstar <- lambda_new * (a / lambda_hist_val)^(k_hist / k)
  S_at_sstar <- exp(-(sstar / lambda_new)^k)

  ok <- S_at_sstar > 1e-3
  pre_O <- cs[ok] / S_at_sstar[ok]
  pre_sstar <- sstar[ok]
  prev_pre <- cs[ok]

  new_yr <- integer(n_years)
  new_O <- numeric(n_years)
  n_new <- 0L
  prev_new <- numeric(0)

  for (i in seq_len(n_years)) {
    t <- sim_years[i]
    target_t <- target_stock[i]
    if (is.na(target_t)) {
      next
    }

    dt <- t - start_year

    eff_ages_pre <- dt + pre_sstar
    S_pre_t <- exp(-(eff_ages_pre / lambda_new)^k)
    stock_pre_t <- pre_O * S_pre_t

    if (n_new > 0L) {
      ages_new <- t - new_yr[seq_len(n_new)]
      stock_new_t <- new_O[seq_len(n_new)] * S_grid[ages_new + 1L]
    } else {
      stock_new_t <- numeric(0)
    }

    waste_pre_t <- sum(prev_pre - stock_pre_t)
    waste_new_t <- if (n_new > 0L) sum(prev_new - stock_new_t) else 0
    waste_t <- waste_pre_t + waste_new_t
    if (waste_t < 0) {
      waste_t <- 0
    }

    total_surviving <- sum(stock_pre_t) + sum(stock_new_t)

    prod_t <- max(0, target_t - total_surviving)
    repl_t <- min(waste_t, prod_t)
    new_t <- prod_t - repl_t

    # BUG FIX: the full production enters the stock as an age-0 cohort.
    # Previously only new_t was registered, so the replacement mass vanished:
    # stock(t) = target - replacement, and later years' waste/production were
    # computed against a stock missing all past replacement cohorts.
    # new/replacement is an ACCOUNTING split of prod_t, not a physical one.
    if (prod_t > 0) {
      n_new <- n_new + 1L
      new_yr[n_new] <- t
      new_O[n_new] <- prod_t
      prev_new <- c(stock_new_t, prod_t)
    } else {
      prev_new <- stock_new_t
    }
    prev_pre <- stock_pre_t

    # Stock now equals target whenever target >= surviving (prod_t closes the gap)
    total_stock[i] <- total_surviving + prod_t
    new_additions[i] <- new_t
    replacement[i] <- repl_t
    production[i] <- prod_t
    waste[i] <- waste_t
  }

  list(
    year = sim_years,
    total_stock = total_stock,
    new_additions = new_additions,
    replacement = replacement,
    production = production,
    waste = waste
  )
}
