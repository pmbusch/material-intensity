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


# -- Forward DSM: 2025-2060 for one (region x end_use x scenario) group -------
#
# cohorts_2024      tibble: cohort_year, surviving_stock_Mt (age profile at start_year)
# target_stock_traj tibble: year, target_stock_Mt (annual targets; must cover start+1..end)
# mean_life, k      Weibull parameters
# start_year        simulation anchor year (default 2024)
# end_year          last simulation year (default 2060)
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
  end_year = 2060L,
  snapshot_years = c(2024L, 2030L, 2040L, 2050L, 2060L)
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

    # Register new cohort (surv_base = 1 at age 0)
    if (new_additions > 0) {
      all_cohorts <- dplyr::bind_rows(
        all_cohorts,
        tibble::tibble(cohort_year = t, base_stock_Mt = new_additions, surv_base = 1.0)
      )
    }

    # Previous-year stocks for next iteration (include new cohort at age 0)
    prev_cohort_stocks <- if (new_additions > 0) {
      dplyr::bind_rows(
        curr_pre |> dplyr::select(cohort_year, stock_Mt),
        tibble::tibble(cohort_year = t, stock_Mt = new_additions)
      )
    } else {
      curr_pre |> dplyr::select(cohort_year, stock_Mt)
    }

    # Fix 3: character key avoids integer-arithmetic indexing bugs
    results_list[[as.character(t)]] <- tibble::tibble(
      year = t,
      total_stock_Mt = total_surviving + new_additions,
      new_additions_Mt = new_additions,
      replacement_Mt = replacement,
      production_Mt = production,
      waste_Mt = waste_total
    )

    if (t %in% snapshot_years) {
      new_row <- if (new_additions > 0) {
        tibble::tibble(cohort_year = t, cohort_age = 0L, surviving_stock_Mt = new_additions)
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
# Args (all plain numeric vectors / scalars, NO tibbles):
#   cohort_years        integer vector: years of pre-base cohorts (e.g. 1925..2024)
#   cohort_stocks_2024  numeric vector: surviving stock at start_year, same length as cohort_years
#   target_stock        numeric vector of length (end_year - start_year):
#                         target stock for years (start_year+1) .. end_year
#   mean_life, k        Weibull params (scalars)
#   start_year          base year (default 2024)
#   end_year            last year (default 2060)
#
# Returns a list of 5 numeric vectors, length (end_year - start_year):
#   total_stock, new_additions, replacement, production, waste
#
# Math:
#   O(c) = effective original placement of cohort c
#        = cohort_stock_2024(c) / S(start_year - c)     for pre-base cohorts
#        = new_additions(t)                              for cohort placed at year t (S(0)=1)
#   stock(c, t) = O(c) * S(t - c)
#   waste(c, t) = O(c) * [S(t-1-c) - S(t-c)]    (monotone decreasing ⇒ always ≥ 0)
#
# No dplyr, no tibbles, no joins. Inner loop is ~10 lines of vector arithmetic.

run_forward_dsm_fast <- function(
  cohort_years,
  cohort_stocks_2024,
  target_stock,
  mean_life,
  k,
  start_year = 2024L,
  end_year = 2060L
) {
  n_years <- end_year - start_year # number of simulated years
  sim_years <- seq.int(start_year + 1L, end_year) # years 2025..2060

  # Output buffers
  total_stock <- numeric(n_years)
  new_additions <- numeric(n_years)
  replacement <- numeric(n_years)
  production <- numeric(n_years)
  waste <- numeric(n_years)

  # Pre-compute Weibull scale once
  lambda <- mean_life / gamma(1 + 1 / k)

  # Survival lookup: S[age + 1] = survival at given age (age 0..n_max)
  # Max possible age: (end_year - min(cohort_years))
  max_age <- end_year - min(cohort_years)
  ages_grid <- 0:max_age
  S_grid <- exp(-(ages_grid / lambda)^k) # length max_age + 1

  # Filter near-zero starting cohorts and compute effective placement O(c)
  keep <- cohort_stocks_2024 > 1e-9
  cy_pre <- cohort_years[keep]
  age_at_base <- start_year - cy_pre # ≥ 0
  S_at_base <- S_grid[age_at_base + 1L]
  # Guard against tiny S_at_base (very old cohorts blow up division)
  ok <- S_at_base > 1e-3
  cy_pre <- cy_pre[ok]
  O_pre <- cohort_stocks_2024[keep][ok] / S_at_base[ok]

  # Storage for cohort placements as they accumulate.
  # Pre-allocate generously: pre-base cohorts + one per sim year.
  max_cohorts <- length(cy_pre) + n_years
  cohort_yr <- integer(max_cohorts)
  cohort_O <- numeric(max_cohorts)
  cohort_yr[seq_along(cy_pre)] <- cy_pre
  cohort_O[seq_along(cy_pre)] <- O_pre
  n_c <- length(cy_pre) # active count

  # Stock at start_year per cohort = O * S(start_year - c) = original stocks
  # We track prev-year stocks for waste calc.
  prev_stock <- cohort_stocks_2024[keep][ok] # = O_pre * S_at_base

  for (i in seq_len(n_years)) {
    t <- sim_years[i]
    target_t <- target_stock[i]
    if (is.na(target_t)) {
      next
    }

    # Vectorized stock at year t for all active cohorts
    ages_t <- t - cohort_yr[seq_len(n_c)]
    S_t <- S_grid[ages_t + 1L]
    stock_t <- cohort_O[seq_len(n_c)] * S_t

    # Waste = prev_stock - stock_t (cohort-wise, ≥ 0 since S is monotone)
    # prev_stock has length matching the n_c BEFORE adding new cohort
    waste_t <- sum(prev_stock - stock_t[seq_along(prev_stock)])
    if (waste_t < 0) {
      waste_t <- 0
    } # numerical safety

    total_surviving <- sum(stock_t)

    # Production logic
    prod_t <- max(0, target_t - total_surviving)
    repl_t <- min(waste_t, prod_t)
    new_t <- prod_t - repl_t

    # Register new cohort if any production
    if (new_t > 0) {
      n_c <- n_c + 1L
      cohort_yr[n_c] <- t
      cohort_O[n_c] <- new_t # O = new_t since S(0) = 1
      prev_stock <- c(stock_t, new_t)
    } else {
      prev_stock <- stock_t
    }

    total_stock[i] <- total_surviving + new_t
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
