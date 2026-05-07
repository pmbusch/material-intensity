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
  stopifnot(length(mean_life) == 1, length(k) == 1)
  ifelse(ages < 0, 0, weibull_survival(pmax(ages, 0), mean_life, k))
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
