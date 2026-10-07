## Figure 4 - Climate - PrepareData.R  (run for Figure 4 - Climate.R)
## Maps each MC run to the ScenarioMIP emissions scenario whose intensity
## trajectory is closest to the run's own intensity draws, and gives the run
## that scenario's FIG_END median warming (used by Figure 4 - Climate.R,
## mapped version).
##
## Link: for the six flow materials, the MC draw u (intensity_<key>_global)
## places the run's 2060/2025 intensity ratio within its SSP's min-max across
## ScenarioMIP runs (01-Sampling.R). Each ScenarioMIP run's own World ratio is
## put in the same u-space: u = (ratio - Min) / (Max - Min), World bounds of
## its SSP (summary_ratio_2060_2025.csv). Distance = RMS over the six u's
## (only scenarios reporting all six). Each run's temperature = inverse-distance
## weighted mean of the K_NEIGHBOURS nearest scenarios of its dominant SSP
## (weights 1/dist^IDW_POWER), so a run between scenarios gets an intermediate
## warming instead of snapping to a single one.
##
## Output: Parameters/Intermediate/Figure4_RunScenarioMap.csv
##   run_id, ssp, t_med (IDW FIG_END median GSAT, °C vs 1850-1900),
##   model / family / dist / t_nearest of the single nearest scenario (reference)

source("Scripts/00-Libraries.R", encoding = "UTF-8")

cat("=== Figure 6 - Prepare Data ===\n\n")

# Parameters ------------------------------------------------------------

RATIO_BASE_YEAR <- 2025L # base year of the ScenarioMIP ratio bounds
FIG_END <- FORECAST_END # 2060
K_NEIGHBOURS <- 3L # nearest scenarios averaged per run
IDW_POWER <- 2 # inverse-distance weight exponent
DIST_FLOOR <- 1e-6 # guards a zero distance (run exactly on a scenario)

CLIMATE_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-0e7dfab0-46ee-486b-b50d-4faf3de27da4.csv"
TEMP_MEDIAN_VAR <- "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICC v7.6.0a3]"
# 33rd / 67th percentile of each scenario's warming (SI: bounds instead of median)
TEMP_P33_VAR <- "Climate Assessment|Surface Temperature (GSAT)|33rd Percentile [MAGICC v7.6.0a3]"
TEMP_P67_VAR <- "Climate Assessment|Surface Temperature (GSAT)|67th Percentile [MAGICC v7.6.0a3]"

# ScenarioMIP /GDP variable -> MC flow material key (same map as 01-Sampling.R)
FLOW_VAR_MAP <- c(
  "coal_extraction_gdp" = "coal",
  "oil_extraction_gdp" = "oil",
  "gas_extraction_gdp" = "gas",
  "forestry_production_gdp" = "wood",
  "agri_crops_gdp" = "crops",
  "agri_livestock_gdp" = "grazed_biomass"
)

# Load data ---------------------------------------------------------------

# Dominant SSP of each run (same rule as Figure 5 - Sensitivity - PrepareData.R)
run_dom <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year == RATIO_BASE_YEAR) |>
  dplyr::distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::collect() |>
  dplyr::mutate(ssp = dplyr::if_else(ssp_share_lo >= 0.5, ssp_lo, ssp_hi)) |>
  dplyr::select(run_id, ssp)

# Run intensity draws, long (run_id, mat_key, u_run)
run_u <- readr::read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE) |>
  dplyr::select(run_id, dplyr::all_of(paste0("intensity_", FLOW_VAR_MAP, "_global"))) |>
  tidyr::pivot_longer(-run_id, names_to = "col", values_to = "u_run") |>
  dplyr::mutate(mat_key = stringr::str_remove_all(col, "^intensity_|_global$")) |>
  dplyr::select(run_id, mat_key, u_run) |>
  dplyr::inner_join(run_dom, by = "run_id")

# World ratio bounds per material x SSP
bounds_world <- readr::read_csv("Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv", show_col_types = FALSE) |>
  dplyr::filter(Region == "World", Variable %in% names(FLOW_VAR_MAP), SSP %in% SSP_SAMPLED) |>
  dplyr::transmute(mat_key = unname(FLOW_VAR_MAP[Variable]), ssp = SSP, r_min = Min, r_max = Max)

# Scenario position in u-space --------------------------------------------

# Each ScenarioMIP run's World FIG_END/RATIO_BASE_YEAR ratio, rescaled into its SSP's [min, max]
scen_u <- readr::read_csv("Parameters/IIASA-Trajectories/metrics_levels_raw.csv", show_col_types = FALSE) |>
  dplyr::filter(
    region == "World", metric %in% names(FLOW_VAR_MAP), ssp %in% SSP_SAMPLED,
    year %in% c(RATIO_BASE_YEAR, FIG_END)
  ) |>
  dplyr::select(model, family, ssp, metric, year, value) |>
  tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "v") |>
  dplyr::filter(.data[[paste0("v", RATIO_BASE_YEAR)]] > 0) |>
  dplyr::mutate(
    mat_key = unname(FLOW_VAR_MAP[metric]),
    ratio = .data[[paste0("v", FIG_END)]] / .data[[paste0("v", RATIO_BASE_YEAR)]]
  ) |>
  dplyr::inner_join(bounds_world, by = c("mat_key", "ssp")) |>
  dplyr::mutate(u_scen = (ratio - r_min) / (r_max - r_min)) |>
  dplyr::select(model, family, ssp, mat_key, u_scen)

# FIG_END median GSAT of every ScenarioMIP run
scen_temp <- readr::read_csv(CLIMATE_FILE, col_types = readr::cols(.default = "c")) |>
  dplyr::distinct() |>
  dplyr::filter(variable %in% c(TEMP_MEDIAN_VAR, TEMP_P33_VAR, TEMP_P67_VAR)) |>
  dplyr::mutate(
    scenario_clean = stringr::str_squish(stringr::str_remove(scenario, stringr::fixed("(Marker)"))),
    ssp = stringr::str_extract(scenario_clean, "SSP[1-5]"),
    family = stringr::str_squish(stringr::str_remove(scenario_clean, "-\\s*SSP[1-5]$")),
    stat = dplyr::case_when(variable == TEMP_MEDIAN_VAR ~ "t_med", variable == TEMP_P33_VAR ~ "t_p33", TRUE ~ "t_p67"),
    value = as.numeric(.data[[as.character(FIG_END)]])
  ) |>
  dplyr::select(model, family, ssp, stat, value) |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  dplyr::filter(!is.na(t_med))

# Run temperature from nearest scenarios -------------------------------------

# RMS distance in u-space between the run's six draws and each scenario of its
# dominant SSP; scenarios missing any of the six materials or without climate
# data are dropped
run_scen_dist <- run_u |>
  dplyr::inner_join(scen_u, by = c("ssp", "mat_key"), relationship = "many-to-many") |>
  dplyr::group_by(run_id, ssp, model, family) |>
  dplyr::summarise(dist = sqrt(mean((u_run - u_scen)^2)), n_dims = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n_dims == length(FLOW_VAR_MAP)) |>
  dplyr::inner_join(scen_temp, by = c("model", "family", "ssp"))

# K nearest per run; temperature = inverse-distance weighted mean, single
# nearest scenario kept for reference
run_map <- run_scen_dist |>
  dplyr::group_by(run_id) |>
  dplyr::slice_min(dist, n = K_NEIGHBOURS, with_ties = FALSE) |>
  dplyr::arrange(dist, .by_group = TRUE) |>
  # (order matters: summarise() columns see the ones redefined before them)
  dplyr::summarise(
    ssp = ssp[1],
    t_nearest = t_med[1],
    t_p33 = sum(t_p33 / pmax(dist, DIST_FLOOR)^IDW_POWER) / sum(1 / pmax(dist, DIST_FLOOR)^IDW_POWER),
    t_p67 = sum(t_p67 / pmax(dist, DIST_FLOOR)^IDW_POWER) / sum(1 / pmax(dist, DIST_FLOOR)^IDW_POWER),
    t_med = sum(t_med / pmax(dist, DIST_FLOOR)^IDW_POWER) / sum(1 / pmax(dist, DIST_FLOOR)^IDW_POWER),
    model = model[1], family = family[1], dist = dist[1],
    .groups = "drop"
  )

stopifnot(nrow(run_map) == dplyr::n_distinct(run_dom$run_id))

cat("  Candidate scenarios per SSP (all six materials + climate):\n")
print(run_scen_dist |> dplyr::distinct(ssp, model, family) |> dplyr::count(ssp, name = "n_scen"))
cat("\n  Run temperature (IDW of", K_NEIGHBOURS, "nearest) vs single nearest, per SSP:\n")
print(
  run_map |>
    dplyr::group_by(ssp) |>
    dplyr::summarise(
      t_idw_min = min(t_med), t_idw_med = median(t_med), t_idw_max = max(t_med),
      t_near_min = min(t_nearest), t_near_med = median(t_nearest), t_near_max = max(t_nearest),
      .groups = "drop"
    ) |>
    as.data.frame()
)

# Save ------------------------------------------------------------------

readr::write_csv(run_map, "Parameters/Intermediate/Figure4_RunScenarioMap.csv")
cat("\n  Saved: Parameters/Intermediate/Figure4_RunScenarioMap.csv\n")
cat("=== Figure 6 Prepare Data done ===\n")

# EoF
