## =============================================================================
## 02_compute_metrics.R
## Builds material/energy/agriculture intensity metrics (ratio of sums, at
## native R10 + World resolution) from the cleaned IIASA ScenarioMIP panel,
## then derives per-run CAGRs (base year -> FORECAST_END; the candidate
## sampling parameters) and a base-year-harmonized level series (for the
## trajectory figure only -- CAGR is scale-invariant under this harmonization,
## so no separate "harmonized CAGR" is produced; see Step 5 note).
## Every ratio metric (numerator/GDP, numerator/capita) is computed from a
## single wide row per (model, family, ssp, marker, region, year) -- see
## Step 1's pivot_wider -- so numerator and denominator always come from the
## same run/region/year cell; there is no cross-model or cross-scenario
## mixing to check for.
## Also adds raw-level ("_total") and per-capita/per-GDP metrics for the 9
## focus variables used by 03_trajectory_figures.R (floor space Res/Com,
## coal/gas/oil extraction, steel, aluminum, cement, crop and livestock production), plus
## roundwood (total/GDP/pc), food intake/waste (per capita only), and /GDP
## metrics for every primary energy source (MJ/USD, 03c) and electricity
## capacity source (W/USD, 03d).
##
## Input:  Parameters/SSP_ScenarioMIP/ssp_long_clean.csv  (from 01_load_and_check.R)
## Outputs (Parameters/SSP_ScenarioMIP/):
##   metric_units.csv            — metric -> unit lookup
##   metrics_levels_raw.csv      — long: id cols + metric + year + value
##   metrics_levels_harmonized.csv
##   metrics_cagr.csv            — one row per (model, family, ssp, region, metric)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

BASE_YEAR <- 2020L # matches 01_load_and_check.R
OUT_DIR <- "Parameters/SSP_ScenarioMIP"

ID_COLS <- c("model", "family", "ssp", "marker", "region")


# Step 1: Load cleaned panel, pivot to one column per raw variable ────────────

cat("STEP 1: Load cleaned panel and pivot wide by variable\n")

ssp_long <- read_csv(file.path(OUT_DIR, "ssp_long_clean.csv"), show_col_types = FALSE)

ssp_wide <- ssp_long %>%
  dplyr::select(all_of(ID_COLS), year, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

cat("  Wide panel:", nrow(ssp_wide), "rows x", ncol(ssp_wide), "cols\n")


# Step 2: Compute intensity metrics (ratio of sums: numerator/denominator are ─
# both taken directly from the same model-scenario-region-year row, never
# averaged across models/regions first) ───────────────────────────────────────

cat("\nSTEP 2: Compute intensity metrics\n")

metrics_wide <- ssp_wide %>%
  mutate(
    # GDP per capita (GDP|MER basis -- NOTE: this differs from GDP|PPP used
    # elsewhere in this project's own GDP/consumption figures; do not compare directly)
    gdp_pc = `GDP|MER` * 1000 / Population, # billion USD / million people -> USD/capita (1e9/1e6 = 1e3)

    # Primary energy / GDP, total and by carrier. Rescaled x1000 to MJ/USD
    # (EJ/billion USD = 1e3 MJ/USD), same convention as fossil extraction below.
    # Geothermal/Hydro/Nuclear/Ocean/Non-Biomass Renewables come from the
    # dedicated energy extract (Ocean: 1 model only).
    pe_gdp = `Primary Energy` / `GDP|MER` * 1000,
    pe_biomass_gdp = `Primary Energy|Biomass` / `GDP|MER` * 1000,
    pe_coal_gdp = `Primary Energy|Coal` / `GDP|MER` * 1000,
    pe_fossil_gdp = `Primary Energy|Fossil` / `GDP|MER` * 1000,
    pe_gas_gdp = `Primary Energy|Gas` / `GDP|MER` * 1000,
    pe_oil_gdp = `Primary Energy|Oil` / `GDP|MER` * 1000,
    pe_other_gdp = `Primary Energy|Other` / `GDP|MER` * 1000,
    pe_geothermal_gdp = `Primary Energy|Geothermal` / `GDP|MER` * 1000,
    pe_hydro_gdp = `Primary Energy|Hydro` / `GDP|MER` * 1000,
    pe_nuclear_gdp = `Primary Energy|Nuclear` / `GDP|MER` * 1000,
    pe_ocean_gdp = `Primary Energy|Ocean` / `GDP|MER` * 1000,
    pe_nonbio_renew_gdp = `Primary Energy|Non-Biomass Renewables` / `GDP|MER` * 1000,

    # Electricity capacity / GDP: GW / billion USD = 1e9 W / 1e9 USD = W/USD (no rescale)
    cap_biomass_gdp = `Capacity|Electricity|Biomass` / `GDP|MER`,
    cap_coal_gdp = `Capacity|Electricity|Coal` / `GDP|MER`,
    cap_fossil_gdp = `Capacity|Electricity|Fossil` / `GDP|MER`,
    cap_gas_gdp = `Capacity|Electricity|Gas` / `GDP|MER`,
    cap_geothermal_gdp = `Capacity|Electricity|Geothermal` / `GDP|MER`,
    cap_hydro_gdp = `Capacity|Electricity|Hydro` / `GDP|MER`,
    cap_hydrogen_gdp = `Capacity|Electricity|Hydrogen` / `GDP|MER`,
    cap_nuclear_gdp = `Capacity|Electricity|Nuclear` / `GDP|MER`,
    cap_oil_gdp = `Capacity|Electricity|Oil` / `GDP|MER`,
    cap_other_gdp = `Capacity|Electricity|Other` / `GDP|MER`,
    cap_solar_csp_gdp = `Capacity|Electricity|Solar|CSP` / `GDP|MER`,
    cap_solar_pv_gdp = `Capacity|Electricity|Solar|PV` / `GDP|MER`,
    cap_wind_offshore_gdp = `Capacity|Electricity|Wind|Offshore` / `GDP|MER`,
    cap_wind_onshore_gdp = `Capacity|Electricity|Wind|Onshore` / `GDP|MER`,

    # Food intake / waste: reported per capita at source (kcal/cap/day) -- used as-is, per-capita only
    food_crops_pc = `Food Intake|Crops [per capita]`,
    food_livestock_pc = `Food Intake|Livestock [per capita]`,
    food_waste_pc = `Food Waste [per capita]`,

    # Roundwood: million m3 / billion USD = 1e-3 m3/USD = L/USD (no rescale);
    # million m3 / million people = m3/capita
    forestry_production_total = `Forestry Production|Roundwood`,
    forestry_production_gdp = `Forestry Production|Roundwood` / `GDP|MER`,
    forestry_production_pc = `Forestry Production|Roundwood` / Population,
    pe_fossil_share = `Primary Energy|Fossil` / `Primary Energy`,
    pe_biomass_share = `Primary Energy|Biomass` / `Primary Energy`,

    # Agriculture: production, crops, residues (residues = "Demand", the only
    # residues series in the source file -- there is no residues *production* variable)
    agri_prod_gdp = `Agricultural Production` / `GDP|MER`,
    agri_prod_pc = `Agricultural Production` / Population,
    agri_crops_gdp = `Agricultural Production|Crops` / `GDP|MER`,
    agri_crops_pc = `Agricultural Production|Crops` / Population,
    agri_livestock_gdp = `Agricultural Production|Livestock` / `GDP|MER`,
    agri_livestock_pc = `Agricultural Production|Livestock` / Population,
    agri_residues_gdp = `Agricultural Demand|Residues` / `GDP|MER`,
    agri_residues_pc = `Agricultural Demand|Residues` / Population,

    # Steel, cement, aluminium production / GDP and per capita
    steel_gdp = `Production|Iron and Steel|Steel` / `GDP|MER`,
    steel_pc = `Production|Iron and Steel|Steel` / Population,
    cement_gdp = `Production|Non-Metallic Minerals|Cement` / `GDP|MER`,
    cement_pc = `Production|Non-Metallic Minerals|Cement` / Population,
    aluminum_gdp = `Production|Non-Ferrous Metals|Aluminum` / `GDP|MER`,
    aluminum_pc = `Production|Non-Ferrous Metals|Aluminum` / Population,

    # Floor space (flow/service-level proxy, NOT an in-use material stock estimate).
    # "Residential and Commercial" is the only split reported by both models that
    # report floor space at all (MESSAGEix-GLOBIOM-GAINS, REMIND-MAgPIE); use it as
    # the primary metric. Residential/Commercial splits exist for MESSAGEix only.
    # Per-capita: billion m2 / million people -> m2/capita needs the same x1000
    # scale correction as gdp_pc (1e9/1e6 = 1e3) -- unlike the mass-based metrics
    # above (million t / million people), where the "million" cancels exactly.
    floor_rescom_gdp = `Building Stock|Residential and Commercial|Floor Space|Gross` / `GDP|MER`,
    floor_rescom_pc = `Building Stock|Residential and Commercial|Floor Space|Gross` * 1000 / Population,
    floor_res_gdp = `Building Stock|Residential|Floor Space|Gross` / `GDP|MER`,
    floor_res_pc = `Building Stock|Residential|Floor Space|Gross` * 1000 / Population,
    floor_com_gdp = `Building Stock|Commercial|Floor Space|Gross` / `GDP|MER`,
    floor_com_pc = `Building Stock|Commercial|Floor Space|Gross` * 1000 / Population,

    # Fossil fuel extraction / GDP (Coal/Gas/Oil extraction are all reported in
    # EJ/yr -- energy-equivalent units, so the total is unit-consistent)
    fossil_extraction_total = `Resource|Extraction|Coal` + `Resource|Extraction|Gas` + `Resource|Extraction|Oil`,
    fossil_extraction_gdp = fossil_extraction_total / `GDP|MER`,

    # Focus variables for 03_trajectory_figures.R: raw level ("_total"), /GDP,
    # and per-capita, individually for Coal/Gas/Oil (fossil_extraction_* above
    # is their combined total, kept as-is for the existing metric set).
    # Per-capita scale: EJ/yr / million people = 1e18 J / 1e6 people = TJ/capita,
    # no rescale needed (same "million cancels" logic as steel/cement/aluminum
    # below); floor space needs the x1000 correction noted above (billion m2 vs
    # million people). GDP-intensity is rescaled x1000 to read in MJ/USD
    # (EJ/billion USD = 1e18 J / 1e9 USD = 1e9 J/USD = 1e3 MJ/USD).
    coal_extraction_total = `Resource|Extraction|Coal`,
    coal_extraction_gdp = `Resource|Extraction|Coal` / `GDP|MER` * 1000,
    coal_extraction_pc = `Resource|Extraction|Coal` / Population,
    gas_extraction_total = `Resource|Extraction|Gas`,
    gas_extraction_gdp = `Resource|Extraction|Gas` / `GDP|MER` * 1000,
    gas_extraction_pc = `Resource|Extraction|Gas` / Population,
    oil_extraction_total = `Resource|Extraction|Oil`,
    oil_extraction_gdp = `Resource|Extraction|Oil` / `GDP|MER` * 1000,
    oil_extraction_pc = `Resource|Extraction|Oil` / Population,

    steel_total = `Production|Iron and Steel|Steel`,
    aluminum_total = `Production|Non-Ferrous Metals|Aluminum`,
    cement_total = `Production|Non-Metallic Minerals|Cement`,
    agri_crops_total = `Agricultural Production|Crops`,
    agri_livestock_total = `Agricultural Production|Livestock`,
    floor_com_total = `Building Stock|Commercial|Floor Space|Gross`,
    floor_res_total = `Building Stock|Residential|Floor Space|Gross`
  )

metric_units <- tibble::tribble(
  ~metric, ~unit,
  "gdp_pc", "USD_2010/capita",
  "pe_gdp", "MJ/USD",
  "pe_biomass_gdp", "MJ/USD",
  "pe_coal_gdp", "MJ/USD",
  "pe_fossil_gdp", "MJ/USD",
  "pe_gas_gdp", "MJ/USD",
  "pe_oil_gdp", "MJ/USD",
  "pe_other_gdp", "MJ/USD",
  "pe_geothermal_gdp", "MJ/USD",
  "pe_hydro_gdp", "MJ/USD",
  "pe_nuclear_gdp", "MJ/USD",
  "pe_ocean_gdp", "MJ/USD",
  "pe_nonbio_renew_gdp", "MJ/USD",
  "cap_biomass_gdp", "W/USD",
  "cap_coal_gdp", "W/USD",
  "cap_fossil_gdp", "W/USD",
  "cap_gas_gdp", "W/USD",
  "cap_geothermal_gdp", "W/USD",
  "cap_hydro_gdp", "W/USD",
  "cap_hydrogen_gdp", "W/USD",
  "cap_nuclear_gdp", "W/USD",
  "cap_oil_gdp", "W/USD",
  "cap_other_gdp", "W/USD",
  "cap_solar_csp_gdp", "W/USD",
  "cap_solar_pv_gdp", "W/USD",
  "cap_wind_offshore_gdp", "W/USD",
  "cap_wind_onshore_gdp", "W/USD",
  "food_crops_pc", "kcal/capita/day",
  "food_livestock_pc", "kcal/capita/day",
  "food_waste_pc", "kcal/capita/day",
  "forestry_production_total", "million m³/yr",
  "forestry_production_gdp", "L/USD",
  "forestry_production_pc", "m³/capita",
  "pe_fossil_share", "fraction of Primary Energy",
  "pe_biomass_share", "fraction of Primary Energy",
  "agri_prod_gdp", "million t DM per billion USD_2010",
  "agri_prod_pc", "t DM/capita",
  "agri_crops_gdp", "kg/USD",
  "agri_crops_pc", "t DM/capita",
  "agri_livestock_gdp", "kg/USD",
  "agri_livestock_pc", "t DM/capita",
  "agri_residues_gdp", "million t DM per billion USD_2010",
  "agri_residues_pc", "t DM/capita",
  "steel_gdp", "kg/USD",
  "steel_pc", "t/capita",
  "cement_gdp", "kg/USD",
  "cement_pc", "t/capita",
  "aluminum_gdp", "kg/USD",
  "aluminum_pc", "t/capita",
  "floor_rescom_gdp", "billion m2 per billion USD_2010",
  "floor_rescom_pc", "m2/capita",
  "floor_res_gdp", "m2/USD",
  "floor_res_pc", "m2/capita",
  "floor_com_gdp", "m2/USD",
  "floor_com_pc", "m2/capita",
  "fossil_extraction_total", "EJ/yr",
  "fossil_extraction_gdp", "EJ per billion USD_2010",
  "coal_extraction_total", "EJ/yr",
  "coal_extraction_gdp", "MJ/USD",
  "coal_extraction_pc", "TJ/capita",
  "gas_extraction_total", "EJ/yr",
  "gas_extraction_gdp", "MJ/USD",
  "gas_extraction_pc", "TJ/capita",
  "oil_extraction_total", "EJ/yr",
  "oil_extraction_gdp", "MJ/USD",
  "oil_extraction_pc", "TJ/capita",
  "steel_total", "Mt",
  "aluminum_total", "Mt",
  "cement_total", "Mt",
  "agri_crops_total", "million t DM",
  "agri_livestock_total", "million t DM",
  "floor_com_total", "billion m2",
  "floor_res_total", "billion m2"
)
write_csv(metric_units, file.path(OUT_DIR, "metric_units.csv"))
cat("  Metrics computed:", nrow(metric_units), "\n")


# Step 3: Long format levels table ─────────────────────────────────────────────

cat("\nSTEP 3: Reshape to long format\n")

metrics_levels_raw <- metrics_wide %>%
  dplyr::select(all_of(ID_COLS), year, all_of(metric_units$metric)) %>%
  pivot_longer(cols = all_of(metric_units$metric), names_to = "metric", values_to = "value") %>%
  filter(is.finite(value))

cat("  Levels table:", nrow(metrics_levels_raw), "rows\n")
write_csv(metrics_levels_raw, file.path(OUT_DIR, "metrics_levels_raw.csv"))


# Step 4: CAGR per run, base year -> FORECAST_END (candidate sampling parameters)

cat("\nSTEP 4: Compute per-run CAGR,", BASE_YEAR, "->", FORECAST_END, "\n")

cagr_input <- metrics_levels_raw %>%
  filter(year %in% c(BASE_YEAR, FORECAST_END)) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y")

base_col <- paste0("y", BASE_YEAR)
end_col <- paste0("y", FORECAST_END)

metrics_cagr <- cagr_input %>%
  filter(!is.na(.data[[base_col]]), !is.na(.data[[end_col]]), .data[[base_col]] > 0) %>%
  mutate(
    value_base = .data[[base_col]],
    value_end = .data[[end_col]],
    cagr = (value_end / value_base)^(1 / (FORECAST_END - BASE_YEAR)) - 1
  ) %>%
  dplyr::select(all_of(ID_COLS), metric, value_base, value_end, cagr)

cat("  CAGR table:", nrow(metrics_cagr), "rows (", n_distinct(metrics_cagr$metric), "metrics x runs )\n")
write_csv(metrics_cagr, file.path(OUT_DIR, "metrics_cagr.csv"))


# Step 5: Base-year-harmonized levels (for the trajectory figure only) ────────
# NOTE: harmonization rescales each run's whole series by one constant factor
# (median_base / run's own base value), computed per region x metric. Because
# CAGR = (end/start)^(1/n) - 1 and the constant cancels in end/start, harmonized
# CAGR == raw CAGR exactly -- so it is not recomputed here.

cat("\nSTEP 5: Base-year harmonization (levels only, for Figure 1)\n")

base_medians <- metrics_levels_raw %>%
  filter(year == BASE_YEAR) %>%
  group_by(region, metric) %>%
  summarise(median_base = median(value, na.rm = TRUE), .groups = "drop")

own_base <- metrics_levels_raw %>%
  filter(year == BASE_YEAR) %>%
  dplyr::select(all_of(ID_COLS), metric, own_base = value)

metrics_levels_harmonized <- metrics_levels_raw %>%
  left_join(own_base, by = c(ID_COLS, "metric")) %>%
  left_join(base_medians, by = c("region", "metric")) %>%
  filter(!is.na(own_base), own_base > 0) %>%
  mutate(value = value * median_base / own_base) %>%
  dplyr::select(all_of(ID_COLS), year, metric, value)

cat("  Harmonized levels table:", nrow(metrics_levels_harmonized), "rows\n")
write_csv(metrics_levels_harmonized, file.path(OUT_DIR, "metrics_levels_harmonized.csv"))

# EoF
