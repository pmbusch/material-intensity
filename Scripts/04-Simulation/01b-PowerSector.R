## =============================================================================
## 01b-PowerSector.R
## Fixed (non-sampled) inputs for the explicit power-sector stock: generation
## capacity + stationary batteries, linked to each run's fossil draws.
##
## Scenario set = the ScenarioMIP runs behind the fossil flow bounds (01-Sampling.R):
## every (model, family, SSP) reporting coal, gas AND oil extraction/GDP at R10
## level. Each such run is one "emissions scenario" point (model x family).
##
## Capacity path per scenario x model region x tech (annual, 2025-FORECAST_END):
##   factor_gw(t) such that target GW(t) = factor_gw(t) x pop index x GDP/cap index
##   intensity I(t) = GDP-weighted mean over R10 of GW/GDP|MER (weights = r10_weights)
##   ratio form: factor = cap_2025 x I(t) / I(2025)   (cap_2025 > 0, I(2025) > 0 and
##               cap_2025 / scenario's own 2025 GW within [1/POWER_CALIB_MAX, POWER_CALIB_MAX])
##   level form: factor = I(t) x scenario 2025 GDP|MER of the region (otherwise: zero or tiny base)
##   cap_2025 = median over the scenario set of R10 2025 GW, split to model
##   regions by each R10's GDP share (missing technology = 0)
##
## Fossil index per scenario x region = sum over coal/gas/oil of 2024 energy share
## x scenario 2060/2025 extraction/GDP ratio (R10 -> region GDP-weighted; R10 cells
## that 04_summary_ratio.R replaced by World use the scenario's World ratio).
## World index = regional indices weighted by 2024 regional fossil energy.
##
## Outputs (Parameters/Simulation/):
##   power_scenarios.csv          scen_id, model, family, ssp, marker, fossil_index_world, t_2060
##   power_scenario_index.csv     scen_id, ssp, region, fossil_index
##   power_fossil_shares.csv      region, fuel, energy_PJ, share, world_weight
##   power_capacity_factor.csv    scen_id, ssp, region, tech, year (MODEL year: scenario year - 1), factor_gw, form
##   power_capacity_anchor.csv    region, tech, cap_2025_gw
##   power_capacity_check_2025.csv tech, file_world_median/min/max, anchor_world
##   power_material_intensity.csv unit_key, material_group, t_per_unit, unit
##   power_carveout_2025.csv      material, region, sub_use, carve_Mt, stock_Mt, remaining_Mt
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

options(width = 200)
YEARS_POWER <- seq(CAPACITY_ANCHOR_YEAR, FORECAST_END)
FOSSIL_VARS <- c("coal" = "coal_extraction_gdp", "gas" = "gas_extraction_gdp", "oil" = "oil_extraction_gdp")
BASE_COL <- paste0("v", CAPACITY_ANCHOR_YEAR)
END_COL <- paste0("v", FORECAST_END)


# Step 1: Scenario set and capacity coverage ------------------------------------

cat("Step 1: scenario set and capacity coverage\n")

metrics_raw <- readr::read_csv("Parameters/IIASA-Trajectories/metrics_levels_raw.csv", show_col_types = FALSE)

# Per scenario x region x fuel: 2060/2025 extraction-per-GDP ratio (base > 0, as in 04_summary_ratio.R)
fossil_scen_ratio <- metrics_raw |>
  dplyr::filter(metric %in% FOSSIL_VARS, year %in% c(CAPACITY_ANCHOR_YEAR, FORECAST_END)) |>
  dplyr::select(model, family, ssp, marker, region, metric, year, value) |>
  tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "v") |>
  dplyr::filter(.data[[BASE_COL]] > 0, !is.na(.data[[END_COL]])) |>
  dplyr::mutate(fuel = names(FOSSIL_VARS)[match(metric, FOSSIL_VARS)], ratio = .data[[END_COL]] / .data[[BASE_COL]])

# Scenario set: runs with all three fuels at R10 level (= runs behind the fossil bounds)
scen_set <- fossil_scen_ratio |>
  dplyr::filter(region != "World") |>
  dplyr::distinct(model, family, ssp, marker, fuel) |>
  dplyr::count(model, family, ssp, marker, name = "n_fuel") |>
  dplyr::filter(n_fuel == 3L) |>
  dplyr::mutate(scen_id = paste(model, family, ssp, sep = " | ")) |>
  dplyr::select(scen_id, model, family, ssp, marker)

ssp_long <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_long_clean.csv", show_col_types = FALSE)

cap_cov <- ssp_long |>
  dplyr::filter(stringr::str_starts(variable, "Capacity\\|Electricity"), region != "World") |>
  dplyr::distinct(model, family, ssp, variable) |>
  dplyr::mutate(var = stringr::str_remove(variable, "Capacity\\|Electricity\\|"), v = 1L) |>
  dplyr::select(-variable) |>
  tidyr::pivot_wider(names_from = var, values_from = v, values_fill = 0L) |>
  dplyr::left_join(scen_set |> dplyr::transmute(model, family, ssp, in_fossil_set = TRUE), by = c("model", "family", "ssp")) |>
  dplyr::mutate(in_fossil_set = tidyr::replace_na(in_fossil_set, FALSE)) |>
  dplyr::arrange(ssp, family, model)
cat("  Capacity coverage, model x scenario x variable (R10; 1 = reported). Fossil (aggregate) is not used:\n")
print(as.data.frame(cap_cov), right = FALSE)
cat("\n  Scenario set (fossil-bound runs) per SSP:\n")
print(table(scen_set$ssp))
# Every fossil-bound run must report capacity
stopifnot(nrow(scen_set) == sum(cap_cov$in_fossil_set))


# Step 2: Annual capacity and GDP per scenario x R10 -------------------------------

cat("\nStep 2: annual capacity per scenario x R10 (missing technology = 0)\n")

cap_r10_5yr <- ssp_long |>
  dplyr::filter(variable %in% POWER_TECHS$variable, region != "World", year >= CAPACITY_ANCHOR_YEAR, year <= FORECAST_END) |>
  dplyr::inner_join(scen_set |> dplyr::select(model, family, ssp, scen_id), by = c("model", "family", "ssp")) |>
  dplyr::transmute(scen_id, ssp, r10 = region, tech = POWER_TECHS$tech[match(variable, POWER_TECHS$variable)], year, gw = value) |>
  tidyr::complete(tidyr::nesting(scen_id, ssp), r10, tech, year, fill = list(gw = 0))

gdp_r10_5yr <- ssp_long |>
  dplyr::filter(variable == "GDP|MER", region != "World", year >= CAPACITY_ANCHOR_YEAR, year <= FORECAST_END) |>
  dplyr::inner_join(scen_set |> dplyr::select(model, family, ssp, scen_id), by = c("model", "family", "ssp")) |>
  dplyr::transmute(scen_id, r10 = region, year, gdp = value)

# Linear interpolation of the 5-year steps to annual values
cap_r10 <- cap_r10_5yr |>
  dplyr::group_by(scen_id, ssp, r10, tech) |>
  dplyr::reframe(gw = stats::approx(year, gw, xout = YEARS_POWER, rule = 2)$y, year = YEARS_POWER)
gdp_r10 <- gdp_r10_5yr |>
  dplyr::group_by(scen_id, r10) |>
  dplyr::reframe(gdp = stats::approx(year, gdp, xout = YEARS_POWER, rule = 2)$y, year = YEARS_POWER)

# Report (keep) large 2025 -> 2030 world jumps -- likely calibration artifacts
jumps <- cap_r10_5yr |>
  dplyr::filter(year %in% c(CAPACITY_ANCHOR_YEAR, CAPACITY_ANCHOR_YEAR + 5L)) |>
  dplyr::group_by(scen_id, tech, year) |>
  dplyr::summarise(gw = sum(gw), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = gw, names_prefix = "gw_") |>
  dplyr::rename(gw_base = 3, gw_next = 4) |>
  dplyr::filter(gw_base > 0, gw_next / gw_base > POWER_JUMP_REPORT) |>
  dplyr::mutate(jump = gw_next / gw_base)
cat("  World 2025->2030 capacity jumps >", POWER_JUMP_REPORT, "x (kept), by technology:\n")
print(
  jumps |>
    dplyr::group_by(tech) |>
    dplyr::summarise(n_scen = dplyr::n(), max_jump = round(max(jump), 2), scen_max = scen_id[which.max(jump)], .groups = "drop") |>
    as.data.frame(),
  right = FALSE
)


# Step 3: Map R10 -> model regions; capacity factor paths --------------------------

cat("\nStep 3: R10 -> model regions, capacity factor paths\n")

# weight = region's GDP share in each R10 (ratios); split = R10's GDP share in each region (absolute GW)
r10w <- readr::read_csv("Parameters/Simulation/r10_region_weights.csv", show_col_types = FALSE) |>
  dplyr::group_by(r10) |>
  dplyr::mutate(split = gdp / sum(gdp)) |>
  dplyr::ungroup()

# GDP-weighted GW/GDP intensity per model region (weights renormalised over R10 cells present)
int_region <- cap_r10 |>
  dplyr::inner_join(gdp_r10, by = c("scen_id", "r10", "year")) |>
  dplyr::filter(gdp > 0) |>
  dplyr::inner_join(r10w |> dplyr::select(region, r10, weight), by = "r10", relationship = "many-to-many") |>
  dplyr::group_by(scen_id, ssp, region, tech, year) |>
  dplyr::summarise(intensity = sum(weight * gw / gdp) / sum(weight), .groups = "drop")

gdp_region_2025 <- gdp_r10 |>
  dplyr::filter(year == CAPACITY_ANCHOR_YEAR) |>
  dplyr::inner_join(r10w |> dplyr::select(region, r10, split), by = "r10", relationship = "many-to-many") |>
  dplyr::group_by(scen_id, region) |>
  dplyr::summarise(gdp_2025 = sum(split * gdp), .groups = "drop")

# Anchor: median 2025 GW across the scenario set per R10, split by GDP share
cap_anchor <- cap_r10 |>
  dplyr::filter(year == CAPACITY_ANCHOR_YEAR) |>
  dplyr::group_by(r10, tech) |>
  dplyr::summarise(gw_med = stats::median(gw), .groups = "drop") |>
  dplyr::inner_join(r10w |> dplyr::select(region, r10, split), by = "r10", relationship = "many-to-many") |>
  dplyr::group_by(region, tech) |>
  dplyr::summarise(cap_2025_gw = sum(split * gw_med), .groups = "drop")

# Capacity factor: ratio form where the anchor and the scenario's 2025 intensity
# are both positive and the calibration factor (anchor / scenario's own 2025 GW)
# is within [1/POWER_CALIB_MAX, POWER_CALIB_MAX]; level form (scenario GW/GDP x
# its 2025 GDP) otherwise
cap_factor <- int_region |>
  dplyr::left_join(
    int_region |> dplyr::filter(year == CAPACITY_ANCHOR_YEAR) |> dplyr::select(scen_id, region, tech, int_2025 = intensity),
    by = c("scen_id", "region", "tech")
  ) |>
  dplyr::left_join(cap_anchor, by = c("region", "tech")) |>
  dplyr::left_join(gdp_region_2025, by = c("scen_id", "region")) |>
  dplyr::mutate(
    calib = cap_2025_gw / (int_2025 * gdp_2025),
    form = dplyr::if_else(cap_2025_gw > 0 & int_2025 > 0 & calib <= POWER_CALIB_MAX & calib >= 1 / POWER_CALIB_MAX, "ratio", "level"),
    factor_gw = dplyr::if_else(form == "ratio", cap_2025_gw * intensity / int_2025, intensity * gdp_2025)
  )
stopifnot(!anyNA(cap_factor$factor_gw))

cat("  Factor form (cells scenario x region x tech):\n")
print(
  cap_factor |>
    dplyr::filter(year == CAPACITY_ANCHOR_YEAR) |>
    dplyr::count(tech, form) |>
    tidyr::pivot_wider(names_from = form, values_from = n, values_fill = 0) |>
    as.data.frame()
)
cat("\n  Largest ratio-form 2060/2025 GW-per-GDP ratios (tiny 2025 bases):\n")
print(
  cap_factor |>
    dplyr::filter(year == FORECAST_END, form == "ratio") |>
    dplyr::mutate(ratio = intensity / int_2025) |>
    dplyr::slice_max(ratio, n = 8) |>
    dplyr::select(scen_id, region, tech, ratio, cap_2025_gw) |>
    as.data.frame(),
  right = FALSE
)

# Check: world 2025 GW in the input file (scenario set) vs the model anchor
cap_check <- ssp_long |>
  dplyr::filter(variable %in% POWER_TECHS$variable, region == "World", year == CAPACITY_ANCHOR_YEAR) |>
  dplyr::inner_join(scen_set |> dplyr::select(model, family, ssp, scen_id), by = c("model", "family", "ssp")) |>
  dplyr::mutate(tech = POWER_TECHS$tech[match(variable, POWER_TECHS$variable)]) |>
  tidyr::complete(scen_id, tech, fill = list(value = 0)) |>
  dplyr::group_by(tech) |>
  dplyr::summarise(file_world_median = stats::median(value), file_world_min = min(value), file_world_max = max(value), .groups = "drop") |>
  dplyr::left_join(cap_anchor |> dplyr::group_by(tech) |> dplyr::summarise(anchor_world = sum(cap_2025_gw)), by = "tech")
cat("\n  2025 world capacity (GW): input file (scenario set) vs model anchor:\n")
print(cap_check |> dplyr::mutate(dplyr::across(-tech, ~ round(.x, 1))) |> as.data.frame())


# Step 4: Fossil energy shares and scenario fossil indices --------------------------

cat("\nStep 4: fossil energy shares (2024) and scenario fossil indices\n")

unep_dmc <- readr::read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
dmc_year <- max(unep_dmc$year[unep_dmc$year <= BASE_YEAR])
fossil_shares <- unep_dmc |>
  dplyr::filter(year == dmc_year, material_category %in% c("Coal", "Natural Gas", "Petroleum")) |>
  dplyr::transmute(
    region = Region,
    fuel = dplyr::recode(material_category, "Coal" = "coal", "Natural Gas" = "gas", "Petroleum" = "oil"),
    mt = pmax(0, DMC_Mt)
  ) |>
  tidyr::complete(region, fuel, fill = list(mt = 0)) |>
  # Mt x MJ/kg = PJ
  dplyr::mutate(energy_PJ = mt * ENERGY_DENSITY_MJ_PER_KG[fuel]) |>
  dplyr::group_by(region) |>
  dplyr::mutate(share = energy_PJ / sum(energy_PJ)) |>
  dplyr::ungroup() |>
  dplyr::group_by(region) |>
  dplyr::mutate(world_weight = sum(energy_PJ)) |>
  dplyr::ungroup() |>
  dplyr::mutate(world_weight = world_weight / sum(world_weight[fuel == "coal"])) |>
  dplyr::select(region, fuel, energy_PJ, share, world_weight)
cat("  2024 (", dmc_year, ") fossil energy shares by region:\n")
print(
  fossil_shares |>
    dplyr::select(region, fuel, share, world_weight) |>
    tidyr::pivot_wider(names_from = fuel, values_from = share) |>
    dplyr::mutate(dplyr::across(-region, ~ round(.x, 3))) |>
    as.data.frame()
)

# R10 cells replaced by the World range in the bounds -> scenario's own World ratio
world_sub_cells <- readr::read_csv("Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv", show_col_types = FALSE) |>
  dplyr::filter(Variable %in% FOSSIL_VARS, stringr::str_detect(dplyr::coalesce(Fix, ""), "world_substituted")) |>
  dplyr::transmute(fuel = names(FOSSIL_VARS)[match(Variable, FOSSIL_VARS)], region = Region, ssp = SSP, world_sub = TRUE)

scen_ratio_world <- fossil_scen_ratio |>
  dplyr::filter(region == "World") |>
  dplyr::inner_join(scen_set |> dplyr::select(model, family, ssp, scen_id), by = c("model", "family", "ssp")) |>
  dplyr::select(scen_id, fuel, ratio_world = ratio)

scen_ratio_region <- fossil_scen_ratio |>
  dplyr::filter(region != "World") |>
  dplyr::inner_join(scen_set |> dplyr::select(model, family, ssp, scen_id), by = c("model", "family", "ssp")) |>
  dplyr::left_join(world_sub_cells, by = c("fuel", "region", "ssp")) |>
  dplyr::left_join(scen_ratio_world, by = c("scen_id", "fuel")) |>
  dplyr::mutate(ratio = dplyr::if_else(dplyr::coalesce(world_sub, FALSE) & !is.na(ratio_world), ratio_world, ratio)) |>
  dplyr::select(scen_id, ssp, r10 = region, fuel, ratio) |>
  dplyr::inner_join(r10w |> dplyr::select(region, r10, weight), by = "r10", relationship = "many-to-many") |>
  dplyr::group_by(scen_id, ssp, region, fuel) |>
  dplyr::summarise(ratio = sum(weight * ratio) / sum(weight), .groups = "drop") |>
  # Region with no R10 cell for a fuel (zero 2025 base everywhere) -> scenario World ratio
  tidyr::complete(tidyr::nesting(scen_id, ssp), region, fuel) |>
  dplyr::left_join(scen_ratio_world, by = c("scen_id", "fuel")) |>
  dplyr::mutate(ratio = dplyr::coalesce(ratio, ratio_world))
stopifnot(!anyNA(scen_ratio_region$ratio))

scen_index <- scen_ratio_region |>
  dplyr::inner_join(fossil_shares |> dplyr::select(region, fuel, share), by = c("region", "fuel")) |>
  dplyr::group_by(scen_id, ssp, region) |>
  dplyr::summarise(fossil_index = sum(share * ratio), .groups = "drop")

scen_index_world <- scen_index |>
  dplyr::inner_join(fossil_shares |> dplyr::distinct(region, world_weight), by = "region") |>
  dplyr::group_by(scen_id) |>
  dplyr::summarise(fossil_index_world = sum(world_weight * fossil_index), .groups = "drop")

# Consistency: run index range implied by the flow bounds vs scenario index range
flow_ratio_bounds <- readr::read_csv("Parameters/Simulation/flow_ratio_bounds.csv", show_col_types = FALSE)
run_index_range <- flow_ratio_bounds |>
  dplyr::filter(material_group == "fossil_fuels") |>
  dplyr::inner_join(fossil_shares |> dplyr::select(region, mat_key = fuel, share), by = c("region", "mat_key")) |>
  dplyr::group_by(region, ssp) |>
  dplyr::summarise(run_min = sum(share * ratio_min), run_max = sum(share * ratio_max), .groups = "drop")
cat("\n  Fossil index: run range from bounds (independent u per fuel) vs scenario range:\n")
print(
  scen_index |>
    dplyr::group_by(region, ssp) |>
    dplyr::summarise(n_scen = dplyr::n(), scen_min = min(fossil_index), scen_max = max(fossil_index), .groups = "drop") |>
    dplyr::inner_join(run_index_range, by = c("region", "ssp")) |>
    dplyr::mutate(dplyr::across(c(scen_min, scen_max, run_min, run_max), ~ round(.x, 2))) |>
    as.data.frame()
)


# Step 5: Scenario 2060 warming -----------------------------------------------------

cat("\nStep 5: scenario", FORECAST_END, "median warming\n")

scen_temp <- readr::read_csv(CLIMATE_FILE, col_types = readr::cols(.default = "c")) |>
  dplyr::distinct() |>
  dplyr::filter(variable == TEMP_MEDIAN_VAR) |>
  dplyr::mutate(
    scenario_clean = stringr::str_squish(stringr::str_remove(scenario, stringr::fixed("(Marker)"))),
    ssp = stringr::str_extract(scenario_clean, "SSP[1-5]"),
    family = stringr::str_squish(stringr::str_remove(scenario_clean, "-\\s*SSP[1-5]$")),
    t_2060 = as.numeric(.data[[as.character(FORECAST_END)]])
  ) |>
  dplyr::distinct(model, family, ssp, t_2060)

power_scenarios <- scen_set |>
  dplyr::left_join(scen_index_world, by = "scen_id") |>
  dplyr::left_join(scen_temp, by = c("model", "family", "ssp"))
cat("  Scenarios with / without warming data, per SSP:\n")
print(table(power_scenarios$ssp, ifelse(is.na(power_scenarios$t_2060), "no_temp", "temp")))


# Step 6: Material intensity per GW (Wang 2023) and per GWh (batteries) -------------

cat("\nStep 6: material intensities\n")

wang <- readxl::read_excel(WANG_FILE, sheet = WANG_SHEET) |>
  dplyr::transmute(wang_tech = Technology, material = stringr::str_trim(Material), t_per_gw = as.numeric(`Intensity value (tons/GW)`)) |>
  dplyr::filter(wang_tech %in% POWER_TECHS$wang_tech, material %in% names(WANG_MATERIAL_GROUP), !is.na(t_per_gw))

cat("  Wang values per technology x material (n, averaged):\n")
print(as.data.frame(wang |> dplyr::count(wang_tech, material) |> tidyr::pivot_wider(names_from = material, values_from = n, values_fill = 0)))

wang_mean <- wang |>
  dplyr::group_by(wang_tech, material) |>
  dplyr::summarise(t_per_gw = mean(t_per_gw), .groups = "drop")
# Aggregates = AGGREGATE_PER_CEMENT x cement (concrete; no raw-material conversion of cement itself)
wang_mean <- dplyr::bind_rows(
  wang_mean,
  wang_mean |> dplyr::filter(material == "Cement") |> dplyr::mutate(material = "Aggregates", t_per_gw = AGGREGATE_PER_CEMENT * t_per_gw)
)
cat("\n  Mean t/GW:\n")
print(as.data.frame(wang_mean |> dplyr::mutate(t_per_gw = round(t_per_gw)) |> tidyr::pivot_wider(names_from = material, values_from = t_per_gw, values_fill = 0)))

mi_gen <- POWER_TECHS |>
  dplyr::select(tech, wang_tech) |>
  dplyr::inner_join(wang_mean, by = "wang_tech", relationship = "many-to-many") |>
  dplyr::mutate(material_group = unname(WANG_MATERIAL_GROUP[material])) |>
  dplyr::group_by(unit_key = tech, material_group) |>
  dplyr::summarise(t_per_unit = sum(t_per_gw), .groups = "drop") |>
  dplyr::mutate(unit = "t/GW")
stopifnot(setequal(unique(mi_gen$unit_key), POWER_TECHS$tech))

battery_raw <- readxl::read_excel(BATTERY_FILE, sheet = BATTERY_SHEET)
stopifnot(all(BATTERY_CHEMISTRIES %in% battery_raw$`Cathode chemistry`))
# kg/kWh x 1e6 kWh/GWh / 1e3 kg/t = t/GWh x 1e3
mi_batt <- battery_raw |>
  dplyr::filter(`Cathode chemistry` %in% BATTERY_CHEMISTRIES) |>
  dplyr::select(`Cathode chemistry`, dplyr::all_of(names(BATTERY_MATERIAL_GROUP))) |>
  tidyr::pivot_longer(-`Cathode chemistry`, names_to = "material", values_to = "kg_per_kwh") |>
  dplyr::group_by(material) |>
  dplyr::summarise(kg_per_kwh = mean(as.numeric(kg_per_kwh)), .groups = "drop") |>
  dplyr::mutate(material_group = unname(BATTERY_MATERIAL_GROUP[material])) |>
  dplyr::group_by(material_group) |>
  dplyr::summarise(t_per_unit = sum(kg_per_kwh) * 1e3, .groups = "drop") |>
  dplyr::mutate(unit_key = "battery", unit = "t/GWh")

power_mi <- dplyr::bind_rows(mi_gen, mi_batt) |>
  tidyr::complete(unit_key, material_group, fill = list(t_per_unit = 0)) |>
  dplyr::mutate(unit = dplyr::if_else(unit_key == "battery", "t/GWh", "t/GW"))
cat("\n  Material intensity by model material group:\n")
print(as.data.frame(power_mi |> dplyr::mutate(t_per_unit = round(t_per_unit)) |> tidyr::pivot_wider(names_from = material_group, values_from = t_per_unit)))


# Step 7: 2025 power-sector stock and carve-out from civil engineering --------------

cat("\nStep 7: 2025 power-sector stock and carve-out\n")

anchor_units <- dplyr::bind_rows(
  cap_anchor |> dplyr::rename(unit_key = tech, units = cap_2025_gw),
  cap_anchor |>
    dplyr::filter(tech %in% BATTERY_SOURCE_TECHS) |>
    dplyr::group_by(region) |>
    dplyr::summarise(units = BATTERY_GWH_PER_GW * sum(cap_2025_gw), .groups = "drop") |>
    dplyr::mutate(unit_key = "battery")
)

MATERIAL_LABEL <- c("metal_fe" = "Metal_Fe", "metal_nonfe" = "Metal_NonFe", "nonmetallic_minerals" = "Non-metallic minerals")
power_stock_2025 <- anchor_units |>
  dplyr::inner_join(power_mi, by = "unit_key", relationship = "many-to-many") |>
  dplyr::group_by(region, material_group) |>
  # GW (GWh) x t/GW (t/GWh) / 1e6 = Mt
  dplyr::summarise(carve_Mt = sum(units * t_per_unit) / 1e6, .groups = "drop") |>
  dplyr::mutate(material = unname(MATERIAL_LABEL[material_group]))

stock_base <- readr::read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |>
  dplyr::distinct() |>
  dplyr::filter(sub_use == POWER_CARVEOUT_SUB_USE) |>
  dplyr::select(material, region = Region, sub_use, stock_Mt)

power_carveout <- power_stock_2025 |>
  dplyr::inner_join(stock_base, by = c("material", "region")) |>
  dplyr::mutate(remaining_Mt = stock_Mt - carve_Mt, carve_share = carve_Mt / stock_Mt) |>
  dplyr::select(material, region, sub_use, carve_Mt, stock_Mt, remaining_Mt, carve_share)
stopifnot(nrow(power_carveout) == nrow(power_stock_2025))

cat("  2025 power-sector stock vs", POWER_CARVEOUT_SUB_USE, "2024 stock:\n")
print(power_carveout |> dplyr::mutate(dplyr::across(c(carve_Mt, stock_Mt, remaining_Mt), ~ round(.x, 1)), carve_share = round(carve_share, 4)) |> as.data.frame())
if (any(power_carveout$remaining_Mt < 0)) {
  stop("Carve-out makes ", POWER_CARVEOUT_SUB_USE, " stock negative -- see table above")
}


# Step 8: Save ------------------------------------------------------------------

readr::write_csv(power_scenarios, "Parameters/Simulation/power_scenarios.csv")
readr::write_csv(scen_index, "Parameters/Simulation/power_scenario_index.csv")
readr::write_csv(fossil_shares, "Parameters/Simulation/power_fossil_shares.csv")
# Scenario 2025 ("today") = model anchor year (BASE_YEAR, DSM start): scenario
# year y drives model year y - (CAPACITY_ANCHOR_YEAR - BASE_YEAR), so the first
# model year does not get a year of GDP growth with a frozen capacity ratio;
# the last scenario year is held for FORECAST_END
cap_factor_model <- cap_factor |>
  dplyr::mutate(year = year - (CAPACITY_ANCHOR_YEAR - BASE_YEAR)) |>
  dplyr::select(scen_id, ssp, region, tech, year, factor_gw, form)
cap_factor_model <- dplyr::bind_rows(
  cap_factor_model,
  cap_factor_model |> dplyr::filter(year == max(year)) |> dplyr::mutate(year = FORECAST_END)
) |>
  dplyr::distinct(scen_id, region, tech, year, .keep_all = TRUE)
readr::write_csv(cap_factor_model, "Parameters/Simulation/power_capacity_factor.csv")
readr::write_csv(cap_anchor, "Parameters/Simulation/power_capacity_anchor.csv")
readr::write_csv(cap_check, "Parameters/Simulation/power_capacity_check_2025.csv")
readr::write_csv(power_mi, "Parameters/Simulation/power_material_intensity.csv")
readr::write_csv(power_carveout, "Parameters/Simulation/power_carveout_2025.csv")
cat("\nSaved power_*.csv to Parameters/Simulation/\n")

# EoF
