## =============================================================================
## 02-RunSimulations.R
## Monte Carlo runner. Reads LHS draws from 01, reads Excel bounds + static
## data, rebuilds all trajectories upfront, runs N_RUNS simulations in parallel.
##
## Flow model (Kaya):  biomass + fossil fuels
## Stock model (DSM):  Metal_Fe + Metal_NonFe + nonmetallic_minerals
##                     8 sub_uses; intensity sampled at super_category level (4),
##                     applied proportionally across sub_uses.
##
## UNITS NOTE (important):
##   metals  -> base *_Mt columns (primary_consumption_Mt, secondary_supply_Mt,
##              new_additions_Mt, replacement_Mt, waste_Mt) are ORE-equivalent
##              (metal / grade); the matching *_Mt_pure columns are METAL mass.
##              Both forms are mass-consistent (base = pure / grade).
##   non-metals/biomass/fossil -> carrier == reported mass (no separate ore
##              stage, grade == 1), so *_Mt_pure mirrors the base columns.
##
## Output: Results/MC/mc_results.parquet
##   key: run_id, region, material_group, material_key, year
##   value: total_inflow_Mt, in_use_stock_Mt,
##          primary_consumption_Mt, secondary_supply_Mt,
##          new_additions_Mt, replacement_Mt, waste_Mt          (ore-equiv for metals)
##          primary_consumption_Mt_pure, secondary_supply_Mt_pure,
##          new_additions_Mt_pure, replacement_Mt_pure, waste_Mt_pure (metal mass)
##          secondary_spilled_Mt        (recovered surplus: pool beyond what demand can absorb)
##          not_recovered_Mt, not_recovered_Mt_pureMetal (waste not recovered)
##            mass balance per region x year x material:
##            waste = not_recovered + secondary + secondary_spilled (allocate_eol)
##          run_neg_primary, run_total_spill_Mt  (per-run diagnostic flags)
##          ssp_lo, ssp_hi, ssp_share_lo
##            (continuous SSP blend used for this run's population, GDP-percap
##             and flow-intensity bounds; no discrete ssp_label -- see STEP 4)
##
## Intensity endpoints are region-specific: endpoint = int_2024 * ratio, with
## ratio = min + u * (max - min) from each region's own bounds (01-Sampling.R),
## u shared by all regions; reached at target_year (smoothstep, log-linear).
## Target-stock growth blends from the historical 2024 rate into the Kaya rate
## over STOCK_GROWTH_BLEND_YRS (00-Parameters.R) to avoid a seam jump in flows.
##
## Power sector (inputs from 01b-PowerSector.R; no new sampled parameters):
##   the run's coal/gas/oil draws -> fossil index -> bracketing ScenarioMIP runs
##   within each of its two SSPs -> blended capacity path (GW) per region x tech;
##   batteries = BATTERY_GWH_PER_GW x (PV + wind) GW. One DSM per technology
##   (fixed lifetimes), materials = units x fixed t/unit, reported as 4 extra
##   end uses ("Power: ...") of metal_fe / metal_nonfe / nonmetallic_minerals,
##   pooled with the other end uses for recycling / downcycling. The 2025
##   power-sector stock is carved out of the civil-engineering base stock.
##   Extra outputs (Results/MC/): mc_power_capacity.csv, mc_power_materials.csv,
##   mc_power_run_link.csv (run fossil index + 2060 warming), mc_power_run_brackets.csv
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
source("Scripts/00-Functions/dsm_functions.R", encoding = "UTF-8")

library(furrr)
library(arrow)
library(future)

cat("=== MC Runner | seed:", GLOBAL_SEED, "===\n\n")

# -- Label maps ---------------------------------------------------------------
BIOMASS_LABEL <- c(
  "crops" = "Crops",
  "grazed_biomass" = "Grazed biomass and fodder crops",
  "other_biomass" = "Other biomass",
  "wood" = "Wood"
)
FOSSIL_LABEL <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum", "other_fossil" = "Other fossil fuels")

# sub_use raw key -> display label
SUB_USE_LABELS <- c(
  "residential" = "Residential",
  "non_residential" = "Non-residential",
  "roads" = "Roads",
  "civil_engineering" = "Civil engineering",
  "machinery_group" = "Machinery",
  "vehicles_group" = "Vehicles",
  "durables" = "Durables",
  "packaging" = "Packaging",
  setNames(POWER_LIFE_CLASSES$label, POWER_LIFE_CLASSES$sub_use) # "Power: ..." end uses
)

# mat_key (Excel super_category) -> sub_uses belonging to it
SUB_USE_BY_MATKEY <- list(
  "buildings" = c("residential", "non_residential"),
  "civil" = c("roads", "civil_engineering"),
  "sl_products" = c("durables", "packaging"),
  "machinery" = c("machinery_group", "vehicles_group")
)


# =============================================================================
# STEP 1: Load LHS draws
# =============================================================================
cat("STEP 1: Load LHS draws\n")

mc_input_matrix <- readr::read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
N_RUNS <- nrow(mc_input_matrix)
cat("  Runs:", N_RUNS, "| Cols:", ncol(mc_input_matrix), "\n")


# =============================================================================
# STEP 2: Read bounds from Excel
# =============================================================================
cat("\nSTEP 2: Read bounds\n")

BIOMASS_KEYS <- c(
  "Crops" = "crops",
  "Grazed biomass and fodder crops" = "grazed_biomass",
  "Wood" = "wood",
  "Other biomass" = "other_biomass"
)

# Reads region x material 2024 intensity from an Excel sheet, auto-detecting
# the region column, the key column, and the "2024" baseline column.
read_intensity_sheet <- function(file, sheet, skip_rows = 0) {
  raw <- readxl::read_excel(file, sheet = sheet, skip = skip_rows, col_types = "text")
  base_col <- names(raw)[stringr::str_detect(names(raw), "2024")][1]
  char_cols <- names(raw)[purrr::map_lgl(raw, is.character)]
  region_col <- char_cols[stringr::str_detect(tolower(char_cols), "region")][1]
  key_col <- setdiff(char_cols, region_col)[1]
  raw |>
    dplyr::select(region = all_of(region_col), material_key = all_of(key_col), int_2024 = all_of(base_col)) |>
    mutate(int_2024 = as.numeric(int_2024)) |>
    filter(!is.na(region), !is.na(material_key), !is.na(int_2024))
}

# Maps the four free-text end-use rows to the canonical super_category key.
classify_super <- function(x) {
  dplyr::case_when(
    str_detect(tolower(x), "build") ~ "buildings",
    str_detect(tolower(x), "civil") ~ "civil",
    str_detect(tolower(x), "short") ~ "sl_products",
    str_detect(tolower(x), "machin") ~ "machinery",
    TRUE ~ NA_character_
  )
}

biomass_bounds <- read_intensity_sheet(ASSUMPTIONS_FILE, "Biomass") |>
  filter(material_key %in% names(BIOMASS_KEYS)) |>
  mutate(mat_key = BIOMASS_KEYS[material_key]) |>
  distinct(region, mat_key, .keep_all = TRUE) |>
  dplyr::select(region, mat_key, int_2024)

fossil_bounds <- read_intensity_sheet(ASSUMPTIONS_FILE, "FossilFuels") |>
  mutate(mat_key = ifelse(material_key == "Other fossil fuels", "other_fossil", material_key)) |>
  distinct(region, mat_key, .keep_all = TRUE) |>
  dplyr::select(region, mat_key, int_2024)

metal_bounds <- read_intensity_sheet(ASSUMPTIONS_FILE, "MetalOres", skip_rows = 1) |>
  mutate(mat_key = classify_super(material_key)) |>
  filter(!is.na(mat_key)) |>
  dplyr::select(region, mat_key, int_2024)

nonmet_bounds <- read_intensity_sheet(ASSUMPTIONS_FILE, "NonMetallicMinerals", skip_rows = 1) |>
  mutate(mat_key = classify_super(material_key)) |>
  filter(!is.na(mat_key)) |>
  dplyr::select(region, mat_key, int_2024)

# Recycling rates (collection/EOL fraction) -- separate Fe and NonFe
recycling_raw <- readxl::read_excel(RECYCLING_FILE, sheet = "Recycling_EOL") |>
  dplyr::filter(!stringr::str_detect(Region, "—"))
recycling_now_fe <- recycling_raw |> dplyr::select(region = Region, rate_now = Recycling_rate_Fe)
recycling_now_nonfe <- recycling_raw |> dplyr::select(region = Region, rate_now = Recycling_rate_NonFe)

# Downcycling (non-metallic minerals): one 2024 anchor per region, shared by the
# four giving/receiving sectors (residential, non-residential, civil engineering, roads)
downcycling_now <- readxl::read_excel(RECYCLING_FILE, sheet = "Downcycling") |>
  dplyr::select(region = Region, rate_now = `Downcycling rate`) |>
  dplyr::filter(!stringr::str_detect(region, "—"))

# Region-specific intensity ratio bounds (endpoint / 2024), built in 01-Sampling.R:
#   flows  -> per region x SSP (ScenarioMIP R10 bounds, GDP-weighted to model regions)
#   stocks -> per region, SSP-independent (Stock_Bounds sheet, End_use via classify_super)
flow_ratio_bounds <- readr::read_csv("Parameters/Simulation/flow_ratio_bounds.csv", show_col_types = FALSE)
stock_ratio_bounds <- readr::read_csv("Parameters/Simulation/stock_ratio_bounds.csv", show_col_types = FALSE)


# =============================================================================
# STEP 3: Static data (SSP drivers, GDP, UNEP anchors, age profiles)
# =============================================================================
cat("\nSTEP 3: Load static data\n")

DSM_START <- 2025L
YEARS_DSM <- seq(DSM_START, FORECAST_END)
N_YR <- length(YEARS_DSM)

ssp_drivers <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)

pop_idx <- ssp_drivers |>
  filter(variable == "Population", year >= DSM_START, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, pop_index = index)
gdp_percap_idx <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= DSM_START, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_percap_index = index)

gdp_base_vals <- readr::read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |>
  filter(year == 2024L) |>
  dplyr::select(region = Region, GDP_2015USD)

# NOTE: gdp_full is consumed only to derive regions_vec / ssp_labels below.
# Kept minimal; drop entirely if those vectors are sourced elsewhere.
gdp_full <- ssp_drivers |>
  filter(variable == "GDP|PPP", year >= 2024L, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_index = index) |>
  left_join(gdp_base_vals, by = "region") |>
  mutate(gdp_billion_usd = GDP_2015USD * gdp_index / 1e9)

unep_dmc <- readr::read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)

# Collapse minor UNEP categories into "Other *" buckets; keep Crop Residues split out.
dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group) |>
  rename(material_category = Material_22)
unep_dmc <- unep_dmc |>
  left_join(dict_mat) |>
  mutate(
    material_category = case_when(
      Material_group == "Biomass" &
        !(material_category %in% c(unname(BIOMASS_LABEL), "Crop Residues")) ~ "Other biomass",
      Material_group == "Fossil fuels" & !(material_category %in% unname(FOSSIL_LABEL)) ~ "Other fossil fuels",
      T ~ material_category
    )
  ) %>%
  group_by(Region, year, Material_group, material_category) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# 2024 mass anchors for the Kaya flow materials
anchor_year <- max(unep_dmc$year[unep_dmc$year <= 2024L])
m_2024_biomass <- unep_dmc |>
  filter(year == anchor_year, material_category %in% c(unname(BIOMASS_LABEL), "Crop Residues")) |>
  dplyr::select(region = Region, unep_label = material_category, M_2024_Mt = DMC_Mt)
m_2024_fossil <- unep_dmc |>
  filter(year == anchor_year, material_category %in% unname(FOSSIL_LABEL)) |>
  mutate(mat_key = names(FOSSIL_LABEL)[match(material_category, FOSSIL_LABEL)]) |>
  dplyr::select(region = Region, mat_key, M_2024_Mt = DMC_Mt)

# 2024 stock baseline -> nested lookup [material][region][sub_use] = stock_Mt
stock_2024_raw <- readr::read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |> rename(region = Region)

# Power-sector carve-out (01b-PowerSector.R): the 2025 generation + storage stock
# is removed from the civil-engineering base stock before the stock-per-GDP rule;
# its age profile is scaled by the same factor below (the DSM starts from cohorts)
power_carveout <- readr::read_csv("Parameters/Simulation/power_carveout_2025.csv", show_col_types = FALSE)
stopifnot(all(power_carveout$remaining_Mt >= 0))
carve_scale <- power_carveout |> transmute(material, region, sub_use, carve_scale = remaining_Mt / stock_Mt)
stock_2024_raw <- stock_2024_raw |>
  left_join(power_carveout |> dplyr::select(material, region, sub_use, carve_Mt), by = c("material", "region", "sub_use")) |>
  mutate(stock_Mt = stock_Mt - dplyr::coalesce(carve_Mt, 0)) |>
  dplyr::select(-carve_Mt)

stock_2024_sub <- stock_2024_raw |>
  dplyr::select(material, region, sub_use, stock_Mt) |>
  split(~material) |>
  lapply(function(md) split(md, ~region) |> lapply(function(rd) setNames(rd$stock_Mt, rd$sub_use)))

# Historical 2024 stock log-growth, log(S_2024 / S_2023), from the historical DSM
# -> nested lookup [material][region][sub_use]; anchors the seam blend (STEP 5)
stock_growth_hist <- readr::read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE) |>
  filter(year %in% c(2023L, 2024L)) |>
  dplyr::select(material, region = Region, sub_use, year, stock_Mt) |>
  tidyr::pivot_wider(names_from = year, values_from = stock_Mt, names_prefix = "S_") |>
  mutate(g_hist = log(S_2024 / S_2023)) |>
  filter(is.finite(g_hist))
stock_growth_hist_sub <- stock_growth_hist |>
  split(~material) |>
  lapply(function(md) split(md, ~region) |> lapply(function(rd) setNames(rd$g_hist, rd$sub_use)))

# Weight on model (Kaya) stock growth per YEARS_DSM: linear over STOCK_GROWTH_BLEND_YRS
w_growth_model <- pmin(1, (YEARS_DSM - 2024L) / (STOCK_GROWTH_BLEND_YRS + 1L))

# Age profiles: annualize 5-year cohort bins by spreading each bin evenly over
# its preceding `gap` years -> removes the staircase artifact in survival curves.
age_profile_raw <- readr::read_csv("Parameters/MISO-Stock/stock_2024_age_profile.csv", show_col_types = FALSE) |>
  rename(region = Region) |>
  group_by(material, region, sub_use, cohort_year) |>
  summarise(surviving_stock_Mt = sum(surviving_stock_Mt), .groups = "drop")

age_profile_raw <- age_profile_raw |>
  filter(cohort_year >= 1884L) |>
  group_by(material, region, sub_use) |>
  arrange(cohort_year, .by_group = TRUE) |>
  mutate(gap = c(5L, diff(cohort_year))) |>
  ungroup() |>
  mutate(years = purrr::map2(cohort_year, gap, ~ seq.int(.x - .y + 1L, .x)), stock_per_yr = surviving_stock_Mt / gap) |>
  tidyr::unnest(years) |>
  transmute(material, region, sub_use, cohort_year = years, surviving_stock_Mt = stock_per_yr)

# Carve-out applied to the civil-engineering cohorts (same factor as the base stock)
age_profile_raw <- age_profile_raw |>
  left_join(carve_scale, by = c("material", "region", "sub_use")) |>
  mutate(surviving_stock_Mt = surviving_stock_Mt * dplyr::coalesce(carve_scale, 1)) |>
  dplyr::select(-carve_scale)

# Power-sector 2025 age profile: cohort shape of the donor sub_use with the
# closest lifetime (POWER_LIFE_CLASSES), all materials pooled, as shares
# -> lookup [region][donor_sub_use] = list(cohort_years, shares)
power_age_lookup <- age_profile_raw |>
  filter(sub_use %in% POWER_LIFE_CLASSES$donor_sub_use) |>
  group_by(region, sub_use, cohort_year) |>
  summarise(s = sum(surviving_stock_Mt), .groups = "drop") |>
  group_by(region, sub_use) |>
  mutate(share = s / sum(s)) |>
  ungroup() |>
  arrange(cohort_year) |>
  split(~region) |>
  lapply(function(rd) split(rd, ~sub_use) |> lapply(function(sd) list(cohort_years = as.integer(sd$cohort_year), shares = sd$share)))

# Lookup: list[region][sub_use] = list(cohort_years, cohort_stocks)
build_age_lookup <- function(material_label) {
  df <- age_profile_raw |> filter(material == material_label)
  out <- list()
  for (rg in unique(df$region)) {
    out[[rg]] <- list()
    for (su in unique(df$sub_use[df$region == rg])) {
      sub <- df |> filter(region == rg, sub_use == su) |> arrange(cohort_year)
      out[[rg]][[su]] <- list(
        cohort_years = as.integer(sub$cohort_year),
        cohort_stocks = as.numeric(sub$surviving_stock_Mt)
      )
    }
  }
  out
}
age_lookup_fe <- build_age_lookup("Metal_Fe")
age_lookup_nonfe <- build_age_lookup("Metal_NonFe")
age_lookup_nonmet <- build_age_lookup("Non-metallic minerals")

# Dense [region x year] matrices per SSP -> avoids repeated joins in the hot loop.
regions_vec <- sort(unique(gdp_full$region))
ssp_labels <- sort(unique(gdp_full$scenario))

make_mat <- function(df, val_col) {
  out <- list()
  for (s in ssp_labels) {
    sub <- df |> filter(scenario == s)
    m <- matrix(
      NA_real_,
      nrow = length(regions_vec),
      ncol = N_YR,
      dimnames = list(regions_vec, as.character(YEARS_DSM))
    )
    for (r in regions_vec) {
      for (yi in seq_along(YEARS_DSM)) {
        v <- sub[[val_col]][sub$region == r & sub$year == YEARS_DSM[yi]]
        if (length(v) > 0) m[r, yi] <- v[1]
      }
    }
    out[[s]] <- m
  }
  out
}
pop_mat <- make_mat(pop_idx, "pop_index")
gdppc_mat <- make_mat(gdp_percap_idx, "gdp_percap_index")

# Power sector: scenario capacity factor paths [scenario, region, tech, year],
# 2025 anchor GW [region, tech], material intensity [unit, material_group] (t/GW, t/GWh)
power_factor <- readr::read_csv("Parameters/Simulation/power_capacity_factor.csv", show_col_types = FALSE) |>
  filter(year %in% YEARS_DSM)
stopifnot(all(power_factor$region %in% regions_vec))
power_scen_ids <- sort(unique(power_factor$scen_id))
POWER_TECH_KEYS <- POWER_TECHS$tech
power_factor_arr <- array(
  0,
  dim = c(length(power_scen_ids), length(regions_vec), length(POWER_TECH_KEYS), N_YR),
  dimnames = list(power_scen_ids, regions_vec, POWER_TECH_KEYS, as.character(YEARS_DSM))
)
power_factor_arr[cbind(
  match(power_factor$scen_id, power_scen_ids),
  match(power_factor$region, regions_vec),
  match(power_factor$tech, POWER_TECH_KEYS),
  match(power_factor$year, YEARS_DSM)
)] <- power_factor$factor_gw

power_anchor <- readr::read_csv("Parameters/Simulation/power_capacity_anchor.csv", show_col_types = FALSE)
power_anchor_mat <- matrix(0, length(regions_vec), length(POWER_TECH_KEYS), dimnames = list(regions_vec, POWER_TECH_KEYS))
power_anchor_mat[cbind(match(power_anchor$region, regions_vec), match(power_anchor$tech, POWER_TECH_KEYS))] <- power_anchor$cap_2025_gw

power_mi <- readr::read_csv("Parameters/Simulation/power_material_intensity.csv", show_col_types = FALSE)
power_mi_mat <- tapply(power_mi$t_per_unit, list(power_mi$unit_key, power_mi$material_group), sum)
power_mi_mat[is.na(power_mi_mat)] <- 0

# Unit (technology or battery) -> lifetime class (= power end use)
POWER_UNIT_CLASS <- c(setNames(POWER_TECHS$sub_use, POWER_TECHS$tech), "battery" = "power_battery")

# World GDP-per-capita growth (2024 -> FORECAST_END) ranking of the sampled SSPs
# (SSP_SAMPLED; SSP4 excluded) -- used only to locate the two bracketing SSPs
# for the continuous ssp_u draw below; the region-level blend itself uses
# pop_mat/gdppc_mat above.
world_agg <- ssp_drivers |>
  filter(variable %in% c("Population", "GDP|PPP"), year %in% c(2024L, FORECAST_END), scenario %in% SSP_SAMPLED) |>
  dplyr::select(scenario, region, variable, year, value) |>
  tidyr::pivot_wider(names_from = variable, values_from = value) |>
  dplyr::group_by(scenario, year) |>
  dplyr::summarise(pop_world = sum(Population, na.rm = TRUE), gdp_world = sum(`GDP|PPP`, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(gdppc_world = gdp_world / pop_world)

world_2024 <- world_agg |> dplyr::filter(year == 2024L) |> dplyr::select(scenario, pop_base = pop_world, gdppc_base = gdppc_world)
world_end <- world_agg |>
  dplyr::filter(year == FORECAST_END) |>
  dplyr::select(scenario, pop_end = pop_world, gdppc_end = gdppc_world)

ssp_rank_gdp <- world_end |>
  dplyr::left_join(world_2024, by = "scenario") |>
  dplyr::mutate(val = gdppc_end / gdppc_base) |>
  dplyr::arrange(val) |>
  dplyr::select(scenario, val)

cat("  Static data loaded.\n")


# =============================================================================
# STEP 4: Rebuild per-run derived quantities (UPFRONT, vectorized over all runs)
# =============================================================================
cat("\nSTEP 4: Rebuild trajectories from LHS draws\n")

# -- Continuous SSP position (one draw -> population, GDP/capita, flow bounds) --
# Equal-coverage mapping of ssp_u onto the N_SSP sampled SSPs ranked by world
# GDP/capita growth (ssp_rank_gdp, STEP 3): p = clamp(N*u - 0.5, 0, N-1),
# k = floor(p), share_hi = p - k; blend the SSPs at rank k and k+1 (0-based).
# Each SSP gets an equal 1/N share of u (half of it pure-ish at the two ends).
N_SSP <- nrow(ssp_rank_gdp)
ssp_p <- pmin(N_SSP - 1, pmax(0, N_SSP * mc_input_matrix$ssp_u - 0.5))
ssp_k <- pmin(N_SSP - 2, floor(ssp_p)) # p = N-1 -> k = N-2 with share_hi = 1 (same point)
ssp_bracket <- tibble::tibble(
  run_id = mc_input_matrix$run_id,
  ssp_lo = ssp_rank_gdp$scenario[ssp_k + 1],
  ssp_hi = ssp_rank_gdp$scenario[ssp_k + 2],
  ssp_share_lo = 1 - (ssp_p - ssp_k)
)
ssp_lo_vec <- ssp_bracket$ssp_lo
ssp_hi_vec <- ssp_bracket$ssp_hi
ssp_share_lo_vec <- ssp_bracket$ssp_share_lo

cat("  SSP rank (world GDP/capita growth):", paste(ssp_rank_gdp$scenario, collapse = " < "), "\n")

# Collect the single global [0,1] draw per material into long format
collect_u <- function(mc, prefix, suffix, mat_keys) {
  cols <- paste0(prefix, mat_keys, suffix)
  mc |>
    dplyr::select(run_id, all_of(cols)) |>
    tidyr::pivot_longer(-run_id, names_to = "col", values_to = "u_global") |>
    mutate(mat_key = stringr::str_remove(stringr::str_remove(col, paste0("^", prefix)), paste0(suffix, "$"))) |>
    dplyr::select(run_id, mat_key, u_global)
}

# Endpoint intensity:  endpoint = int_2024 * ratio, ratio = min + u * (max - min)
#   u         = one draw per material, shared by all regions for the run
#   [min,max] = the region's OWN ratio bounds (endpoint / 2024)
#   flows (ratio_bounds has an `ssp` column): ratio computed with the same u
#   under both bracketing SSPs' bounds, blended linearly by ssp_share_lo
build_endpoints <- function(bounds, ratio_bounds, mc, prefix, suffix, fixed_keys = character(0)) {
  mat_keys <- setdiff(sort(unique(bounds$mat_key)), fixed_keys)
  if (length(mat_keys) == 0) {
    return(NULL)
  }

  u_global <- collect_u(mc, prefix, suffix, mat_keys)

  if ("ssp" %in% names(ratio_bounds)) {
    ratio_df <- u_global |>
      left_join(ssp_bracket, by = "run_id") |>
      left_join(
        ratio_bounds |> dplyr::select(region, mat_key, ssp_lo = ssp, min_lo = ratio_min, max_lo = ratio_max),
        by = c("mat_key", "ssp_lo"),
        relationship = "many-to-many"
      ) |>
      left_join(
        ratio_bounds |> dplyr::select(region, mat_key, ssp_hi = ssp, min_hi = ratio_min, max_hi = ratio_max),
        by = c("region", "mat_key", "ssp_hi")
      ) |>
      mutate(
        ratio = ssp_share_lo * (min_lo + u_global * (max_lo - min_lo)) +
          (1 - ssp_share_lo) * (min_hi + u_global * (max_hi - min_hi))
      )
  } else {
    ratio_df <- u_global |>
      left_join(
        ratio_bounds |> dplyr::select(region, mat_key, ratio_min, ratio_max),
        by = "mat_key",
        relationship = "many-to-many"
      ) |>
      mutate(ratio = ratio_min + u_global * (ratio_max - ratio_min))
  }

  sampled <- bounds |>
    filter(mat_key %in% mat_keys) |>
    left_join(ratio_df |> dplyr::select(run_id, region, mat_key, ratio), by = c("region", "mat_key")) |>
    mutate(endpoint = int_2024 * ratio) |>
    dplyr::select(run_id, region, mat_key, int_2024, endpoint)
  if (anyNA(sampled$endpoint)) {
    stop("Missing ratio bounds for: ", paste(unique(paste(sampled$region, sampled$mat_key)[is.na(sampled$endpoint)]), collapse = ", "))
  }

  # Fixed materials: endpoint frozen at 2024 value for every run
  if (length(fixed_keys) > 0) {
    fixed <- bounds |>
      filter(mat_key %in% fixed_keys) |>
      tidyr::crossing(run_id = seq_len(nrow(mc))) |>
      mutate(endpoint = int_2024) |>
      dplyr::select(run_id, region, mat_key, int_2024, endpoint)
    sampled <- bind_rows(sampled, fixed)
  }
  sampled
}

ep_biomass <- build_endpoints(
  biomass_bounds,
  flow_ratio_bounds |> filter(material_group == "biomass"),
  mc_input_matrix,
  "intensity_",
  "_global",
  fixed_keys = "other_biomass"
) |>
  mutate(material_group = "biomass")

ep_fossil <- build_endpoints(
  fossil_bounds,
  flow_ratio_bounds |> filter(material_group == "fossil_fuels"),
  mc_input_matrix,
  "intensity_",
  "_global",
  fixed_keys = "other_fossil"
) |>
  mutate(material_group = "fossil_fuels")

ep_metal <- build_endpoints(
  metal_bounds,
  stock_ratio_bounds |> filter(material_group == "metal_ores"),
  mc_input_matrix,
  "intensity_",
  "_metalOres_global"
) |>
  mutate(material_group = "metal_ores")

ep_nonmet <- build_endpoints(
  nonmet_bounds,
  stock_ratio_bounds |> filter(material_group == "nonmetallic_minerals"),
  mc_input_matrix,
  "intensity_",
  "_nonMetallic_global",
  fixed_keys = c("sl_products", "machinery")
) |>
  mutate(material_group = "nonmetallic_minerals")

intensity_ep <- bind_rows(ep_biomass, ep_fossil, ep_metal, ep_nonmet)
intensity_by_run <- split(intensity_ep, intensity_ep$run_id)

# -- Power sector: fossil draws -> bracketing emissions scenarios -------------
# No new draws. Under EACH of the run's two SSPs, its 2060 coal/gas/oil ratios
# (same u as the flows, that SSP's own bounds) are averaged with the region's
# 2024 fossil energy shares -> fossil index. Within that SSP the two ScenarioMIP
# runs whose index brackets it are interpolated linearly (clamped at the ends;
# one-scenario SSP -> that scenario); the two SSP results are blended with the
# run's SSP weights. Regional index -> capacity path; world index (2024 energy
# weights) -> 2060 warming. Same blend as population, GDP/cap and fossil flows.
power_shares <- readr::read_csv("Parameters/Simulation/power_fossil_shares.csv", show_col_types = FALSE)
power_scen_index <- readr::read_csv("Parameters/Simulation/power_scenario_index.csv", show_col_types = FALSE)
power_scenarios <- readr::read_csv("Parameters/Simulation/power_scenarios.csv", show_col_types = FALSE)

run_fossil_side <- collect_u(mc_input_matrix, "intensity_", "_global", c("coal", "gas", "oil")) |>
  left_join(ssp_bracket, by = "run_id") |>
  tidyr::pivot_longer(c(ssp_lo, ssp_hi), names_to = "side", values_to = "ssp") |>
  mutate(side_share = if_else(side == "ssp_lo", ssp_share_lo, 1 - ssp_share_lo)) |>
  inner_join(
    flow_ratio_bounds |> filter(material_group == "fossil_fuels") |> dplyr::select(region, mat_key, ssp, ratio_min, ratio_max),
    by = c("mat_key", "ssp"),
    relationship = "many-to-many"
  ) |>
  inner_join(power_shares |> dplyr::select(region, mat_key = fuel, share, world_weight), by = c("region", "mat_key")) |>
  group_by(run_id, side, ssp, side_share, region, world_weight) |>
  summarise(fossil_index = sum(share * (ratio_min + u_global * (ratio_max - ratio_min))), n_fuel = n(), .groups = "drop")
stopifnot(all(run_fossil_side$n_fuel == 3L), nrow(run_fossil_side) == N_RUNS * 2L * length(regions_vec))

run_power_bracket <- run_fossil_side |>
  inner_join(power_scen_index |> rename(scen_index = fossil_index), by = c("ssp", "region"), relationship = "many-to-many") |>
  group_by(run_id, side, ssp, side_share, region, fossil_index) |>
  summarise(
    scen_lo = if (any(scen_index <= fossil_index)) scen_id[scen_index <= fossil_index][which.max(scen_index[scen_index <= fossil_index])] else scen_id[which.min(scen_index)],
    scen_hi = if (any(scen_index >= fossil_index)) scen_id[scen_index >= fossil_index][which.min(scen_index[scen_index >= fossil_index])] else scen_id[which.max(scen_index)],
    idx_lo = scen_index[scen_id == scen_lo],
    idx_hi = scen_index[scen_id == scen_hi],
    .groups = "drop"
  ) |>
  mutate(w_hi = if_else(idx_hi > idx_lo, (fossil_index - idx_lo) / (idx_hi - idx_lo), 0))

# Per run x region: weight on each scenario's capacity path (sums to 1)
run_power_weights <- bind_rows(
  run_power_bracket |> transmute(run_id, region, scen_id = scen_lo, w = side_share * (1 - w_hi)),
  run_power_bracket |> transmute(run_id, region, scen_id = scen_hi, w = side_share * w_hi)
) |>
  group_by(run_id, region, scen_id) |>
  summarise(w = sum(w), .groups = "drop") |>
  filter(w > 0)
stopifnot(all(abs(tapply(run_power_weights$w, paste(run_power_weights$run_id, run_power_weights$region), sum) - 1) < 1e-9))
power_w_by_run <- split(run_power_weights, run_power_weights$run_id)

# World bracket -> 2060 median warming (scenarios with climate data only)
run_power_world <- run_fossil_side |>
  group_by(run_id, side, ssp, side_share) |>
  summarise(fossil_index = sum(world_weight * fossil_index), .groups = "drop") |>
  inner_join(
    power_scenarios |> filter(!is.na(t_2060)) |> dplyr::select(scen_id, ssp, scen_index = fossil_index_world, t_2060),
    by = "ssp",
    relationship = "many-to-many"
  ) |>
  group_by(run_id, side, ssp, side_share, fossil_index) |>
  summarise(
    scen_lo = if (any(scen_index <= fossil_index)) scen_id[scen_index <= fossil_index][which.max(scen_index[scen_index <= fossil_index])] else scen_id[which.min(scen_index)],
    scen_hi = if (any(scen_index >= fossil_index)) scen_id[scen_index >= fossil_index][which.min(scen_index[scen_index >= fossil_index])] else scen_id[which.max(scen_index)],
    idx_lo = scen_index[scen_id == scen_lo],
    idx_hi = scen_index[scen_id == scen_hi],
    t_lo = t_2060[scen_id == scen_lo],
    t_hi = t_2060[scen_id == scen_hi],
    .groups = "drop"
  ) |>
  mutate(w_hi = if_else(idx_hi > idx_lo, (fossil_index - idx_lo) / (idx_hi - idx_lo), 0), t_side = t_lo + w_hi * (t_hi - t_lo))

run_power_link <- run_power_world |>
  group_by(run_id) |>
  summarise(fossil_index_world = sum(side_share * fossil_index), t_2060 = sum(side_share * t_side), .groups = "drop")
stopifnot(nrow(run_power_link) == N_RUNS)

cat(
  "  Power bracket: share of run x region x SSP cells clamped at the scenario range:",
  round(mean(run_power_bracket$scen_lo == run_power_bracket$scen_hi), 3), "\n"
)

# -- Global scalar parameters (semi-uniform around the central value) ---------
# value = min + 2u (central - min)            if u < 0.5
#       = central + 2(u - 0.5) (max - central) otherwise
# -> half of the draws on each side of central; min = central = max -> fixed.
# One column per MC_PARAMS row (lowercase name), one row per run.
param_draws <- mc_input_matrix |>
  dplyr::select(run_id, all_of(MC_PARAMS$col)) |>
  tidyr::pivot_longer(-run_id, names_to = "col", values_to = "u") |>
  left_join(MC_PARAMS, by = "col") |>
  mutate(value = if_else(u < 0.5, min + 2 * u * (central - min), central + 2 * (u - 0.5) * (max - central))) |>
  dplyr::select(run_id, col, value) |>
  tidyr::pivot_wider(names_from = col, values_from = value) |>
  arrange(run_id)

# -- Recycling endpoints + trajectories (convergence with smoothstep ramp) -----
recyc_conv_yr <- param_draws |>
  transmute(run_id, recyc_convergence_yr = as.integer(round(recyc_convergence_yr)))

# Endpoint rate = the run's sampled global value (recycling_rate_fe,
# recycling_rate_nonfe, downcycling), same for all regions (SSP-independent).
# Each region ramps from its own 2024 rate to that endpoint by
# recyc_convergence_yr, holds after (no clipping, no GDP-weighted anchor).
#
# 2024 anchor is the Excel end-of-life recycling rate itself (rate_now, from
# the "Recycling_EOL" sheet -- fraction of end-of-life waste recovered). This
# is applied to waste_Mt downstream (run_dsm_metal), not to production, so it
# stays a physical recovery-from-scrap rate throughout. (Previously anchored
# to Parameters/Intermediate/nonprimary_share_2024.csv, an empirical share of
# PRODUCTION that bundles domestic recycling with embodied net imports of
# already-processed metal -- a different, trade-based quantity that does not
# belong in a waste-based recycling rate; this model makes no trade
# assumptions, so that anchor has been dropped.)
make_recycling_traj <- function(recycling_now, G_col) {
  G_df <- param_draws |>
    dplyr::select(run_id, G = all_of(G_col))
  recycling_now <- recycling_now |> mutate(anchor_2024 = rate_now)
  ep <- recycling_now |>
    tidyr::crossing(run_id = seq_len(N_RUNS)) |>
    left_join(G_df, by = "run_id") |>
    left_join(recyc_conv_yr, by = "run_id") |>
    mutate(recycling_endpoint = G)
  ep |>
    tidyr::crossing(year = YEARS_DSM) |>
    mutate(
      # Smoothstep ease (zero slope at both 2024 and recyc_convergence_yr)
      # instead of a linear-ramp-then-flat step -- a slope kink at the
      # convergence year would otherwise show up as a jump in primary
      # consumption, since production ~ d(target_stock)/dt.
      w_lin = pmin(1, pmax(0, (year - 2024L) / pmax(1L, recyc_convergence_yr - 2024L))),
      w_smooth = w_lin^2 * (3 - 2 * w_lin),
      recycling_rate = anchor_2024 + (recycling_endpoint - anchor_2024) * w_smooth
    ) |>
    dplyr::select(run_id, region, year, recycling_rate)
}

recycling_traj_fe <- make_recycling_traj(recycling_now_fe, "recycling_rate_fe")
recycling_traj_nonfe <- make_recycling_traj(recycling_now_nonfe, "recycling_rate_nonfe")
recycling_by_run_fe <- split(recycling_traj_fe, recycling_traj_fe$run_id)
recycling_by_run_nonfe <- split(recycling_traj_nonfe, recycling_traj_nonfe$run_id)

# -- Downcycling trajectory (same ramp; one shared endpoint for all 4 sectors) --
downcycling_traj <- make_recycling_traj(downcycling_now, "downcycling") |>
  dplyr::select(run_id, region, year, downcycling_rate = recycling_rate)
downcycling_by_run <- split(downcycling_traj, downcycling_traj$run_id)

# -- Lifetime params (4 super_category draws -> expanded to 8 sub_uses) ---------
lifetime_dr <- mc_input_matrix |>
  dplyr::select(run_id, matches("^lifetime_(mean|k)_")) |>
  tidyr::pivot_longer(-run_id, names_to = c("param", "super_cat"), names_pattern = "^lifetime_(mean|k)_(.+)") |>
  tidyr::pivot_wider(names_from = param, values_from = value) |>
  rename(u_mean = mean, u_k = k) |>
  left_join(LIFETIME_SAMPLE_PARAMS, by = c("super_cat" = "super_category")) |>
  mutate(
    mean_life_hist = mean_life, # central value from Lifetimes sheet (used by CEM)
    k_hist = weibull_k,
    mean_life = pmax(LIFETIME_MIN, mean_life_min + u_mean * (mean_life_max - mean_life_min)),
    weibull_k = pmax(0.1, k_min + u_k * (k_max - k_min))
  ) |>
  dplyr::select(run_id, sub_use, mean_life, weibull_k, mean_life_hist, k_hist)
lifetime_by_run <- split(lifetime_dr, lifetime_dr$run_id)

# -- Ore grade (metal -> ore conversion for primary AND avoided-ore secondary) -
# grade_ore_fe/nonfe here is the per-run sampled TARGET (endpoint); each run
# ramps linearly from the 2024 baseline (GRADE_ORE_*_NOW) to this target, over
# the same convergence year drawn for intensity (target_year_vec, built below).
GRADE_ORE_FE_NOW <- GRADE_ORE_FE_2024 # Scripts/00-CommonParameters.R
GRADE_ORE_NONFE_NOW <- GRADE_ORE_NONFE_2024

grade_params <- param_draws |>
  dplyr::select(run_id, grade_ore_fe, grade_ore_nonfe)
grade_by_run <- split(grade_params, grade_params$run_id)

# -- Secondary room parameters (non-metallic minerals) ---------------------------
scalar_params <- param_draws |>
  dplyr::select(
    run_id,
    max_secondary_build_civil,
    max_secondary_roads,
    share_concrete_buildings,
    share_concrete_civil,
    share_agg_concrete,
    share_granular_road
  )
scalar_by_run <- split(scalar_params, scalar_params$run_id)

target_year_vec <- as.integer(round(param_draws$target_year))

cat("  Trajectories rebuilt.\n")


# =============================================================================
# STEP 5: Single-run worker
# =============================================================================
run_one <- function(i) {
  ie_i <- intensity_by_run[[i]]
  lp_i <- lifetime_by_run[[i]]
  rec_fe_i <- recycling_by_run_fe[[i]]
  rec_nonfe_i <- recycling_by_run_nonfe[[i]]
  dow_i <- downcycling_by_run[[i]]
  sc_i <- scalar_by_run[[i]]
  gr_i <- grade_by_run[[i]]

  # Continuous SSP blend: convex combination of the two bracketing SSPs'
  # region x year matrices -- same bracket and share for population and GDP
  # per capita (and for the flow-intensity bounds, see build_endpoints).
  pop_i <- ssp_share_lo_vec[i] * pop_mat[[ssp_lo_vec[i]]] +
    (1 - ssp_share_lo_vec[i]) * pop_mat[[ssp_hi_vec[i]]]
  gdppc_i <- ssp_share_lo_vec[i] * gdppc_mat[[ssp_lo_vec[i]]] +
    (1 - ssp_share_lo_vec[i]) * gdppc_mat[[ssp_hi_vec[i]]]

  yr_vec <- YEARS_DSM
  # alpha: 0->1 time weight, smoothstep-eased (zero slope at both 2024 and
  # target_year) rather than a linear ramp -- a slope kink at target_year
  # would otherwise show up as a jump in production, since production ~
  # d(target_stock)/dt. Intensities ramp log-linearly (geometrically) from
  # 2024 value to endpoint as int_2024 * (endpoint / int_2024)^alpha
  alpha_lin <- pmin(1, pmax(0, (yr_vec - 2024L) / (target_year_vec[i] - 2024L)))
  alpha <- alpha_lin^2 * (3 - 2 * alpha_lin)
  # Hermite slope basis (0 at both ends, unit initial slope in alpha_lin): stock
  # log-intensity += h10 * g_int * span gives an initial slope of g_int per year
  h10 <- alpha_lin^3 - 2 * alpha_lin^2 + alpha_lin
  span_yrs <- target_year_vec[i] - 2024L

  # Ore grade: same smoothstep-eased ramp from the 2024 baseline to the
  # sampled target, using the same convergence year (alpha) as intensity.
  grade_ore_fe_traj <- setNames(GRADE_ORE_FE_NOW + (gr_i$grade_ore_fe - GRADE_ORE_FE_NOW) * alpha, as.character(yr_vec))
  grade_ore_nonfe_traj <- setNames(
    GRADE_ORE_NONFE_NOW + (gr_i$grade_ore_nonfe - GRADE_ORE_NONFE_NOW) * alpha,
    as.character(yr_vec)
  )

  # -- Power sector: capacity (GW) and storage (GWh) DSM per technology --------
  # Target units = bracket/SSP-weighted scenario capacity factor x pop index x
  # GDP/cap index. One DSM per technology (no netting of a retiring technology
  # against a growing one), then material = units x fixed t/unit, summed per
  # lifetime class (= power end use).
  pw_w_i <- power_w_by_run[[i]]
  cap_rows <- list()
  pw_rows <- list()
  for (rg in regions_vec) {
    w_rg <- pw_w_i[pw_w_i$region == rg, ]
    f_rg <- matrix(0, length(POWER_TECH_KEYS), N_YR, dimnames = list(POWER_TECH_KEYS, NULL))
    for (k in seq_len(nrow(w_rg))) {
      f_rg <- f_rg + w_rg$w[k] * power_factor_arr[w_rg$scen_id[k], rg, , ]
    }
    target_units <- sweep(f_rg, 2, pop_i[rg, ] * gdppc_i[rg, ], "*")
    # Storage: battery GWh = BATTERY_GWH_PER_GW x (PV + onshore + offshore GW)
    target_units <- rbind(target_units, battery = BATTERY_GWH_PER_GW * colSums(target_units[BATTERY_SOURCE_TECHS, , drop = FALSE]))
    anchor_units <- c(power_anchor_mat[rg, ], battery = BATTERY_GWH_PER_GW * sum(power_anchor_mat[rg, BATTERY_SOURCE_TECHS]))

    for (uk in rownames(target_units)) {
      lc <- POWER_LIFE_CLASSES[POWER_LIFE_CLASSES$sub_use == POWER_UNIT_CLASS[[uk]], ]
      age <- power_age_lookup[[rg]][[lc$donor_sub_use]]
      if (is.null(age)) {
        age <- list(cohort_years = integer(0), shares = numeric(0))
      }
      donor <- lifetime_params[lifetime_params$sub_use == lc$donor_sub_use, ]
      res <- run_forward_dsm_fast(
        cohort_years = age$cohort_years,
        cohort_stocks_2024 = anchor_units[[uk]] * age$shares,
        target_stock = target_units[uk, ],
        mean_life = lc$mean_life,
        k = lc$weibull_k,
        mean_life_hist = donor$mean_life[1],
        k_hist = donor$weibull_k[1],
        start_year = 2024L,
        end_year = FORECAST_END
      )
      cap_rows[[length(cap_rows) + 1L]] <- data.frame(
        run_id = i,
        region = rg,
        tech = uk,
        year = res$year,
        stock_units = res$total_stock, # GW (battery: GWh)
        inflow_units = res$production,
        outflow_units = res$waste,
        target_units = target_units[uk, ]
      )
      for (mg in colnames(power_mi_mat)) {
        mi <- power_mi_mat[uk, mg] / 1e6 # t/unit -> Mt/unit
        if (mi <= 0) {
          next
        }
        pw_rows[[length(pw_rows) + 1L]] <- data.frame(
          region = rg,
          sub_use = lc$sub_use,
          super_key = "power",
          material_group = mg,
          year = res$year,
          total_stock_Mt = res$total_stock * mi,
          new_additions_Mt = res$new_additions * mi,
          replacement_Mt = res$replacement * mi,
          production_Mt = res$production * mi,
          waste_Mt = res$waste * mi,
          target_stock_Mt = target_units[uk, ] * mi
        )
      }
    }
  }
  cap_i <- do.call(rbind, cap_rows)
  pw_i <- do.call(rbind, pw_rows) |>
    group_by(region, sub_use, super_key, material_group, year) |>
    summarise(across(ends_with("_Mt"), sum), .groups = "drop") |>
    as.data.frame()

  out_list <- list()

  # Helper to emit a Kaya flow row. For non-metals carrier == reported mass, so
  # the *_Mt_pure columns mirror the headline columns and spill is 0.
  emit_flow <- function(rg, mg, key, M_Mt) {
    data.frame(
      run_id = i,
      region = rg,
      material_group = mg,
      material_key = key,
      year = yr_vec,
      total_inflow_Mt = M_Mt,
      in_use_stock_Mt = NA_real_,
      primary_consumption_Mt = M_Mt,
      secondary_supply_Mt = 0,
      primary_consumption_Mt_pure = M_Mt,
      secondary_supply_Mt_pure = 0,
      secondary_spilled_Mt = 0,
      not_recovered_Mt = NA_real_,
      not_recovered_Mt_pureMetal = NA_real_,
      new_additions_Mt = NA_real_,
      replacement_Mt = NA_real_,
      waste_Mt = NA_real_,
      new_additions_Mt_pure = NA_real_,
      replacement_Mt_pure = NA_real_,
      waste_Mt_pure = NA_real_,
      target_stock_Mt = NA_real_
    )
  }

  # -- Biomass (Kaya): M(t) = M_2024 * pop_idx * gdp_pcap_idx * intensity_idx ---
  bio <- ie_i |>
    filter(material_group == "biomass") |>
    mutate(unep_label = BIOMASS_LABEL[mat_key]) |>
    left_join(m_2024_biomass, by = c("region", "unep_label"))

  for (j in seq_len(nrow(bio))) {
    rg <- bio$region[j]
    # log-linear ramp: geometric interpolation from 2024 intensity to endpoint
    int_2024_j <- max(bio$int_2024[j], 1e-12)
    endpoint_j <- max(bio$endpoint[j], 1e-12)
    mg_ratio <- int_2024_j * (endpoint_j / int_2024_j)^alpha
    mg_index <- mg_ratio / max(abs(bio$int_2024[j]), 1e-12)
    M_Mt <- bio$M_2024_Mt[j] * pop_i[rg, ] * gdppc_i[rg, ] * mg_index
    if (any(is.na(M_Mt))) {
      next
    }
    out_list[[length(out_list) + 1L]] <- emit_flow(rg, "biomass", bio$unep_label[j], M_Mt)
  }

  # Crop residues: share crops' intensity index, own 2024 anchor
  crops_rows <- bio[bio$mat_key == "crops", ]
  cr_m2024 <- m_2024_biomass |> filter(unep_label == "Crop Residues")
  for (j in seq_len(nrow(crops_rows))) {
    rg <- crops_rows$region[j]
    cr <- cr_m2024$M_2024_Mt[cr_m2024$region == rg]
    if (length(cr) == 0) {
      next
    }
    int_2024_j <- max(crops_rows$int_2024[j], 1e-12)
    endpoint_j <- max(crops_rows$endpoint[j], 1e-12)
    mg_ratio <- int_2024_j * (endpoint_j / int_2024_j)^alpha
    mg_index <- mg_ratio / max(abs(crops_rows$int_2024[j]), 1e-12)
    M_Mt <- cr * pop_i[rg, ] * gdppc_i[rg, ] * mg_index
    out_list[[length(out_list) + 1L]] <- emit_flow(rg, "biomass", "Crop Residues", M_Mt)
  }

  # -- Fossil (Kaya) -----------------------------------------------------------
  fos <- ie_i |> filter(material_group == "fossil_fuels") |> left_join(m_2024_fossil, by = c("region", "mat_key"))
  for (j in seq_len(nrow(fos))) {
    rg <- fos$region[j]
    if (is.na(fos$M_2024_Mt[j])) {
      next
    }
    int_base <- fos$int_2024[j]
    int_base_j <- max(int_base, 1e-12)
    endpoint_j <- max(fos$endpoint[j], 1e-12)
    mg_ratio <- int_base_j * (endpoint_j / int_base_j)^alpha
    mg_index <- if (abs(int_base) < 1e-12) rep(1.0, N_YR) else mg_ratio / int_base
    M_Mt <- fos$M_2024_Mt[j] * pop_i[rg, ] * gdppc_i[rg, ] * mg_index
    out_list[[length(out_list) + 1L]] <- emit_flow(rg, "fossil_fuels", FOSSIL_LABEL[fos$mat_key[j]], M_Mt)
  }

  # -- DSM for one metal material (Fe or NonFe) --------------------------------
  # Recycling only, no quality loss. Secondary cannot exceed demand; unabsorbed
  # scrap is recorded as spill (not silently dropped). Primary >= 0 by construction.
  # Reported base *_Mt columns are ore-equivalent (metal / grade);
  # matching *_Mt_pure columns keep metal mass.
  run_dsm_metal <- function(mat_label, age_lookup, rec_i, grade_traj) {
    ie_g <- ie_i |> filter(material_group == "metal_ores") |> mutate(mat_key_super = mat_key)

    sr_rows <- list()
    for (j in seq_len(nrow(ie_g))) {
      rg <- ie_g$region[j]
      mk <- ie_g$mat_key_super[j]
      su_list <- SUB_USE_BY_MATKEY[[mk]]
      if (is.null(su_list)) {
        next
      }

      int_2024_j <- max(ie_g$int_2024[j], 1e-12)
      endpoint_j <- max(ie_g$endpoint[j], 1e-12)
      stock_intensity_index <- (int_2024_j * (endpoint_j / int_2024_j)^alpha) / max(abs(ie_g$int_2024[j]), 1e-12)

      for (su in su_list) {
        age <- age_lookup[[rg]][[su]]
        if (is.null(age)) {
          next
        }
        s2024 <- stock_2024_sub[[mat_label]][[rg]][[su]]
        if (is.null(s2024) || s2024 <= 0) {
          next
        }
        # Intensity ramp starts at the slope that continues 2024 stock growth:
        # g_int = historical stock log-growth - this run's 2025 GDP log-growth (Hermite slope term)
        g_s <- stock_growth_hist_sub[[mat_label]][[rg]][su]
        g_int <- if (length(g_s) == 1 && !is.na(g_s)) g_s - log(pop_i[rg, 1] * gdppc_i[rg, 1]) else 0
        target_stock <- s2024 * pop_i[rg, ] * gdppc_i[rg, ] * stock_intensity_index * exp(h10 * g_int * span_yrs)
        # Seam blend: log stock growth = w * model + (1 - w) * historical 2024 rate
        g_hist <- stock_growth_hist_sub[[mat_label]][[rg]][su]
        if (length(g_hist) == 1 && !is.na(g_hist)) {
          g_model <- diff(log(c(s2024, target_stock)))
          target_stock <- s2024 * exp(cumsum(w_growth_model * g_model + (1 - w_growth_model) * g_hist))
        }
        lp_row <- lp_i[lp_i$sub_use == su, ]
        if (nrow(lp_row) == 0) {
          next
        }
        res <- run_forward_dsm_fast(
          cohort_years = age$cohort_years,
          cohort_stocks_2024 = age$cohort_stocks,
          target_stock = target_stock,
          mean_life = lp_row$mean_life[1],
          k = lp_row$weibull_k[1],
          mean_life_hist = lp_row$mean_life_hist[1],
          k_hist = lp_row$k_hist[1],
          start_year = 2024L,
          end_year = FORECAST_END
        )
        sr_rows[[length(sr_rows) + 1L]] <- data.frame(
          region = rg,
          sub_use = su,
          year = res$year,
          total_stock_Mt = res$total_stock,
          new_additions_Mt = res$new_additions,
          replacement_Mt = res$replacement,
          production_Mt = res$production,
          waste_Mt = res$waste,
          target_stock_Mt = target_stock
        )
      }
    }
    # Power-sector end uses (built above) join the same end-of-life pool
    pw_g <- pw_i[pw_i$material_group == if (mat_label == "Metal_Fe") "metal_fe" else "metal_nonfe", ]
    if (nrow(pw_g) > 0) {
      sr_rows[[length(sr_rows) + 1L]] <- pw_g[, c("region", "sub_use", "year", "total_stock_Mt", "new_additions_Mt", "replacement_Mt", "production_Mt", "waste_Mt", "target_stock_Mt")]
    }
    if (length(sr_rows) == 0) {
      return(NULL)
    }
    sr <- do.call(rbind, sr_rows)

    sr <- merge(sr, rec_i, by = c("region", "year"), all.x = TRUE)
    recycling_rate <- sr$recycling_rate
    recycling_rate[is.na(recycling_rate)] <- 0

    prod_metal <- pmax(0, sr$production_Mt) # guard against DSM negatives
    waste_metal <- pmax(0, sr$waste_Mt)
    # End-of-life (allocate_eol): pool = rate x waste over the 8 end-uses per
    # region x year; secondary = min(pool, total demand), split by demand share;
    # surplus = pool - secondary (no trade). Room = demand for metals.
    eol <- allocate_eol(paste(sr$region, sr$year), prod_metal, waste_metal, recycling_rate, prod_metal)
    recovered <- eol$secondary
    spilled <- eol$surplus
    primary_metal <- eol$primary

    grade_safe <- pmax(grade_traj[as.character(sr$year)], 1e-6)

    data.frame(
      run_id = i,
      region = sr$region,
      material_group = if (mat_label == "Metal_Fe") "metal_fe" else "metal_nonfe",
      material_key = SUB_USE_LABELS[sr$sub_use],
      year = sr$year,
      total_inflow_Mt = sr$replacement_Mt + sr$new_additions_Mt,
      in_use_stock_Mt = sr$total_stock_Mt,
      primary_consumption_Mt = primary_metal / grade_safe, # ore extracted
      secondary_supply_Mt = recovered / grade_safe, # ore avoided; recycled from end-of-life waste
      primary_consumption_Mt_pure = primary_metal,
      secondary_supply_Mt_pure = recovered,
      secondary_spilled_Mt = spilled / grade_safe, # recovered scrap beyond what current demand could absorb
      not_recovered_Mt = eol$not_recovered / grade_safe,
      not_recovered_Mt_pureMetal = eol$not_recovered,
      new_additions_Mt = sr$new_additions_Mt / grade_safe, # ore
      replacement_Mt = sr$replacement_Mt / grade_safe,
      waste_Mt = sr$waste_Mt / grade_safe,
      new_additions_Mt_pure = sr$new_additions_Mt, # metal
      replacement_Mt_pure = sr$replacement_Mt,
      waste_Mt_pure = sr$waste_Mt,
      target_stock_Mt = sr$target_stock_Mt
    )
  }

  fe_out <- run_dsm_metal("Metal_Fe", age_lookup_fe, rec_fe_i, grade_ore_fe_traj)
  nonfe_out <- run_dsm_metal("Metal_NonFe", age_lookup_nonfe, rec_nonfe_i, grade_ore_nonfe_traj)
  if (!is.null(fe_out)) {
    out_list[[length(out_list) + 1L]] <- fe_out
  }
  if (!is.null(nonfe_out)) {
    out_list[[length(out_list) + 1L]] <- nonfe_out
  }

  # -- Non-metallic minerals: DSM + downcycling cascade (8 sub_uses) -----------
  ie_g_nm <- ie_i |> filter(material_group == "nonmetallic_minerals") |> mutate(mat_key_super = mat_key)

  nm_rows <- list()
  for (j in seq_len(nrow(ie_g_nm))) {
    rg <- ie_g_nm$region[j]
    mk <- ie_g_nm$mat_key_super[j]
    su_list <- SUB_USE_BY_MATKEY[[mk]]
    if (is.null(su_list)) {
      next
    }
    int_2024_j <- max(ie_g_nm$int_2024[j], 1e-12)
    endpoint_j <- max(ie_g_nm$endpoint[j], 1e-12)
    stock_intensity_index <- (int_2024_j * (endpoint_j / int_2024_j)^alpha) / max(abs(ie_g_nm$int_2024[j]), 1e-12)

    for (su in su_list) {
      age <- age_lookup_nonmet[[rg]][[su]]
      if (is.null(age)) {
        next
      }
      s2024 <- stock_2024_sub[["Non-metallic minerals"]][[rg]][[su]]
      if (is.null(s2024) || s2024 <= 0) {
        next
      }
      # Intensity ramp starts at the slope that continues 2024 stock growth:
      # g_int = historical stock log-growth - this run's 2025 GDP log-growth (Hermite slope term)
      g_s <- stock_growth_hist_sub[["Non-metallic minerals"]][[rg]][su]
      g_int <- if (length(g_s) == 1 && !is.na(g_s)) g_s - log(pop_i[rg, 1] * gdppc_i[rg, 1]) else 0
      target_stock <- s2024 * pop_i[rg, ] * gdppc_i[rg, ] * stock_intensity_index * exp(h10 * g_int * span_yrs)
      # Seam blend: log stock growth = w * model + (1 - w) * historical 2024 rate
      g_hist <- stock_growth_hist_sub[["Non-metallic minerals"]][[rg]][su]
      if (length(g_hist) == 1 && !is.na(g_hist)) {
        g_model <- diff(log(c(s2024, target_stock)))
        target_stock <- s2024 * exp(cumsum(w_growth_model * g_model + (1 - w_growth_model) * g_hist))
      }
      lp_row <- lp_i[lp_i$sub_use == su, ]
      if (nrow(lp_row) == 0) {
        next
      }
      res <- run_forward_dsm_fast(
        cohort_years = age$cohort_years,
        cohort_stocks_2024 = age$cohort_stocks,
        target_stock = target_stock,
        mean_life = lp_row$mean_life[1],
        k = lp_row$weibull_k[1],
        mean_life_hist = lp_row$mean_life_hist[1],
        k_hist = lp_row$k_hist[1],
        start_year = 2024L,
        end_year = FORECAST_END
      )
      nm_rows[[length(nm_rows) + 1L]] <- data.frame(
        region = rg,
        sub_use = su,
        super_key = mk,
        year = res$year,
        total_stock_Mt = res$total_stock,
        new_additions_Mt = res$new_additions,
        replacement_Mt = res$replacement,
        production_Mt = pmax(0, res$production),
        waste_Mt = res$waste,
        target_stock_Mt = target_stock
      )
    }
  }

  # Power-sector minerals join the same downcycling cascade
  pw_nm <- pw_i[pw_i$material_group == "nonmetallic_minerals", ]
  if (nrow(pw_nm) > 0) {
    nm_rows[[length(nm_rows) + 1L]] <- pw_nm[, c("region", "sub_use", "super_key", "year", "total_stock_Mt", "new_additions_Mt", "replacement_Mt", "production_Mt", "waste_Mt", "target_stock_Mt")]
  }

  if (length(nm_rows) > 0) {
    nm_sr <- do.call(rbind, nm_rows)
    nm_sr <- merge(nm_sr, dow_i, by = c("region", "year"), all.x = TRUE)

    # End-of-life (allocate_eol): only residential, non-residential, civil
    # engineering and roads give and receive. Pool = sum of downcycling rate x
    # waste over these sectors per region x year; each sector's room caps the
    # secondary it can absorb; secondary = min(pool, total room), split by room
    # share; surplus = pool - secondary (no trade).
    is_bldg <- nm_sr$sub_use %in% c("residential", "non_residential")
    # Power-sector minerals (plants, foundations) give and receive like civil engineering
    is_civil <- nm_sr$sub_use %in% c("civil_engineering", POWER_LIFE_CLASSES$sub_use)
    is_roads <- nm_sr$sub_use == "roads"
    demand_nm <- pmax(0, nm_sr$production_Mt)
    waste_nm <- pmax(0, nm_sr$waste_Mt)
    rate_nm <- ifelse(is_bldg | is_civil | is_roads, tidyr::replace_na(nm_sr$downcycling_rate, 0), 0)
    room_share <- ifelse(
      is_bldg,
      sc_i$max_secondary_build_civil * sc_i$share_concrete_buildings * sc_i$share_agg_concrete,
      ifelse(
        is_civil,
        sc_i$max_secondary_build_civil * sc_i$share_concrete_civil * sc_i$share_agg_concrete,
        ifelse(is_roads, sc_i$max_secondary_roads * sc_i$share_granular_road, 0)
      )
    )
    eol <- allocate_eol(paste(nm_sr$region, nm_sr$year), demand_nm, waste_nm, rate_nm, room_share * demand_nm)
    recovered <- eol$secondary
    spilled <- eol$surplus
    nm_sr$production_Mt <- eol$primary

    out_list[[length(out_list) + 1L]] <- data.frame(
      run_id = i,
      region = nm_sr$region,
      material_group = "nonmetallic_minerals",
      material_key = SUB_USE_LABELS[nm_sr$sub_use],
      year = nm_sr$year,
      total_inflow_Mt = nm_sr$replacement_Mt + nm_sr$new_additions_Mt,
      in_use_stock_Mt = nm_sr$total_stock_Mt,
      primary_consumption_Mt = nm_sr$production_Mt,
      secondary_supply_Mt = recovered,
      primary_consumption_Mt_pure = nm_sr$production_Mt, # carrier == mass (no ore conv.)
      secondary_supply_Mt_pure = recovered,
      secondary_spilled_Mt = spilled,
      not_recovered_Mt = eol$not_recovered,
      not_recovered_Mt_pureMetal = eol$not_recovered, # carrier == mass (no ore conv.)
      new_additions_Mt = nm_sr$new_additions_Mt,
      replacement_Mt = nm_sr$replacement_Mt,
      waste_Mt = nm_sr$waste_Mt,
      new_additions_Mt_pure = nm_sr$new_additions_Mt, # carrier == mass (no ore conv.)
      replacement_Mt_pure = nm_sr$replacement_Mt,
      waste_Mt_pure = nm_sr$waste_Mt,
      target_stock_Mt = nm_sr$target_stock_Mt
    )
  }

  out <- bind_rows(out_list)

  # Continuous SSP blend used by this run (replaces the discrete ssp_label) --
  # exact reconstruction of pop_i/gdppc_i requires only these three values.
  out$ssp_lo <- ssp_lo_vec[i]
  out$ssp_hi <- ssp_hi_vec[i]
  out$ssp_share_lo <- ssp_share_lo_vec[i]

  # Per-run physical-sanity flags (kept, not filtered) -> screen pathological
  # runs downstream before SHAP/Sobol rather than letting them contaminate.
  out$run_neg_primary <- any(out$primary_consumption_Mt < -1e-9, na.rm = TRUE)
  out$run_total_spill_Mt <- sum(out$secondary_spilled_Mt, na.rm = TRUE)
  list(main = out, cap = cap_i)
}


# =============================================================================
# STEP 6: Parallel execution
# =============================================================================
cat("\nSTEP 6: Run", N_RUNS, "simulations\n")

options(future.globals.maxSize = 2 * 1024^3) # 2 GiB; default is 500 MiB
if (.Platform$OS.type == "windows") {
  future::plan(multisession, workers = max(1, parallel::detectCores() - 1))
} else {
  future::plan(multicore, workers = max(1, parallel::detectCores() - 1))
}
cat("  Workers:", future::nbrOfWorkers(), "\n")

t0 <- proc.time()
results_list <- furrr::future_map(seq_len(N_RUNS), run_one, .options = furrr_options(seed = TRUE), .progress = TRUE)
elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  Done in %.0f s (%.3f s/run)\n", elapsed, elapsed / N_RUNS))

results <- bind_rows(lapply(results_list, `[[`, "main"))
power_capacity <- bind_rows(lapply(results_list, `[[`, "cap"))
rm(results_list)


# =============================================================================
# STEP 7: Save
# =============================================================================
cat("\nSTEP 7: Save\n")
arrow::write_parquet(results, "Results/MC/mc_results.parquet")
cat("  Saved:", nrow(results), "rows to Results/MC/mc_results.parquet\n")

# Quick spill audit -> if non-trivial, recycling/downcycling params are
# saturating demand and their sensitivity contribution is suspect.
spill_summary <- results |>
  dplyr::distinct(run_id, run_total_spill_Mt) |>
  dplyr::summarise(
    runs_with_spill = sum(run_total_spill_Mt > 1e-6),
    median_spill_Mt = median(run_total_spill_Mt),
    max_spill_Mt = max(run_total_spill_Mt)
  )
cat(sprintf(
  "  Spill: %d/%d runs >0 | median %.3f Mt | max %.3f Mt\n",
  spill_summary$runs_with_spill,
  N_RUNS,
  spill_summary$median_spill_Mt,
  spill_summary$max_spill_Mt
))


# =============================================================================
# STEP 7b: Power-sector outputs and checks
# =============================================================================
cat("\nSTEP 7b: Power-sector outputs and checks\n")

POWER_LABELS <- POWER_LIFE_CLASSES$label

# Stocks / inflows / waste in metal mass for metals; primary / secondary ore-equivalent
power_materials <- results |>
  filter(material_key %in% POWER_LABELS) |>
  group_by(run_id, region, material_group, year) |>
  summarise(
    in_use_stock_Mt = sum(in_use_stock_Mt),
    total_inflow_Mt = sum(total_inflow_Mt),
    waste_Mt_pure = sum(waste_Mt_pure),
    primary_consumption_Mt = sum(primary_consumption_Mt),
    secondary_supply_Mt = sum(secondary_supply_Mt),
    .groups = "drop"
  )

readr::write_csv(power_capacity, "Results/MC/mc_power_capacity.csv")
readr::write_csv(power_materials, "Results/MC/mc_power_materials.csv")
readr::write_csv(run_power_link, "Results/MC/mc_power_run_link.csv")
readr::write_csv(run_power_bracket, "Results/MC/mc_power_run_brackets.csv")
cat("  Saved: mc_power_capacity.csv, mc_power_materials.csv, mc_power_run_link.csv, mc_power_run_brackets.csv\n")

# Check 1: 2025 world capacity vs the input file
cat("\n  [Check 1] 2025 world capacity (GW; battery GWh): input file vs model\n")
print(
  readr::read_csv("Parameters/Simulation/power_capacity_check_2025.csv", show_col_types = FALSE) |>
    full_join(
      power_capacity |>
        filter(year == DSM_START) |>
        group_by(run_id, tech) |>
        summarise(units = sum(stock_units), .groups = "drop") |>
        group_by(tech) |>
        summarise(model_2025_median = median(units), .groups = "drop"),
      by = "tech"
    ) |>
    mutate(across(-tech, ~ round(.x, 1))) |>
    as.data.frame()
)

# Check 2: power-sector share of total in-use stock, by material group
cat("\n  [Check 2] Power-sector share of world in-use stock (p05 / median / p95 across runs)\n")
print(
  results |>
    filter(year %in% c(DSM_START, FORECAST_END), material_group %in% c("metal_fe", "metal_nonfe", "nonmetallic_minerals")) |>
    group_by(run_id, year, material_group) |>
    summarise(share = sum(in_use_stock_Mt[material_key %in% POWER_LABELS]) / sum(in_use_stock_Mt), .groups = "drop") |>
    group_by(year, material_group) |>
    summarise(
      p05 = scales::percent(quantile(share, 0.05), 0.1),
      median = scales::percent(median(share), 0.1),
      p95 = scales::percent(quantile(share, 0.95), 0.1),
      .groups = "drop"
    ) |>
    as.data.frame()
)

# Check 3: correlation across runs, world fossil index vs cumulative power-sector inflows
cum_power_inflow <- power_materials |>
  group_by(run_id, material_group) |>
  summarise(cum_inflow_Mt = sum(total_inflow_Mt), .groups = "drop") |>
  bind_rows(power_materials |> group_by(run_id) |> summarise(cum_inflow_Mt = sum(total_inflow_Mt), .groups = "drop") |> mutate(material_group = "all")) |>
  inner_join(run_power_link, by = "run_id")
cat("\n  [Check 3] Correlation across runs: world fossil index vs cumulative", DSM_START, "-", FORECAST_END, "power-sector inflow (expected < 0)\n")
print(
  cum_power_inflow |>
    group_by(material_group) |>
    summarise(
      pearson = round(cor(fossil_index_world, cum_inflow_Mt), 3),
      spearman = round(cor(fossil_index_world, cum_inflow_Mt, method = "spearman"), 3),
      .groups = "drop"
    ) |>
    as.data.frame()
)
cat(
  "  Run", FORECAST_END, "warming (bracket + SSP blend):", round(min(run_power_link$t_2060), 2), "-",
  round(max(run_power_link$t_2060), 2), "°C | cor(fossil index, warming):",
  round(cor(run_power_link$fossil_index_world, run_power_link$t_2060), 3), "\n"
)

# Check 4: no negative stocks
n_neg_stock <- sum(results$in_use_stock_Mt < -1e-9, na.rm = TRUE)
n_neg_cap <- sum(power_capacity$stock_units < -1e-9)
cat("\n  [Check 4] Negative in-use stock rows:", n_neg_stock, "| negative capacity rows:", n_neg_cap, "\n")
if (n_neg_stock + n_neg_cap > 0) {
  warning("Negative stocks found -- see Check 4")
}


# =============================================================================
# STEP 8: Historical flows on the model basis (1970-2024)
# =============================================================================
# UNEP DMC (territorial) is not what the DSM projects: the DSM inflow is
# MISO-scope (incl. scrap and embodied trade, calibrated). For a seamless
# history -> projection, historical primary = DSM inflow - secondary, with the
# same end-of-life allocation as the projection (allocate_eol, per region x
# material x year): recovery rate = each region's 2024 anchor (Recycling_EOL
# Fe/NonFe, Downcycling for the 4 mineral sectors), held constant; mineral room
# from the central parameter values. Metals converted to ore-equivalent with
# the historical grade (same basis as primary_consumption_Mt).
cat("\nSTEP 8: Historical flows on the model basis\n")

hist_rates <- dplyr::bind_rows(
  recycling_now_fe |> dplyr::mutate(material = "Metal_Fe"),
  recycling_now_nonfe |> dplyr::mutate(material = "Metal_NonFe"),
  downcycling_now |> dplyr::mutate(material = "Non-metallic minerals")
) |>
  dplyr::select(region, material, rate_now)

hist_flows <- readr::read_csv("Parameters/Intermediate/flow_trajectory_subenduse.csv", show_col_types = FALSE) |>
  dplyr::rename(region = Region) |>
  dplyr::left_join(hist_rates, by = c("region", "material")) |>
  dplyr::arrange(region, material, sub_use, year) |>
  dplyr::group_by(region, material, sub_use) |>
  dplyr::mutate(outflow_Mt = dplyr::coalesce(outflow_Mt, dplyr::lead(outflow_Mt))) |> # first year has no outflow
  dplyr::ungroup() |>
  dplyr::mutate(
    outflow_Mt = tidyr::replace_na(outflow_Mt, 0),
    demand = pmax(0, inflow_Mt),
    waste = pmax(0, outflow_Mt),
    is_metal = material %in% c("Metal_Fe", "Metal_NonFe"),
    is_eol_sector = sub_use %in% c("residential", "non_residential", "civil_engineering", "roads"),
    rate = ifelse(is_metal | is_eol_sector, tidyr::replace_na(rate_now, 0), 0),
    room = demand *
      dplyr::case_when(
        is_metal ~ 1,
        sub_use %in% c("residential", "non_residential") ~
          MAX_SECONDARY_BUILD_CIVIL_CENTRAL * SHARE_CONCRETE_BUILDINGS_CENTRAL * SHARE_AGG_CONCRETE_CENTRAL,
        sub_use == "civil_engineering" ~
          MAX_SECONDARY_BUILD_CIVIL_CENTRAL * SHARE_CONCRETE_CIVIL_CENTRAL * SHARE_AGG_CONCRETE_CENTRAL,
        sub_use == "roads" ~ MAX_SECONDARY_ROADS_CENTRAL * SHARE_GRANULAR_ROAD_CENTRAL,
        TRUE ~ 0
      )
  )

eol_hist <- allocate_eol(
  paste(hist_flows$region, hist_flows$material, hist_flows$year),
  hist_flows$demand,
  hist_flows$waste,
  hist_flows$rate,
  hist_flows$room
)

hist_flows_model_basis <- hist_flows |>
  dplyr::mutate(
    secondary_Mt = eol_hist$secondary,
    primary_Mt = eol_hist$primary,
    surplus_Mt = eol_hist$surplus,
    not_recovered_Mt = eol_hist$not_recovered,
    dplyr::across(
      c(inflow_Mt, outflow_Mt, secondary_Mt, primary_Mt, surplus_Mt, not_recovered_Mt),
      \(x) x / grade
    ) # ore-equivalent for metals
  ) |>
  dplyr::select(
    region,
    material,
    super_category,
    sub_use,
    year,
    inflow_Mt,
    outflow_Mt,
    secondary_Mt,
    primary_Mt,
    surplus_Mt,
    not_recovered_Mt
  )

readr::write_csv(hist_flows_model_basis, "Results/MC/hist_flows_model_basis.csv")
cat("  Saved: Results/MC/hist_flows_model_basis.csv (", nrow(hist_flows_model_basis), "rows )\n")

cat("\n=== MC run complete ===\n")

# EoF
