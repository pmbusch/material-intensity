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
##   key: run_id, ssp_label, region, material_group, material_key, year
##   value: total_inflow_Mt, in_use_stock_Mt,
##          primary_consumption_Mt, secondary_supply_Mt,
##          new_additions_Mt, replacement_Mt, waste_Mt          (ore-equiv for metals)
##          primary_consumption_Mt_pure, secondary_supply_Mt_pure,
##          new_additions_Mt_pure, replacement_Mt_pure, waste_Mt_pure (metal mass)
##          secondary_spilled_Mt        (recyclable mass with no demand to absorb it)
##          run_neg_primary, run_total_spill_Mt  (per-run diagnostic flags)
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
  "packaging" = "Packaging"
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

mc_input_matrix <- readr::read_csv("Parameters/MC/mc_input_matrix.csv", show_col_types = FALSE)
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

downcycling_now <- readxl::read_excel(RECYCLING_FILE, sheet = "Downcycling") |>
  dplyr::select(
    region = Region,
    downcycling_buildings_now = Downcycling_Buildings,
    downcycling_civil_infrastructure_now = Downcycling_Civil,
    downcycling_roads_now = Downcycling_Roads
  ) |>
  dplyr::filter(!stringr::str_detect(region, "—"))

# Global intensity min/max envelopes (convergence targets clip to these)
intensity_raw <- readxl::read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Intensity")

intensity_bounds_biomass <- intensity_raw |>
  dplyr::filter(Category == "Biomass") |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(Detail), "residue") ~ NA_character_,
      stringr::str_detect(tolower(Detail), "grazed") ~ "grazed_biomass",
      stringr::str_detect(tolower(Detail), "crop") ~ "crops",
      stringr::str_detect(tolower(Detail), "wood") ~ "wood",
      stringr::str_detect(tolower(Detail), "other") ~ "other_biomass",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::select(mat_key, int_min = min, int_max = max)

intensity_bounds_fossil <- intensity_raw |>
  dplyr::filter(stringr::str_detect(tolower(Category), "fossil")) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(Detail), "coal") ~ "coal",
      stringr::str_detect(tolower(Detail), "gas") ~ "gas",
      stringr::str_detect(tolower(Detail), "petroleum|oil") ~ "oil",
      stringr::str_detect(tolower(Detail), "other") ~ "other_fossil",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::select(mat_key, int_min = min, int_max = max)

intensity_bounds_metal <- intensity_raw |>
  dplyr::filter(stringr::str_detect(tolower(Category), "metal ore")) |>
  dplyr::mutate(mat_key = classify_super(Detail)) |>
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::select(mat_key, int_min = min, int_max = max)

intensity_bounds_nonmet <- intensity_raw |>
  dplyr::filter(stringr::str_detect(tolower(Category), "metalic")) |>
  dplyr::mutate(mat_key = classify_super(Detail)) |>
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::select(mat_key, int_min = min, int_max = max)


# =============================================================================
# STEP 3: Static data (SSP drivers, GDP, UNEP anchors, age profiles)
# =============================================================================
cat("\nSTEP 3: Load static data\n")

DSM_START <- 2025L
YEARS_DSM <- seq(DSM_START, FORECAST_END)
N_YR <- length(YEARS_DSM)

ssp_drivers <- readr::read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)

pop_idx <- ssp_drivers |>
  filter(variable == "Population", year >= DSM_START, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, pop_index = index)
gdp_percap_idx <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= DSM_START, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_percap_index = index)

gdp_base_vals <- readr::read_csv("Parameters/gdp_region.csv", show_col_types = FALSE) |>
  filter(year == 2024L) |>
  dplyr::select(region = Region, GDP_2015USD)

# NOTE: gdp_full is consumed only to derive regions_vec / ssp_labels below.
# Kept minimal; drop entirely if those vectors are sourced elsewhere.
gdp_full <- ssp_drivers |>
  filter(variable == "GDP|PPP", year >= 2024L, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_index = index) |>
  left_join(gdp_base_vals, by = "region") |>
  mutate(gdp_billion_usd = GDP_2015USD * gdp_index / 1e9)

unep_dmc <- readr::read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

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
stock_2024_raw <- readr::read_csv("Parameters/stock_2024_total.csv", show_col_types = FALSE) |> rename(region = Region)
stock_2024_sub <- stock_2024_raw |>
  dplyr::select(material, region, sub_use, stock_Mt) |>
  split(~material) |>
  lapply(function(md) split(md, ~region) |> lapply(function(rd) setNames(rd$stock_Mt, rd$sub_use)))

# Age profiles: annualize 5-year cohort bins by spreading each bin evenly over
# its preceding `gap` years -> removes the staircase artifact in survival curves.
age_profile_raw <- readr::read_csv("Parameters/stock_2024_age_profile.csv", show_col_types = FALSE) |>
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

cat("  Static data loaded.\n")


# =============================================================================
# STEP 4: Rebuild per-run derived quantities (UPFRONT, vectorized over all runs)
# =============================================================================
cat("\nSTEP 4: Rebuild trajectories from LHS draws\n")

# GDP-weighted 2024 intensity per super_category (the global anchor for convergence)
int_wavg_fn <- function(df) {
  df |>
    left_join(gdp_base_vals |> rename(GDP = GDP_2015USD), by = "region") |>
    group_by(mat_key) |>
    summarise(int_2024_wavg = weighted.mean(int_2024, w = GDP, na.rm = TRUE), .groups = "drop")
}

# Collect the single global [0,1] draw per material into long format
collect_u <- function(mc, prefix, suffix, mat_keys) {
  cols <- paste0(prefix, mat_keys, suffix)
  mc |>
    dplyr::select(run_id, all_of(cols)) |>
    tidyr::pivot_longer(-run_id, names_to = "col", values_to = "u_global") |>
    mutate(mat_key = stringr::str_remove(stringr::str_remove(col, paste0("^", prefix)), paste0(suffix, "$"))) |>
    dplyr::select(run_id, mat_key, u_global)
}

# Endpoint intensity:  endpoint = G * (int_2024 / int_2024_wavg)^lambda, clipped.
#   G      = global convergence target (one draw, same across regions for the run)
#   lambda = gap persistence; 0 = full convergence to G, 1 = preserve regional ratio
build_endpoints <- function(bounds, intensity_bounds, mc, prefix, suffix, lambda_col, fixed_keys = character(0)) {
  mat_keys <- setdiff(sort(unique(bounds$mat_key)), fixed_keys)
  if (length(mat_keys) == 0) {
    return(NULL)
  }

  u_global <- collect_u(mc, prefix, suffix, mat_keys)
  int_wavg <- int_wavg_fn(bounds)
  lambda_df <- mc |> dplyr::select(run_id, lambda = all_of(lambda_col))

  G_df <- u_global |>
    left_join(intensity_bounds |> dplyr::select(mat_key, int_min, int_max), by = "mat_key") |>
    mutate(G = int_min + u_global * (int_max - int_min)) |>
    dplyr::select(run_id, mat_key, G)

  sampled <- bounds |>
    filter(mat_key %in% mat_keys) |>
    left_join(int_wavg, by = "mat_key") |>
    left_join(G_df, by = "mat_key", relationship = "many-to-many") |>
    left_join(lambda_df, by = "run_id") |>
    left_join(intensity_bounds |> dplyr::select(mat_key, int_min, int_max), by = "mat_key") |>
    mutate(
      endpoint = G * (int_2024 / pmax(int_2024_wavg, 1e-12))^lambda,
      endpoint = pmax(int_min, pmin(int_max, endpoint))
    ) |>
    dplyr::select(run_id, region, mat_key, int_2024, endpoint)

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
  intensity_bounds_biomass,
  mc_input_matrix,
  "intensity_",
  "_global",
  "gap_persistence_biomass",
  fixed_keys = "other_biomass"
) |>
  mutate(material_group = "biomass")

ep_fossil <- build_endpoints(
  fossil_bounds,
  intensity_bounds_fossil,
  mc_input_matrix,
  "intensity_",
  "_global",
  "gap_persistence_fossilfuels",
  fixed_keys = "other_fossil"
) |>
  mutate(material_group = "fossil_fuels")

ep_metal <- build_endpoints(
  metal_bounds,
  intensity_bounds_metal,
  mc_input_matrix,
  "intensity_",
  "_metalOres_global",
  "gap_persistence_metal_construction"
) |>
  mutate(material_group = "metal_ores")

ep_nonmet <- build_endpoints(
  nonmet_bounds,
  intensity_bounds_nonmet,
  mc_input_matrix,
  "intensity_",
  "_nonMetallic_global",
  "gap_persistence_metal_construction",
  fixed_keys = c("sl_products", "machinery")
) |>
  mutate(material_group = "nonmetallic_minerals")

intensity_ep <- bind_rows(ep_biomass, ep_fossil, ep_metal, ep_nonmet)
intensity_by_run <- split(intensity_ep, intensity_ep$run_id)

# -- Recycling endpoints + trajectories (convergence with linear ramp) ---------
recyc_conv_yr <- mc_input_matrix |>
  dplyr::select(run_id, u = recyc_convergence_yr_global) |>
  mutate(
    recyc_convergence_yr = as.integer(round(
      RECYC_CONVERGENCE_YR_MIN + u * (RECYC_CONVERGENCE_YR_MAX - RECYC_CONVERGENCE_YR_MIN)
    ))
  ) |>
  dplyr::select(run_id, recyc_convergence_yr)

lambda_rates <- mc_input_matrix |> dplyr::select(run_id, lambda = gap_persistence_rates)

# Same convergence shape as intensity, but on rates: ramp to endpoint by
# recyc_convergence_yr, hold after, clip to [min, max] throughout.
make_recycling_traj <- function(recycling_now, G_col, min_rate, max_rate) {
  wavg <- recycling_now |>
    left_join(gdp_base_vals |> rename(GDP = GDP_2015USD), by = "region") |>
    summarise(rate_mean = weighted.mean(rate_now, w = GDP, na.rm = TRUE)) |>
    pull(rate_mean)
  G_df <- mc_input_matrix |>
    dplyr::select(run_id, u = all_of(G_col)) |>
    mutate(G = min_rate + u * (max_rate - min_rate)) |>
    dplyr::select(run_id, G)
  ep <- recycling_now |>
    tidyr::crossing(run_id = seq_len(N_RUNS)) |>
    left_join(G_df, by = "run_id") |>
    left_join(lambda_rates, by = "run_id") |>
    left_join(recyc_conv_yr, by = "run_id") |>
    mutate(recycling_endpoint = pmax(min_rate, pmin(max_rate, G + lambda * (rate_now - wavg))))
  ep |>
    tidyr::crossing(year = YEARS_DSM) |>
    mutate(
      recycling_rate = if_else(
        year >= recyc_convergence_yr,
        recycling_endpoint,
        rate_now + (recycling_endpoint - rate_now) * (year - 2024L) / pmax(1L, recyc_convergence_yr - 2024L)
      ),
      recycling_rate = pmax(min_rate, pmin(max_rate, recycling_rate))
    ) |>
    dplyr::select(run_id, region, year, recycling_rate)
}

recycling_traj_fe <- make_recycling_traj(
  recycling_now_fe,
  "recycling_Fe_global",
  RECYCLING_RATE_FE_MIN,
  RECYCLING_RATE_FE_MAX
)
recycling_traj_nonfe <- make_recycling_traj(
  recycling_now_nonfe,
  "recycling_NonFe_global",
  RECYCLING_RATE_NONFE_MIN,
  RECYCLING_RATE_NONFE_MAX
)
recycling_by_run_fe <- split(recycling_traj_fe, recycling_traj_fe$run_id)
recycling_by_run_nonfe <- split(recycling_traj_nonfe, recycling_traj_nonfe$run_id)

# -- Downcycling endpoints + trajectories --------------------------------------
downcycling_long <- downcycling_now |>
  tidyr::pivot_longer(
    c(downcycling_buildings_now, downcycling_civil_infrastructure_now, downcycling_roads_now),
    names_to = "end_use",
    names_pattern = "downcycling_(.+)_now",
    values_to = "rate_now"
  )

downcycling_wavg <- downcycling_long |>
  left_join(gdp_base_vals |> rename(GDP = GDP_2015USD), by = "region") |>
  group_by(end_use) |>
  summarise(rate_mean = weighted.mean(rate_now, w = GDP, na.rm = TRUE), .groups = "drop")

downcycling_G <- mc_input_matrix |>
  dplyr::select(run_id, downcycling_buildings_global, downcycling_civil_infrastructure_global) |>
  tidyr::pivot_longer(-run_id, names_to = "end_use", values_to = "u", names_pattern = "downcycling_(.+)_global") |>
  mutate(G = DOWNCYCLING_MIN + u * (DOWNCYCLING_MAX - DOWNCYCLING_MIN)) |>
  dplyr::select(run_id, end_use, G)
# roads shares civil_infrastructure's endpoint (same super-category, no own draw)
downcycling_G <- bind_rows(
  downcycling_G,
  downcycling_G |> dplyr::filter(end_use == "civil_infrastructure") |> dplyr::mutate(end_use = "roads")
)

downcycling_ep <- downcycling_long |>
  left_join(downcycling_wavg, by = "end_use") |>
  left_join(downcycling_G, by = "end_use", relationship = "many-to-many") |>
  left_join(lambda_rates, by = "run_id") |>
  left_join(recyc_conv_yr, by = "run_id") |>
  mutate(downcycling_endpoint = pmax(DOWNCYCLING_MIN, pmin(DOWNCYCLING_MAX, G + lambda * (rate_now - rate_mean))))

downcycling_traj <- downcycling_ep |>
  tidyr::crossing(year = YEARS_DSM) |>
  mutate(
    downcycling_rate = if_else(
      year >= recyc_convergence_yr,
      downcycling_endpoint,
      rate_now + (downcycling_endpoint - rate_now) * (year - 2024L) / pmax(1L, recyc_convergence_yr - 2024L)
    ),
    downcycling_rate = pmax(DOWNCYCLING_MIN, pmin(DOWNCYCLING_MAX, downcycling_rate))
  ) |>
  dplyr::select(run_id, region, end_use, year, downcycling_rate)

downcycling_wide <- downcycling_traj |>
  tidyr::pivot_wider(names_from = end_use, values_from = downcycling_rate, names_prefix = "downcycling_") |>
  dplyr::select(run_id, region, year, downcycling_buildings, downcycling_civil_infrastructure, downcycling_roads)
downcycling_by_run <- split(downcycling_wide, downcycling_wide$run_id)

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
grade_params <- mc_input_matrix |>
  dplyr::select(run_id, grade_ore_fe_u, grade_ore_nonfe_u) |>
  mutate(
    grade_ore_fe = GRADE_ORE_FE_MIN + grade_ore_fe_u * (GRADE_ORE_FE_MAX - GRADE_ORE_FE_MIN),
    grade_ore_nonfe = GRADE_ORE_NONFE_MIN + grade_ore_nonfe_u * (GRADE_ORE_NONFE_MAX - GRADE_ORE_NONFE_MIN)
  ) |>
  dplyr::select(run_id, grade_ore_fe, grade_ore_nonfe)
grade_by_run <- split(grade_params, grade_params$run_id)

# -- Scalar params -------------------------------------------------------------
# >>> REMOVE upstream: `downcycling_bldg_to_roads_share` is now dead.
#     Buildings follows the civil pattern (total fraction split internally), so
#     the standalone buildings->roads share draw is no longer read here.
#     Delete its column from 01-Sampling.R's LHS matrix and from the Excel
#     parameter sheet + DOWNCYCLING_BLDG_TO_ROADS_MIN/MAX in 00-Parameters.R.
# >>> REMOVE upstream: SUB_FACTOR_METAL and MAX_RECYCLING_CAP (00-Parameters.R)
#     are no longer used (metals are recycling-only, no quality loss, no cap).
scalar_params <- mc_input_matrix |>
  dplyr::select(
    run_id,
    u2 = sub_factor_recycling_same,
    u2b = sub_factor_recycling_same_civil,
    u3 = max_secondary_roads,
    u4 = sub_factor_downcycling_roads
  ) |>
  mutate(
    sub_factor_recycling_same = SUB_FACTOR_RECYCLING_SAME_MIN +
      u2 * (SUB_FACTOR_RECYCLING_SAME_MAX - SUB_FACTOR_RECYCLING_SAME_MIN),
    sub_factor_recycling_same_civil = SUB_FACTOR_RECYCLING_SAME_CIVIL_MIN +
      u2b * (SUB_FACTOR_RECYCLING_SAME_CIVIL_MAX - SUB_FACTOR_RECYCLING_SAME_CIVIL_MIN),
    max_secondary_roads = MAX_SECONDARY_ROADS_MIN + u3 * (MAX_SECONDARY_ROADS_MAX - MAX_SECONDARY_ROADS_MIN),
    sub_factor_downcycling_roads = SUB_FACTOR_DOWNCYCLING_ROADS_MIN +
      u4 * (SUB_FACTOR_DOWNCYCLING_ROADS_MAX - SUB_FACTOR_DOWNCYCLING_ROADS_MIN)
  ) |>
  dplyr::select(
    run_id,
    sub_factor_recycling_same,
    sub_factor_recycling_same_civil,
    max_secondary_roads,
    sub_factor_downcycling_roads
  )
scalar_by_run <- split(scalar_params, scalar_params$run_id)

target_year_vec <- as.integer(round(
  TARGET_YEAR_MIN + mc_input_matrix$target_year_u * (TARGET_YEAR_MAX - TARGET_YEAR_MIN)
))
ssp_labels_vec <- mc_input_matrix$ssp_label
cat("  Trajectories rebuilt.\n")


# =============================================================================
# STEP 5: Single-run worker
# =============================================================================
run_one <- function(i) {
  ssp <- ssp_labels_vec[i]
  ie_i <- intensity_by_run[[i]]
  lp_i <- lifetime_by_run[[i]]
  rec_fe_i <- recycling_by_run_fe[[i]]
  rec_nonfe_i <- recycling_by_run_nonfe[[i]]
  dow_i <- downcycling_by_run[[i]]
  sc_i <- scalar_by_run[[i]]
  gr_i <- grade_by_run[[i]]

  pop_i <- pop_mat[[ssp]]
  gdppc_i <- gdppc_mat[[ssp]]

  yr_vec <- YEARS_DSM
  # alpha: 0->1 time weight; intensities ramp log-linearly (geometrically) from
  # 2024 value to endpoint as int_2024 * (endpoint / int_2024)^alpha
  alpha <- pmin(1, pmax(0, (yr_vec - 2024L) / (target_year_vec[i] - 2024L)))

  out_list <- list()

  # Helper to emit a Kaya flow row. For non-metals carrier == reported mass, so
  # the *_Mt_pure columns mirror the headline columns and spill is 0.
  emit_flow <- function(rg, mg, key, M_Mt) {
    data.frame(
      run_id = i,
      ssp_label = ssp,
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
  run_dsm_metal <- function(mat_label, age_lookup, rec_i, grade) {
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
        target_stock <- s2024 * pop_i[rg, ] * gdppc_i[rg, ] * stock_intensity_index
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
    if (length(sr_rows) == 0) {
      return(NULL)
    }
    sr <- do.call(rbind, sr_rows)

    sr <- merge(sr, rec_i, by = c("region", "year"), all.x = TRUE)
    recyc_rate <- sr$recycling_rate
    recyc_rate[is.na(recyc_rate)] <- 0

    prod_metal <- pmax(0, sr$production_Mt) # guard against DSM negatives
    recoverable <- sr$waste_Mt * recyc_rate # metal mass, no quality loss
    secondary_metal <- pmin(recoverable, prod_metal) # cannot exceed demand
    spilled_metal <- recoverable - secondary_metal # recyclable but unabsorbed
    primary_metal <- prod_metal - secondary_metal # >= 0 by construction

    grade_safe <- max(grade, 1e-6)

    data.frame(
      run_id = i,
      ssp_label = ssp,
      region = sr$region,
      material_group = if (mat_label == "Metal_Fe") "metal_fe" else "metal_nonfe",
      material_key = SUB_USE_LABELS[sr$sub_use],
      year = sr$year,
      total_inflow_Mt = sr$replacement_Mt + sr$new_additions_Mt,
      in_use_stock_Mt = sr$total_stock_Mt,
      primary_consumption_Mt = primary_metal / grade_safe, # ore extracted
      secondary_supply_Mt = secondary_metal / grade_safe, # ore avoided
      primary_consumption_Mt_pure = primary_metal,
      secondary_supply_Mt_pure = secondary_metal,
      secondary_spilled_Mt = spilled_metal,
      new_additions_Mt = sr$new_additions_Mt / grade_safe, # ore
      replacement_Mt = sr$replacement_Mt / grade_safe,
      waste_Mt = sr$waste_Mt / grade_safe,
      new_additions_Mt_pure = sr$new_additions_Mt, # metal
      replacement_Mt_pure = sr$replacement_Mt,
      waste_Mt_pure = sr$waste_Mt,
      target_stock_Mt = sr$target_stock_Mt
    )
  }

  fe_out <- run_dsm_metal("Metal_Fe", age_lookup_fe, rec_fe_i, gr_i$grade_ore_fe)
  nonfe_out <- run_dsm_metal("Metal_NonFe", age_lookup_nonfe, rec_nonfe_i, gr_i$grade_ore_nonfe)
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
      target_stock <- s2024 * pop_i[rg, ] * gdppc_i[rg, ] * stock_intensity_index
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

  if (length(nm_rows) > 0) {
    nm_sr <- do.call(rbind, nm_rows)
    nm_sr <- merge(nm_sr, dow_i, by = c("region", "year"), all.x = TRUE)

    is_bldg <- nm_sr$super_key == "buildings"
    is_civil <- nm_sr$super_key == "civil"
    is_roads <- nm_sr$sub_use == "roads"

    # Same-sector recovery (B->B, CI->CI). Buildings now follows the civil
    # pattern: downcycling_buildings is the TOTAL recovered fraction, split
    # internally by sub_factor_recycling_same into same-sector vs roads.
    recovered_same <- ifelse(
      is_bldg,
      nm_sr$waste_Mt * nm_sr$downcycling_buildings * sc_i$sub_factor_recycling_same,
      ifelse(
        is_civil,
        nm_sr$waste_Mt * nm_sr$downcycling_civil_infrastructure * sc_i$sub_factor_recycling_same_civil,
        0
      )
    )

    # Cross-sector material sent toward road demand = the (1 - same_split)
    # complement of each source's total downcycled fraction. Mass-conserving:
    # same-sector + roads share = total downcycled fraction, for both bldg & civil.
    sent_to_roads <- ifelse(
      is_bldg,
      nm_sr$waste_Mt *
        nm_sr$downcycling_buildings *
        (1 - sc_i$sub_factor_recycling_same) *
        sc_i$sub_factor_downcycling_roads,
      ifelse(
        is_roads,
        nm_sr$waste_Mt * nm_sr$downcycling_roads * sc_i$sub_factor_downcycling_roads,
        ifelse(
          is_civil & !is_roads,
          nm_sr$waste_Mt *
            nm_sr$downcycling_civil_infrastructure *
            (1 - sc_i$sub_factor_recycling_same_civil) *
            sc_i$sub_factor_downcycling_roads,
          0
        )
      )
    )

    # Pool all road-bound secondary at region x year, cap against road demand;
    # excess beyond the cap is spilled (auditable, not silently dropped).
    rd <- data.frame(
      region = nm_sr$region,
      year = nm_sr$year,
      sent_to_roads = sent_to_roads,
      prod_roads = ifelse(is_roads, nm_sr$production_Mt, 0)
    ) |>
      dplyr::group_by(region, year) |>
      dplyr::summarise(sec_avail = sum(sent_to_roads), road_prod = sum(prod_roads), .groups = "drop") |>
      dplyr::mutate(
        sec_for_roads = pmin(sec_avail, sc_i$max_secondary_roads * road_prod),
        spill_roads = sec_avail - sec_for_roads
      ) |>
      dplyr::select(region, year, sec_for_roads, spill_roads)

    nm_sr <- merge(nm_sr, rd, by = c("region", "year"), all.x = TRUE)

    # Same-sector spill: recovery capped at that sub_use's own production.
    recovered_same_raw <- recovered_same
    recovered_same <- pmin(recovered_same, nm_sr$production_Mt)
    spill_same <- recovered_same_raw - recovered_same

    # Credit recovery: same-sector to bldg/civil rows, pooled roads to roads rows.
    recovered <- ifelse(
      is_bldg,
      recovered_same,
      ifelse(
        is_roads,
        ifelse(is.na(nm_sr$sec_for_roads), 0, nm_sr$sec_for_roads),
        ifelse(is_civil & !is_roads, recovered_same, 0)
      )
    )
    # Roads-pool spill is credited once, on roads rows only (already region-pooled).
    spilled <- spill_same + ifelse(is_roads, ifelse(is.na(nm_sr$spill_roads), 0, nm_sr$spill_roads), 0)

    nm_sr$production_Mt <- pmax(0, nm_sr$production_Mt - recovered)

    out_list[[length(out_list) + 1L]] <- data.frame(
      run_id = i,
      ssp_label = ssp,
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

  # Per-run physical-sanity flags (kept, not filtered) -> screen pathological
  # runs downstream before SHAP/Sobol rather than letting them contaminate.
  out$run_neg_primary <- any(out$primary_consumption_Mt < -1e-9, na.rm = TRUE)
  out$run_total_spill_Mt <- sum(out$secondary_spilled_Mt, na.rm = TRUE)
  out
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

results <- bind_rows(results_list)


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

cat("\n=== MC run complete ===\n")

# EoF
