## =============================================================================
## 02b-DeterministicRuns.R
## Deterministic simulation: one run per SSP using central (midpoint) values
## for all sampled parameters. Intended as the "SSP spine" to overlay with
## Monte Carlo confidence intervals in figures.
##
## All u draws are fixed at 0.5 (midpoint of each [0,1] LHS range), which maps
## each parameter to the midpoint of its sampling bounds.
##
## Flow model (Kaya):  biomass + fossil fuels
## Stock model (DSM):  Metal_Fe + Metal_NonFe + nonmetallic_minerals
##
## Output: Results/deterministic_results.parquet
##   Same schema as Results/MC/mc_results.parquet.
##   one run_id per SSP in SSP_SAMPLED (ssp_label column carries the SSP).
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
source("Scripts/00-Functions/dsm_functions.R", encoding = "UTF-8")

library(arrow)

cat("=== Deterministic Runner (central values × 5 SSPs) ===\n\n")

# -- Label maps ---------------------------------------------------------------
BIOMASS_LABEL <- c(
  "crops" = "Crops",
  "grazed_biomass" = "Grazed biomass and fodder crops",
  "other_biomass" = "Other biomass",
  "wood" = "Wood"
)
FOSSIL_LABEL <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum", "other_fossil" = "Other fossil fuels")

STOCK_ENDUSE_LABEL <- c(
  "buildings" = "Buildings",
  "civil" = "Civil infrastructure",
  "sl_products" = "Short-lived products",
  "machinery" = "Machinery"
)

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

SUB_USE_BY_MATKEY <- list(
  "buildings" = c("residential", "non_residential"),
  "civil" = c("roads", "civil_engineering"),
  "sl_products" = c("durables", "packaging"),
  "machinery" = c("machinery_group", "vehicles_group")
)

SUPER_BY_SUBUSE <- c(
  "residential" = "buildings",
  "non_residential" = "buildings",
  "roads" = "civil",
  "civil_engineering" = "civil",
  "machinery_group" = "machinery",
  "vehicles_group" = "machinery",
  "durables" = "sl_products",
  "packaging" = "sl_products"
)


# =============================================================================
# STEP 1: Build central input matrix (5 rows, one per SSP, all u = 0.5)
# =============================================================================
cat("STEP 1: Build central input matrix\n")

BIOMASS_KEYS <- c(
  "Crops" = "crops",
  "Grazed biomass and fodder crops" = "grazed_biomass",
  "Wood" = "wood",
  "Other biomass" = "other_biomass"
)

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
  mutate(
    mat_key = case_when(
      str_detect(tolower(material_key), "build") ~ "buildings",
      str_detect(tolower(material_key), "civil") ~ "civil",
      str_detect(tolower(material_key), "short") ~ "sl_products",
      str_detect(tolower(material_key), "machinery") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(mat_key)) |>
  dplyr::select(region, mat_key, int_2024)

nonmet_bounds <- read_intensity_sheet(ASSUMPTIONS_FILE, "NonMetallicMinerals", skip_rows = 1) |>
  mutate(
    mat_key = case_when(
      str_detect(tolower(material_key), "build") ~ "buildings",
      str_detect(tolower(material_key), "civil") ~ "civil",
      str_detect(tolower(material_key), "short") ~ "sl_products",
      str_detect(tolower(material_key), "machinery") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(mat_key)) |>
  dplyr::select(region, mat_key, int_2024)

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

# Region-specific intensity ratio bounds (endpoint / 2024), built in 01-Sampling.R
flow_ratio_bounds <- readr::read_csv("Parameters/Simulation/flow_ratio_bounds.csv", show_col_types = FALSE)
stock_ratio_bounds <- readr::read_csv("Parameters/Simulation/stock_ratio_bounds.csv", show_col_types = FALSE)

# Derive column names from the bounds tables (consistent with 01-Sampling.R)
biomass_mats_sampled <- sort(unique(flow_ratio_bounds$mat_key[flow_ratio_bounds$material_group == "biomass"]))
fossil_mats_sampled <- sort(unique(flow_ratio_bounds$mat_key[flow_ratio_bounds$material_group == "fossil_fuels"]))
stock_combos <- sort(c(
  paste0(sort(unique(stock_ratio_bounds$mat_key[stock_ratio_bounds$material_group == "metal_ores"])), "_metalOres"),
  paste0(
    sort(setdiff(
      unique(stock_ratio_bounds$mat_key[stock_ratio_bounds$material_group == "nonmetallic_minerals"]),
      c("sl_products", "machinery")
    )),
    "_nonMetallic"
  )
))
lifetime_super_cats <- sort(unique(LIFETIME_SAMPLE_PARAMS$super_category))

u_col_names <- c(
  "target_year_u",
  paste0("intensity_", biomass_mats_sampled, "_global"),
  paste0("intensity_", fossil_mats_sampled, "_global"),
  paste0("intensity_", stock_combos, "_global"),
  "recycling_Fe_global",
  "recycling_NonFe_global",
  "recyc_convergence_yr_global",
  "downcycling_buildings_global",
  "downcycling_civil_infrastructure_global",
  "sub_factor_recycling_same",
  "sub_factor_recycling_same_civil",
  "max_secondary_roads",
  "sub_factor_downcycling_roads",
  "grade_ore_fe_u",
  "grade_ore_nonfe_u",
  paste0("lifetime_mean_", lifetime_super_cats),
  paste0("lifetime_k_", lifetime_super_cats)
)

N_RUNS <- length(SSP_SAMPLED) # one run per sampled SSP (SSP4 excluded, see 00-CommonParameters.R)
mc_input_matrix <- tibble::tibble(run_id = 1:N_RUNS, ssp_label = SSP_SAMPLED)
for (col in u_col_names) {
  mc_input_matrix[[col]] <- 0.5
}
ssp_labels_vec <- mc_input_matrix$ssp_label

cat("  Matrix:", nrow(mc_input_matrix), "×", ncol(mc_input_matrix), "\n")
cat("  SSP labels:", paste(ssp_labels_vec, collapse = ", "), "\n")


# =============================================================================
# STEP 2: Static data (SSP drivers, GDP, UNEP anchors, age profiles)
# =============================================================================
cat("\nSTEP 2: Load static data\n")

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

gdp_full <- ssp_drivers |>
  filter(variable == "GDP|PPP", year >= 2024L, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_index = index) |>
  left_join(gdp_base_vals, by = "region") |>
  mutate(gdp_billion_usd = GDP_2015USD * gdp_index / 1e9)

unep_dmc <- readr::read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
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

anchor_year <- max(unep_dmc$year[unep_dmc$year <= 2024L])
m_2024_biomass <- unep_dmc |>
  filter(year == anchor_year, material_category %in% c(unname(BIOMASS_LABEL), "Crop Residues")) |>
  dplyr::select(region = Region, unep_label = material_category, M_2024_Mt = DMC_Mt)
m_2024_fossil <- unep_dmc |>
  filter(year == anchor_year, material_category %in% unname(FOSSIL_LABEL)) |>
  mutate(mat_key = names(FOSSIL_LABEL)[match(material_category, FOSSIL_LABEL)]) |>
  dplyr::select(region = Region, mat_key, M_2024_Mt = DMC_Mt)

stock_2024_raw <- readr::read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |> rename(region = Region)
stock_2024_sub <- stock_2024_raw |>
  dplyr::select(material, region, sub_use, stock_Mt) |>
  split(~material) |>
  lapply(function(md) {
    split(md, ~region) |> lapply(function(rd) setNames(rd$stock_Mt, rd$sub_use))
  })

# Historical 2024 stock log-growth, log(S_2024 / S_2023), from the historical DSM
# -> nested lookup [material][region][sub_use]; anchors the seam blend (same as 02-RunSimulations.R)
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

regions_vec <- sort(unique(gdp_full$region))
ssp_labels_all <- sort(unique(gdp_full$scenario))

make_mat <- function(df, val_col) {
  out <- list()
  for (s in ssp_labels_all) {
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

make_m2024_vec <- function(df, key_col, key_val) {
  v <- setNames(numeric(length(regions_vec)), regions_vec)
  sub <- df |> filter(.data[[key_col]] == key_val)
  v[sub$region] <- sub$M_2024_Mt
  v
}

cat("  Static data loaded.\n")


# =============================================================================
# STEP 3: Rebuild trajectories from central matrix
# (Same math as 02-RunSimulations.R STEP 4)
# =============================================================================
cat("\nSTEP 3: Rebuild trajectories from central values\n")

# Endpoint = int_2024 * ratio, ratio = midpoint (u = 0.5) of the region's own
# ratio bounds; flows use the run's pure SSP bounds
build_endpoints <- function(bounds, ratio_bounds, mc, fixed_keys = character(0)) {
  mat_keys <- setdiff(sort(unique(bounds$mat_key)), fixed_keys)
  if (length(mat_keys) == 0) {
    return(NULL)
  }
  if ("ssp" %in% names(ratio_bounds)) {
    ratio_df <- mc |>
      dplyr::select(run_id, ssp = ssp_label) |>
      left_join(ratio_bounds, by = "ssp", relationship = "many-to-many")
  } else {
    ratio_df <- ratio_bounds |> tidyr::crossing(run_id = mc$run_id)
  }
  ratio_df <- ratio_df |>
    mutate(ratio = (ratio_min + ratio_max) / 2) |>
    dplyr::select(run_id, region, mat_key, ratio)
  sampled <- bounds |>
    filter(mat_key %in% mat_keys) |>
    left_join(ratio_df, by = c("region", "mat_key")) |>
    mutate(endpoint = int_2024 * ratio) |>
    dplyr::select(run_id, region, mat_key, int_2024, endpoint)
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
  fixed_keys = "other_biomass"
) |>
  mutate(material_group = "biomass")

ep_fossil <- build_endpoints(
  fossil_bounds,
  flow_ratio_bounds |> filter(material_group == "fossil_fuels"),
  mc_input_matrix,
  fixed_keys = "other_fossil"
) |>
  mutate(material_group = "fossil_fuels")

ep_metal <- build_endpoints(
  metal_bounds,
  stock_ratio_bounds |> filter(material_group == "metal_ores"),
  mc_input_matrix
) |>
  mutate(material_group = "metal_ores")

ep_nonmet <- build_endpoints(
  nonmet_bounds,
  stock_ratio_bounds |> filter(material_group == "nonmetallic_minerals"),
  mc_input_matrix,
  fixed_keys = c("sl_products", "machinery")
) |>
  mutate(material_group = "nonmetallic_minerals")

intensity_ep <- bind_rows(ep_biomass, ep_fossil, ep_metal, ep_nonmet)
intensity_by_run <- split(intensity_ep, intensity_ep$run_id)

recyc_conv_yr <- mc_input_matrix |>
  dplyr::select(run_id, u = recyc_convergence_yr_global) |>
  mutate(
    recyc_convergence_yr = as.integer(round(
      RECYC_CONVERGENCE_YR_MIN + u * (RECYC_CONVERGENCE_YR_MAX - RECYC_CONVERGENCE_YR_MIN)
    ))
  ) |>
  dplyr::select(run_id, recyc_convergence_yr)

# Endpoint rate = midpoint of the absolute Parameters-sheet bounds, same for all regions.
# 2024 anchor is the Excel end-of-life recycling rate itself (rate_now, from
# the "Recycling_EOL" sheet -- fraction of end-of-life waste recovered). This
# is applied to waste_Mt downstream, not to production, so it stays a
# physical recovery-from-scrap rate throughout. (Previously anchored to
# Parameters/Intermediate/nonprimary_share_2024.csv, an empirical share of
# PRODUCTION that bundles domestic recycling with embodied net imports of
# already-processed metal -- a different, trade-based quantity that does not
# belong in a waste-based recycling rate; this model makes no trade
# assumptions, so that anchor has been dropped. See Scripts/04-Simulation/
# 02-RunSimulations.R for the matching MC-runner fix.)
make_recycling_traj <- function(recycling_now, G_col, min_rate, max_rate, mat_label) {
  G_df <- mc_input_matrix |>
    dplyr::select(run_id, u = all_of(G_col)) |>
    mutate(G = min_rate + u * (max_rate - min_rate)) |>
    dplyr::select(run_id, G)
  recycling_now <- recycling_now |> mutate(anchor_2024 = rate_now)
  ep <- recycling_now |>
    tidyr::crossing(run_id = seq_len(N_RUNS)) |>
    left_join(G_df, by = "run_id") |>
    left_join(recyc_conv_yr, by = "run_id") |>
    mutate(recycling_endpoint = G)
  ep |>
    tidyr::crossing(year = YEARS_DSM) |>
    mutate(
      recycling_rate = if_else(
        year >= recyc_convergence_yr,
        recycling_endpoint,
        anchor_2024 + (recycling_endpoint - anchor_2024) * (year - 2024L) / pmax(1L, recyc_convergence_yr - 2024L)
      )
    ) |>
    dplyr::select(run_id, region, year, recycling_rate)
}

recycling_traj_fe <- make_recycling_traj(
  recycling_now_fe,
  "recycling_Fe_global",
  RECYCLING_RATE_FE_MIN,
  RECYCLING_RATE_FE_MAX,
  "Metal_Fe"
)
recycling_traj_nonfe <- make_recycling_traj(
  recycling_now_nonfe,
  "recycling_NonFe_global",
  RECYCLING_RATE_NONFE_MIN,
  RECYCLING_RATE_NONFE_MAX,
  "Metal_NonFe"
)
recycling_by_run_fe <- split(recycling_traj_fe, recycling_traj_fe$run_id)
recycling_by_run_nonfe <- split(recycling_traj_nonfe, recycling_traj_nonfe$run_id)

downcycling_long <- downcycling_now |>
  tidyr::pivot_longer(
    c(downcycling_buildings_now, downcycling_civil_infrastructure_now, downcycling_roads_now),
    names_to = "end_use",
    names_pattern = "downcycling_(.+)_now",
    values_to = "rate_now"
  )
downcycling_G <- mc_input_matrix |>
  dplyr::select(run_id, downcycling_buildings_global, downcycling_civil_infrastructure_global) |>
  tidyr::pivot_longer(-run_id, names_to = "end_use", values_to = "u", names_pattern = "downcycling_(.+)_global") |>
  mutate(G = DOWNCYCLING_MIN + u * (DOWNCYCLING_MAX - DOWNCYCLING_MIN)) |>
  dplyr::select(run_id, end_use, G)
# roads shares civil_infrastructure endpoint (same super-category, no separate LHS draw)
downcycling_G <- bind_rows(
  downcycling_G,
  downcycling_G |> dplyr::filter(end_use == "civil_infrastructure") |> dplyr::mutate(end_use = "roads")
)
downcycling_ep <- downcycling_long |>
  left_join(downcycling_G, by = "end_use", relationship = "many-to-many") |>
  left_join(recyc_conv_yr, by = "run_id") |>
  mutate(downcycling_endpoint = G)
downcycling_traj <- downcycling_ep |>
  tidyr::crossing(year = YEARS_DSM) |>
  mutate(
    downcycling_rate = if_else(
      year >= recyc_convergence_yr,
      downcycling_endpoint,
      rate_now + (downcycling_endpoint - rate_now) * (year - 2024L) / pmax(1L, recyc_convergence_yr - 2024L)
    )
  ) |>
  dplyr::select(run_id, region, end_use, year, downcycling_rate)
downcycling_wide <- downcycling_traj |>
  tidyr::pivot_wider(names_from = end_use, values_from = downcycling_rate, names_prefix = "downcycling_") |>
  dplyr::select(run_id, region, year, downcycling_buildings, downcycling_civil_infrastructure, downcycling_roads)
downcycling_by_run <- split(downcycling_wide, downcycling_wide$run_id)

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

# grade_ore_fe/nonfe is the per-run sampled TARGET (endpoint); each run ramps
# linearly from the 2024 baseline (GRADE_ORE_*_NOW) to this target, over the
# same convergence year drawn for intensity (target_year_vec, built below).
GRADE_ORE_FE_NOW <- (GRADE_ORE_FE_MIN + GRADE_ORE_FE_MAX) / 2
GRADE_ORE_NONFE_NOW <- (GRADE_ORE_NONFE_MIN + GRADE_ORE_NONFE_MAX) / 2

grade_params <- mc_input_matrix |>
  dplyr::select(run_id, grade_ore_fe_u, grade_ore_nonfe_u) |>
  mutate(
    grade_ore_fe = GRADE_ORE_FE_MIN + grade_ore_fe_u * (GRADE_ORE_FE_MAX - GRADE_ORE_FE_MIN),
    grade_ore_nonfe = GRADE_ORE_NONFE_MIN + grade_ore_nonfe_u * (GRADE_ORE_NONFE_MAX - GRADE_ORE_NONFE_MIN)
  ) |>
  dplyr::select(run_id, grade_ore_fe, grade_ore_nonfe)
grade_by_run <- split(grade_params, grade_params$run_id)

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

cat("  Trajectories rebuilt.\n")


# =============================================================================
# STEP 4: Single-run worker (identical logic to 02-RunSimulations.R)
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
  # Hermite slope basis (0 at both ends): stock log-intensity += h10 * g_int * span
  h10 <- alpha^3 - 2 * alpha^2 + alpha
  span_yrs <- target_year_vec[i] - 2024L

  # Ore grade: linear ramp from the 2024 baseline to the sampled target, using
  # the same convergence year (alpha) as intensity.
  grade_ore_fe_traj <- setNames(GRADE_ORE_FE_NOW + (gr_i$grade_ore_fe - GRADE_ORE_FE_NOW) * alpha, as.character(yr_vec))
  grade_ore_nonfe_traj <- setNames(
    GRADE_ORE_NONFE_NOW + (gr_i$grade_ore_nonfe - GRADE_ORE_NONFE_NOW) * alpha,
    as.character(yr_vec)
  )

  out_list <- list()

  # ── Biomass (Kaya) ----------------------------------------------------------
  bio <- ie_i |>
    filter(material_group == "biomass") |>
    mutate(unep_label = BIOMASS_LABEL[mat_key]) |>
    left_join(m_2024_biomass, by = c("region", "unep_label"))

  for (j in seq_len(nrow(bio))) {
    rg <- bio$region[j]
    ul <- bio$unep_label[j]
    int_2024_j <- max(bio$int_2024[j], 1e-12)
    endpoint_j <- max(bio$endpoint[j], 1e-12)
    mg_ratio <- int_2024_j * (endpoint_j / int_2024_j)^alpha
    mg_index <- mg_ratio / max(abs(bio$int_2024[j]), 1e-12)
    M_Mt <- bio$M_2024_Mt[j] * pop_i[rg, ] * gdppc_i[rg, ] * mg_index
    if (any(is.na(M_Mt))) {
      next
    }
    out_list[[length(out_list) + 1L]] <- data.frame(
      run_id = i,
      ssp_label = ssp,
      region = rg,
      material_group = "biomass",
      material_key = ul,
      year = yr_vec,
      total_inflow_Mt = M_Mt,
      in_use_stock_Mt = NA_real_,
      primary_consumption_Mt = M_Mt,
      secondary_supply_Mt = 0,
      primary_consumption_Mt_pure = M_Mt,
      secondary_supply_Mt_pure = 0
    )
  }

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
    out_list[[length(out_list) + 1L]] <- data.frame(
      run_id = i,
      ssp_label = ssp,
      region = rg,
      material_group = "biomass",
      material_key = "Crop Residues",
      year = yr_vec,
      total_inflow_Mt = M_Mt,
      in_use_stock_Mt = NA_real_,
      primary_consumption_Mt = M_Mt,
      secondary_supply_Mt = 0,
      primary_consumption_Mt_pure = M_Mt,
      secondary_supply_Mt_pure = 0
    )
  }

  # ── Fossil (Kaya) -----------------------------------------------------------
  fos <- ie_i |> filter(material_group == "fossil_fuels") |> left_join(m_2024_fossil, by = c("region", "mat_key"))
  for (j in seq_len(nrow(fos))) {
    rg <- fos$region[j]
    mk <- fos$mat_key[j]
    if (is.na(fos$M_2024_Mt[j])) {
      next
    }
    int_2024_j <- max(fos$int_2024[j], 1e-12)
    endpoint_j <- max(fos$endpoint[j], 1e-12)
    mg_ratio <- int_2024_j * (endpoint_j / int_2024_j)^alpha
    mg_index <- mg_ratio / max(abs(fos$int_2024[j]), 1e-12)
    M_Mt <- fos$M_2024_Mt[j] * pop_i[rg, ] * gdppc_i[rg, ] * mg_index
    out_list[[length(out_list) + 1L]] <- data.frame(
      run_id = i,
      ssp_label = ssp,
      region = rg,
      material_group = "fossil_fuels",
      material_key = FOSSIL_LABEL[mk],
      year = yr_vec,
      total_inflow_Mt = M_Mt,
      in_use_stock_Mt = NA_real_,
      primary_consumption_Mt = M_Mt,
      secondary_supply_Mt = 0,
      primary_consumption_Mt_pure = M_Mt,
      secondary_supply_Mt_pure = 0
    )
  }

  # ── DSM for one metal material (Fe or NonFe) --------------------------------
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
      stock_intensity <- int_2024_j * (endpoint_j / int_2024_j)^alpha
      stock_intensity_index <- stock_intensity / max(abs(ie_g$int_2024[j]), 1e-12)
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
    if (length(sr_rows) == 0) {
      return(NULL)
    }
    sr <- do.call(rbind, sr_rows)
    sr <- merge(sr, rec_i, by = c("region", "year"), all.x = TRUE)
    recycling_rate <- sr$recycling_rate
    recycling_rate[is.na(recycling_rate)] <- 0
    prod_metal <- pmax(0, sr$production_Mt)
    waste_metal <- pmax(0, sr$waste_Mt)
    # Secondary = the recycling rate applied to actual end-of-life waste (not
    # production), capped at what current production can absorb; recovered
    # scrap beyond that is spilled (no trade/import assumption -- this model
    # only tracks domestic recovery from local waste).
    recovered_raw <- waste_metal * recycling_rate
    recovered <- pmin(recovered_raw, prod_metal)
    spilled <- recovered_raw - recovered
    primary_metal_Mt <- prod_metal - recovered # always >= 0: recovered is capped at prod_metal
    grade_safe <- pmax(grade_traj[as.character(sr$year)], 1e-6)
    primary_ore_Mt <- primary_metal_Mt / grade_safe
    secondary_ore_Mt <- recovered / grade_safe
    data.frame(
      run_id = i,
      ssp_label = ssp,
      region = sr$region,
      material_group = if (mat_label == "Metal_Fe") "metal_fe" else "metal_nonfe",
      material_key = SUB_USE_LABELS[sr$sub_use],
      year = sr$year,
      total_inflow_Mt = sr$replacement_Mt + sr$new_additions_Mt,
      in_use_stock_Mt = sr$total_stock_Mt,
      primary_consumption_Mt = primary_ore_Mt, # ore extracted
      secondary_supply_Mt = secondary_ore_Mt, # ore avoided; recycled from end-of-life waste
      primary_consumption_Mt_pure = primary_metal_Mt,
      secondary_supply_Mt_pure = recovered,
      secondary_spilled_Mt = spilled / grade_safe, # recovered scrap beyond what current demand could absorb
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

  # ── Non-metallic minerals: DSM + downcycling cascade -----------------------
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
    stock_intensity <- int_2024_j * (endpoint_j / int_2024_j)^alpha
    stock_intensity_index <- stock_intensity / max(abs(ie_g_nm$int_2024[j]), 1e-12)
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
        production_Mt = res$production,
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

    # Same-sector recovery: buildings (B→B) and civil infra (CI→CI)
    recovered_same <- ifelse(
      is_bldg,
      nm_sr$waste_Mt * nm_sr$downcycling_buildings * sc_i$sub_factor_recycling_same,
      ifelse(
        is_civil,
        nm_sr$waste_Mt * nm_sr$downcycling_civil_infrastructure * sc_i$sub_factor_recycling_same_civil,
        0
      )
    )

    # Cross-sector material sent toward road demand; buildings follows civil pattern
    # (downcycling_buildings is total recovered, split by sub_factor_recycling_same)
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

    rd <- data.frame(
      region = nm_sr$region,
      year = nm_sr$year,
      sent_to_roads = sent_to_roads,
      prod_roads = ifelse(is_roads, nm_sr$production_Mt, 0)
    ) |>
      dplyr::group_by(region, year) |>
      dplyr::summarise(sec_avail = sum(sent_to_roads), road_prod = sum(prod_roads), .groups = "drop") |>
      dplyr::mutate(sec_for_roads = pmin(sec_avail, sc_i$max_secondary_roads * road_prod)) |>
      dplyr::select(region, year, sec_for_roads)

    nm_sr <- merge(nm_sr, rd, by = c("region", "year"), all.x = TRUE)

    recovered_same <- pmin(recovered_same, nm_sr$production_Mt)
    recovered <- ifelse(
      is_bldg,
      recovered_same,
      ifelse(
        is_roads,
        ifelse(is.na(nm_sr$sec_for_roads), 0, nm_sr$sec_for_roads),
        ifelse(is_civil & !is_roads, recovered_same, 0)
      )
    )
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
      new_additions_Mt = nm_sr$new_additions_Mt,
      replacement_Mt = nm_sr$replacement_Mt,
      waste_Mt = nm_sr$waste_Mt,
      new_additions_Mt_pure = nm_sr$new_additions_Mt, # carrier == mass (no ore conv.)
      replacement_Mt_pure = nm_sr$replacement_Mt,
      waste_Mt_pure = nm_sr$waste_Mt,
      target_stock_Mt = nm_sr$target_stock_Mt
    )
  }

  bind_rows(out_list)
}


# =============================================================================
# STEP 5: Run sequentially (5 SSP deterministic runs)
# =============================================================================
cat("\nSTEP 5: Run", N_RUNS, "deterministic simulations\n")

t0 <- proc.time()
results_list <- lapply(seq_len(N_RUNS), function(i) {
  cat("  SSP", ssp_labels_vec[i], "...")
  r <- run_one(i)
  cat(" done\n")
  r
})
elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  Done in %.0f s\n", elapsed))

results <- bind_rows(results_list)


# =============================================================================
# STEP 6: Save
# =============================================================================
cat("\nSTEP 6: Save\n")
arrow::write_parquet(results, "Results/deterministic_results.parquet")
cat("  Saved:", nrow(results), "rows to Results/deterministic_results.parquet\n")
write_csv(results, "Results/deterministic_results.csv")
cat("\n=== Deterministic run complete ===\n")

# EoF
