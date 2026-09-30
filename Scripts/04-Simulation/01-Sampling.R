## =============================================================================
## 01-Sampling.R
## Latin Hypercube Sampling for Monte Carlo simulation.
##
## Builds region-specific intensity bounds (ratios endpoint / 2024), draws
## N_RUNS LHS samples in [0,1] for every sampled parameter, saves the raw draw
## matrix and the bounds tables.
##
## Outputs:
##   Parameters/Simulation/mc_input_matrix.csv
##     columns: run_id, ssp_u, <one column per sampled parameter, all in [0,1]>
##   Parameters/Simulation/flow_ratio_bounds.csv   (region, material_group, mat_key, ssp, ratio_min, ratio_max)
##   Parameters/Simulation/stock_ratio_bounds.csv  (region, material_group, mat_key, ratio_min, ratio_max)
##   Parameters/Simulation/r10_region_weights.csv  (region, r10, weight = share of region's 2024 GDP in r10)
##   Parameters/Simulation/intensity_world_bounds.csv (param, bound_min, bound_max, int_2024; world kg/$,
##     2024-GDP-weighted -- display only, for figures that express a draw u in kg/$)
##
## 02-RunSimulations.R reads this matrix + the bounds tables + the Excel 2024
## values to reconstruct trajectories on the fly. Each [0,1] draw is shared by
## all regions and mapped into each region's own [ratio_min, ratio_max].
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

# Step 1: Flow-intensity ratio bounds (ScenarioMIP R10 -> model regions) ----------

cat("Step 1: flow-intensity ratio bounds from ScenarioMIP summary\n")

# ScenarioMIP /GDP variable -> model flow material key (other_biomass,
# other_fossil stay fixed at 2024; Crop Residues follows crops in 02)
FLOW_VAR_MAP <- c(
  "coal_extraction_gdp" = "coal",
  "oil_extraction_gdp" = "oil",
  "gas_extraction_gdp" = "gas",
  "forestry_production_gdp" = "wood",
  "agri_crops_gdp" = "crops",
  "agri_livestock_gdp" = "grazed_biomass"
)
FLOW_GROUP <- c(
  "coal" = "fossil_fuels",
  "oil" = "fossil_fuels",
  "gas" = "fossil_fuels",
  "wood" = "biomass",
  "crops" = "biomass",
  "grazed_biomass" = "biomass"
)

# Fixed (post-fix) Min/Max of the 2060/2025 ratio per R10 region x SSP
ratio_r10 <- readr::read_csv("Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv", show_col_types = FALSE) |>
  dplyr::filter(Variable %in% names(FLOW_VAR_MAP), Region != "World") |>
  dplyr::transmute(mat_key = unname(FLOW_VAR_MAP[Variable]), r10 = Region, ssp = SSP, ratio_min = Min, ratio_max = Max)

# Keep only sampled SSPs (SSP_SAMPLED, 00-CommonParameters.R; SSP4 excluded).
# If SSP4 is re-added, its rows already carry SSP2 bounds (04_summary_ratio.R Fix 2).
ratio_r10 <- ratio_r10 |> dplyr::filter(ssp %in% SSP_SAMPLED)

# Country 2024 GDP (World Bank, constant 2015 USD -- same source as gdp_region.csv);
# latest available year <= 2024 when 2024 is missing
gdp_wb <- suppressWarnings(readxl::read_excel(
  "Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls",
  sheet = "Data",
  skip = 3
))
gdp_country_2024 <- gdp_wb |>
  dplyr::select(ISO3 = `Country Code`, dplyr::any_of(as.character(2015:2024))) |>
  tidyr::pivot_longer(-ISO3, names_to = "year", values_to = "gdp") |>
  dplyr::mutate(year = as.integer(year), gdp = suppressWarnings(as.numeric(gdp))) |>
  dplyr::filter(!is.na(gdp)) |>
  dplyr::group_by(ISO3) |>
  dplyr::slice_max(year, n = 1) |>
  dplyr::ungroup()

# Country -> (model region, R10 region), via UNEP name -> ISO3
country_r10 <- readxl::read_excel("Inputs/Dict_Countries.xlsx", sheet = "R10_Agg") |>
  dplyr::left_join(
    readxl::read_excel("Inputs/Dict_Countries.xlsx", sheet = "UNEP_Agg") |> dplyr::select(UNEP_Name = UNEP_name, ISO3),
    by = "UNEP_Name"
  ) |>
  dplyr::left_join(gdp_country_2024 |> dplyr::select(ISO3, gdp, gdp_year = year), by = "ISO3")

country_dropped <- country_r10 |>
  dplyr::filter(is.na(Region) | Region == "NA" | is.na(IIASA_R10) | IIASA_R10 == "NA" | is.na(gdp))
cat(
  "  Countries dropped from weights (no R10/region or no GDP):",
  nrow(country_dropped),
  "| their GDP (bn USD):",
  round(sum(country_dropped$gdp, na.rm = TRUE) / 1e9),
  "\n"
)
cat("  Countries using GDP of a year < 2024:", sum(country_r10$gdp_year < 2024L, na.rm = TRUE), "\n")

# Weight = share of each model region's 2024 GDP falling in each R10 region
r10_weights <- country_r10 |>
  dplyr::anti_join(country_dropped |> dplyr::select(UNEP_Name), by = "UNEP_Name") |>
  dplyr::group_by(region = Region, r10 = IIASA_R10) |>
  dplyr::summarise(gdp = sum(gdp), .groups = "drop") |>
  dplyr::group_by(region) |>
  dplyr::mutate(weight = gdp / sum(gdp)) |>
  dplyr::ungroup() |>
  dplyr::select(region, r10, weight)

cat("\n  R10 -> model region weight matrix (rows sum to 1):\n")
print(
  r10_weights |>
    dplyr::mutate(weight = round(weight, 3), r10 = stringr::str_remove(r10, " \\(R10\\)")) |>
    tidyr::pivot_wider(names_from = r10, values_from = weight, values_fill = 0) |>
    as.data.frame()
)

# GDP-weighted average of R10 min and max separately, per material x SSP;
# weights renormalised over the R10 cells present for that variable
flow_ratio_bounds <- r10_weights |>
  dplyr::left_join(ratio_r10, by = "r10", relationship = "many-to-many") |>
  dplyr::filter(!is.na(ratio_min), !is.na(ratio_max)) |>
  dplyr::group_by(region, mat_key, ssp) |>
  dplyr::summarise(
    ratio_min = sum(weight * ratio_min) / sum(weight),
    ratio_max = sum(weight * ratio_max) / sum(weight),
    weight_covered = sum(weight),
    .groups = "drop"
  ) |>
  dplyr::mutate(material_group = unname(FLOW_GROUP[mat_key])) |>
  dplyr::select(region, material_group, mat_key, ssp, ratio_min, ratio_max, weight_covered)

cat("\n  Flow bound cells:", nrow(flow_ratio_bounds), "| min weight covered:", round(min(flow_ratio_bounds$weight_covered), 3), "\n")


# Step 2: Stock-intensity ratio bounds (MC_Assumptions.xlsx Stock_Bounds) ---------

cat("\nStep 2: stock-intensity ratio bounds from Stock_Bounds sheet\n")

stock_ratio_bounds <- readxl::read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Stock_Bounds") |>
  dplyr::transmute(
    region = Region,
    material_group = dplyr::if_else(stringr::str_detect(tolower(Material), "metal ore"), "metal_ores", "nonmetallic_minerals"),
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(End_use), "build") ~ "buildings",
      stringr::str_detect(tolower(End_use), "civil") ~ "civil",
      stringr::str_detect(tolower(End_use), "short") ~ "sl_products",
      stringr::str_detect(tolower(End_use), "machin") ~ "machinery",
      TRUE ~ NA_character_
    ),
    ratio_min = min,
    ratio_max = max
  ) |>
  dplyr::filter(!is.na(mat_key))

# Step 2b: World display bounds (kg/$) for downstream figures -------------------
# World endpoint = sum_r w_r * int_2024_r * (min_r + u * (max_r - min_r)) with
# w_r = region's 2024 GDP share -- linear in u, so each LHS intensity column
# gets a single world [bound_min, bound_max] in kg/$ (flows: mean across SSPs).
# Display only: the simulation itself always uses the regional bounds.

gdp_w_2024 <- readr::read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |>
  dplyr::filter(year == 2024L) |>
  dplyr::transmute(region = Region, w = GDP_2015USD / sum(GDP_2015USD))

int_2024_flow <- dplyr::bind_rows(
  readxl::read_excel(ASSUMPTIONS_FILE, sheet = "Biomass") |>
    dplyr::transmute(
      region = Region,
      mat_key = dplyr::recode(Category, "Crops" = "crops", "Grazed biomass and fodder crops" = "grazed_biomass", "Wood" = "wood", .default = NA_character_),
      int_2024 = `kg/USD 2024`
    ),
  readxl::read_excel(ASSUMPTIONS_FILE, sheet = "FossilFuels") |>
    dplyr::transmute(region = Region, mat_key = fuel, int_2024 = `kg/USD 2024`)
) |>
  dplyr::filter(!is.na(region), !is.na(mat_key)) |>
  dplyr::distinct(region, mat_key, .keep_all = TRUE)

int_2024_stock <- dplyr::bind_rows(
  readxl::read_excel(ASSUMPTIONS_FILE, sheet = "MetalOres", skip = 1) |>
    dplyr::transmute(region, End_use = end_use, material_group = "metal_ores", int_2024 = `kg/USD 2024`),
  readxl::read_excel(ASSUMPTIONS_FILE, sheet = "NonMetallicMinerals", skip = 1) |>
    dplyr::transmute(region, End_use = end_use, material_group = "nonmetallic_minerals", int_2024 = `kg/USD 2024`)
) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(End_use), "build") ~ "buildings",
      stringr::str_detect(tolower(End_use), "civil") ~ "civil",
      stringr::str_detect(tolower(End_use), "short") ~ "sl_products",
      stringr::str_detect(tolower(End_use), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(region), !is.na(mat_key)) |>
  dplyr::distinct(region, material_group, mat_key, .keep_all = TRUE)

intensity_world_bounds <- dplyr::bind_rows(
  flow_ratio_bounds |>
    dplyr::inner_join(int_2024_flow, by = c("region", "mat_key")) |>
    dplyr::inner_join(gdp_w_2024, by = "region") |>
    dplyr::group_by(mat_key, ssp) |>
    dplyr::summarise(
      bound_min = sum(w * int_2024 * ratio_min),
      bound_max = sum(w * int_2024 * ratio_max),
      int_2024 = sum(w * int_2024),
      .groups = "drop"
    ) |>
    dplyr::group_by(mat_key) |>
    dplyr::summarise(dplyr::across(c(bound_min, bound_max, int_2024), mean), .groups = "drop") |>
    dplyr::mutate(param = paste0("intensity_", mat_key, "_global")),
  stock_ratio_bounds |>
    dplyr::inner_join(int_2024_stock |> dplyr::select(region, material_group, mat_key, int_2024), by = c("region", "material_group", "mat_key")) |>
    dplyr::inner_join(gdp_w_2024, by = "region") |>
    dplyr::group_by(material_group, mat_key) |>
    dplyr::summarise(
      bound_min = sum(w * int_2024 * ratio_min),
      bound_max = sum(w * int_2024 * ratio_max),
      int_2024 = sum(w * int_2024),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      param = paste0("intensity_", mat_key, dplyr::if_else(material_group == "metal_ores", "_metalOres", "_nonMetallic"), "_global")
    )
) |>
  dplyr::select(param, bound_min, bound_max, int_2024)

write_csv(intensity_world_bounds, "Parameters/Simulation/intensity_world_bounds.csv")

write_csv(flow_ratio_bounds, "Parameters/Simulation/flow_ratio_bounds.csv")
write_csv(stock_ratio_bounds, "Parameters/Simulation/stock_ratio_bounds.csv")
write_csv(r10_weights, "Parameters/Simulation/r10_region_weights.csv")

# other_biomass and other_fossil fixed at 2024; sl_products and machinery fixed for non-metallic minerals only
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

cat("  Biomass sampled:", paste(biomass_mats_sampled, collapse = ", "), "\n")
cat("  Fossil sampled: ", paste(fossil_mats_sampled, collapse = ", "), "\n")
cat("  Stock combos:   ", paste(stock_combos, collapse = ", "), "\n")


# =============================================================================
# Step 3: Build LHS column names
# =============================================================================

cat("\nStep 3: build LHS column names\n")

lifetime_super_cats <- sort(unique(LIFETIME_SAMPLE_PARAMS$super_category))

lhs_col_names <- c(
  # Continuous SSP position (single draw): 02-RunSimulations.R maps it onto the
  # SSP_SAMPLED SSPs ranked by world GDP/capita growth and blends the two bracketing SSPs
  # for population, GDP per capita AND flow-intensity bounds -- no discrete label.
  "ssp_u",
  "target_year_u",
  # Intensity draws (one per material/group, shared by all regions, all in [0,1])
  paste0("intensity_", biomass_mats_sampled, "_global"),
  paste0("intensity_", fossil_mats_sampled, "_global"),
  paste0("intensity_", stock_combos, "_global"),
  # Recycling draws — separate for Fe and NonFe metals
  "recycling_Fe_global",
  "recycling_NonFe_global",
  "recyc_convergence_yr_global",
  "downcycling_buildings_global",
  "downcycling_civil_infrastructure_global",
  # Scalar parameters
  "sub_factor_recycling_same",
  "sub_factor_recycling_same_civil",
  "max_secondary_roads",
  "sub_factor_downcycling_roads",
  # Ore grade draws (primary ore → metal, central Fe=0.40, NonFe=0.016)
  "grade_ore_fe_u",
  "grade_ore_nonfe_u",
  # Lifetime params (mean and k per super_category; applied proportionally to sub_uses)
  paste0("lifetime_mean_", lifetime_super_cats),
  paste0("lifetime_k_", lifetime_super_cats)
)

n_cols <- length(lhs_col_names)
cat("  LHS columns:", n_cols, "| N_RUNS:", N_RUNS, "\n")


# =============================================================================
# Step 4: LHS draw
# =============================================================================

cat("\nStep 4: LHS draw\n")

set.seed(GLOBAL_SEED)
lhs_mat <- lhs::randomLHS(n = N_RUNS, k = n_cols)
colnames(lhs_mat) <- lhs_col_names

mc_input_matrix <- tibble::as_tibble(lhs_mat) |>
  mutate(run_id = seq_len(N_RUNS)) |>
  dplyr::select(run_id, everything())

cat("  Matrix:", nrow(mc_input_matrix), "×", ncol(mc_input_matrix), "\n")
cat("  ssp_u: continuous [0,1], interpolated between SSPs downstream\n")


# =============================================================================
# Step 5: Save
# =============================================================================

write_csv(mc_input_matrix, "Parameters/Simulation/mc_input_matrix.csv")
cat("\nSaved: Parameters/Simulation/mc_input_matrix.csv, flow_ratio_bounds.csv, stock_ratio_bounds.csv, r10_region_weights.csv\n")
cat("Sampling complete.\n\n")

# EoF
