## =============================================================================
## 01-Sampling.R
## Latin Hypercube Sampling for Monte Carlo simulation.
##
## Reads bounds from Excel inputs, draws N_RUNS LHS samples in [0,1] for every
## sampled parameter, saves the raw draw matrix.
##
## Output: Parameters/MC/mc_input_matrix.csv
##   columns: run_id, ssp_label, <one column per sampled parameter, all in [0,1]>
##
## 02-RunSimulations.R reads this matrix + the same Excel inputs to reconstruct
## trajectories on the fly.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

# =============================================================================
# Step 1: Read material intensity bounds from MC_Assumptions.xlsx Intensity sheet
# =============================================================================

cat("Step 1: read intensity bounds from MC_Assumptions.xlsx\n")

intensity_raw <- readxl::read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Intensity")

intensity_biomass <- intensity_raw |>
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
  dplyr::filter(!is.na(mat_key))

intensity_fossil <- intensity_raw |>
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
  dplyr::filter(!is.na(mat_key))

intensity_metal <- intensity_raw |>
  dplyr::filter(stringr::str_detect(tolower(Category), "metal ore")) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(Detail), "build") ~ "buildings",
      stringr::str_detect(tolower(Detail), "civil") ~ "civil",
      stringr::str_detect(tolower(Detail), "short") ~ "sl_products",
      stringr::str_detect(tolower(Detail), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(mat_key))

intensity_nonmet <- intensity_raw |>
  dplyr::filter(stringr::str_detect(tolower(Category), "metalic")) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(Detail), "build") ~ "buildings",
      stringr::str_detect(tolower(Detail), "civil") ~ "civil",
      stringr::str_detect(tolower(Detail), "short") ~ "sl_products",
      stringr::str_detect(tolower(Detail), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(mat_key))

# other_biomass and other_fossil fixed at 2024; sl_products and machinery fixed for non-metallic minerals only
biomass_mats_sampled <- sort(setdiff(unique(intensity_biomass$mat_key), "other_biomass"))
fossil_mats_sampled <- sort(setdiff(unique(intensity_fossil$mat_key), "other_fossil"))

stock_combos <- sort(c(
  paste0(sort(unique(intensity_metal$mat_key)), "_metalOres"),
  paste0(sort(setdiff(unique(intensity_nonmet$mat_key), c("sl_products", "machinery"))), "_nonMetallic")
))

cat("  Biomass sampled:", paste(biomass_mats_sampled, collapse = ", "), "\n")
cat("  Fossil sampled: ", paste(fossil_mats_sampled, collapse = ", "), "\n")
cat("  Stock combos:   ", paste(stock_combos, collapse = ", "), "\n")


# =============================================================================
# Step 2: Build LHS column names
# =============================================================================

cat("\nStep 2: build LHS column names\n")

lifetime_super_cats <- sort(unique(LIFETIME_SAMPLE_PARAMS$super_category))

lhs_col_names <- c(
  # Continuous SSP position (independent draws): 02-RunSimulations.R locates
  # the two bracketing SSPs at FORECAST_END and blends their full trajectory
  # by the resulting share -- no discrete SSP label is assigned here.
  "pop_ssp_u",
  "gdppc_ssp_u",
  "target_year_u",
  # Global intensity draws (one per material/group, all in [0,1])
  paste0("intensity_", biomass_mats_sampled, "_global"),
  paste0("intensity_", fossil_mats_sampled, "_global"),
  paste0("intensity_", stock_combos, "_global"),
  # Gap-persistence λ per group
  "gap_persistence_biomass",
  "gap_persistence_fossilfuels",
  "gap_persistence_metal_construction",
  # Recycling draws — separate for Fe and NonFe metals
  "recycling_Fe_global",
  "recycling_NonFe_global",
  "recyc_convergence_yr_global",
  "downcycling_buildings_global",
  "downcycling_civil_infrastructure_global",
  "gap_persistence_rates",
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
# Step 3: LHS draw
# =============================================================================

cat("\nStep 3: LHS draw\n")

set.seed(GLOBAL_SEED)
lhs_mat <- lhs::randomLHS(n = N_RUNS, k = n_cols)
colnames(lhs_mat) <- lhs_col_names

mc_input_matrix <- tibble::as_tibble(lhs_mat) |>
  mutate(run_id = seq_len(N_RUNS)) |>
  dplyr::select(run_id, everything())

cat("  Matrix:", nrow(mc_input_matrix), "×", ncol(mc_input_matrix), "\n")
cat("  pop_ssp_u / gdppc_ssp_u: continuous [0,1], interpolated between SSPs downstream\n")


# =============================================================================
# Step 4: Save
# =============================================================================

write_csv(mc_input_matrix, "Parameters/MC/mc_input_matrix.csv")
cat("\nSaved: Parameters/MC/mc_input_matrix.csv\n")
cat("Sampling complete.\n\n")

# EoF
