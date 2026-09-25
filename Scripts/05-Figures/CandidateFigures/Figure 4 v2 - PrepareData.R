## =============================================================================
## Figure 4 v2 - PrepareData.R
## Runs the expensive modelling behind the new 4-panel Figure 4 v2 once and
## caches the results, so "Figure 4 v2 - FourPanel.R" only has to load CSVs
## and plot.
##
## Growth outcome used throughout (all 4 panels): TOTAL (population-inclusive)
## material consumption CAGR, 2025-FORECAST_END, from mc_decoupling.parquet
## (variant_id == "window_2025_2060", material_group == "Total") -- mf_total_cagr
## (mat_cagr) and gdp_total_cagr (gdp_cagr). "Absolute decoupling" in this
## figure is simply mat_cagr < 0 (deliberately simpler than the 4-class
## decoupling_class scheme 04-Decoupling.R also produces, which this figure
## does not use).
##
## Panel a: LightGBM + TreeSHAP surrogate of mat_cagr ~ every MC lever
##   (u-space), mean |SHAP| aggregated within 4 growth-rate bins (<0%, 0-1%,
##   1-2%, >2%) instead of Figure 4 - PrepareData.R's continuous
##   consumption-level bins -- same top-10-plus-Other collapsing/family-shaded
##   colour recipe reused verbatim.
## Panel b: the run-level growth_df (mat_cagr, gdp_cagr, abs_decouple), plus a
##   "selected" top-decile high-GDP-growth/low-material-growth subset (same
##   percentile-rank-sum recipe "Figure 4 - PrepareData.R" uses, on growth
##   rates instead of 2060 levels).
## Panel c: multivariate lm(mat_cagr ~ ., all 36 levers, real units) --
##   coefficient x a FIXED, physically meaningful delta per lever (e.g. "+1pp
##   population growth", "+0.1 kg/USD intensity", "+20yr lifetime" -- see
##   LEVER_META in STEP 7) = pp effect on growth, with a 95% CI. Real-unit
##   bound construction (STEP 3-5 below) is reused verbatim from
##   "Figure 7 - PrepareData.R", except pop_ssp_u/gdppc_ssp_u's bound is
##   redefined as an ANNUALIZED growth rate (see STEP 3) so their fixed delta
##   ("+1pp") is directly interpretable.
## Panel d: raw [0,1] LHS draws (u-space, no rescaling) for the same 22-lever
##   subset/order as panel c, long format, one row per run x lever, plus
##   per-lever mean-draw "diamond" markers for the abs-decoupling and
##   selected subsets.
##
## Output (Parameters/Intermediate/):
##   Figure4v2_GrowthImportance.csv - panel a stacked-bar data
##   Figure4v2_Scatter.csv          - panel b run-level growth/decoupling/selected
##   Figure4v2_LeverEffects.csv     - panel c regression effects + 95% CI (22 levers)
##   Figure4v2_LeverSamples.csv     - panel d raw u-draws, long (22 levers)
##   Figure4v2_LeverDiamonds.csv    - panel d per-lever abs-decouple/selected mean markers
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)
library(lightgbm)
library(broom)

cat("=== Figure 4 v2 - Prepare Data ===\n\n")

GROWTH_WINDOW_START <- 2025L # matches FIG_VARIANT_ID below
FIG_VARIANT_ID <- "window_2025_2060"
N_TOP <- 10L # panel a: top-N levers by global mean |SHAP|, rest collapsed to "Other"

GROWTH_BIN_BREAKS <- c(-Inf, 0, 0.01, 0.02, Inf)
GROWTH_BIN_LEVELS <- c("<0%", "0-1%", "1-2%", ">2%")

SELECT_FRACTION <- 0.1 # panel b/d "selected" subset: top 10% high GDP growth + low material growth
SSP_GROWTH_YEARS <- FORECAST_END - 2024L # pop_ssp_u/gdppc_ssp_u bound window (2024->FORECAST_END)

family_pal <- c(
  "Driver SSP" = "#1f78b4",
  "Regional divergence" = "#6a3d9a",
  "Intensity" = "#e31a1c",
  "Material recovery" = "#33a02c",
  "Mining" = "#8c510a",
  "Lifetime" = "#ff7f00",
  "Other" = "#999999"
)


# STEP 1: Load data ---------------------------------------------------------------

cat("STEP 1: Load data\n")

input_matrix <- read_csv("Parameters/MC/mc_input_matrix.csv", show_col_types = FALSE) |> arrange(run_id)
decoupling <- arrow::read_parquet("Results/MC/mc_decoupling.parquet")
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
intensity_raw <- readxl::read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Intensity")

feature_cols <- input_matrix |> dplyr::select(-run_id) |> names()
n_feat <- length(feature_cols)

cat("  Runs:", n_distinct(input_matrix$run_id), " | Features:", n_feat, "\n\n")


# STEP 2: Growth outcome per run -- TOTAL material/GDP CAGR, 2025-FORECAST_END ----

cat("STEP 2: Growth outcome per run (Total, ", FIG_VARIANT_ID, ")\n", sep = "")

growth_df <- decoupling |>
  dplyr::filter(variant_id == FIG_VARIANT_ID, material_group == "Total") |>
  dplyr::transmute(run_id, mat_cagr = mf_total_cagr, gdp_cagr = gdp_total_cagr) |>
  tidyr::drop_na(mat_cagr, gdp_cagr) |>
  dplyr::mutate(
    abs_decouple = mat_cagr < 0,
    growth_bin = cut(mat_cagr, breaks = GROWTH_BIN_BREAKS, labels = GROWTH_BIN_LEVELS, right = FALSE)
  )

# "Selected" subset (panels b/d): top 10% by composite percentile rank of high
# GDP growth + low material growth -- same percentile-rank-sum + slice_max(prop)
# recipe "Figure 4 - PrepareData.R" STEP 3 uses for its "Top 10% High GDP Low
# Material" subset, just computed on GROWTH RATES (this figure's own basis)
# instead of that script's 2060 consumption/GDP-per-capita LEVELS.
growth_df <- growth_df |>
  dplyr::mutate(
    pctile_gdp_growth = dplyr::percent_rank(gdp_cagr),
    pctile_low_mat_growth = dplyr::percent_rank(dplyr::desc(mat_cagr)),
    decoupling_score = pctile_gdp_growth + pctile_low_mat_growth
  )
selected_ids <- growth_df |> dplyr::slice_max(decoupling_score, prop = SELECT_FRACTION, with_ties = FALSE) |> dplyr::pull(run_id)
growth_df <- growth_df |> dplyr::mutate(selected = run_id %in% selected_ids) |> dplyr::select(-pctile_gdp_growth, -pctile_low_mat_growth, -decoupling_score)

cat("  Runs with valid growth outcome:", nrow(growth_df), "| Absolute decoupling:", sum(growth_df$abs_decouple), "| Selected (top 10% high GDP/low material growth):", sum(growth_df$selected), "\n")
cat("  Growth bin counts:\n")
print(table(growth_df$growth_bin))
cat("\n")


# STEP 3: Real-value bounds & units per parameter (reused from "Figure 7 - PrepareData.R" STEP 3, verbatim) ----

cat("STEP 3: Real-value bounds & units per parameter\n")

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
  dplyr::transmute(param = paste0("intensity_", mat_key, "_global"), bound_min = min, bound_max = max)

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
  dplyr::transmute(param = paste0("intensity_", mat_key, "_global"), bound_min = min, bound_max = max)

intensity_bounds_metal <- intensity_raw |>
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
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::transmute(param = paste0("intensity_", mat_key, "_metalOres_global"), bound_min = min, bound_max = max)

intensity_bounds_nonmet <- intensity_raw |>
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
  dplyr::filter(!is.na(mat_key)) |>
  dplyr::transmute(param = paste0("intensity_", mat_key, "_nonMetallic_global"), bound_min = min, bound_max = max)

bound_lkp_intensity <- dplyr::bind_rows(
  intensity_bounds_biomass,
  intensity_bounds_fossil,
  intensity_bounds_metal,
  intensity_bounds_nonmet
) |>
  dplyr::mutate(unit = "kg_usd")

# SSP position (population / GDP-per-capita) real-unit range: the [0,1] draw
# locates a point between the two bracketing SSPs' world growth ratios
# (2024 -> FORECAST_END), same ranking as 02-RunSimulations.R STEP 3.
world_agg_bounds <- ssp_drivers |>
  dplyr::filter(variable %in% c("Population", "GDP|PPP"), year %in% c(2024L, FORECAST_END)) |>
  dplyr::select(scenario, region, variable, year, value) |>
  tidyr::pivot_wider(names_from = variable, values_from = value) |>
  dplyr::group_by(scenario, year) |>
  dplyr::summarise(
    pop_world = sum(Population, na.rm = TRUE),
    gdp_world = sum(`GDP|PPP`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(gdppc_world = gdp_world / pop_world)

world_2024_bounds <- world_agg_bounds |>
  dplyr::filter(year == 2024L) |>
  dplyr::select(scenario, pop_base = pop_world, gdppc_base = gdppc_world)
world_end_bounds <- world_agg_bounds |>
  dplyr::filter(year == FORECAST_END) |>
  dplyr::select(scenario, pop_end = pop_world, gdppc_end = gdppc_world)

ssp_ratio_pop <- world_end_bounds |>
  dplyr::left_join(world_2024_bounds, by = "scenario") |>
  dplyr::mutate(val = pop_end / pop_base)
ssp_ratio_gdp <- world_end_bounds |>
  dplyr::left_join(world_2024_bounds, by = "scenario") |>
  dplyr::mutate(val = gdppc_end / gdppc_base)

lifetime_bounds <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::group_by(super_category) |>
  dplyr::summarise(
    mean_life_min = mean(mean_life_min),
    mean_life_max = mean(mean_life_max),
    k_min = mean(k_min),
    k_max = mean(k_max),
    .groups = "drop"
  )

bound_lkp_lifetime <- dplyr::bind_rows(
  lifetime_bounds |>
    dplyr::transmute(param = paste0("lifetime_mean_", super_category), bound_min = mean_life_min, bound_max = mean_life_max, unit = "yrs"),
  lifetime_bounds |>
    dplyr::transmute(param = paste0("lifetime_k_", super_category), bound_min = k_min, bound_max = k_max, unit = "shape")
)

bound_lkp_scalar <- tibble::tribble(
  ~param                                    , ~bound_min                          , ~bound_max                          , ~unit      ,
  "target_year_u"                           , TARGET_YEAR_MIN                     , TARGET_YEAR_MAX                     , "year_abs" ,
  "recyc_convergence_yr_global"             , RECYC_CONVERGENCE_YR_MIN            , RECYC_CONVERGENCE_YR_MAX            , "year_abs" ,
  "grade_ore_fe_u"                          , GRADE_ORE_FE_MIN                    , GRADE_ORE_FE_MAX                    , "pct"      ,
  "grade_ore_nonfe_u"                       , GRADE_ORE_NONFE_MIN                 , GRADE_ORE_NONFE_MAX                 , "pct"      ,
  "recycling_Fe_global"                     , RECYCLING_RATE_FE_MIN               , RECYCLING_RATE_FE_MAX               , "pct"      ,
  "recycling_NonFe_global"                  , RECYCLING_RATE_NONFE_MIN            , RECYCLING_RATE_NONFE_MAX            , "pct"      ,
  "downcycling_buildings_global"            , DOWNCYCLING_MIN                     , DOWNCYCLING_MAX                     , "pct"      ,
  "downcycling_civil_infrastructure_global" , DOWNCYCLING_MIN                     , DOWNCYCLING_MAX                     , "pct"      ,
  "sub_factor_recycling_same"               , SUB_FACTOR_RECYCLING_SAME_MIN       , SUB_FACTOR_RECYCLING_SAME_MAX       , "pct"      ,
  "sub_factor_recycling_same_civil"         , SUB_FACTOR_RECYCLING_SAME_CIVIL_MIN , SUB_FACTOR_RECYCLING_SAME_CIVIL_MAX , "pct"      ,
  "max_secondary_roads"                     , MAX_SECONDARY_ROADS_MIN             , MAX_SECONDARY_ROADS_MAX             , "pct"      ,
  "sub_factor_downcycling_roads"            , SUB_FACTOR_DOWNCYCLING_ROADS_MIN    , SUB_FACTOR_DOWNCYCLING_ROADS_MAX    , "pct"      ,
  "gap_persistence_biomass"                 ,                                   0 ,                                   1 , "pct"      ,
  "gap_persistence_fossilfuels"             ,                                   0 ,                                   1 , "pct"      ,
  "gap_persistence_metal_construction"      ,                                   0 ,                                   1 , "pct"      ,
  "gap_persistence_rates"                   ,                                   0 ,                                   1 , "pct"      ,
  # Population/GDP-per-capita: real-unit bound is the ANNUALIZED equivalent
  # growth rate (ratio^(1/SSP_GROWTH_YEARS) - 1), not the raw cumulative
  # 2024->FORECAST_END ratio -- so a lever regression coefficient here is
  # directly "growth-outcome pp per +1pp of population/GDP-per-capita annual
  # growth", matching every other lever's fixed real-unit delta convention
  # used in panel c (STEP 7 below).
  "pop_ssp_u"                               , min(ssp_ratio_pop$val)^(1 / SSP_GROWTH_YEARS) - 1 , max(ssp_ratio_pop$val)^(1 / SSP_GROWTH_YEARS) - 1 , "pct_annual" ,
  "gdppc_ssp_u"                             , min(ssp_ratio_gdp$val)^(1 / SSP_GROWTH_YEARS) - 1 , max(ssp_ratio_gdp$val)^(1 / SSP_GROWTH_YEARS) - 1 , "pct_annual"
)

bound_lkp <- dplyr::bind_rows(bound_lkp_scalar, bound_lkp_intensity, bound_lkp_lifetime)

missing_bounds <- setdiff(feature_cols, bound_lkp$param)
if (length(missing_bounds) > 0L) {
  cat("  WARNING: no real-unit bounds for:", paste(missing_bounds, collapse = ", "), "-- using raw [0,1] draw as its own unit.\n")
  bound_lkp <- dplyr::bind_rows(bound_lkp, tibble::tibble(param = missing_bounds, bound_min = 0, bound_max = 1, unit = "u_raw"))
}
stopifnot(all(feature_cols %in% bound_lkp$param))

cat("  Parameters with real-value bounds:", nrow(bound_lkp), "of", n_feat, "\n\n")


# STEP 4: Rescale draws to real units (feeds panel c's regression, STEP 7) --------

cat("STEP 4: Rescale draws to real units\n")

real_matrix <- input_matrix
for (p in feature_cols) {
  bmin <- bound_lkp$bound_min[bound_lkp$param == p]
  bmax <- bound_lkp$bound_max[bound_lkp$param == p]
  real_matrix[[p]] <- bmin + real_matrix[[p]] * (bmax - bmin)
}


# STEP 5: Display labels & family classification (36 levers, reused from "Figure 7 - PrepareData.R" STEP 5, verbatim) ----

cat("STEP 5: Display labels & family classification\n")

label_map <- c(
  pop_ssp_u = "Population",
  gdppc_ssp_u = "GDP/capita",
  target_year_u = "Intensity target year",
  intensity_crops_global = "M/G: Crops",
  intensity_grazed_biomass_global = "M/G: Grazed biomass",
  intensity_wood_global = "M/G: Wood",
  intensity_coal_global = "M/G: Coal",
  intensity_gas_global = "M/G: Natural gas",
  intensity_oil_global = "M/G: Oil",
  intensity_buildings_metalOres_global = "S/G: Buildings metal",
  intensity_buildings_nonMetallic_global = "S/G: Buildings minerals",
  intensity_civil_metalOres_global = "S/G: Infrastructure metal",
  intensity_civil_nonMetallic_global = "S/G: Infrastructure minerals",
  intensity_machinery_metalOres_global = "S/G: Machinery metal",
  intensity_sl_products_metalOres_global = "S/G: Short-lived metal",
  gap_persistence_biomass = "Gap persistence: Biomass",
  gap_persistence_fossilfuels = "Gap persistence: Fossil fuels",
  gap_persistence_metal_construction = "Gap persistence: Metal & Mineral",
  recycling_Fe_global = "Recycling % - Fe",
  recycling_NonFe_global = "Recycling % - NonFe",
  grade_ore_fe_u = "Ore grade - Fe",
  grade_ore_nonfe_u = "Ore grade - NonFe",
  recyc_convergence_yr_global = "Recycling target year",
  downcycling_buildings_global = "Downcycling: Buildings",
  downcycling_civil_infrastructure_global = "Downcycling: Infrastructure",
  gap_persistence_rates = "Gap persistence: Recycling %",
  sub_factor_recycling_same = "Recycling substitution factor",
  sub_factor_recycling_same_civil = "Recycling substitution (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Downcycling substitution factor",
  lifetime_mean_buildings = "Lifetime Buildings",
  lifetime_mean_civil_infrastructure = "Lifetime Infrastructure",
  lifetime_mean_machinery = "Lifetime Machinery",
  lifetime_mean_short_lived = "Lifetime Short-lived",
  lifetime_k_buildings = "Lifetime shape: Buildings",
  lifetime_k_civil_infrastructure = "Lifetime shape: Infrastructure",
  lifetime_k_machinery = "Lifetime shape: Machinery",
  lifetime_k_short_lived = "Lifetime shape: Short-lived"
)
col_labels <- label_map[feature_cols]
names(col_labels) <- feature_cols

family_by_param <- tibble::tibble(param = feature_cols) |>
  dplyr::mutate(
    family = dplyr::case_when(
      param %in% c("pop_ssp_u", "gdppc_ssp_u") ~ "Driver SSP",
      stringr::str_detect(param, "gap_persistence") ~ "Regional divergence",
      stringr::str_detect(param, "grade_ore") ~ "Mining",
      stringr::str_detect(param, "intensity|target_year") ~ "Intensity",
      stringr::str_detect(param, "recyc|downcycl|sub_factor|max_secondary") ~ "Material recovery",
      stringr::str_detect(param, "lifetime") ~ "Lifetime",
      TRUE ~ "Other"
    ),
    display_label = col_labels[param]
  ) |>
  dplyr::left_join(bound_lkp |> dplyr::select(param, unit), by = "param")


# STEP 6: Panel a -- LightGBM + TreeSHAP on growth rate, binned by growth category ----

cat("STEP 6: Panel a -- LightGBM + SHAP on growth rate\n")

df_shap <- input_matrix |> dplyr::inner_join(growth_df |> dplyr::select(run_id, mat_cagr, growth_bin), by = "run_id")

X <- as.matrix(df_shap[, feature_cols])
y <- df_shap$mat_cagr

set.seed(GLOBAL_SEED)
n <- nrow(X)
train_idx <- sample(n, floor(0.8 * n))
test_idx <- setdiff(seq_len(n), train_idx)

dtrain <- lgb.Dataset(X[train_idx, ], label = y[train_idx])
lgb_params <- list(
  objective = "regression", metric = "rmse", num_leaves = 63L, learning_rate = 0.05,
  feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 5L, verbose = -1L
)
model <- lgb.train(params = lgb_params, data = dtrain, nrounds = 1000L, verbose = -1L)

y_pred_test <- predict(model, X[test_idx, ])
r2 <- 1 - sum((y[test_idx] - y_pred_test)^2) / sum((y[test_idx] - mean(y[test_idx]))^2)

if (r2 < 0.80) {
  cat(sprintf("  LightGBM R2 = %.4f < 0.80 -- retrying with a larger surrogate\n", r2))
  lgb_params$num_leaves <- 127L
  model <- lgb.train(params = lgb_params, data = dtrain, nrounds = 1500L, verbose = -1L)
  y_pred_test <- predict(model, X[test_idx, ])
  r2 <- 1 - sum((y[test_idx] - y_pred_test)^2) / sum((y[test_idx] - mean(y[test_idx]))^2)
}
cat(sprintf("  LightGBM held-out R2 = %.4f (growth rate outcome)\n", r2))

shap_vals <- predict(model, X, type = "contrib")[, seq_len(n_feat)]
colnames(shap_vals) <- feature_cols

# Top-N levers by GLOBAL mean |SHAP| (same param set shown in every growth
# bin, so a lever's presence/absence across bins is comparable) + "Other".
global_mean_shap <- colMeans(abs(shap_vals))
param_order <- sort(global_mean_shap, decreasing = TRUE)
top_params <- names(param_order)[seq_len(N_TOP)]
other_params <- names(param_order)[-seq_len(N_TOP)]

shap_abs_df <- as.data.frame(abs(shap_vals))
shap_abs_df$growth_bin <- df_shap$growth_bin

bin_means <- shap_abs_df |> dplyr::group_by(growth_bin) |> dplyr::summarise(dplyr::across(dplyr::all_of(feature_cols), mean), .groups = "drop")
bin_mat <- as.matrix(bin_means[, feature_cols])
rownames(bin_mat) <- as.character(bin_means$growth_bin)
bin_norm <- sweep(bin_mat, 1, rowSums(bin_mat), "/") * 100

bin_top <- bin_norm[, top_params, drop = FALSE]
bin_top <- cbind(bin_top, Other = rowSums(bin_norm[, other_params, drop = FALSE]))

# Family-shaded colour ramp (identical recipe to "Figure 4 - PrepareData.R"
# STEP 7, project-wide family_pal) -- ramped across EVERY member of a family
# (not just the top-10 subset shown here), so a given lever's shade matches
# whatever this same family_pal/family_by_param recipe assigns it in Figure 7
# / Figure 4, keeping colour consistent across figures (Figure Design
# Pre-Prompt: "Enforce color consistency across figures within the same
# project").
stack_order_params <- rev(top_params)
all_labels_ordered <- c(col_labels[top_params], "Other")

fam_rows_all <- family_by_param |> dplyr::arrange(family, display_label)
SHAP_PARAM_COLORS <- c("Other" = family_pal[["Other"]])
for (fam in setdiff(names(family_pal), "Other")) {
  fam_rows <- fam_rows_all |> dplyr::filter(family == fam)
  n_fam <- nrow(fam_rows)
  if (n_fam == 0L) next
  base_rgb <- grDevices::col2rgb(family_pal[[fam]]) / 255
  light_rgb <- pmin(1, base_rgb + (1 - base_rgb) * 0.6)
  dark_rgb <- base_rgb * 0.55
  light_hex <- grDevices::rgb(light_rgb[1], light_rgb[2], light_rgb[3])
  dark_hex <- grDevices::rgb(dark_rgb[1], dark_rgb[2], dark_rgb[3])
  fam_shades <- if (n_fam == 1) family_pal[[fam]] else grDevices::colorRampPalette(c(light_hex, family_pal[[fam]], dark_hex))(n_fam)
  SHAP_PARAM_COLORS <- c(SHAP_PARAM_COLORS, stats::setNames(fam_shades, fam_rows$display_label))
}
fill_vals <- setNames(
  vapply(as.character(all_labels_ordered), function(lbl) if (lbl %in% names(SHAP_PARAM_COLORS)) SHAP_PARAM_COLORS[[lbl]] else "#AAAAAA", character(1L)),
  all_labels_ordered
)

plot_df_a <- as_tibble(bin_top, rownames = "growth_bin") |>
  dplyr::mutate(growth_bin = factor(growth_bin, levels = GROWTH_BIN_LEVELS)) |>
  tidyr::pivot_longer(cols = -growth_bin, names_to = "parameter", values_to = "pct") |>
  dplyr::mutate(
    display_label = if_else(parameter == "Other", "Other", col_labels[parameter]),
    fill_hex = fill_vals[display_label],
    display_label_factor = factor(display_label, levels = rev(all_labels_ordered)),
    stack_order = as.integer(display_label_factor)
  )

cat("  Top parameters:", paste(col_labels[top_params], collapse = ", "), "\n\n")


# STEP 7: Panel c -- multivariate regression, growth-rate effect per lever --------
# Effect per lever = regression coefficient x a FIXED, physically meaningful
# delta (user-specified per lever/lever-group below) instead of that lever's
# own p10->p90 sampled span -- e.g. "+1pp population growth", "+0.1 kg/USD
# material intensity", "+20yr building lifetime" -- so every row answers "if
# this lever moved by a real, intuitive amount, how much would growth move,"
# with a 95% CI (coefficient +/- 1.96 x std. error) instead of a bare point
# estimate. Levers are grouped (group_key) below the family level, since
# e.g. "M/G" (biomass/fossil intensity) and "S/G" metal/mineral stock
# intensity share the "Intensity" family but use DIFFERENT deltas -- a group
# whose delta is uniform across its members gets ONE header (group label +
# delta) shown once at the top of the group instead of repeated per row.

cat("STEP 7: Panel c -- growth-rate regression effects, fixed real-unit deltas\n")

df_reg <- real_matrix |> dplyr::inner_join(growth_df |> dplyr::select(run_id, mat_cagr), by = "run_id")
mod <- lm(mat_cagr ~ ., data = df_reg |> dplyr::select(dplyr::all_of(feature_cols), mat_cagr))
r2_reg <- summary(mod)$r.squared
cat(sprintf("  Regression R2 = %.4f\n", r2_reg))

coefs <- broom::tidy(mod) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::rename(param = term, coefficient = estimate, std_error = std.error, p_value = p.value)

# Fixed lever order/labels/grouping/delta requested for panels c/d -- pop,
# G/P, intensity (by material detail: biomass/fossil/metal/minerals),
# recycling Fe/NonFe, ore grade Fe/NonFe, lifetime (buildings/civil/
# machinery/short-lived).
LEVER_META <- tibble::tribble(
  ~param                                    , ~row_label       , ~group_key  , ~group_label          , ~delta_real , ~delta_label ,
  "pop_ssp_u"                                , "Population"     , "growth"    , "Annual growth rate"  , 0.01        , "+1%/yr"     ,
  "gdppc_ssp_u"                               , "GDP/capita"     , "growth"    , "Annual growth rate"  , 0.01        , "+1%/yr"     ,
  "intensity_crops_global"                    , "Crops"          , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_grazed_biomass_global"           , "Grazed biomass" , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_wood_global"                     , "Wood"           , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_coal_global"                     , "Coal"           , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_gas_global"                      , "Natural gas"    , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_oil_global"                      , "Oil"            , "mg"        , "M/G intensity"       , 0.1         , "+0.1 kg/USD",
  "intensity_buildings_metalOres_global"       , "Buildings"      , "sg_metal"  , "Metal stock (S/G)"   , 0.1         , "+0.1 kg/USD",
  "intensity_civil_metalOres_global"          , "Infrastructure" , "sg_metal"  , "Metal stock (S/G)"   , 0.1         , "+0.1 kg/USD",
  "intensity_machinery_metalOres_global"       , "Machinery"      , "sg_metal"  , "Metal stock (S/G)"   , 0.1         , "+0.1 kg/USD",
  "intensity_sl_products_metalOres_global"     , "Short-lived"    , "sg_metal"  , "Metal stock (S/G)"   , 0.1         , "+0.1 kg/USD",
  "intensity_buildings_nonMetallic_global"     , "Buildings"      , "sg_mineral", "Mineral stock (S/G)" , 1.0         , "+1 kg/USD"  ,
  "intensity_civil_nonMetallic_global"        , "Infrastructure" , "sg_mineral", "Mineral stock (S/G)" , 1.0         , "+1 kg/USD"  ,
  "recycling_Fe_global"                       , "Fe"             , "recycling" , "Recycling rate"      , 0.10        , "+10%"       ,
  "recycling_NonFe_global"                    , "NonFe"          , "recycling" , "Recycling rate"      , 0.10        , "+10%"       ,
  "grade_ore_fe_u"                            , "Fe"             , "ore_grade" , "Ore grade"            , 0.10        , "+10%"       ,
  "grade_ore_nonfe_u"                         , "NonFe"          , "ore_grade" , "Ore grade"            , 0.01        , "+1%"        ,
  "lifetime_mean_buildings"                   , "Buildings"      , "lifetime"  , "Lifetime"             , 20          , "+20yr"      ,
  "lifetime_mean_civil_infrastructure"        , "Infrastructure" , "lifetime"  , "Lifetime"             , 20          , "+20yr"      ,
  "lifetime_mean_machinery"                   , "Machinery"      , "lifetime"  , "Lifetime"             , 5           , "+5yr"       ,
  "lifetime_mean_short_lived"                 , "Short-lived"    , "lifetime"  , "Lifetime"             , 1           , "+1yr"
) |>
  dplyr::mutate(order = dplyr::row_number())

# Group header: delta value shown once when uniform across the group's rows
# (mg/sg_metal/sg_mineral/recycling/growth); when it varies (ore_grade,
# lifetime) the header carries only the group name and each row keeps its own
# delta inline instead.
LEVER_META <- LEVER_META |>
  dplyr::group_by(group_key) |>
  dplyr::mutate(group_uniform = dplyr::n_distinct(delta_label) == 1L) |>
  dplyr::ungroup() |>
  dplyr::mutate(row_text = dplyr::if_else(group_uniform, row_label, paste0(row_label, " (", delta_label, ")")))

effects_sel <- coefs |>
  dplyr::inner_join(LEVER_META, by = "param") |>
  dplyr::left_join(family_by_param |> dplyr::select(param, family), by = "param") |>
  dplyr::mutate(
    effect_pp = coefficient * delta_real * 100, # pp of growth rate per fixed delta
    ci_lo_pp = (coefficient - 1.96 * std_error) * delta_real * 100,
    ci_hi_pp = (coefficient + 1.96 * std_error) * delta_real * 100,
    significant = p_value < 0.05,
    # Plain text, NOT a factor: row_text repeats across lever groups (e.g.
    # "Buildings" is a row in both the metal-stock and mineral-stock groups),
    # so it can't double as a unique factor level -- `order` is the unique
    # plotting key the figure script positions rows by.
    display_label = row_text
  ) |>
  dplyr::arrange(order)

cat("  Levers in panel c/d:", nrow(effects_sel), "\n\n")


# STEP 8: Panel d -- raw [0,1] LHS draws, long format, same 22-lever order ---------

cat("STEP 8: Panel d -- raw u-space draws (long)\n")

samples_df <- input_matrix |>
  dplyr::select(run_id, dplyr::all_of(LEVER_META$param)) |>
  tidyr::pivot_longer(-run_id, names_to = "param", values_to = "u_value") |>
  dplyr::inner_join(growth_df |> dplyr::select(run_id, abs_decouple, selected), by = "run_id") |>
  dplyr::left_join(LEVER_META |> dplyr::select(param, order, row_text), by = "param") |>
  dplyr::mutate(display_label = row_text) # plain text -- see note above

cat("  Sample rows (runs x levers):", nrow(samples_df), "\n\n")

# Per-lever diamond markers (panel d): mean sampled u-value among the
# absolute-decoupling runs, and among the "selected" (top decile high-GDP/
# low-material-growth) runs.
diamonds_df <- dplyr::bind_rows(
  samples_df |> dplyr::filter(abs_decouple) |> dplyr::group_by(order, row_text, display_label) |> dplyr::summarise(mean_u = mean(u_value), .groups = "drop") |> dplyr::mutate(metric = "abs_decouple"),
  samples_df |> dplyr::filter(selected) |> dplyr::group_by(order, row_text, display_label) |> dplyr::summarise(mean_u = mean(u_value), .groups = "drop") |> dplyr::mutate(metric = "selected")
)


# STEP 9: Save outputs --------------------------------------------------------------

cat("STEP 9: Save outputs\n")

write_csv(
  plot_df_a |> dplyr::select(growth_bin, display_label, stack_order, pct, fill_hex),
  "Parameters/Intermediate/Figure4v2_GrowthImportance.csv"
)
write_csv(growth_df, "Parameters/Intermediate/Figure4v2_Scatter.csv")
write_csv(
  effects_sel |> dplyr::select(order, param, display_label, row_label, group_key, group_label, group_uniform, delta_label, family, coefficient, std_error, p_value, significant, effect_pp, ci_lo_pp, ci_hi_pp),
  "Parameters/Intermediate/Figure4v2_LeverEffects.csv"
)
write_csv(
  samples_df |> dplyr::select(order, param, display_label, run_id, u_value, abs_decouple, selected),
  "Parameters/Intermediate/Figure4v2_LeverSamples.csv"
)
write_csv(diamonds_df, "Parameters/Intermediate/Figure4v2_LeverDiamonds.csv")

cat("  Saved: Figure4v2_GrowthImportance.csv, Figure4v2_Scatter.csv,\n")
cat("         Figure4v2_LeverEffects.csv, Figure4v2_LeverSamples.csv, Figure4v2_LeverDiamonds.csv\n\n")

cat("=== Figure 4 v2 - Prepare Data done ===\n")

# EoF
