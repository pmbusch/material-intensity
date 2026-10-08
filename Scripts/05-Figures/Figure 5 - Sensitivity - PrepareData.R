## =============================================================================
## Figure 5 - Sensitivity - PrepareData.R  (run for Figure 5 - Sensitivity.R)
## Runs the expensive modelling behind the new 4-panel Figure 4 v2 once and
## caches the results, so "Figure 5 - Sensitivity.R" only has to load CSVs
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
##   "selected" subset: runs below the lm(mat_cagr ~ gdp_cagr) line by more
##   than SE_MULT x its residual standard error.
## Panel c: multivariate lm(mat_cagr ~ ., all 36 levers, real units) --
##   coefficient x a FIXED, physically meaningful delta per lever (e.g. "+1pp
##   population growth", "+0.1 kg/USD intensity", "+20yr lifetime" -- see
##   LEVER_META in STEP 7) = pp effect on growth, with a 95% CI. Real-unit
##   bound construction (STEP 3-5 below) is reused verbatim from
##   "Figure 7 - PrepareData.R", except ssp_u's bound is
##   redefined as an ANNUALIZED growth rate (see STEP 3) so their fixed delta
##   ("+1pp") is directly interpretable.
## Panel d: per run, FORECAST_END world primary consumption per capita by
##   material category, split into Low / High groups by a 2060 world metric
##   (DENSITY_GROUPS: biomass t/cap, fossil energy intensity MJ/$, metal and
##   mineral stock intensity kg/$), plus 0% / +2.5%/yr growth reference lines
##   from the 2025 level. (Raw u-draw samples/diamonds are still written for
##   reference but no longer plotted.)
##
## Output (Parameters/Intermediate/):
##   Figure5_GrowthImportance.csv - panel a stacked-bar data
##   Figure5_Scatter.csv          - panel b run-level growth/decoupling/selected
##   Figure5_LeverEffects.csv     - panel c regression effects + 95% CI (22 levers)
##   Figure5_LeverSamples.csv     - panel d raw u-draws, long (22 levers)
##   Figure5_LeverDiamonds.csv    - per-lever abs-decouple/selected mean markers (not plotted)
##   Figure5_Densities.csv        - panel d per-run 2060 per-capita consumption + group
##   Figure5_DensityLines.csv     - panel d 0% / +2.5%/yr reference lines + group thresholds
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)
library(lightgbm)
library(broom)

cat("=== Figure 4 v2 - Prepare Data ===\n\n")

GROWTH_WINDOW_START <- 2025L # matches FIG_VARIANT_ID below
FIG_VARIANT_ID <- "window_2025_2060"
N_TOP_OTHER <- 8L # panel a: population + GDP-per-capita growth + top-N other levers by global mean |SHAP|, rest collapsed to "Other"

GROWTH_BIN_BREAKS <- c(-Inf, 0, 0.01, 0.02, Inf)
GROWTH_BIN_LEVELS <- c("<0%", "0-1%", "1-2%", ">2%")

SE_MULT <- 1 # panel b "selected" subset: residual below the growth regression line by more than SE_MULT x sigma
ENERGY_DENSITY_MJ_PER_KG <- c("Coal" = 25.8, "Petroleum" = 42.3, "Natural Gas" = 48.0) # IPCC (2006) default NCVs, same as Figure 3

# Panel d: 2060 world metric per material that splits runs into a Low / High
# group; metals/minerals also require the run's sampled recovery rate (mean of
# Fe/NonFe recycling, or the shared downcycling target rate):
#   Low  = metric < lo AND rate > low_rate_min;  High = metric > hi AND rate < high_rate_max
DENSITY_GROUPS <- tibble::tribble(
  ~material               , ~metric_label              , ~unit   , ~lo , ~hi  , ~rate_label   , ~low_rate_min , ~high_rate_max ,
  "Biomass"               , "biomass consumption"      , "t/cap" , 3.2  , 3.5  , NA_character_ , NA_real_      , NA_real_       ,
  "Fossil fuels"          , "primary energy intensity" , "MJ/$"  , 3    , 6    , NA_character_ , NA_real_      , NA_real_       ,
  "Metal ores"            , "metal stock intensity"    , "kg/$"  , 0.41 , 0.42 , "recycled"    , 0.55          , 0.45           ,
  "Non-metallic minerals" , "mineral stock intensity"  , "kg/$"  , 14.5 , 15.5 , "downcycled"  , 0.55          , 0.45
) # thresholds relaxed so each Low/High group holds >= ~150 runs
SSP_GROWTH_YEARS <- FORECAST_END - 2024L # ssp_u bound window (2024->FORECAST_END)

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

input_matrix <- read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE) |> arrange(run_id)
decoupling <- arrow::read_parquet("Results/MC/mc_decoupling.parquet")
ssp_drivers <- read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)

# Each run's SSP label (closest world GDP/cap growth, 02-RunSimulations.R STEP 4)
# and its achieved world population / GDP-per-capita annual growth 2024 ->
# FORECAST_END (panel a driver features)
ssp_assign <- read_csv("Results/MC/mc_ssp_assignment.csv", show_col_types = FALSE)
run_dom <- ssp_assign |> dplyr::select(run_id, ssp = ssp_label)

feature_cols <- input_matrix |> dplyr::select(-run_id) |> names()
n_feat <- length(feature_cols)

cat("  Runs:", n_distinct(input_matrix$run_id), " | Features:", n_feat, "\n\n")


# STEP 2: Growth outcome per run -- TOTAL material/GDP CAGR, 2025-FORECAST_END ----

cat("STEP 2: Growth outcome per run (Total, ", FIG_VARIANT_ID, ")\n", sep = "")

growth_df <- decoupling |>
  dplyr::filter(variant_id == FIG_VARIANT_ID, material_group == "Total") |>
  dplyr::transmute(run_id, mat_cagr = mf_total_cagr, gdp_cagr = gdp_total_cagr) |>
  tidyr::drop_na(mat_cagr, gdp_cagr) |>
  dplyr::left_join(run_dom, by = "run_id") |> # dominant SSP (panel b point colour)
  # 2060 median warming per run: fossil-index bracket of emissions scenarios
  # within each of the run's two SSPs, blended by the SSP weights (02-RunSimulations.R)
  dplyr::left_join(
    readr::read_csv("Results/MC/mc_power_run_link.csv", show_col_types = FALSE) |> dplyr::select(run_id, fossil_index_world, t_2060),
    by = "run_id"
  ) |>
  dplyr::mutate(
    abs_decouple = mat_cagr < 0,
    growth_bin = cut(mat_cagr, breaks = GROWTH_BIN_BREAKS, labels = GROWTH_BIN_LEVELS, right = FALSE)
  )

# "Selected" subset (panel b): runs clearly BELOW the material-vs-GDP growth
# regression line -- residual of lm(mat_cagr ~ gdp_cagr) below -SE_MULT x the
# regression's residual standard error (sigma), i.e. less material growth
# than their GDP growth would predict. Replaces the former top-10%
# composite-rank "high GDP / low material growth" subset.
fit_growth <- lm(mat_cagr ~ gdp_cagr, data = growth_df)
growth_df <- growth_df |>
  dplyr::mutate(
    fit_mat_cagr = unname(fitted(fit_growth)),
    resid_sigma = summary(fit_growth)$sigma,
    se_mult = SE_MULT,
    selected = (mat_cagr - fit_mat_cagr) < -SE_MULT * resid_sigma
  )

cat("  Runs with valid growth outcome:", nrow(growth_df), "| Absolute decoupling:", sum(growth_df$abs_decouple), "| Selected (below regression line by >", SE_MULT, "SE):", sum(growth_df$selected), "\n")
cat("  Growth bin counts:\n")
print(table(growth_df$growth_bin))
cat("\n")


# STEP 3: Real-value bounds & units per parameter (reused from "Figure 7 - PrepareData.R" STEP 3, verbatim) ----

cat("STEP 3: Real-value bounds & units per parameter\n")

# Intensity bounds (world kg/$ = 2024-GDP-weighted regional endpoint bounds,
# flows averaged across SSPs) -- written by 01-Sampling.R; display only.
bound_lkp_intensity <- readr::read_csv("Parameters/Simulation/intensity_world_bounds.csv", show_col_types = FALSE) |>
  dplyr::select(param, bound_min, bound_max) |>
  dplyr::mutate(unit = "kg_usd")

# SSP position (population / GDP-per-capita) real-unit range: the [0,1] draw
# locates a point between the two bracketing SSPs' world growth ratios
# (2024 -> FORECAST_END), same ranking as 02-RunSimulations.R STEP 3.
world_agg_bounds <- ssp_drivers |>
  dplyr::filter(variable %in% c("Population", "GDP|PPP"), year %in% c(2024L, FORECAST_END), scenario %in% SSP_SAMPLED) |>
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
# Total (not per-capita) world GDP ratio -- ssp_u moves population and GDP/capita
# together, so its panel c effect is expressed per +1pp of TOTAL GDP growth
ssp_ratio_gdp_total <- world_end_bounds |>
  dplyr::left_join(world_2024_bounds, by = "scenario") |>
  dplyr::mutate(val = (pop_end * gdppc_end) / (pop_base * gdppc_base))

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

# Global scalars (MC_PARAMS, 00-Parameters.R): min/central/max, sampled
# semi-uniformly around the central value (STEP 4 applies the same mapping)
bound_lkp_scalar <- dplyr::bind_rows(
  MC_PARAMS |>
    dplyr::transmute(
      param = col,
      bound_min = min,
      bound_max = max,
      bound_central = central,
      unit = dplyr::if_else(col %in% c("target_year", "recyc_convergence_yr"), "year_abs", "pct")
    ),
  # Population/GDP-per-capita: real-unit bound is the ANNUALIZED equivalent
  # growth rate (ratio^(1/SSP_GROWTH_YEARS) - 1), not the raw cumulative
  # 2024->FORECAST_END ratio -- so a lever regression coefficient here is
  # directly "growth-outcome pp per +1pp of population/GDP-per-capita annual
  # growth", matching every other lever's fixed real-unit delta convention
  # used in panel c (STEP 7 below).
  tibble::tibble(
    param = "ssp_u",
    bound_min = min(ssp_ratio_gdp_total$val)^(1 / SSP_GROWTH_YEARS) - 1,
    bound_max = max(ssp_ratio_gdp_total$val)^(1 / SSP_GROWTH_YEARS) - 1,
    bound_central = NA_real_,
    unit = "pct_annual"
  )
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

# Linear min..max, except global scalars with a central value: semi-uniform
# (u < 0.5 -> min..central, else central..max), as in 02-RunSimulations.R
real_matrix <- input_matrix
for (p in feature_cols) {
  bmin <- bound_lkp$bound_min[bound_lkp$param == p]
  bmax <- bound_lkp$bound_max[bound_lkp$param == p]
  bcen <- bound_lkp$bound_central[bound_lkp$param == p]
  u <- real_matrix[[p]]
  if (is.na(bcen)) {
    real_matrix[[p]] <- bmin + u * (bmax - bmin)
  } else {
    real_matrix[[p]] <- ifelse(u < 0.5, bmin + 2 * u * (bcen - bmin), bcen + 2 * (u - 0.5) * (bmax - bcen))
  }
}


# STEP 5: Display labels & family classification (36 levers, reused from "Figure 7 - PrepareData.R" STEP 5, verbatim) ----

cat("STEP 5: Display labels & family classification\n")

label_map <- c(
  ssp_u = "SSP position",
  target_year = "Intensity target year",
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
  recycling_rate_fe = "Recycling % - Fe",
  recycling_rate_nonfe = "Recycling % - NonFe",
  grade_ore_fe = "Ore grade - Fe",
  grade_ore_nonfe = "Ore grade - NonFe",
  recyc_convergence_yr = "Recycling target year",
  downcycling = "Downcycling %",
  max_secondary_build_civil = "Max secondary: Buildings & civil",
  max_secondary_roads = "Max secondary: Roads",
  share_concrete_buildings = "Concrete share: Buildings",
  share_concrete_civil = "Concrete share: Civil",
  share_agg_concrete = "Aggregate share of concrete",
  share_granular_road = "Granular share: Roads",
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
      param %in% c("ssp_u") ~ "Driver SSP",
      stringr::str_detect(param, "grade_ore") ~ "Mining",
      stringr::str_detect(param, "intensity|target_year") ~ "Intensity",
      stringr::str_detect(param, "recyc|downcycl|max_secondary|^share_") ~ "Material recovery",
      stringr::str_detect(param, "lifetime") ~ "Lifetime",
      TRUE ~ "Other"
    ),
    display_label = col_labels[param]
  ) |>
  dplyr::left_join(bound_lkp |> dplyr::select(param, unit), by = "param")

# Panel a only: the ssp_u draw is replaced by the run's achieved world
# population and GDP-per-capita annual growth, so each driver gets its own segment
ssp_feats <- c("pop_growth", "gdppc_growth")
col_labels[ssp_feats] <- c("Population", "GDP per capita")
family_by_param <- family_by_param |>
  dplyr::bind_rows(tibble::tibble(param = ssp_feats, family = "Driver SSP", display_label = col_labels[ssp_feats], unit = "pct_annual"))


# STEP 6: Panel a -- LightGBM + TreeSHAP on growth rate, binned by growth category ----

cat("STEP 6: Panel a -- LightGBM + SHAP on growth rate\n")

# World population and GDP-per-capita growth instead of the ssp_u draw
shap_cols <- c(setdiff(feature_cols, "ssp_u"), ssp_feats)

df_shap <- input_matrix |>
  dplyr::inner_join(growth_df |> dplyr::select(run_id, mat_cagr, growth_bin), by = "run_id") |>
  dplyr::left_join(ssp_assign |> dplyr::select(run_id, dplyr::all_of(ssp_feats)), by = "run_id")

X <- as.matrix(df_shap[, shap_cols])
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

shap_vals <- predict(model, X, type = "contrib")[, seq_len(length(shap_cols))]
colnames(shap_vals) <- shap_cols

# Top-N levers by GLOBAL mean |SHAP| (same param set shown in every growth
# bin, so a lever's presence/absence across bins is comparable) + "Other".
global_mean_shap <- colMeans(abs(shap_vals))
param_order <- sort(global_mean_shap, decreasing = TRUE)
# Population and GDP per capita are always shown, plus the top other levers
top_params <- c(ssp_feats, head(setdiff(names(param_order), ssp_feats), N_TOP_OTHER))
other_params <- setdiff(names(param_order), top_params)

shap_abs_df <- as.data.frame(abs(shap_vals))
shap_abs_df$growth_bin <- df_shap$growth_bin

bin_means <- shap_abs_df |> dplyr::group_by(growth_bin) |> dplyr::summarise(dplyr::across(dplyr::all_of(shap_cols), mean), .groups = "drop")
bin_mat <- as.matrix(bin_means[, shap_cols])
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

fam_rows_all <- family_by_param |> dplyr::filter(param != "ssp_u") |> dplyr::arrange(family, display_label) # ssp_u not in panel a
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
  "ssp_u"                                    , "GDP"            , "growth"    , "Annual growth rate"  , 0.01        , "+1%/yr"     ,
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
  "recycling_rate_fe"                         , "Fe"             , "recycling" , "Recycling rate"      , 0.25        , "+25%"       ,
  "recycling_rate_nonfe"                      , "NonFe"          , "recycling" , "Recycling rate"      , 0.25        , "+25%"       ,
  "grade_ore_fe"                              , "Fe"             , "ore_grade" , "Ore grade"            , 0.25        , "+25%"       ,
  "grade_ore_nonfe"                           , "NonFe"          , "ore_grade" , "Ore grade"            , 0.01        , "+1%"        ,
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


# STEP 8b: Panel d -- 2060 per-capita consumption per material, Low/High groups ----
# World totals first, then ratios (same aggregation as Figure 2): per run,
# primary consumption, in-use stock and fossil energy are summed over regions
# and divided by the run's world population / GDP (continuous SSP blend,
# region-level 2024 anchors x blended indices).

cat("STEP 8b: Panel d -- per-capita consumption densities\n")

results_fig <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year %in% c(2025L, FORECAST_END)) |>
  dplyr::select(run_id, region, material_group, material_key, year, primary_consumption_Mt, in_use_stock_Mt, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::collect()

run_ssp <- results_fig |> dplyr::distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)

base_2024 <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |>
  dplyr::filter(year == 2024L) |>
  dplyr::select(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::left_join(
    read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE) |>
      dplyr::filter(year == 2024L) |>
      dplyr::select(region = Region, pop_2024 = population),
    by = "region"
  )

ssp_idx <- ssp_drivers |>
  dplyr::filter(variable %in% c("Population", "GDP|PPP [per capita]"), year %in% c(2025L, FORECAST_END)) |>
  dplyr::select(scenario, region, year, variable, index)

world_by_run <- run_ssp |>
  dplyr::left_join(ssp_idx |> dplyr::rename(ssp_lo = scenario, idx_lo = index), by = "ssp_lo", relationship = "many-to-many") |>
  dplyr::left_join(ssp_idx |> dplyr::rename(ssp_hi = scenario, idx_hi = index), by = c("ssp_hi", "region", "year", "variable")) |>
  dplyr::mutate(idx = ssp_share_lo * idx_lo + (1 - ssp_share_lo) * idx_hi) |>
  dplyr::select(run_id, region, year, variable, idx) |>
  tidyr::pivot_wider(names_from = variable, values_from = idx) |>
  dplyr::left_join(base_2024, by = "region") |>
  dplyr::mutate(pop = pop_2024 * Population, gdp = gdp_2024 * Population * `GDP|PPP [per capita]`) |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(world_pop = sum(pop, na.rm = TRUE), world_gdp = sum(gdp, na.rm = TRUE), .groups = "drop")

# Per run x year x material: world totals, then per-capita consumption and the grouping metric
mat_by_run <- results_fig |>
  dplyr::mutate(
    material = dplyr::case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group %in% c("metal_fe", "metal_nonfe") ~ "Metal ores",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals"
    ),
    energy_MJ = primary_consumption_Mt * 1e9 * dplyr::coalesce(unname(ENERGY_DENSITY_MJ_PER_KG[material_key]), 0)
  ) |>
  dplyr::group_by(run_id, year, material) |>
  dplyr::summarise(
    primary_Mt = sum(primary_consumption_Mt, na.rm = TRUE),
    stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), # metals: metal mass
    energy_MJ = sum(energy_MJ, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(world_by_run, by = c("run_id", "year")) |>
  dplyr::mutate(
    percap_t = primary_Mt * 1e6 / world_pop,
    metric = dplyr::case_when(
      material == "Biomass" ~ percap_t, # t/cap
      material == "Fossil fuels" ~ energy_MJ / world_gdp, # MJ/$
      TRUE ~ stock_Mt * 1e9 / world_gdp # kg stock/$
    )
  )

# Sampled target recovery rates per run (real units): metals = mean Fe/NonFe
# recycling, minerals = shared downcycling endpoint (real_matrix: semi-uniform mapped)
rate_by_run <- real_matrix |>
  dplyr::transmute(
    run_id,
    `Metal ores` = (recycling_rate_fe + recycling_rate_nonfe) / 2,
    `Non-metallic minerals` = downcycling
  ) |>
  tidyr::pivot_longer(-run_id, names_to = "material", values_to = "rate")

density_df <- mat_by_run |>
  dplyr::filter(year == FORECAST_END) |>
  dplyr::left_join(DENSITY_GROUPS, by = "material") |>
  dplyr::left_join(rate_by_run, by = c("run_id", "material")) |>
  dplyr::mutate(
    grp = dplyr::case_when(
      metric < lo & (is.na(low_rate_min) | rate > low_rate_min) ~ "Low",
      metric > hi & (is.na(high_rate_max) | rate < high_rate_max) ~ "High",
      TRUE ~ NA_character_
    ),
    mg = primary_Mt * 1e9 / world_gdp, # material consumption per GDP, kg/$ (Alt1 v2 panel c)
    total_Gt = primary_Mt / 1e3 # world primary consumption, Gt (Alt1 v2 panel c)
  ) |>
  # sampled target ore grades (real units), for the Alt1 v2 panel c metal groups
  dplyr::left_join(real_matrix |> dplyr::select(run_id, grade_ore_fe, grade_ore_nonfe), by = "run_id") |>
  dplyr::select(run_id, material, percap_t, total_Gt, metric, grp, mg, rate, grade_ore_fe, grade_ore_nonfe)

# Reference lines: 2025 level (median across runs; per capita v0, total v0_Gt) held
# flat (0%/yr) or grown at 2.5%/yr to FORECAST_END
density_lines <- mat_by_run |>
  dplyr::filter(year == 2025L) |>
  dplyr::group_by(material) |>
  dplyr::summarise(v0 = median(percap_t, na.rm = TRUE), v0_Gt = median(primary_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  dplyr::mutate(v25 = v0 * 1.025^(FORECAST_END - 2025L)) |>
  dplyr::left_join(DENSITY_GROUPS, by = "material")

# Extra row: TOTAL primary consumption per capita (all 4 materials), one group per dominant SSP
total_by_run <- mat_by_run |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(percap_t = sum(percap_t), .groups = "drop")

density_df <- dplyr::bind_rows(
  density_df,
  total_by_run |>
    dplyr::filter(year == FORECAST_END) |>
    dplyr::left_join(run_dom, by = "run_id") |>
    dplyr::transmute(run_id, material = "All materials", percap_t, metric = NA_real_, grp = ssp)
)
density_lines <- dplyr::bind_rows(
  density_lines,
  total_by_run |>
    dplyr::filter(year == 2025L) |>
    dplyr::summarise(v0 = median(percap_t, na.rm = TRUE)) |>
    dplyr::mutate(material = "All materials", v25 = v0 * 1.025^(FORECAST_END - 2025L))
)

cat("  2060 metric range and group sizes:\n")
print(
  density_df |>
    dplyr::group_by(material) |>
    dplyr::summarise(
      metric_p05 = quantile(metric, 0.05, na.rm = TRUE), metric_p50 = median(metric, na.rm = TRUE), metric_p95 = quantile(metric, 0.95, na.rm = TRUE),
      n_low = sum(grp == "Low", na.rm = TRUE), n_high = sum(grp == "High", na.rm = TRUE), n_total = dplyr::n(),
      .groups = "drop"
    )
)
cat("\n")


# STEP 9: Save outputs --------------------------------------------------------------

cat("STEP 9: Save outputs\n")

write_csv(
  plot_df_a |> dplyr::select(growth_bin, display_label, stack_order, pct, fill_hex),
  "Parameters/Intermediate/Figure5_GrowthImportance.csv"
)
write_csv(growth_df, "Parameters/Intermediate/Figure5_Scatter.csv")
write_csv(
  effects_sel |> dplyr::select(order, param, display_label, row_label, group_key, group_label, group_uniform, delta_label, family, coefficient, std_error, p_value, significant, effect_pp, ci_lo_pp, ci_hi_pp),
  "Parameters/Intermediate/Figure5_LeverEffects.csv"
)
write_csv(
  samples_df |> dplyr::select(order, param, display_label, run_id, u_value, abs_decouple, selected),
  "Parameters/Intermediate/Figure5_LeverSamples.csv"
)
write_csv(diamonds_df, "Parameters/Intermediate/Figure5_LeverDiamonds.csv")
write_csv(density_df, "Parameters/Intermediate/Figure5_Densities.csv")
write_csv(density_lines, "Parameters/Intermediate/Figure5_DensityLines.csv")

cat("  Saved: Figure5_GrowthImportance.csv, Figure5_Scatter.csv,\n")
cat("         Figure5_LeverEffects.csv, Figure5_LeverSamples.csv, Figure5_LeverDiamonds.csv\n\n")

cat("=== Figure 4 v2 - Prepare Data done ===\n")

# EoF
