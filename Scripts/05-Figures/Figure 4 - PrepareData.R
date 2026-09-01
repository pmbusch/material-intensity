## =============================================================================
## Figure 4 - PrepareData.R
## Runs the expensive models behind Figure 4 once and caches the results, so
## "Figure 4 - VariableImportance.R" only has to load CSVs and plot.
##
## Merges the data pipelines of:
##   Scripts/05-Exploratory/04-VariableImportance.R (Part B: LightGBM/TreeSHAP
##     variable importance on DMC 2050 -- only the pieces behind its "flipped"
##     panel, STEP 9B; the SRRC part and the XGBoost interaction analysis are
##     not used by Figure 4 and are skipped here)
##   Scripts/05-Exploratory/14-LowHighConsumptionParameters.R (decoupling
##     subset: top 10% of runs with jointly high 2050 GDP/capita and low 2050
##     primary consumption per capita)
## Both scripts reconstruct the same per-run 2050 world GDP/capita from the
## continuous SSP blend; that reconstruction is done once here.
##
## Output (Parameters/Intermediate/):
##   Figure3_RunScatter.csv      - per-run DMC/GDP-per-capita, per-capita CAGRs
##                                 (mat_pc_cagr/gdp_pc_cagr, 2025-2050) + decoupling flag
##   Figure3_ParamImportance.csv - SHAP stacked-area data (flipped panel)
##   Figure3_ParamLabels.csv     - stacked-area in-panel label positions
##   Figure3_ParamBars.csv       - decoupling-subset parameter bar stats
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)
library(lightgbm)

cat("=== Figure 4 - Prepare Data ===\n\n")

# Constants -------------------------------------------------------------------
SELECT_FRACTION <- 0.10
ALPHA_SIG <- 0.05
N_TOP <- 12L
N_BINS <- 20L


# STEP 1: Load data -------------------------------------------------------------

cat("STEP 1: Load data\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet")
input_matrix <- read_csv("Parameters/MC/mc_input_matrix.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)

results_target <- results |> filter(year == TARGET_YEAR)

cat("  Runs:", n_distinct(results_target$run_id), "\n\n")


# STEP 2: Per-run 2050 DMC, world GDP/capita & population ------------------------
# Same SSP-blend reconstruction that 04-VariableImportance.R (STEP 1B) and
# 14-LowHighConsumptionParameters.R (STEP 2) each did independently -- done
# once here, shared by the scatter panel and the decoupling selection below.

cat("STEP 2: Per-run 2050 DMC, GDP/capita & population\n")

dmc_2050 <- results_target |>
  group_by(run_id) |>
  summarise(DMC_2050_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop")

run_ssp <- results_target |>
  distinct(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo)

gdp_2024_region <- gdp_region_hist |>
  filter(year == 2024) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == 2024) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year == TARGET_YEAR) |>
  dplyr::select(scenario, region, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year == TARGET_YEAR) |>
  dplyr::select(scenario, region, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo) |>
  left_join(pop_idx_region |> rename(pop_ssp_lo = scenario), by = "pop_ssp_lo", relationship = "many-to-many") |>
  left_join(pop_idx_region |> rename(pop_ssp_hi = scenario, pop_idx_hi = pop_idx), by = c("pop_ssp_hi", "region")) |>
  mutate(pop_idx_blend = pop_ssp_share_lo * pop_idx + (1 - pop_ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(gdppc_ssp_lo = scenario), by = "gdppc_ssp_lo", relationship = "many-to-many") |>
  left_join(
    gdppc_idx_region |> rename(gdppc_ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("gdppc_ssp_hi", "region")
  ) |>
  mutate(gdppc_idx_blend = gdppc_ssp_share_lo * gdppc_idx + (1 - gdppc_ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, gdppc_idx_blend)

run_gdp_pop <- pop_idx_blend |>
  left_join(pop_2024_region, by = "region") |>
  left_join(gdppc_idx_blend, by = c("run_id", "region")) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(pop_run = pop_2024 * pop_idx_blend, gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend) |>
  group_by(run_id) |>
  summarise(world_pop = sum(pop_run, na.rm = TRUE), world_gdp = sum(gdp_run, na.rm = TRUE), .groups = "drop") |>
  mutate(GDPcap_2050 = world_gdp / world_pop / 1e3) |> # '000 USD per person
  dplyr::select(run_id, world_pop, GDPcap_2050)

run_data <- dmc_2050 |>
  left_join(run_gdp_pop, by = "run_id") |>
  filter(!is.na(GDPcap_2050)) |>
  mutate(primary_percap_t = DMC_2050_Mt * 1e6 / world_pop) # tonnes per capita

cat(
  "  GDP/capita 2050 range: [",
  round(min(run_data$GDPcap_2050)),
  ",",
  round(max(run_data$GDPcap_2050)),
  "] k USD\n\n"
)

# 2024 world GDP/capita anchor (same units as GDPcap_2050), for the scatter's
# "today" reference line -- world totals from the same 2024 region tables used
# to reconstruct GDPcap_2050 above, not run-dependent so it's a single number.
gdpcap_2024_kUSD <- sum(gdp_2024_region$gdp_2024) / sum(pop_2024_region$pop_2024) / 1e3
run_data <- run_data |> mutate(gdpcap_2024_kUSD = gdpcap_2024_kUSD)
cat("  GDP/capita 2024 (world):", round(gdpcap_2024_kUSD), "k USD\n\n")


# STEP 3: Decoupling subset (top 10% high GDP/capita & low consumption) ----------

cat("STEP 3: Select top", SELECT_FRACTION * 100, "% decoupling subset\n")

# Composite percentile rank: high GDP/capita + low consumption per capita, each in [0, 1]
run_data <- run_data |>
  mutate(
    pctile_gdppc = percent_rank(GDPcap_2050),
    pctile_low_consumption = percent_rank(desc(primary_percap_t)),
    decoupling_score = pctile_gdppc + pctile_low_consumption
  )

selected_ids <- run_data |> slice_max(decoupling_score, prop = SELECT_FRACTION, with_ties = FALSE) |> pull(run_id)
run_data <- run_data |> mutate(selected = run_id %in% selected_ids)

cat("  Selected:", length(selected_ids), "of", nrow(run_data), "runs\n\n")

# Join the rigorous CAGR-based decoupling classification (Total material group,
# "main_2050" variant: window 2025-2050, actual population weights) from
# 19-Decoupling.R, for Figure 4's scatter colour -- additive column, independent
# of the percentile-rank `selected` flag above. Uses the 2025-2050 variant (not
# the 2040-2060 main_actual variant) so the classification window ends at the
# same year the scatter plots (2050) -- comparing CAGR through 2060 against a
# 2050 snapshot is why a "No decoupling" point can otherwise sit to the left of
# a "Relative decoupling" one: level (this scatter) and growth-rate (CAGR
# through a different endpoint) are different comparisons.
# mat_pc_cagr/gdp_pc_cagr are pulled straight from 19-Decoupling.R rather than
# re-derived here, so Figure 4 panel b's growth-rate axes are numerically
# identical to the CAGRs the colour classification itself is based on.
decoupling_total <- arrow::read_parquet("Results/MC/mc_decoupling.parquet") |>
  filter(variant_id == "main_2050", material_group == "Total") |>
  dplyr::select(run_id, decoupling_class, mat_pc_cagr = mf_percap_cagr, gdp_pc_cagr = gdp_percap_cagr)

run_data <- run_data |> left_join(decoupling_total, by = "run_id")
cat("  Decoupling class matched:", sum(!is.na(run_data$decoupling_class)), "of", nrow(run_data), "runs\n\n")

write_csv(run_data, "Parameters/Intermediate/Figure3_RunScatter.csv")
cat("  Saved: Parameters/Intermediate/Figure3_RunScatter.csv\n\n")


# STEP 4: Fit LightGBM surrogate on DMC 2050 -------------------------------------

cat("STEP 4: Fit LightGBM surrogate (80/20 split, seed =", GLOBAL_SEED, ")\n")

feature_cols <- input_matrix |> dplyr::select(-run_id) |> names()

df_model <- input_matrix |> arrange(run_id) |> left_join(dmc_2050, by = "run_id")
stopifnot(sum(is.na(df_model$DMC_2050_Mt)) == 0L)

X <- as.matrix(df_model[, feature_cols])
y <- df_model$DMC_2050_Mt

set.seed(GLOBAL_SEED)
n <- nrow(X)
train_idx <- sample(n, floor(0.8 * n))
test_idx <- setdiff(seq_len(n), train_idx)

dtrain <- lgb.Dataset(X[train_idx, ], label = y[train_idx])

params <- list(
  objective = "regression",
  metric = "rmse",
  num_leaves = 63L,
  learning_rate = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5L,
  verbose = -1L
)

model <- lgb.train(params = params, data = dtrain, nrounds = 1000L, verbose = -1L)

y_pred_test <- predict(model, X[test_idx, ])
ss_res <- sum((y[test_idx] - y_pred_test)^2)
ss_tot <- sum((y[test_idx] - mean(y[test_idx]))^2)
r2 <- 1 - ss_res / ss_tot

cat(sprintf("\n  *** Held-out R^2 = %.4f ***\n\n", r2))

if (r2 < 0.80) {
  warning(sprintf("R^2 = %.4f < 0.80 -- surrogate unreliable; SHAP attributions may be meaningless. Stopping.", r2))
  stop("Low R2 -- aborting.")
}


# STEP 5: Compute TreeSHAP & bin by DMC 2050 -------------------------------------

cat("STEP 5: Compute TreeSHAP (full dataset,", n, "rows)\n")

shap_raw <- predict(model, X, type = "contrib")
shap_vals <- shap_raw[, seq_len(ncol(shap_raw) - 1L)]
colnames(shap_vals) <- feature_cols

bin_breaks <- quantile(y, probs = seq(0, 1, length.out = N_BINS + 1L))
bin_ids <- cut(y, breaks = bin_breaks, include.lowest = TRUE, labels = FALSE)

bin_lo <- tapply(y, bin_ids, min)
bin_hi <- tapply(y, bin_ids, max)
bin_mid_gt <- as.numeric((bin_lo + bin_hi) / 2e3)


# STEP 6: Aggregate mean |SHAP| per bin, collapse to top N + Other --------------

cat("STEP 6: Aggregate mean |SHAP| per bin, collapse to top", N_TOP, "+ Other\n")

shap_abs_df <- as.data.frame(abs(shap_vals))
shap_abs_df$bin <- bin_ids

bin_means <- shap_abs_df |> group_by(bin) |> summarise(across(everything(), mean), .groups = "drop") |> arrange(bin)

bin_mat <- as.matrix(bin_means[, feature_cols])
bin_norm <- sweep(bin_mat, 1, rowSums(bin_mat), "/") * 100

global_mean_shap <- colMeans(abs(shap_vals))
param_order <- sort(global_mean_shap, decreasing = TRUE)

top_params <- names(param_order)[seq_len(N_TOP)]
other_params <- names(param_order)[-seq_len(N_TOP)]

bin_top <- bin_norm[, top_params, drop = FALSE]
if (length(other_params) > 0L) {
  bin_top <- cbind(bin_top, Other = rowSums(bin_norm[, other_params, drop = FALSE]))
}

# Display labels for the SHAP panel. "S/G" = stock materials (metal
# ores/non-metallic minerals intensity, i.e. stock-per-GDP); "M/G" = flow
# materials (biomass/fossil Kaya intensity, i.e. mass-per-GDP).
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
  recycling_Fe_global = "Recycling % – Fe",
  recycling_NonFe_global = "Recycling % – NonFe",
  grade_ore_fe_u = "Ore grade – Fe",
  grade_ore_nonfe_u = "Ore grade – NonFe",
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

cat("  Top parameters (sorted by global mean |SHAP|):\n")
for (p in top_params) {
  cat(sprintf("    %-45s  mean |SHAP| = %7.1f Mt\n", col_labels[p], global_mean_shap[p]))
}
cat("\n")


# STEP 7: Build stacked-area data & in-panel label positions ---------------------

cat("STEP 7: Build stacked-area plot data (flipped panel)\n")

stack_order_params <- rev(top_params)
all_display <- c(if (length(other_params) > 0L) "Other", stack_order_params)
all_labels_ordered <- c(col_labels[top_params], if (length(other_params) > 0L) "Other")

# Family palette -- identical to the bar panel's family_pal (Figure 4 -
# VariableImportance.R) so both panels read as one consistent colour system:
# every SHAP colour below is a shade of its family's base hue instead of an
# independently-chosen colour (previously e.g. GDP/capita was purple here vs.
# blue in the bar panel's "Driver SSP" family -- now both are blue shades).
family_pal <- c(
  "Driver SSP" = "#1f78b4",
  "Regional divergence" = "#6a3d9a",
  "Intensity" = "#e31a1c",
  "Material recovery" = "#33a02c",
  "Mining" = "#8c510a",
  "Lifetime" = "#ff7f00",
  "Other" = "#999999"
)

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
  dplyr::arrange(family, display_label)

# Ramp each family from a light tint to a dark shade of its base colour
# (grDevices only, no new package), one shade per member, so same-family
# parameters read as a gradient instead of unrelated hues.
SHAP_PARAM_COLORS <- c("Other" = family_pal[["Other"]])
for (fam in setdiff(names(family_pal), "Other")) {
  fam_rows <- family_by_param |> dplyr::filter(family == fam)
  n_fam <- nrow(fam_rows)
  base_rgb <- grDevices::col2rgb(family_pal[[fam]]) / 255
  light_rgb <- pmin(1, base_rgb + (1 - base_rgb) * 0.6)
  dark_rgb <- base_rgb * 0.55
  light_hex <- grDevices::rgb(light_rgb[1], light_rgb[2], light_rgb[3])
  dark_hex <- grDevices::rgb(dark_rgb[1], dark_rgb[2], dark_rgb[3])
  fam_shades <- if (n_fam == 1) {
    family_pal[[fam]]
  } else {
    grDevices::colorRampPalette(c(light_hex, family_pal[[fam]], dark_hex))(n_fam)
  }
  SHAP_PARAM_COLORS <- c(SHAP_PARAM_COLORS, stats::setNames(fam_shades, fam_rows$display_label))
}

fill_vals <- setNames(
  vapply(
    as.character(all_labels_ordered),
    function(lbl) if (lbl %in% names(SHAP_PARAM_COLORS)) SHAP_PARAM_COLORS[[lbl]] else "#AAAAAA",
    character(1L)
  ),
  all_labels_ordered
)

plot_df <- as_tibble(bin_top, rownames = "bin") |>
  mutate(bin_idx = as.integer(bin), x_gt = bin_mid_gt[bin_idx]) |>
  pivot_longer(cols = -c(bin, bin_idx, x_gt), names_to = "parameter", values_to = "pct") |>
  mutate(
    display_label = if_else(parameter == "Other", "Other", col_labels[parameter]),
    fill_hex = fill_vals[display_label],
    # Internal-only factor to fix the stacking order (bottom -> top of the
    # area chart); stack_order is what gets persisted so the figure script
    # can rebuild the same order without redoing the SHAP ranking.
    display_label_factor = factor(display_label, levels = rev(all_labels_ordered)),
    stack_order = as.integer(display_label_factor)
  )

mid_bin_idx <- ceiling(N_BINS / 2)
label_df <- plot_df |>
  filter(bin_idx == mid_bin_idx) |>
  arrange(desc(display_label_factor)) |>
  mutate(
    cum_top = cumsum(pct),
    cum_bot = lag(cum_top, default = 0),
    mid_y = (cum_top + cum_bot) / 2,
    show_label = pct >= 1,
    mid_x_gt = x_gt[1] + 5
  )

# Text colour per label (white on dark fills, black on light fills), via
# relative luminance (WCAG formula) computed directly on the hex fill.
r <- strtoi(substr(label_df$fill_hex, 2, 3), 16L) / 255
g <- strtoi(substr(label_df$fill_hex, 4, 5), 16L) / 255
b <- strtoi(substr(label_df$fill_hex, 6, 7), 16L) / 255
lin_r <- ifelse(r <= 0.04045, r / 12.92, ((r + 0.055) / 1.055)^2.4)
lin_g <- ifelse(g <= 0.04045, g / 12.92, ((g + 0.055) / 1.055)^2.4)
lin_b <- ifelse(b <= 0.04045, b / 12.92, ((b + 0.055) / 1.055)^2.4)
lum <- 0.2126 * lin_r + 0.7152 * lin_g + 0.0722 * lin_b
label_df$text_col <- ifelse(lum < 0.25, "white", "black")

write_csv(
  plot_df |> dplyr::select(bin_idx, x_gt, display_label, stack_order, pct, fill_hex),
  "Parameters/Intermediate/Figure3_ParamImportance.csv"
)
write_csv(
  label_df |> dplyr::select(display_label, stack_order, mid_x_gt, mid_y, pct, fill_hex, text_col, show_label),
  "Parameters/Intermediate/Figure3_ParamLabels.csv"
)
cat("  Saved: Parameters/Intermediate/Figure3_ParamImportance.csv, Figure3_ParamLabels.csv\n\n")


# STEP 8: Parameter setup for the decoupling-subset bar panel --------------------

cat("STEP 8: Parameter setup for decoupling-subset bar panel\n")

# NOTE: only names(param_labels) is used below (family_lkp/num_cols) -- the
# values themselves aren't displayed anywhere, but kept in sync with
# label_map's naming conventions to avoid a confusing 3rd, stale dictionary.
param_labels <- c(
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
  recycling_Fe_global = "Recycling %: Fe",
  recycling_NonFe_global = "Recycling %: NonFe",
  recyc_convergence_yr_global = "Recycling target year",
  downcycling_buildings_global = "Downcycling: Buildings",
  downcycling_civil_infrastructure_global = "Downcycling: Infrastructure",
  gap_persistence_rates = "Gap persistence: Recycling %",
  sub_factor_recycling_same = "Recycling substitution factor",
  sub_factor_recycling_same_civil = "Recycling substitution (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Downcycling substitution factor",
  grade_ore_fe_u = "Ore grade: Fe",
  grade_ore_nonfe_u = "Ore grade: NonFe",
  lifetime_mean_buildings = "Lifetime Buildings",
  lifetime_mean_civil_infrastructure = "Lifetime Infrastructure",
  lifetime_mean_machinery = "Lifetime Machinery",
  lifetime_mean_short_lived = "Lifetime Short-lived",
  lifetime_k_buildings = "Lifetime shape: Buildings",
  lifetime_k_civil_infrastructure = "Lifetime shape: Infrastructure",
  lifetime_k_machinery = "Lifetime shape: Machinery",
  lifetime_k_short_lived = "Lifetime shape: Short-lived"
)

# EDIT THESE: dictionary of display labels for the bar panel (param name ->
# text shown on the plot). Edit values freely; keep them unique within a
# family (duplicate text collapses two rows into one factor level).
short_labels <- c(
  pop_ssp_u = "Population",
  gdppc_ssp_u = "GDP/capita",
  target_year_u = "Target year (intensity)",
  intensity_crops_global = "Crops",
  intensity_grazed_biomass_global = "Grazed biomass",
  intensity_wood_global = "Wood",
  intensity_coal_global = "Coal",
  intensity_gas_global = "Natural gas",
  intensity_oil_global = "Oil",
  intensity_buildings_metalOres_global = "Buildings metal",
  intensity_buildings_nonMetallic_global = "Buildings minerals",
  intensity_civil_metalOres_global = "Infrastructure metal",
  intensity_civil_nonMetallic_global = "Infrastructure minerals",
  intensity_machinery_metalOres_global = "Machinery metal",
  intensity_sl_products_metalOres_global = "Short-lived metal",
  gap_persistence_biomass = "Biomass",
  gap_persistence_fossilfuels = "Fossil fuels",
  gap_persistence_metal_construction = "Metal & Mineral",
  recycling_Fe_global = "Recycling Fe",
  recycling_NonFe_global = "Recycling NonFe",
  recyc_convergence_yr_global = "Target year (recycling)",
  downcycling_buildings_global = "Downcycling (Buildings)",
  downcycling_civil_infrastructure_global = "Downcycling (Infrastructure)",
  gap_persistence_rates = "Recycling %",
  sub_factor_recycling_same = "Substitution factor (recycling)",
  sub_factor_recycling_same_civil = "Substitution factor (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Substitution factor (roads)",
  grade_ore_fe_u = "Grade ore Fe",
  grade_ore_nonfe_u = "Grade ore NonFe",
  # (mean)/(shape) qualifiers kept as-is (not "Lifetime ..." like panel a) --
  # dropping them here would make the mean and shape rows of the same category
  # share one label and collapse into a single factor level (see note above).
  lifetime_mean_buildings = "Buildings (mean)",
  lifetime_mean_civil_infrastructure = "Infrastructure (mean)",
  lifetime_mean_machinery = "Machinery (mean)",
  lifetime_mean_short_lived = "Short-lived (mean)",
  lifetime_k_buildings = "Buildings (shape)",
  lifetime_k_civil_infrastructure = "Infrastructure (shape)",
  lifetime_k_machinery = "Machinery (shape)",
  lifetime_k_short_lived = "Short-lived (shape)"
)

family_lkp <- tibble(param = names(param_labels)) |>
  mutate(
    family = dplyr::case_when(
      param %in% c("pop_ssp_u", "gdppc_ssp_u") ~ "Driver SSP",
      stringr::str_detect(param, "gap_persistence") ~ "Regional divergence",
      stringr::str_detect(param, "grade_ore") ~ "Mining",
      stringr::str_detect(param, "intensity|target_year") ~ "Intensity",
      stringr::str_detect(param, "recyc|downcycl|sub_factor|max_secondary") ~ "Material recovery",
      stringr::str_detect(param, "lifetime") ~ "Lifetime",
      TRUE ~ "Other"
    ),
    label = short_labels[param]
  )

num_cols <- names(param_labels) # all are numeric LHS draws

cat("  Numeric parameters:", length(num_cols), "\n\n")


# STEP 8b: Real-value bounds & units per parameter (for bar-panel labels) --------
# Every sampled parameter is a linear rescale of its raw LHS draw,
# value = bound_min + u*(bound_max - bound_min) -- the same formula
# 02-RunSimulations.R uses everywhere to turn draws into physical values. This
# rebuilds just the bounds (not full trajectories) so the bar panel can show
# each parameter's selected-subset mean in its own units instead of a bare %.

cat("STEP 8b: Real-value bounds & units per parameter\n")

# Intensity bounds (kg/USD per material) -- same sheet/classification as
# 01-Sampling.R / 02-RunSimulations.R, reconstructed here for units only.
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

# SSP position (population / GDP-per-capita): the [0,1] draw locates a point
# between the two bracketing SSPs' world growth ratios (2024 -> FORECAST_END)
# -- same ranking as 02-RunSimulations.R STEP 3, rebuilt here just for its
# [min, max] range so the draw's real value reads as a growth multiple.
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

# Lifetime: each super_category's single draw is shared by 2 sub_uses with
# different Excel bounds (e.g. buildings: residential [60,100] vs
# non_residential [35,65]) -- the plain average of the two sub-uses' bounds is
# used here as the super-category's representative range (display only; the
# simulation itself still applies each sub-use's own bounds).
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
    dplyr::transmute(
      param = paste0("lifetime_mean_", super_category),
      bound_min = mean_life_min,
      bound_max = mean_life_max,
      unit = "yrs"
    ),
  lifetime_bounds |>
    dplyr::transmute(
      param = paste0("lifetime_k_", super_category),
      bound_min = k_min,
      bound_max = k_max,
      unit = "shape"
    )
)

# Scalar parameters bounded by the Excel "Parameters" sheet (already loaded as
# <NAME>_MIN/<NAME>_MAX globals by 00-Parameters.R) + gap-persistence params,
# which are used directly as the [0,1] convergence exponent with no separate
# Excel bound (02-RunSimulations.R's build_endpoints()) -- bound is [0,1].
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
  "pop_ssp_u"                               , min(ssp_ratio_pop$val)              , max(ssp_ratio_pop$val)              , "ratio"    ,
  "gdppc_ssp_u"                             , min(ssp_ratio_gdp$val)              , max(ssp_ratio_gdp$val)              , "ratio"
)

# Fixed decimal-place count per parameter for the bar-panel labels (both the
# bold value and the bracketed range use this) -- set per parameter rather
# than a generic significant-figures rule so e.g. coal intensity (small,
# 3 decimals) and building minerals intensity (large, 1 decimal) each get a
# sensible number of digits.
digits_lkp <- tibble::tribble(
  ~param, ~digits,
  "pop_ssp_u", 1L,
  "gdppc_ssp_u", 1L,
  "target_year_u", 0L,
  "intensity_crops_global", 2L,
  "intensity_grazed_biomass_global", 3L,
  "intensity_wood_global", 2L,
  "intensity_coal_global", 3L,
  "intensity_gas_global", 2L,
  "intensity_oil_global", 3L,
  "intensity_buildings_metalOres_global", 2L,
  "intensity_buildings_nonMetallic_global", 1L,
  "intensity_civil_metalOres_global", 2L,
  "intensity_civil_nonMetallic_global", 1L,
  "intensity_machinery_metalOres_global", 2L,
  "intensity_sl_products_metalOres_global", 3L,
  "gap_persistence_biomass", 0L,
  "gap_persistence_fossilfuels", 0L,
  "gap_persistence_metal_construction", 0L,
  "recycling_Fe_global", 0L,
  "recycling_NonFe_global", 0L,
  "recyc_convergence_yr_global", 0L,
  "downcycling_buildings_global", 0L,
  "downcycling_civil_infrastructure_global", 0L,
  "gap_persistence_rates", 0L,
  "sub_factor_recycling_same", 0L,
  "sub_factor_recycling_same_civil", 0L,
  "max_secondary_roads", 0L,
  "sub_factor_downcycling_roads", 0L,
  "grade_ore_fe_u", 0L,
  "grade_ore_nonfe_u", 1L,
  "lifetime_mean_buildings", 0L,
  "lifetime_mean_civil_infrastructure", 0L,
  "lifetime_mean_machinery", 0L,
  "lifetime_mean_short_lived", 1L,
  "lifetime_k_buildings", 1L,
  "lifetime_k_civil_infrastructure", 1L,
  "lifetime_k_machinery", 1L,
  "lifetime_k_short_lived", 1L
)

bound_lkp <- dplyr::bind_rows(bound_lkp_scalar, bound_lkp_intensity, bound_lkp_lifetime) |>
  left_join(digits_lkp, by = "param")

cat("  Parameters with real-value bounds:", nrow(bound_lkp), "of", length(num_cols), "\n\n")


# STEP 9: Mean draw value & significance per parameter (selected 10% subset) -----
# Replaces 14-LowHighConsumptionParameters.R's "share of draws above 0.5" +
# binomial test with the average draw value of the selected subset + a
# one-sample t-test of that average against 0.5 (the uniform-draw mean).

cat("STEP 9: Parameter mean draw value & significance (selected 10% subset)\n")

sel_long <- input_matrix |>
  filter(run_id %in% selected_ids) |>
  dplyr::select(run_id, all_of(num_cols)) |>
  pivot_longer(all_of(num_cols), names_to = "param", values_to = "u") |>
  left_join(family_lkp, by = "param")

sel_stats <- sel_long |>
  group_by(param, label, family) |>
  summarise(n = n(), mean_u = mean(u), p_value = t.test(u, mu = 0.5)$p.value, .groups = "drop") |>
  left_join(bound_lkp, by = "param") |>
  mutate(
    significant = p_value < ALPHA_SIG,
    # Mirrored bar geometry: bar length = majority_pct (the larger of
    # mean_u/(1-mean_u), *100), direction = whichever side is larger.
    majority_pct = pmax(mean_u, 1 - mean_u) * 100,
    signed_pct = ifelse(mean_u >= 0.5, majority_pct, -majority_pct),
    bar_right = mean_u * 100,
    bar_left = bar_right - 100,
    majority_center = signed_pct / 2,
    minority_pct = 100 - majority_pct,
    minority_center = ifelse(mean_u >= 0.5, bar_left / 2, bar_right / 2),
    param_x = ifelse(mean_u >= 0.5, bar_left - 2, bar_right + 2),
    param_hjust = ifelse(mean_u >= 0.5, 1, 0),
    # Real-world value of the selected subset's mean draw, in the parameter's
    # own unit -- same bound_min + u*(bound_max-bound_min) rescale the model
    # itself uses (see STEP 8b) -- and the parameter's full sampled range, so
    # the bar panel can show e.g. "64" anchored against "[50-90 yrs]" instead
    # of a bare, context-free percentage of the raw draw. Units live only in
    # the bracketed range (range_label); the bold in-bar number (value_label)
    # is unit-free. Decimal places come from digits_lkp (STEP 8b), fixed per
    # parameter -- sprintf("%.Nf", ...) is always fixed-notation (unlike
    # format(), whose scientific-vs-fixed choice depends on the whole column's
    # magnitude range, not just the one value being printed).
    real_value = bound_min + mean_u * (bound_max - bound_min),
    value_label = dplyr::case_when(
      unit == "pct" ~ sprintf(paste0("%.", digits, "f"), real_value * 100),
      TRUE ~ sprintf(paste0("%.", digits, "f"), real_value)
    ),
    range_label = dplyr::case_when(
      unit == "yrs" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min), "-",
        sprintf(paste0("%.", digits, "f"), bound_max), " yrs"
      ),
      unit == "year_abs" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min), "-",
        sprintf(paste0("%.", digits, "f"), bound_max)
      ),
      unit == "kg_usd" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min), "-",
        sprintf(paste0("%.", digits, "f"), bound_max), " kg/USD"
      ),
      unit == "pct" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min * 100), "-",
        sprintf(paste0("%.", digits, "f"), bound_max * 100), "%"
      ),
      unit == "ratio" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min), "-",
        sprintf(paste0("%.", digits, "f"), bound_max), "×"
      ),
      unit == "shape" ~ paste0(
        sprintf(paste0("%.", digits, "f"), bound_min), "-",
        sprintf(paste0("%.", digits, "f"), bound_max)
      ),
      TRUE ~ ""
    ),
    label_expr = ifelse(
      stringr::str_detect(label, "\\((mean|shape)\\)$"),
      paste0(
        "'",
        stringr::str_remove(label, "\\s*\\((mean|shape)\\)$"),
        " ('*italic('",
        stringr::str_extract(label, "mean|shape"),
        "')*') [",
        range_label,
        "]'"
      ),
      paste0("'", label, " [", range_label, "]'")
    )
  )

# Absolute-decoupling-only mean draw per parameter -- a different (rigorously
# CAGR-classified) subset than the percentile-rank `selected` top 10%, shown
# in the bar panel as a single reference point per parameter (no bar, no
# label). Unlike the bars, this is NOT independently mirrored to its own
# majority side: it's always placed on the main bar's majority side (sign of
# mean_u - 0.5) using the same bar_right/bar_left*100 metric, so e.g. a 70%
# bar (mean_u = 0.7) with mean_u_abs = 0.3 puts the point at x = 30, i.e. 3/7
# along the bar's length, instead of flipping to the opposite side.
abs_decoupling_ids <- run_data |> filter(decoupling_class == "Absolute decoupling") |> pull(run_id)

abs_stats <- input_matrix |>
  filter(run_id %in% abs_decoupling_ids) |>
  dplyr::select(run_id, all_of(num_cols)) |>
  pivot_longer(all_of(num_cols), names_to = "param", values_to = "u") |>
  group_by(param) |>
  summarise(mean_u_abs = mean(u), .groups = "drop")

cat("  Absolute-decoupling subset for panel c reference point:", length(abs_decoupling_ids), "runs\n\n")

sel_stats <- sel_stats |>
  left_join(abs_stats, by = "param") |>
  mutate(abs_point_x = ifelse(mean_u >= 0.5, mean_u_abs * 100, (mean_u_abs - 1) * 100))

# Rows grouped by family (colour blocks together), and within each family
# sorted by the same mean-draw criterion; both keys ascending top-to-bottom
# (arrange(desc()) here because the last row becomes the top of the plot for
# a discrete y-axis).
sel_stats <- sel_stats |>
  group_by(family) |>
  mutate(family_mean_u = mean(mean_u)) |>
  ungroup() |>
  arrange(desc(family_mean_u), desc(mean_u))

sel_factor_levels <- sel_stats$label
sel_stats <- sel_stats |> mutate(row_y = as.numeric(factor(label, levels = sel_factor_levels)))

cat("  Non-significant (faded):", sum(!sel_stats$significant), "of", nrow(sel_stats), "parameters\n\n")

write_csv(sel_stats, "Parameters/Intermediate/Figure3_ParamBars.csv")
cat("  Saved: Parameters/Intermediate/Figure3_ParamBars.csv\n\n")

cat("=== Figure 4 - Prepare Data done ===\n")

# EoF
