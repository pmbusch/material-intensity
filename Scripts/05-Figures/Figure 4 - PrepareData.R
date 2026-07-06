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
##   Figure4_RunScatter.csv      - per-run DMC/GDP-per-capita + decoupling flag
##   Figure4_ParamImportance.csv - SHAP stacked-area data (flipped panel)
##   Figure4_ParamLabels.csv     - stacked-area in-panel label positions
##   Figure4_ParamBars.csv       - decoupling-subset parameter bar stats
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

cat("  GDP/capita 2050 range: [", round(min(run_data$GDPcap_2050)), ",", round(max(run_data$GDPcap_2050)), "] k USD\n\n")


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
# main variant: window 2040-2060, actual population weights) from
# 19-Decoupling.R, for Figure 4's scatter colour -- additive column, independent
# of the percentile-rank `selected` flag above.
decoupling_total <- arrow::read_parquet("Results/MC/mc_decoupling.parquet") |>
  filter(variant_id == "main_actual", material_group == "Total") |>
  dplyr::select(run_id, decoupling_class)

run_data <- run_data |> left_join(decoupling_total, by = "run_id")
cat("  Decoupling class matched:", sum(!is.na(run_data$decoupling_class)), "of", nrow(run_data), "runs\n\n")

write_csv(run_data, "Parameters/Intermediate/Figure4_RunScatter.csv")
cat("  Saved: Parameters/Intermediate/Figure4_RunScatter.csv\n\n")


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

label_map <- c(
  pop_ssp_u = "SSP position: Population",
  gdppc_ssp_u = "SSP position: GDP/capita",
  target_year_u = "Intensity conv. year",
  intensity_crops_global = "Intensity: Crops",
  intensity_grazed_biomass_global = "Intensity: Grazed biomass",
  intensity_wood_global = "Intensity: Wood",
  intensity_coal_global = "Intensity: Coal",
  intensity_gas_global = "Intensity: Natural gas",
  intensity_oil_global = "Intensity: Oil",
  intensity_buildings_metalOres_global = "Intensity: Bldg metal ores",
  intensity_buildings_nonMetallic_global = "Intensity: Bldg minerals",
  intensity_civil_metalOres_global = "Intensity: Infra metal ores",
  intensity_civil_nonMetallic_global = "Intensity: Infra minerals",
  intensity_machinery_metalOres_global = "Intensity: Machinery metal ores",
  intensity_sl_products_metalOres_global = "Intensity: Short-lived metal ores",
  gap_persistence_biomass = "Gap persistence: Biomass",
  gap_persistence_fossilfuels = "Gap persistence: Fossil fuels",
  gap_persistence_metal_construction = "Gap persistence: Metal/mineral",
  recycling_Fe_global = "Recycling rate – Fe",
  recycling_NonFe_global = "Recycling rate – NonFe",
  grade_ore_fe_u = "Ore grade – Fe",
  grade_ore_nonfe_u = "Ore grade – NonFe",
  recyc_convergence_yr_global = "Recycling conv. year",
  downcycling_buildings_global = "Downcycling: Buildings",
  downcycling_civil_infrastructure_global = "Downcycling: Infrastructure",
  gap_persistence_rates = "Gap persistence: Recycling rates",
  sub_factor_recycling_same = "Recycling substitution factor",
  sub_factor_recycling_same_civil = "Recycling substitution (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Downcycling substitution factor",
  lifetime_mean_buildings = "Lifetime mean: Buildings",
  lifetime_mean_civil_infrastructure = "Lifetime mean: Infrastructure",
  lifetime_mean_machinery = "Lifetime mean: Machinery",
  lifetime_mean_short_lived = "Lifetime mean: Short-lived",
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

SHAP_PARAM_COLORS <- c(
  "Other" = "#C0C0C0",
  "SSP position: Population" = "#6a3d9a",
  "SSP position: GDP/capita" = "#9673B9",
  "Intensity conv. year" = "#A0785A",
  "Intensity: Grazed biomass" = "#558B2F",
  "Intensity: Wood" = "#4E342E",
  "Intensity: Coal" = "#1C1C1C",
  "Intensity: Natural gas" = "#90A4AE",
  "Intensity: Oil" = "#5D4037",
  "Intensity: Crops" = "#F9A825",
  "Intensity: Bldg metal ores" = "#C77B00",
  "Intensity: Bldg minerals" = "#607D8B",
  "Intensity: Infra metal ores" = "#8B0000",
  "Intensity: Infra minerals" = "#78909C",
  "Intensity: Machinery metal ores" = "#D84315",
  "Intensity: Short-lived metal ores" = "#FF7043",
  "Gap persistence: Biomass" = "#8DC54B",
  "Gap persistence: Fossil fuels" = "#6D4C28",
  "Gap persistence: Metal/mineral" = "#B71C1C",
  "Gap persistence: Recycling rates" = "#9575CD",
  "Recycling rate – Fe" = "#AB47BC",
  "Recycling rate – NonFe" = "#E1BEE7",
  "Ore grade – Fe" = "#8B4513",
  "Ore grade – NonFe" = "#B8860B",
  "Recycling conv. year" = "#CE93D8",
  "Downcycling: Buildings" = "#4A7FC1",
  "Downcycling: Infrastructure" = "#3D7A8A",
  "Bldg waste to roads share" = "#90CAF9",
  "Recycling substitution factor" = "#7986CB",
  "Recycling substitution (civil)" = "#5C6BC0",
  "Max secondary roads" = "#80CBC4",
  "Downcycling substitution factor" = "#4DB6AC",
  "Lifetime mean: Buildings" = "#1B4F8A",
  "Lifetime mean: Infrastructure" = "#7A5230",
  "Lifetime mean: Machinery" = "#2D6A4F",
  "Lifetime mean: Short-lived" = "#7B2D8B",
  "Lifetime shape: Buildings" = "#5285C0",
  "Lifetime shape: Infrastructure" = "#B8896A",
  "Lifetime shape: Machinery" = "#5DAA80",
  "Lifetime shape: Short-lived" = "#B068C0"
)

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
  "Parameters/Intermediate/Figure4_ParamImportance.csv"
)
write_csv(
  label_df |> dplyr::select(display_label, stack_order, mid_x_gt, mid_y, pct, fill_hex, text_col, show_label),
  "Parameters/Intermediate/Figure4_ParamLabels.csv"
)
cat("  Saved: Parameters/Intermediate/Figure4_ParamImportance.csv, Figure4_ParamLabels.csv\n\n")


# STEP 8: Parameter setup for the decoupling-subset bar panel --------------------

cat("STEP 8: Parameter setup for decoupling-subset bar panel\n")

param_labels <- c(
  pop_ssp_u = "SSP position: Population",
  gdppc_ssp_u = "SSP position: GDP/capita",
  target_year_u = "Intensity conv. year",
  intensity_crops_global = "Intensity: Crops",
  intensity_grazed_biomass_global = "Intensity: Grazed biomass",
  intensity_wood_global = "Intensity: Wood",
  intensity_coal_global = "Intensity: Coal",
  intensity_gas_global = "Intensity: Natural gas",
  intensity_oil_global = "Intensity: Oil",
  intensity_buildings_metalOres_global = "Intensity: Bldg metal ores",
  intensity_buildings_nonMetallic_global = "Intensity: Bldg minerals",
  intensity_civil_metalOres_global = "Intensity: Infra metal ores",
  intensity_civil_nonMetallic_global = "Intensity: Infra minerals",
  intensity_machinery_metalOres_global = "Intensity: Machinery metal ores",
  intensity_sl_products_metalOres_global = "Intensity: Short-lived metal ores",
  gap_persistence_biomass = "Gap persistence: Biomass",
  gap_persistence_fossilfuels = "Gap persistence: Fossil fuels",
  gap_persistence_metal_construction = "Gap persistence: Metal/mineral",
  recycling_Fe_global = "Recycling rate: Fe",
  recycling_NonFe_global = "Recycling rate: NonFe",
  recyc_convergence_yr_global = "Recycling conv. year",
  downcycling_buildings_global = "Downcycling: Buildings",
  downcycling_civil_infrastructure_global = "Downcycling: Infrastructure",
  gap_persistence_rates = "Gap persistence: Recycling rates",
  sub_factor_recycling_same = "Recycling substitution factor",
  sub_factor_recycling_same_civil = "Recycling substitution (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Downcycling substitution factor",
  grade_ore_fe_u = "Ore grade: Fe",
  grade_ore_nonfe_u = "Ore grade: NonFe",
  lifetime_mean_buildings = "Lifetime mean: Buildings",
  lifetime_mean_civil_infrastructure = "Lifetime mean: Infrastructure",
  lifetime_mean_machinery = "Lifetime mean: Machinery",
  lifetime_mean_short_lived = "Lifetime mean: Short-lived",
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
  target_year_u = "Conv. year (intensity)",
  intensity_crops_global = "Crops",
  intensity_grazed_biomass_global = "Grazed biomass",
  intensity_wood_global = "Wood",
  intensity_coal_global = "Coal",
  intensity_gas_global = "Natural gas",
  intensity_oil_global = "Oil",
  intensity_buildings_metalOres_global = "Bldg metal ores",
  intensity_buildings_nonMetallic_global = "Bldg minerals",
  intensity_civil_metalOres_global = "Infra metal ores",
  intensity_civil_nonMetallic_global = "Infra minerals",
  intensity_machinery_metalOres_global = "Machinery metal ores",
  intensity_sl_products_metalOres_global = "Short-lived metal ores",
  gap_persistence_biomass = "Biomass",
  gap_persistence_fossilfuels = "Fossil fuels",
  gap_persistence_metal_construction = "Metal/mineral",
  recycling_Fe_global = "Recycling Fe",
  recycling_NonFe_global = "Recycling NonFe",
  recyc_convergence_yr_global = "Conv. year (recycling)",
  downcycling_buildings_global = "Downcycling (Bldg)",
  downcycling_civil_infrastructure_global = "Downcycling (Infra)",
  gap_persistence_rates = "Recycling rates",
  sub_factor_recycling_same = "Substitution factor (recycling)",
  sub_factor_recycling_same_civil = "Substitution factor (civil)",
  max_secondary_roads = "Max secondary roads",
  sub_factor_downcycling_roads = "Substitution factor (roads)",
  grade_ore_fe_u = "Grade ore Fe",
  grade_ore_nonfe_u = "Grade ore NonFe",
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
  mutate(
    significant = p_value < ALPHA_SIG,
    # Same mirrored-bar geometry as before, just re-keyed on mean_u (in [0, 1],
    # centred at 0.5 under Uniform(0,1)) instead of the share of draws > 0.5.
    majority_pct = pmax(mean_u, 1 - mean_u) * 100,
    signed_pct = ifelse(mean_u >= 0.5, majority_pct, -majority_pct),
    bar_right = mean_u * 100,
    bar_left = bar_right - 100,
    majority_center = signed_pct / 2,
    minority_pct = 100 - majority_pct,
    minority_center = ifelse(mean_u >= 0.5, bar_left / 2, bar_right / 2),
    param_x = ifelse(mean_u >= 0.5, bar_left - 2, bar_right + 2),
    param_hjust = ifelse(mean_u >= 0.5, 1, 0),
    label_expr = ifelse(
      stringr::str_detect(label, "\\((mean|shape)\\)$"),
      paste0(
        "'",
        stringr::str_remove(label, "\\s*\\((mean|shape)\\)$"),
        " ('*italic('",
        stringr::str_extract(label, "mean|shape"),
        "')*')'"
      ),
      paste0("'", label, "'")
    )
  )

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

write_csv(sel_stats, "Parameters/Intermediate/Figure4_ParamBars.csv")
cat("  Saved: Parameters/Intermediate/Figure4_ParamBars.csv\n\n")

cat("=== Figure 4 - Prepare Data done ===\n")

# EoF
