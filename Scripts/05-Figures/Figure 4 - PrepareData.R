## =============================================================================
## Figure 4 - PrepareData.R
## Builds the Kaya cascade-waterfall data behind Figure 4: decomposes the
## 2024->2050 change in primary material consumption into Population, GDP/P,
## intensity (M/GDP or S/GDP), and -- for stocks -- Secondary material effects,
## for each of the 4 big material categories, at three scenario selections
## (median of all MC runs; the existing Figure 3 top-10% "decoupling" subset;
## and the Figure 3 CAGR-based "Absolute decoupling" class).
##
## mc_results.parquet has no year==2024 rows (DSM starts 2025) -- the 2024
## baseline is spliced in from historical series (materials_region_DMC.csv for
## flows, historical_secondary_flows.csv for stocks), same splice approach as
## Figure 2 - KayaProjection.R (SECTION F2).
##
## Output (Parameters/Intermediate/):
##   Figure4_Cascade.csv - one row per bar per panel (66 rows: material_group x
##                          selection x bar_order)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")
library(arrow)

cat("=== Figure 4 - Prepare Data ===\n\n")

# Constants ---------------------------------------------------------------

# Full raw-category buckets (the "nec./other" rows folded in) -- NOT the
# narrower list Figure 2 - KayaProjection.R SECTION F uses for its historical
# stack, which silently drops rows like "Wild catch and harvest" since no row
# in the raw CSV is literally named "Other biomass".
BIOMASS_CATS_NAMED <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS_NAMED <- c("Coal", "Natural Gas", "Petroleum")
BIOMASS_CATS_ALL <- c(
  BIOMASS_CATS_NAMED,
  "Wild catch and harvest",
  "Non-wild animal products",
  "Products mainly from biomass nec."
)
FOSSIL_CATS_ALL <- c(
  FOSSIL_CATS_NAMED,
  "Oil shale and tar sands",
  "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
  "Other products mainly from fossil fuels e.g. plastics"
)

MATGROUP_LABELS_ALL <- c(
  "biomass" = "Biomass",
  "fossil_fuels" = "Fossil fuels",
  "metal_ores" = "Metal ores",
  "nonmetallic_minerals" = "Non-metallic minerals"
)


# STEP 1: Load data ---------------------------------------------------------

cat("STEP 1: Load data\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))

pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
secondary_hist <- read_csv("Parameters/Intermediate/historical_secondary_flows.csv", show_col_types = FALSE)
run_scatter <- read_csv("Parameters/Intermediate/Figure3_RunScatter.csv", show_col_types = FALSE)

selected_ids <- run_scatter |> filter(selected) |> pull(run_id)
absdecoupling_ids <- run_scatter |> filter(decoupling_class == "Absolute decoupling") |> pull(run_id)

cat(
  "  MC runs:", n_distinct(results$run_id),
  "| Top 10% selected:", length(selected_ids),
  "| Absolute decoupling:", length(absdecoupling_ids), "\n\n"
)


# STEP 2: Per-run world population & GDP at BASE_YEAR and TARGET_YEAR -------
# Same SSP-blend reconstruction as Figure 2 - KayaProjection.R SECTION C2,
# restricted to just the two years the cascade needs. Population and GDP/cap
# are independent continuous blends between two bracketing SSPs per run (no
# discrete ssp_label column exists in mc_results). ssp_drivers' index is 1 for
# every scenario/region at the base year, so every run collapses to the same
# BASE_YEAR world total automatically -- no special-casing needed.

cat("STEP 2: Per-run world population & GDP (", BASE_YEAR, "&", TARGET_YEAR, ")\n")

gdp_2024_region <- gdp_region_hist |>
  filter(year == BASE_YEAR) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == BASE_YEAR) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

run_ssp <- results |>
  distinct(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo)

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year %in% c(BASE_YEAR, TARGET_YEAR)) |>
  dplyr::select(scenario, region, year, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year %in% c(BASE_YEAR, TARGET_YEAR)) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo) |>
  left_join(pop_idx_region |> rename(pop_ssp_lo = scenario), by = "pop_ssp_lo", relationship = "many-to-many") |>
  left_join(
    pop_idx_region |> rename(pop_ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("pop_ssp_hi", "region", "year")
  ) |>
  mutate(pop_idx_blend = pop_ssp_share_lo * pop_idx + (1 - pop_ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(gdppc_ssp_lo = scenario), by = "gdppc_ssp_lo", relationship = "many-to-many") |>
  left_join(
    gdppc_idx_region |> rename(gdppc_ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("gdppc_ssp_hi", "region", "year")
  ) |>
  mutate(gdppc_idx_blend = gdppc_ssp_share_lo * gdppc_idx + (1 - gdppc_ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

world_pop_by_run <- pop_idx_blend |>
  left_join(pop_2024_region, by = "region") |>
  mutate(pop_run = pop_2024 * pop_idx_blend) |>
  group_by(run_id, year) |>
  summarise(world_pop = sum(pop_run, na.rm = TRUE), .groups = "drop")

world_gdp_by_run <- pop_idx_blend |>
  left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend) |>
  group_by(run_id, year) |>
  summarise(world_gdp = sum(gdp_run, na.rm = TRUE), .groups = "drop")

cat("  Reconstructed world pop/GDP for", n_distinct(world_pop_by_run$run_id), "runs x 2 years\n\n")


# STEP 3: BASE_YEAR historical baseline, flows (Biomass, Fossil fuels) ------

cat("STEP 3: ", BASE_YEAR, "historical baseline (flows)\n")

biomass_2024 <- dmc_hist |>
  filter(year == BASE_YEAR, material_category %in% BIOMASS_CATS_ALL) |>
  summarise(M_2024 = sum(DMC_Mt, na.rm = TRUE)) |>
  mutate(material_group = "Biomass")

fossil_2024 <- dmc_hist |>
  filter(year == BASE_YEAR, material_category %in% FOSSIL_CATS_ALL) |>
  summarise(M_2024 = sum(DMC_Mt, na.rm = TRUE)) |>
  mutate(material_group = "Fossil fuels")

flow_baseline_2024 <- bind_rows(biomass_2024, fossil_2024) |> mutate(S_2024 = 0)

cat("  Biomass:", round(biomass_2024$M_2024), "Mt | Fossil fuels:", round(fossil_2024$M_2024), "Mt\n\n")


# STEP 4: BASE_YEAR historical baseline, stocks (Metal ores, Non-metallic minerals) --

cat("STEP 4: ", BASE_YEAR, "historical baseline (stocks)\n")

MATGROUP_LABELS_STOCK <- c("metal_ores" = "Metal ores", "nonmetallic_minerals" = "Non-metallic minerals")

stock_baseline_2024 <- secondary_hist |>
  filter(year == BASE_YEAR) |>
  group_by(material_group) |>
  summarise(M_2024 = sum(primary_Mt, na.rm = TRUE), S_2024 = sum(secondary_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(material_group = MATGROUP_LABELS_STOCK[material_group])

baseline_2024 <- bind_rows(flow_baseline_2024, stock_baseline_2024)

cat(
  "  Metal ores: primary",
  round(stock_baseline_2024$M_2024[stock_baseline_2024$material_group == "Metal ores"]),
  "Mt, secondary",
  round(stock_baseline_2024$S_2024[stock_baseline_2024$material_group == "Metal ores"]),
  "Mt\n"
)
cat(
  "  Non-metallic minerals: primary",
  round(stock_baseline_2024$M_2024[stock_baseline_2024$material_group == "Non-metallic minerals"]),
  "Mt, secondary",
  round(stock_baseline_2024$S_2024[stock_baseline_2024$material_group == "Non-metallic minerals"]),
  "Mt\n\n"
)

# Diagnostic only: the MC model's own year-2025 secondary supply for metal
# ores is known to sit well above this 2024 historical figure -- different
# reconstruction methodologies either side of the historical/MC splice (same
# kind of mismatch Figure 2 - KayaProjection.R SECTION F2 already documents),
# not a bug. Flagged here so it's visible rather than silently absorbed into
# the Secondary material effect bar.
metal_2025_check <- results |>
  filter(year == 2025, material_group == "metal_ores") |>
  group_by(run_id) |>
  summarise(S_2025 = sum(secondary_supply_Mt, na.rm = TRUE), .groups = "drop")
cat(
  "  [diagnostic] Metal ores secondary: 2024 historical =",
  round(stock_baseline_2024$S_2024[stock_baseline_2024$material_group == "Metal ores"]),
  "Mt vs. 2025 MC median =",
  round(median(metal_2025_check$S_2025)),
  "Mt\n\n"
)


# STEP 5: TARGET_YEAR per-run primary/secondary by material_group -----------

cat("STEP 5: ", TARGET_YEAR, "per-run consumption by material group\n")

results_2050 <- results |>
  filter(year == TARGET_YEAR, material_group %in% names(MATGROUP_LABELS_ALL)) |>
  group_by(run_id, material_group) |>
  summarise(
    M_run = sum(primary_consumption_Mt, na.rm = TRUE),
    S_run = sum(secondary_supply_Mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(material_group = MATGROUP_LABELS_ALL[material_group])

cat("  Rows:", nrow(results_2050), "(", n_distinct(results_2050$run_id), "runs x 4 material groups )\n\n")


# STEP 6: Marginal-median aggregation for the 3 selections --------------------
# "Median of all simulations", "Top 10% selected scenarios" and "Absolute
# decoupling" (the rigorous CAGR-based decoupling_class from Figure 3, not the
# percentile-rank `selected` flag) are each built from marginal medians: the
# median of world_pop, world_gdp, primary consumption and secondary supply,
# each computed independently -- the same convention Figure 2 -
# KayaProjection.R's P50 envelope lines use (per-variable quantiles across
# runs, not a single representative run). Every ratio used downstream is
# therefore a ratio of medians, not a median of ratios (e.g. GDPcap_2050 =
# median(world_gdp) / median(world_pop)).

cat("STEP 6: Marginal-median aggregation (median / top-10% / absolute-decoupling selections)\n")

pop_2050_by_run <- world_pop_by_run |> filter(year == TARGET_YEAR)
gdp_2050_by_run <- world_gdp_by_run |> filter(year == TARGET_YEAR)

agg_median <- results_2050 |>
  group_by(material_group) |>
  summarise(M_2050 = median(M_run), S_2050 = median(S_run), .groups = "drop") |>
  mutate(
    selection = "median",
    selection_label = "Median of all simulations",
    pop_2050 = median(pop_2050_by_run$world_pop),
    gdp_2050 = median(gdp_2050_by_run$world_gdp)
  )

agg_top10 <- results_2050 |>
  filter(run_id %in% selected_ids) |>
  group_by(material_group) |>
  summarise(M_2050 = median(M_run), S_2050 = median(S_run), .groups = "drop") |>
  mutate(
    selection = "top10",
    selection_label = "Top 10% selected scenarios",
    pop_2050 = median(pop_2050_by_run$world_pop[pop_2050_by_run$run_id %in% selected_ids]),
    gdp_2050 = median(gdp_2050_by_run$world_gdp[gdp_2050_by_run$run_id %in% selected_ids])
  )

agg_absdecoupling <- results_2050 |>
  filter(run_id %in% absdecoupling_ids) |>
  group_by(material_group) |>
  summarise(M_2050 = median(M_run), S_2050 = median(S_run), .groups = "drop") |>
  mutate(
    selection = "absdecoupling",
    selection_label = "Absolute decoupling",
    pop_2050 = median(pop_2050_by_run$world_pop[pop_2050_by_run$run_id %in% absdecoupling_ids]),
    gdp_2050 = median(gdp_2050_by_run$world_gdp[gdp_2050_by_run$run_id %in% absdecoupling_ids])
  )

agg_2050 <- bind_rows(agg_median, agg_top10, agg_absdecoupling)

cat("  Built", nrow(agg_2050), "material_group x selection rows\n\n")


# STEP 7: Assemble wide Kaya table -------------------------------------------

cat("STEP 7: Assemble wide Kaya table\n")

pop_2024_world <- sum(pop_2024_region$pop_2024)
gdp_2024_world <- sum(gdp_2024_region$gdp_2024)

kaya <- agg_2050 |>
  left_join(baseline_2024, by = "material_group") |>
  mutate(
    material_type = if_else(material_group %in% c("Biomass", "Fossil fuels"), "flow", "stock"),
    pop_2024 = pop_2024_world,
    gdpcap_2024 = gdp_2024_world / pop_2024_world,
    gdpcap_2050 = gdp_2050 / pop_2050,
    Q_2024 = M_2024 + S_2024,
    Q_2050 = M_2050 + S_2050,
    intensity_2024 = if_else(material_type == "flow", M_2024 / gdp_2024_world, Q_2024 / gdp_2024_world),
    intensity_2050 = if_else(material_type == "flow", M_2050 / gdp_2050, Q_2050 / gdp_2050),
    mq_2024 = if_else(material_type == "stock", M_2024 / Q_2024, NA_real_),
    mq_2050 = if_else(material_type == "stock", M_2050 / Q_2050, NA_real_)
  )

cat("  ", nrow(kaya), "material_group x selection rows\n\n")


# STEP 8: Sequential (chain-substitution) decomposition -------------------------
# Each step updates ONE driver to its TARGET_YEAR value, holding the rest at
# their current (already-updated / still-BASE_YEAR) values, in the exact bar
# order: Population -> GDP/P -> intensity (M/GDP or S/GDP) -> [Secondary
# material, stocks only]. Population and GDP/P are updated FIRST, before the
# material-specific intensity term enters, so M_after_pop/M_2024 = pop_2050/
# pop_2024 and M_after_gdpcap/M_after_pop = gdpcap_2050/gdpcap_2024 exactly --
# both are pure global-driver ratios, identical across every material_group,
# by construction. This is the key difference from LMDI (whose log-mean weight
# let each material's own total growth rate leak into the population/GDP-per-
# capita effect sizes even though those two drivers are shared): here all
# material-specific variation is confined to the intensity/secondary steps,
# where it belongs. Exact by telescoping sum -- no residual, no edge case.

cat("STEP 8: Sequential decomposition\n")

kaya <- kaya |>
  mutate(
    mq_2024_eff = if_else(material_type == "stock", mq_2024, 1),
    M_after_pop = pop_2050 * gdpcap_2024 * intensity_2024 * mq_2024_eff,
    M_after_gdpcap = pop_2050 * gdpcap_2050 * intensity_2024 * mq_2024_eff,
    M_after_intensity = pop_2050 * gdpcap_2050 * intensity_2050 * mq_2024_eff,
    effect_pop = M_after_pop - M_2024,
    effect_gdpcap = M_after_gdpcap - M_after_pop,
    effect_intensity = M_after_intensity - M_after_gdpcap,
    effect_secondary = if_else(material_type == "stock", M_2050 - M_after_intensity, NA_real_)
  ) |>
  dplyr::select(-mq_2024_eff)

residual <- with(
  kaya,
  (M_2050 - M_2024) - (effect_pop + effect_gdpcap + effect_intensity + coalesce(effect_secondary, 0))
)
stopifnot(all(abs(residual) < 1e-6 * pmax(abs(kaya$M_2050 - kaya$M_2024), 1)))

pop_pct_by_selection <- kaya |> group_by(selection) |> summarise(n_distinct_pop_pct = n_distinct(round(effect_pop / M_2024, 6)), .groups = "drop")
stopifnot(all(pop_pct_by_selection$n_distinct_pop_pct == 1))

cat("  Residual check passed (max |residual| =", format(max(abs(residual)), scientific = TRUE), "Mt )\n")
cat("  Population effect % confirmed identical across all 4 materials, within each selection\n\n")


# STEP 9: Build cascade bar table ---------------------------------------------
# Bottom-of-panel bar name, fill_key (colour lookup) and each bar's signed
# delta, in the fixed order requested: flows = 5 bars, stocks = 6 bars. Only
# the first (BASE_YEAR) and last (TARGET_YEAR) bars are anchored at y=0; the
# effect bars float, each starting where the previous bar's cumulative level
# ended (cumsum/lag idiom, same as Figure 1 - TimeSeries.R:58 / Figure 3 -
# PrepareData.R:388-390).

cat("STEP 9: Build cascade bar table\n")

flow_bars <- tribble(
  ~bar_order , ~bar_key     , ~bar_label   , ~fill_key     , ~bar_type ,
  1L         , "total_2024" , "2024"       , NA_character_ , "total"   ,
  2L         , "population" , "Population" , "Population"  , "effect"  ,
  3L         , "gdp_cap"    , "GDP/P"      , "GDP/P"       , "effect"  ,
  4L         , "intensity"  , "M/GDP"      , "Intensity"   , "effect"  ,
  5L         , "total_2050" , "2050"       , NA_character_ , "total"
) |>
  mutate(material_type = "flow")

stock_bars <- tribble(
  ~bar_order , ~bar_key     , ~bar_label            , ~fill_key            , ~bar_type ,
  1L         , "total_2024" , "2024"                , NA_character_        , "total"   ,
  2L         , "population" , "Population"          , "Population"         , "effect"  ,
  3L         , "gdp_cap"    , "GDP/P"               , "GDP/P"              , "effect"  ,
  4L         , "intensity"  , "S/GDP"               , "Intensity"          , "effect"  ,
  5L         , "secondary"  , "Secondary\nMaterial" , "Secondary material" , "effect"  ,
  6L         , "total_2050" , "2050"                , NA_character_        , "total"
) |>
  mutate(material_type = "stock")

bar_spec <- bind_rows(flow_bars, stock_bars)

bars <- kaya |>
  left_join(bar_spec, by = "material_type", relationship = "many-to-many") |>
  mutate(
    # 2024 total bar keeps the material's own colour; 2050 uses a lighter tint
    # of that SAME material's colour (resolved in Figure 4 - Cascade.R) so the
    # end point still reads as belonging to its column, just visually distinct
    # from the 2024 start point.
    fill_key = case_when(
      bar_key == "total_2024" ~ material_group,
      bar_key == "total_2050" ~ paste(material_group, "2050"),
      TRUE ~ fill_key
    ),
    delta = case_when(
      bar_key == "total_2024" ~ M_2024,
      bar_key == "population" ~ effect_pop,
      bar_key == "gdp_cap" ~ effect_gdpcap,
      bar_key == "intensity" ~ effect_intensity,
      bar_key == "secondary" ~ effect_secondary,
      TRUE ~ NA_real_
    )
  )

bars <- bars |>
  arrange(material_group, selection, bar_order) |>
  group_by(material_group, selection) |>
  mutate(
    cum_top = cumsum(coalesce(delta, 0)),
    cum_bot = lag(cum_top, default = 0),
    is_last = bar_order == max(bar_order),
    top = if_else(is_last, M_2050, cum_top),
    bot = if_else(bar_order == 1 | is_last, 0, cum_bot),
    ymin = pmin(bot, top),
    ymax = pmax(bot, top)
  ) |>
  ungroup() |>
  dplyr::select(-cum_top, -cum_bot, -is_last)

cat("  Built", nrow(bars), "bars across", n_distinct(paste(bars$material_group, bars$selection)), "panels\n\n")


# STEP 10: Percent labels ------------------------------------------------------
# Every bar's own contribution as % of the BASE_YEAR baseline. Effect bars'
# percentages sum exactly to the final (TARGET_YEAR) bar's total % change,
# since they all share the same M_2024 denominator within a panel. The
# TARGET_YEAR total bar is special-cased: its `bot` is the y=0 anchor (not the
# previous cumulative level), so its own % must be computed from `top` against
# M_2024 directly rather than from (top-bot), which would otherwise read
# M_2050/M_2024*100 instead of the correct (M_2050-M_2024)/M_2024*100.

cat("STEP 10: Percent labels\n")

bars <- bars |>
  mutate(
    pct_value = case_when(
      bar_order == 1 ~ 0,
      bar_type == "total" ~ (top - M_2024) / M_2024 * 100,
      TRUE ~ (top - bot) / M_2024 * 100
    ),
    pct_label = if_else(bar_order == 1, "", sprintf("%+.0f%%", pct_value))
  )

pct_check <- bars |>
  filter(bar_order != 1) |>
  group_by(material_group, selection) |>
  summarise(
    sum_effect_pct = sum(pct_value[bar_type == "effect"]),
    total_pct = pct_value[bar_type == "total"][1],
    .groups = "drop"
  ) |>
  mutate(diff = abs(sum_effect_pct - total_pct))
stopifnot(all(pct_check$diff < 1e-6))

cat(
  "  Effect-bar percentages sum exactly to total % change (max diff =",
  format(max(pct_check$diff), scientific = TRUE),
  ")\n\n"
)


# STEP 11: Column-level shared y-axis range -------------------------------------
# Shared range within each material_group (across its 3 selection panels) --
# materials are the figure's COLUMNS, so this keeps the median/top-10%/
# absolute-decoupling panels of the same material directly comparable; ranges
# stay independent across the 4 material columns since magnitudes differ
# hugely (e.g. Non-metallic minerals vs. Metal ores). No padding below
# col_min: the panel's bottom edge sits flush with the bars so the "2024"/
# "2050" x-axis text has no dead space between it and the lowest bar.

cat("STEP 11: Column-level y-axis ranges\n")

bars <- bars |>
  group_by(material_group) |>
  mutate(
    col_max = max(ymax),
    col_min = min(0, min(ymin)),
    col_span = col_max - col_min,
    col_y_min = col_min,
    col_y_max = col_max + 0.14 * col_span,
    label_y_top = ymax + 0.03 * col_span
  ) |>
  ungroup() |>
  dplyr::select(-col_max, -col_min, -col_span)


# STEP 12: Save -----------------------------------------------------------------

cat("STEP 12: Save\n")

write_csv(bars, "Parameters/Intermediate/Figure4_Cascade.csv")
cat("  Saved: Parameters/Intermediate/Figure4_Cascade.csv\n\n")

cat("=== Figure 4 - Prepare Data done ===\n")

# EoF
