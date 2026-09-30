## =============================================================================
## 04-Decoupling.R
## Decoupling analysis: CAGR of primary material consumption per capita vs CAGR
## of GDP per capita, global level, per run x material group. Classified as
## absolute / peak / relative / no decoupling. Main window 2040-2060; robustness:
## two alternate windows (2030-2060, 2025-2060) and a fixed-2025-population-
## weights variant of the main window (isolates within-region trend from
## cross-region compositional shift).
##
## Classification (case_when, first match wins):
##   Absolute decoupling - mf/cap CAGR < 0 and GDP/cap CAGR > 0 (net decrease
##     window_start -> window_end).
##   Peak decoupling     - mf/cap CAGR >= 0 (net level still up over the full
##     window) but mf/cap peaks strictly before window_end and is below that
##     peak at window_end -- i.e. declining at the window's endpoint even
##     though the endpoint-to-endpoint change is still net positive.
##   Relative decoupling - mf/cap CAGR >= 0 and below GDP/cap CAGR.
##   No decoupling       - everything else (mf/cap CAGR >= GDP/cap CAGR).
## Peak detection needs the full annual trajectory (not just window
## endpoints), so YEARS_NEEDED now spans every year across all variant
## windows, not just their start/end years.
##
## FRAMING: results are enabling conditions under this model's own sampled
## assumptions, not probabilities of real-world decoupling.
##
## SSP note: population, GDP/capita and flow-intensity bounds share ONE
## continuous draw per run (ssp_u; see 02-RunSimulations.R) blending the two
## bracketing SSPs' full region x year trajectory by the same share for every
## region. There is no discrete SSP label -- ssp_u rides along as a plain
## continuous column throughout; "stratified by SSP" below means conditioning
## on this draw directly, never collapsing it into SSP1-5 buckets.
##
## Canonical producer of Results/MC/mc_decoupling.parquet, consumed by
## Figure 3 - Contours-GrowthRate.R, Figure 5 - Sensitivity - PrepareData.R,
## Supporting-Figures/S12 - TimeSeriesProjection.R.
## Scripts/05-Exploratory/19-Decoupling.R sources this script and adds
## exploratory diagnostic figures on top (decoupling_all etc. stay available
## in the calling environment after source()).
##
## Input:  Results/MC/mc_results.parquet
##         Parameters/Simulation/mc_input_matrix.csv
##         Parameters/Worldbank-GDP/gdp_region.csv, Parameters/UN-Population/population_region_historical.csv
##         Parameters/IIASA-Trajectories/ssp_drivers.csv
## Output: Results/MC/mc_decoupling.parquet
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)

cat("=== Decoupling Analysis: CAGR classification ===\n\n")

# Config ------------------------------------------------------------------------

WINDOWS <- list(c(2040L, 2060L), c(2030L, 2060L), c(2025L, 2060L), c(2025L, 2050L)) # (start, end); first = MAIN
FIXED_WEIGHT_YEAR <- 2025L
N_BINS_SSP_MARGINAL <- 10L
# Every year spanning the earliest window start to the latest window end --
# not just the endpoints -- so peak-year detection (STEP 5) has the full
# annual trajectory inside every window, not just its two boundary years.
YEARS_NEEDED <- seq(min(unlist(WINDOWS), FIXED_WEIGHT_YEAR), max(unlist(WINDOWS), FIXED_WEIGHT_YEAR))

variant_specs <- tibble::tribble(
  ~variant_id, ~variant_label, ~window_start, ~window_end, ~weight_variant,
  "main_actual", "2040-2060 (main)", 2040L, 2060L, "actual",
  "window_2030_2060", "2030-2060", 2030L, 2060L, "actual",
  "window_2025_2060", "2025-2060", 2025L, 2060L, "actual",
  "main_fixed2025w", "2040-2060, fixed 2025 pop weights", 2040L, 2060L, "fixed2025",
  # Ends at 2050 (earliest available year, 2025, to 2050) so the classification
  # window matches Figure 3's 2050 GDP/capita-vs-consumption snapshot exactly --
  # the other variants above compare CAGR through 2060, which can disagree with
  # a run's 2050 level (level vs. growth-rate are different comparisons).
  "main_2050", "2025-2050 (matches Fig. 4 snapshot)", 2025L, 2050L, "actual"
)

GROUP_LEVELS <- c("Total", "Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
# PALETTE_DECOUPLING is defined in Scripts/00-CommonParameters.R (sourced via 00-Libraries.R)


# STEP 1: Load data ---------------------------------------------------------------

cat("STEP 1: Load data\n")

# Push the year filter + column selection down to arrow before collecting --
# mc_results.parquet spans 36 years (2025-2060); YEARS_NEEDED now covers all
# of them (2025-2060) so peak-year detection has every year, not just the
# window boundaries.
results <- arrow::open_dataset("Results/MC/mc_results.parquet", format = "parquet") |>
  dplyr::filter(year %in% YEARS_NEEDED) |>
  dplyr::select(
    run_id, region, material_group, year, primary_consumption_Mt,
    ssp_lo, ssp_hi, ssp_share_lo
  ) |>
  dplyr::collect() |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))

input_matrix <- read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
pop_region_hist <- read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)

cat("  Runs:", n_distinct(results$run_id), "| Years loaded:", paste(YEARS_NEEDED, collapse = ", "), "\n\n")


# STEP 2: Region x run x year population & GDP from the continuous SSP blend ------
# Same reconstruction as Figure 2 - Assumptions.R SECTION C2, widened to the
# specific years this analysis needs instead of the full historical+2060 range.

cat("STEP 2: Reconstruct region x run x year population & GDP\n")

gdp_2024_region <- gdp_region_hist |>
  filter(year == 2024) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == 2024) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

run_ssp <- results |>
  distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year %in% YEARS_NEEDED) |>
  dplyr::select(scenario, region, year, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year %in% YEARS_NEEDED) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(pop_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(
    pop_idx_region |> rename(ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  mutate(pop_idx_blend = ssp_share_lo * pop_idx + (1 - ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(
    gdppc_idx_region |> rename(ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  mutate(gdppc_idx_blend = ssp_share_lo * gdppc_idx + (1 - ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

pop_region_run_year <- pop_idx_blend |>
  left_join(pop_2024_region, by = "region") |>
  mutate(pop = pop_2024 * pop_idx_blend) |>
  dplyr::select(run_id, region, year, pop)

# Total regional GDP(t) = GDP_2024 * pop_idx(t) * gdppc_idx(t) exactly, since
# GDP_total_index = Population_index * GDP_per_capita_index (ssp_drivers construction).
gdp_region_run_year <- pop_idx_blend |>
  left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(gdp_usd = gdp_2024 * pop_idx_blend * gdppc_idx_blend) |>
  dplyr::select(run_id, region, year, gdp_usd)

cat("  Done\n\n")


# STEP 3: Region x run x year primary consumption per material group (+Total) -----

cat("STEP 3: Primary consumption by region x material group (+Total)\n")

mf_region_year_group <- results |>
  group_by(run_id, region, year, material_group) |>
  summarise(mf_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(
    material_group = dplyr::recode(
      material_group,
      biomass = "Biomass",
      fossil_fuels = "Fossil fuels",
      metal_ores = "Metal ores",
      nonmetallic_minerals = "Non-metallic minerals"
    )
  )

mf_region_year_total <- mf_region_year_group |>
  group_by(run_id, region, year) |>
  summarise(mf_Mt = sum(mf_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(material_group = "Total")

mf_region_year <- bind_rows(mf_region_year_group, mf_region_year_total)

cat("  Done\n\n")


# STEP 4: Global per-capita series, actual weights & fixed-2025-weights -----------

cat("STEP 4: Global MF/cap & GDP/cap -- actual weights and fixed-2025-weights\n")

world_pop_year <- pop_region_run_year |> group_by(run_id, year) |> summarise(world_pop = sum(pop), .groups = "drop")
world_gdp_year <- gdp_region_run_year |> group_by(run_id, year) |> summarise(world_gdp = sum(gdp_usd), .groups = "drop")

gdp_percap_actual <- world_gdp_year |>
  left_join(world_pop_year, by = c("run_id", "year")) |>
  mutate(gdp_percap_usd = world_gdp / world_pop) |>
  dplyr::select(run_id, year, gdp_percap_usd)

mf_percap_actual <- mf_region_year |>
  group_by(run_id, year, material_group) |>
  summarise(world_mf_Mt = sum(mf_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_pop_year, by = c("run_id", "year")) |>
  mutate(mf_percap_t = world_mf_Mt * 1e6 / world_pop) |> # Mt -> t, / persons
  dplyr::select(run_id, year, material_group, world_mf_Mt, mf_percap_t)

# Fixed-2025-weights: freeze each run's own 2025 regional population SHARE;
# each region's own population & consumption still follow its actual simulated
# trajectory -- only the cross-region weighting is frozen.
pop_share_2025 <- pop_region_run_year |>
  filter(year == FIXED_WEIGHT_YEAR) |>
  group_by(run_id) |>
  mutate(pop_share_2025 = pop / sum(pop)) |>
  ungroup() |>
  dplyr::select(run_id, region, pop_share_2025)

mf_percap_fixedw <- mf_region_year |>
  left_join(pop_region_run_year, by = c("run_id", "region", "year")) |>
  mutate(mf_percap_region_t = mf_Mt * 1e6 / pop) |>
  left_join(pop_share_2025, by = c("run_id", "region")) |>
  group_by(run_id, year, material_group) |>
  summarise(mf_percap_t = sum(pop_share_2025 * mf_percap_region_t, na.rm = TRUE), .groups = "drop")

mf_percap_long <- bind_rows(
  mf_percap_actual |> mutate(weight_variant = "actual"),
  mf_percap_fixedw |> mutate(weight_variant = "fixed2025")
)

cat("  Done\n\n")


# STEP 5: CAGR + decoupling classification, per variant ---------------------------

cat("STEP 5: CAGR & classification per variant\n")

decoupling_list <- vector("list", nrow(variant_specs))

for (i in seq_len(nrow(variant_specs))) {
  vs <- variant_specs[i, ]
  n_years <- vs$window_end - vs$window_start

  mf_start <- mf_percap_long |>
    filter(year == vs$window_start, weight_variant == vs$weight_variant) |>
    dplyr::select(run_id, material_group, mf_start_t = mf_percap_t, mf_start_total_Mt = world_mf_Mt)
  mf_end <- mf_percap_long |>
    filter(year == vs$window_end, weight_variant == vs$weight_variant) |>
    dplyr::select(run_id, material_group, mf_end_t = mf_percap_t, mf_end_total_Mt = world_mf_Mt)

  # Peak year within the window (inclusive): needed for "Peak decoupling"
  # (peaks and is declining by window_end even though the endpoint-to-endpoint
  # CAGR is still >= 0). Ties broken toward the earliest year (arrange() before
  # slice_max), the conservative choice for "already past its peak".
  mf_peak <- mf_percap_long |>
    filter(year >= vs$window_start, year <= vs$window_end, weight_variant == vs$weight_variant) |>
    arrange(run_id, material_group, year) |>
    group_by(run_id, material_group) |>
    slice_max(mf_percap_t, n = 1, with_ties = FALSE) |>
    ungroup() |>
    dplyr::select(run_id, material_group, peak_year = year, peak_val = mf_percap_t)

  gdp_start <- gdp_percap_actual |> filter(year == vs$window_start) |> dplyr::select(run_id, gdp_start_usd = gdp_percap_usd)
  gdp_end <- gdp_percap_actual |> filter(year == vs$window_end) |> dplyr::select(run_id, gdp_end_usd = gdp_percap_usd)
  gdp_start_total <- world_gdp_year |>
    filter(year == vs$window_start) |>
    dplyr::select(run_id, gdp_start_total_usd = world_gdp)
  gdp_end_total <- world_gdp_year |>
    filter(year == vs$window_end) |>
    dplyr::select(run_id, gdp_end_total_usd = world_gdp)

  decoupling_list[[i]] <- mf_start |>
    inner_join(mf_end, by = c("run_id", "material_group")) |>
    left_join(mf_peak, by = c("run_id", "material_group")) |>
    left_join(gdp_start, by = "run_id") |>
    left_join(gdp_end, by = "run_id") |>
    left_join(gdp_start_total, by = "run_id") |>
    left_join(gdp_end_total, by = "run_id") |>
    mutate(
      variant_id = vs$variant_id,
      variant_label = vs$variant_label,
      window_start = vs$window_start,
      window_end = vs$window_end,
      weight_variant = vs$weight_variant,
      valid_cagr = mf_start_t > 0 & mf_end_t > 0 & gdp_start_usd > 0 & gdp_end_usd > 0,
      mf_percap_cagr = ifelse(valid_cagr, (mf_end_t / mf_start_t)^(1 / n_years) - 1, NA_real_),
      gdp_percap_cagr = ifelse(valid_cagr, (gdp_end_usd / gdp_start_usd)^(1 / n_years) - 1, NA_real_),
      # Total (population-inclusive) counterparts to the per-capita CAGRs above --
      # only meaningful for weight_variant == "actual" (mf_start/end_total_Mt is
      # NA for the fixed-2025-weights variant, which has no real world total).
      valid_total_cagr = valid_cagr & !is.na(mf_start_total_Mt) & !is.na(mf_end_total_Mt) &
        mf_start_total_Mt > 0 & mf_end_total_Mt > 0 & gdp_start_total_usd > 0 & gdp_end_total_usd > 0,
      mf_total_cagr = ifelse(valid_total_cagr, (mf_end_total_Mt / mf_start_total_Mt)^(1 / n_years) - 1, NA_real_),
      gdp_total_cagr = ifelse(valid_total_cagr, (gdp_end_total_usd / gdp_start_total_usd)^(1 / n_years) - 1, NA_real_),
      peaked_and_declining = valid_cagr & peak_year < window_end & mf_end_t < peak_val,
      decoupling_class = dplyr::case_when(
        !valid_cagr ~ NA_character_,
        mf_percap_cagr < 0 & gdp_percap_cagr > 0 ~ "Absolute decoupling",
        peaked_and_declining & gdp_percap_cagr > 0 ~ "Peak decoupling",
        mf_percap_cagr >= 0 & mf_percap_cagr < gdp_percap_cagr ~ "Relative decoupling",
        TRUE ~ "No decoupling"
      )
    )
}

decoupling_all <- bind_rows(decoupling_list) |>
  left_join(input_matrix |> dplyr::select(run_id, ssp_u), by = "run_id") |>
  mutate(
    material_group = factor(material_group, levels = GROUP_LEVELS),
    decoupling_class = factor(decoupling_class, levels = names(PALETTE_DECOUPLING))
  ) |>
  dplyr::select(
    run_id, material_group, variant_id, variant_label, window_start, window_end, weight_variant,
    mf_start_t, mf_end_t, mf_percap_cagr, gdp_start_usd, gdp_end_usd, gdp_percap_cagr,
    mf_start_total_Mt, mf_end_total_Mt, mf_total_cagr,
    gdp_start_total_usd, gdp_end_total_usd, gdp_total_cagr, valid_total_cagr,
    peak_year, peaked_and_declining, valid_cagr, decoupling_class, ssp_u
  )

n_invalid <- sum(!decoupling_all$valid_cagr)
cat(
  "  Invalid CAGR (non-positive start/end value):", n_invalid, "of", nrow(decoupling_all),
  "rows (", round(100 * n_invalid / nrow(decoupling_all), 2), "% ) -- flagged, not dropped\n\n"
)

write_parquet(decoupling_all, "Results/MC/mc_decoupling.parquet")
cat("  Saved: Results/MC/mc_decoupling.parquet\n\n")

# EoF
