## =============================================================================
## 01_load_and_check.R
## Loads the IIASA ScenarioMIP CMIP7 IAMC-format extract (R10 regions + World),
## parses scenario names into policy family / SSP / marker flag, checks for
## marker-vs-non-marker duplication, reports coverage, quantifies what
## "Other (R10)" contains, and checks whether GDP/Population are exogenous
## (SSP-only) or policy-responsive (vary by forcing family too).
##
## Inputs:
##   Inputs/IIASA_SSP/2026-MIP-CMIP7/region_iamc_data-74f8a0cb-....csv  (R10)
##   Inputs/IIASA_SSP/2026-MIP-CMIP7/iamc_data-fbea2f95-....csv        (World)
##   Inputs/IIASA_SSP/2026-MIP-CMIP7/food_iamc_data-bcbf31b9-....csv     (R10 + World: food intake/waste, roundwood)
##   Inputs/IIASA_SSP/2026-MIP-CMIP7/energy_iamc_data-6b18b585-....csv   (R10 + World: primary energy by source)
##   Inputs/IIASA_SSP/2026-MIP-CMIP7/capacity_iamc_data-04a640c5-....csv (R10 + World: electricity capacity)
##   The energy extract repeats 6 Primary Energy variables already in the
##   R10/World files (identical values) -- exact duplicate rows are dropped.
##
## Outputs (Parameters/SSP_ScenarioMIP/):
##   ssp_long_clean.csv        — tidy long panel, "Other (R10)" excluded, 2020-2060
##   coverage_model_scenario_variable_region.csv
##   other_r10_share.csv
##   gdp_pop_consistency.csv
##   assumptions_log.txt
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Base year: both 2020 and 2025 are fully populated in the source data; 2020
# chosen as the anchor (confirmed with user). FORECAST_END (2060) is the
# project-wide horizon already set in 00-CommonParameters.R -- reused as-is.
BASE_YEAR <- 2020L
YEARS_KEEP <- seq(BASE_YEAR, FORECAST_END, by = 5L)

REGION_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/region_iamc_data-74f8a0cb-0306-47d2-a5fc-7367e4ca407f.csv"
WORLD_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/iamc_data-fbea2f95-b9d4-430e-a6ab-594c4b288903.csv"
FOOD_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/food_iamc_data-bcbf31b9-c943-4cdd-b9f0-f5513b9c4b6b.csv"
ENERGY_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/energy_iamc_data-6b18b585-989f-46ab-ae08-25a0e51a3139.csv"
CAPACITY_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/capacity_iamc_data-04a640c5-84c4-46e1-bf89-eb7a27ec1f39.csv"

OUT_DIR <- "Parameters/SSP_ScenarioMIP"

log_lines <- c() # collect assumption / drop notes as we go, dumped to assumptions_log.txt at the end


# Step 1: Load R10 + World + food/energy/capacity IAMC files and stack them ───

cat("STEP 1: Load R10 + World + food/energy/capacity IAMC files\n")

region_raw <- read_csv(REGION_FILE, col_types = cols(.default = "c"))
world_raw <- read_csv(WORLD_FILE, col_types = cols(.default = "c"))
food_raw <- read_csv(FOOD_FILE, col_types = cols(.default = "c"))
energy_raw <- read_csv(ENERGY_FILE, col_types = cols(.default = "c"))
capacity_raw <- read_csv(CAPACITY_FILE, col_types = cols(.default = "c"))

cat("  Region file:", nrow(region_raw), "rows |  World file:", nrow(world_raw), "rows\n")
cat("  Food file:", nrow(food_raw), "rows |  Energy file:", nrow(energy_raw), "rows |  Capacity file:", nrow(capacity_raw), "rows\n")

yr_cols <- names(region_raw)[grepl("^[0-9]{4}$", names(region_raw))]
stopifnot(identical(yr_cols, names(world_raw)[grepl("^[0-9]{4}$", names(world_raw))]))
stopifnot(identical(yr_cols, names(food_raw)[grepl("^[0-9]{4}$", names(food_raw))]))
stopifnot(identical(yr_cols, names(energy_raw)[grepl("^[0-9]{4}$", names(energy_raw))]))
stopifnot(identical(yr_cols, names(capacity_raw)[grepl("^[0-9]{4}$", names(capacity_raw))]))

raw_wide <- bind_rows(region_raw, world_raw, food_raw, energy_raw, capacity_raw)
n_stacked <- nrow(raw_wide)

# The energy extract repeats Primary Energy|{Biomass,Coal,Fossil,Gas,Oil,Other}
# already present in the R10/World files with identical values -> drop exact duplicate rows
raw_wide <- raw_wide %>%
  distinct() %>%
  mutate(across(all_of(yr_cols), as.numeric), version = as.integer(version))

cat("  Stacked:", n_stacked, "rows | after dropping exact duplicate rows:", nrow(raw_wide), "\n")
cat("  Regions:", paste(sort(unique(raw_wide$region)), collapse = ", "), "\n")
log_lines <- c(log_lines, paste0(
  "Dropped ", n_stacked - nrow(raw_wide), " exact duplicate rows after stacking (energy extract repeats ",
  "Primary Energy variables already in the R10/World files, with identical values)."
))


# Step 2: Pivot to long, restrict to the analysis time window ─────────────────

cat("\nSTEP 2: Pivot to long, restrict to", BASE_YEAR, "-", FORECAST_END, "(5-yr grid)\n")

raw_long <- raw_wide %>%
  pivot_longer(cols = all_of(yr_cols), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(value))

n_before <- nrow(raw_long)
raw_long <- raw_long %>% filter(year %in% YEARS_KEEP)
n_after <- nrow(raw_long)

cat("  Rows before year filter:", n_before, "| after (5-yr grid, ", BASE_YEAR, "-", FORECAST_END, "):", n_after, "\n")
log_lines <- c(
  log_lines,
  paste0(
    "Dropped ", n_before - n_after, " rows outside the ", BASE_YEAR, "-", FORECAST_END,
    " 5-yr grid (includes the near-empty 2021 column, which is a data artifact, not a real annual series)."
  )
)


# Step 3: Parse scenario strings into family / SSP / marker flag ──────────────

cat("\nSTEP 3: Parse scenario names (family, SSP, marker)\n")

raw_long <- raw_long %>%
  mutate(
    marker = str_detect(scenario, fixed("(Marker)")),
    scenario_clean = str_squish(str_remove(scenario, fixed("(Marker)"))),
    ssp = str_extract(scenario_clean, "SSP[1-5]"),
    family = str_squish(str_remove(scenario_clean, "-\\s*SSP[1-5]$"))
  )

n_unparsed <- sum(is.na(raw_long$ssp) | raw_long$family == "")
cat("  Distinct scenario strings:", n_distinct(raw_long$scenario), "\n")
cat("  Families detected:", paste(sort(unique(raw_long$family)), collapse = ", "), "\n")
cat("  Rows with unparsed family/SSP:", n_unparsed, "\n")
if (n_unparsed > 0) {
  cat("[NOTE] Unparsed scenario strings:\n")
  print(raw_long %>% filter(is.na(ssp) | family == "") %>% distinct(scenario))
}


# Step 4: Check marker-vs-non-marker duplication within a model ───────────────

cat("\nSTEP 4: Check for marker/non-marker duplication within the same model\n")

dup_check <- raw_long %>%
  group_by(model, family, ssp, region, variable, year) %>%
  summarise(n = n(), n_marker = sum(marker), .groups = "drop") %>%
  filter(n > 1)

if (nrow(dup_check) == 0) {
  cat("  No duplication found: for every (model, family, SSP), the marker flag is a fixed property of\n")
  cat("  that scenario string, not a duplicate run -- each model contributes at most one run per\n")
  cat("  family x SSP x region x variable x year cell. No deduplication needed.\n")
  log_lines <- c(log_lines, "Checked for (Marker) vs non-marker duplication within the same model: none found.")
} else {
  cat("[NOTE]", nrow(dup_check), "duplicated cells found -- keeping the marker run where available\n")
  dup_keys <- dup_check %>% dplyr::select(model, family, ssp, region, variable, year)
  raw_long <- raw_long %>%
    anti_join(dup_keys, by = c("model", "family", "ssp", "region", "variable", "year")) %>%
    bind_rows(
      raw_long %>%
        inner_join(dup_keys, by = c("model", "family", "ssp", "region", "variable", "year")) %>%
        group_by(model, family, ssp, region, variable, year) %>%
        slice_max(marker, n = 1, with_ties = FALSE) %>%
        ungroup()
    )
  log_lines <- c(log_lines, paste0(nrow(dup_check), " duplicated (model, family, SSP, region, variable, year) cells found; marker run kept."))
}


# Step 5: Coverage report — model x scenario x variable x region ──────────────

cat("\nSTEP 5: Coverage report\n")

coverage <- raw_long %>%
  distinct(model, family, ssp, marker, region, variable) %>%
  count(family, ssp, variable, region, name = "n_models")

coverage_model_summary <- raw_long %>%
  distinct(model, family, ssp) %>%
  count(family, ssp, name = "n_models") %>%
  arrange(family, ssp)

cat("  Model count per family x SSP (min/median/max):",
  min(coverage_model_summary$n_models), "/",
  median(coverage_model_summary$n_models), "/",
  max(coverage_model_summary$n_models), "\n")
cat("  family x SSP cells with only 1 model:", sum(coverage_model_summary$n_models == 1), "of", nrow(coverage_model_summary), "\n")

write_csv(coverage, file.path(OUT_DIR, "coverage_model_scenario_variable_region.csv"))
write_csv(coverage_model_summary, file.path(OUT_DIR, "coverage_model_count_by_family_ssp.csv"))
cat("  Saved coverage_model_scenario_variable_region.csv, coverage_model_count_by_family_ssp.csv\n")


# Step 6: "Other (R10)" content and its share of World ─────────────────────────

cat("\nSTEP 6: Quantify 'Other (R10)' and its share of World\n")

other_share <- raw_long %>%
  filter(region %in% c("Other (R10)", "World")) %>%
  dplyr::select(model, family, ssp, marker, variable, year, region, value) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  filter(!is.na(`Other (R10)`), !is.na(World), World != 0) %>%
  mutate(other_share_pct = `Other (R10)` / World * 100)

other_share_summary <- other_share %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    median_share_pct = median(other_share_pct),
    p10_share_pct = quantile(other_share_pct, 0.1),
    p90_share_pct = quantile(other_share_pct, 0.9),
    .groups = "drop"
  ) %>%
  arrange(desc(median_share_pct))

cat("  'Other (R10)' median share of World, by variable:\n")
print(other_share_summary, n = Inf)

write_csv(other_share, file.path(OUT_DIR, "other_r10_share.csv"))
log_lines <- c(
  log_lines,
  "'Other (R10)' is excluded from the region set used in metrics/analysis scripts (02+); its share of World is logged in other_r10_share.csv for reference.",
  paste0(
    "Largest 'Other (R10)' median share of World: ", other_share_summary$variable[1],
    " (", round(other_share_summary$median_share_pct[1], 1), "%)."
  )
)


# Step 7: GDP / Population consistency — exogenous or policy-responsive? ──────

cat("\nSTEP 7: GDP/Population consistency across models and across families\n")

driver_long <- raw_long %>% filter(variable %in% c("GDP|MER", "Population"), region == "World")

# (a) Across models, holding (family, SSP) fixed -- tests whether the SSP driver is harmonized/exogenous
across_models <- driver_long %>%
  group_by(variable, family, ssp, year) %>%
  summarise(n_models = n_distinct(model), cv_pct = sd(value) / mean(value) * 100, .groups = "drop") %>%
  filter(n_models > 1)

# (b) Across families, holding (model, SSP) fixed -- tests whether the driver responds to policy stringency
across_families <- driver_long %>%
  group_by(variable, model, ssp, year) %>%
  summarise(n_families = n_distinct(family), cv_pct = sd(value) / mean(value) * 100, .groups = "drop") %>%
  filter(n_families > 1)

cat("  Across models (same family+SSP) -- median CV%:", round(median(across_models$cv_pct), 2), "| max CV%:", round(max(across_models$cv_pct), 2), "\n")
cat("  Across families (same model+SSP) -- median CV%:", round(median(across_families$cv_pct), 2), "| max CV%:", round(max(across_families$cv_pct), 2), "\n")

gdp_pop_consistency <- bind_rows(
  across_models %>% mutate(comparison = "across_models_same_family_ssp"),
  across_families %>% mutate(comparison = "across_families_same_model_ssp")
)
write_csv(gdp_pop_consistency, file.path(OUT_DIR, "gdp_pop_consistency.csv"))

interp <- if (median(across_families$cv_pct) < 0.5) {
  "GDP/Population vary negligibly across forcing families for a given model+SSP -> denominator is effectively exogenous (SSP-determined), not policy-responsive."
} else {
  "GDP/Population vary non-trivially across forcing families for a given model+SSP -> denominator is at least partly policy-responsive, not purely exogenous."
}
cat("  Interpretation:", interp, "\n")
log_lines <- c(log_lines, interp)


# Step 8: Drop "Other (R10)", save cleaned panel ───────────────────────────────

cat("\nSTEP 8: Save cleaned long panel\n")

ssp_long_clean <- raw_long %>%
  filter(region != "Other (R10)") %>%
  dplyr::select(model, family, ssp, marker, scenario_orig = scenario, region, variable, unit, year, value)

write_csv(ssp_long_clean, file.path(OUT_DIR, "ssp_long_clean.csv"))
cat("  Saved ssp_long_clean.csv (", nrow(ssp_long_clean), "rows )\n")

writeLines(c(paste0("Assumptions / drop log -- generated ", Sys.Date()), "", paste0("- ", log_lines)), file.path(OUT_DIR, "assumptions_log.txt"))
cat("  Saved assumptions_log.txt\n")

# EoF
