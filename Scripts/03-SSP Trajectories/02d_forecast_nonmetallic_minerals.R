## =============================================================================
## 02d_forecast_nonmetallic_minerals.R
## Forward Dynamic Stock Model (DSM) forecast for Non-metallic Minerals, 2025-2060.
## Five SSP scenarios; cohort-based; lifetime distributions from Script 02b.
##
## Inputs:
##   Inputs/MatIntensity_Assumptions.xlsx  sheet NonMetallicMinerals
##   Parameters/IIASA/ssp_drivers.csv      GDP by region x scenario x year
##   Parameters/stock_2024_age_profile.csv cohort-level in-use stock at 2024
##   Parameters/Intermediate/stock_trajectory_1970_2024.csv  (Plot A historical)
##   Parameters/stock_2024_total.csv        (sanity check anchor)
##
## Outputs:
##   Results/stock_trajectory_nonmetallic_minerals.csv
##   Results/stock_age_profile_nonmetallic_minerals.csv
##   Results/nonmetallic_minerals_forecast.csv
##   Figures/Forecast/nonmetallic_minerals_stock_trajectory.png
##   Figures/Forecast/nonmetallic_minerals_production.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")
source("Scripts/00-Functions/dsm_functions.R", encoding = "UTF-8")

# ── Script-specific constants ─────────────────────────────────────────────────

MATERIAL_LABEL <- "Non-metallic minerals"
ASSUMPTIONS_SHEET <- "NonMetallicMinerals"
MATERIAL_KEY <- "nonmetallic_minerals"
FORECAST_START <- 2025L # DSM starts year after base year


# Step 1: Load inputs ----------------------------------------------------------

cat("STEP 1: Load inputs\n")

# -- Assumptions ---------------------------------------------------------------

raw_assumptions <- read_excel(ASSUMPTIONS_FILE, sheet = ASSUMPTIONS_SHEET, skip = 1, col_types = "text")
raw_assumptions <- raw_assumptions |>
  mutate(
    `kg/USD 2024` = as.numeric(`kg/USD 2024`),
    SSP1 = as.numeric(SSP1),
    SSP2 = as.numeric(SSP2),
    SSP3 = as.numeric(SSP3),
    SSP4 = as.numeric(SSP4),
    SSP5 = as.numeric(SSP5)
  )

cat(
  "  Assumption sheet '",
  ASSUMPTIONS_SHEET,
  "': ",
  nrow(raw_assumptions),
  " rows x ",
  ncol(raw_assumptions),
  " cols\n",
  sep = ""
)
cat("  Columns:", paste(names(raw_assumptions), collapse = ", "), "\n")
print(head(raw_assumptions, 3))

# Auto-detect column roles by name pattern (no hardcoding)
ssp_cols <- names(raw_assumptions)[str_detect(names(raw_assumptions), "^SSP[1-9]")]
base_col <- names(raw_assumptions)[str_detect(names(raw_assumptions), "2024")][1]
char_cols <- names(raw_assumptions)[map_lgl(raw_assumptions, is.character)]
region_col <- char_cols[str_detect(tolower(char_cols), "region")][1]
enduse_col <- setdiff(char_cols, region_col)[1]

cat(
  "  Detected -> region:",
  region_col,
  "| end_use:",
  enduse_col,
  "| base:",
  base_col,
  "| SSPs:",
  paste(ssp_cols, collapse = ", "),
  "\n"
)

assumptions <- raw_assumptions |>
  dplyr::select(
    region = all_of(region_col),
    end_use = all_of(enduse_col),
    int_2024 = all_of(base_col),
    all_of(ssp_cols)
  ) |>
  filter(!is.na(region), !is.na(end_use), !is.na(int_2024))

cat("  Assumption rows (region x end_use):", nrow(assumptions), "\n")

# -- SSP drivers ---------------------------------------------------------------

ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)

# Use index, concante to 2024 GDP series from world bank
gdp <- ssp_drivers |> filter(variable == "GDP|PPP", year >= 2024) |> dplyr::select(scenario, region, year, index)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE) |>
  filter(year == 2024) |>
  mutate(year = NULL) |>
  rename(region = Region)
gdp <- gdp |> left_join(df_gdp) |> mutate(gdp_billion_usd = GDP_2015USD * index / 1e9) |> filter(year <= FORECAST_END)

cat(
  "  GDP rows:",
  nrow(gdp),
  "| scenarios:",
  paste(sort(unique(gdp$scenario)), collapse = ", "),
  "| years:",
  min(gdp$year),
  "-",
  max(gdp$year),
  "\n"
)
cat("  Regions in GDP:", paste(sort(unique(gdp$region)), collapse = ", "), "\n")

# -- Age profile ---------------------------------------------------------------

age_profile_all <- read_csv("Parameters/stock_2024_age_profile.csv", show_col_types = FALSE) |>
  rename(region = Region) |>
  filter(material == MATERIAL_LABEL) |>
  group_by(region, end_use, cohort_year, cohort_age) |>
  summarise(surviving_stock_Mt = sum(surviving_stock_Mt), .groups = "drop")


cat("  Age profile rows for '", MATERIAL_LABEL, "': ", nrow(age_profile_all), "\n", sep = "")
cat("  End-uses in age profile:", paste(sort(unique(age_profile_all$end_use)), collapse = ", "), "\n")
cat("  Regions in age profile: ", paste(sort(unique(age_profile_all$region)), collapse = ", "), "\n")

# -- Calibrated stock 2024 (sanity check) --------------------------------------

stock_2024_cal <- read_csv("Parameters/stock_2024_total.csv", show_col_types = FALSE) |>
  rename(region = Region) |>
  filter(material == MATERIAL_LABEL) |>
  summarise(global_stock_Mt = sum(stock_Mt, na.rm = TRUE)) |>
  pull(global_stock_Mt)

cat("  Calibrated global stock 2024 (Script 02b):", round(stock_2024_cal / 1e3, 1), "Gt\n")


# Step 2: Build target stock intensity trajectory (2024-2060) ------------------

cat("\nSTEP 2: Build stock intensity trajectory\n")

forecast_years <- seq(BASE_YEAR, FORECAST_END)

assumptions_long <- assumptions |>
  pivot_longer(cols = all_of(ssp_cols), names_to = "scenario", values_to = "int_target")

intensity_trajectory <- assumptions_long |>
  crossing(year = forecast_years) |>
  mutate(
    # For fixed end_uses, set target equal to 2024 so interpolation is flat
    int_target_eff = if_else(end_use %in% ENDUSE_LABELS[FIXED_INTENSITY_ENDUSES], int_2024, int_target),
    stock_intensity = case_when(
      end_use %in% ENDUSE_LABELS[FIXED_INTENSITY_ENDUSES] ~ int_2024,
      year <= TARGET_YEAR ~ int_2024 + (int_target_eff - int_2024) * (year - BASE_YEAR) / (TARGET_YEAR - BASE_YEAR),
      TRUE ~ int_target_eff
    )
  ) |>
  dplyr::select(region, end_use, scenario, year, stock_intensity)

cat("  Intensity trajectory rows:", nrow(intensity_trajectory), "\n")
cat("  Scenarios:", paste(sort(unique(intensity_trajectory$scenario)), collapse = ", "), "\n")


# Step 3: Compute target stock (Mt) and sanity check ---------------------------

cat("\nSTEP 3: Compute target stock (Mt) and sanity check\n")

# unit check: stock_intensity (kg / constant USD) x GDP (billion USD)
# = kg/USD x 10^9 USD = 10^9 kg = 1 Mt  -> conversion factor = 1
target_stock <- intensity_trajectory |>
  left_join(gdp, by = c("scenario", "region", "year")) |>
  mutate(target_stock_Mt = stock_intensity * gdp_billion_usd)

unmatched_gdp <- target_stock |> filter(is.na(gdp_billion_usd)) |> distinct(scenario, region) |> nrow()
if (unmatched_gdp > 0) {
  warning(sprintf("[WARN] %d scenario x region combinations had no GDP match -- NAs in target_stock_Mt", unmatched_gdp))
}

target_stock <- target_stock |> filter(!is.na(target_stock_Mt))

# Sanity check: global target stock at 2024 vs calibrated stock
global_target_2024 <- target_stock |>
  filter(year == BASE_YEAR, scenario == first(unique(scenario))) |>
  summarise(stock_Mt = sum(target_stock_Mt, na.rm = TRUE)) |>
  pull(stock_Mt)

pct_diff <- abs(global_target_2024 - stock_2024_cal) / max(stock_2024_cal, 1) * 100
cat("  Global target stock 2024 (intensity x GDP):", round(global_target_2024, 1), "Mt\n")
cat("  Calibrated stock 2024  (Script 02b)        :", round(stock_2024_cal, 1), "Mt\n")
cat("  Difference:", round(pct_diff, 1), "%")
if (pct_diff > 20) {
  cat("  [WARN] >20% divergence -- check intensity assumptions or GDP units")
}
cat("\n")


# Step 4: Prepare nested inputs ------------------------------------------------

cat("\nSTEP 4: Prepare nested inputs for DSM\n")

# Nest target stock by region x end_use x scenario
target_nested <- target_stock |>
  filter(year >= FORECAST_START) |>
  dplyr::select(region, end_use, scenario, year, target_stock_Mt) |>
  group_by(region, end_use, scenario) |>
  nest(target_stock_traj = c(year, target_stock_Mt)) |>
  ungroup()

# Nest 2024 age profile by region x end_use
age_nested <- age_profile_all |>
  dplyr::select(region, end_use, cohort_year, surviving_stock_Mt) |>
  mutate(end_use = ENDUSE_LABELS[end_use]) |>
  group_by(region, end_use) |>
  nest(cohorts_2024 = c(cohort_year, surviving_stock_Mt)) |>
  ungroup()

cat("  Target groups (region x end_use x scenario):", nrow(target_nested), "\n")
cat("  Age profile groups (region x end_use):", nrow(age_nested), "\n")

# Join; warn about missing age profiles
dsm_input <- target_nested |>
  left_join(mutate(lifetime_params, end_use = ENDUSE_LABELS[end_use]), by = "end_use") |>
  left_join(age_nested, by = c("region", "end_use"))

missing_profile <- dsm_input |> filter(map_lgl(cohorts_2024, is.null))
if (nrow(missing_profile) > 0) {
  cat("[WARN] Missing 2024 age profile for", nrow(missing_profile), "group(s) -- skipping:\n")
  print(missing_profile |> distinct(region, end_use), n = Inf)
}

dsm_input <- dsm_input |> filter(!map_lgl(cohorts_2024, is.null))
cat("  DSM groups after filtering:", nrow(dsm_input), "\n")


# Step 5: Run forward DSM -------------------------------------------------------

cat("\nSTEP 5: Run forward DSM for all region x end_use x scenario groups\n")

dsm_output <- dsm_input |>
  mutate(
    dsm = purrr::pmap(
      list(cohorts_2024, target_stock_traj, mean_life, weibull_k),
      run_forward_dsm,
      start_year = 2024L,
      end_year = FORECAST_END,
      snapshot_years = SNAPSHOT_YEARS
    )
  )

cat("  DSM complete:", nrow(dsm_output), "group(s) processed\n")


# Step 6: Extract results -------------------------------------------------------

cat("\nSTEP 6: Extract results\n")

summary_raw <- dsm_output |>
  mutate(summary = map(dsm, "summary")) |>
  dplyr::select(region, end_use, scenario, summary) |>
  unnest(summary)

snapshots_raw <- dsm_output |>
  mutate(snaps = map(dsm, "snapshots")) |>
  dplyr::select(region, end_use, scenario, snaps) |>
  unnest(snaps)

cat("  Summary rows:", nrow(summary_raw), "\n")
cat("  Snapshot rows:", nrow(snapshots_raw), "\n")

# Check for negatives
neg_stock <- sum(summary_raw$total_stock_Mt < -1e-6, na.rm = TRUE)
neg_prod <- sum(summary_raw$production_Mt < -1e-6, na.rm = TRUE)
neg_waste <- sum(summary_raw$waste_Mt < -1e-6, na.rm = TRUE)
if (neg_stock + neg_prod + neg_waste > 0) {
  warning(sprintf("[WARN] Negative values detected -- stock: %d, prod: %d, waste: %d", neg_stock, neg_prod, neg_waste))
}

# Years where production was zero due to stock overshoot
overshoot_count <- summary_raw |> filter(production_Mt == 0, total_stock_Mt > 0) |> nrow()
cat("  Year-group combos with production=0 due to stock overshoot:", overshoot_count, "\n")


# Step 6.5: Waste recovery (downcycling for non-metallic minerals) --------------

cat("\nSTEP 6.5: Waste recovery -- calculate downcycled mineral flows\n")

downcycling_rates <- read_excel("Inputs/Recycling_Assumptions.xlsx", sheet = "Downcycling") |>
  dplyr::select(region = Region, Downcycling_Buildings, Downcycling_Roads)

unmatched_downcycling <- summary_raw |> distinct(region) |> anti_join(downcycling_rates, by = "region")
if (nrow(unmatched_downcycling) > 0) {
  warning(sprintf(
    "[WARN] %d region(s) have no match in Downcycling sheet: %s",
    nrow(unmatched_downcycling),
    paste(unmatched_downcycling$region, collapse = ", ")
  ))
}

# Compute raw recovery flows
summary_raw <- summary_raw |>
  left_join(downcycling_rates, by = "region") |>
  mutate(
    recovered_same_sector = if_else(
      str_detect(end_use, "Building"),
      waste_Mt * Downcycling_Buildings * SUB_FACTOR_RECYCLING_SAME,
      0
    ),
    recovered_to_roads = case_when(
      str_detect(end_use, "Building") ~ waste_Mt * Downcycling_Roads * SUB_FACTOR_DOWNCYCLING_ROADS,
      str_detect(end_use, "Civil") ~ waste_Mt *
        (Downcycling_Buildings + Downcycling_Roads) *
        SUB_FACTOR_DOWNCYCLING_ROADS,
      TRUE ~ 0
    )
  )

# Aggregate secondary material available for roads, cap directly — no ratio
road_recovery <- summary_raw |>
  group_by(scenario, region, year) |>
  summarise(
    secondary_available = sum(recovered_to_roads, na.rm = TRUE),
    road_production = sum(production_Mt[str_detect(end_use, "Civil")], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(secondary_for_roads = pmin(secondary_available, MAX_SECONDARY_ROADS * road_production)) |>
  dplyr::select(scenario, region, year, secondary_for_roads)

# Apply credits to the correct end-use row
summary_raw <- summary_raw |>
  left_join(road_recovery, by = c("scenario", "region", "year")) |>
  mutate(
    recovered_same_sector = pmin(recovered_same_sector, production_Mt),
    recovered_material_Mt = case_when(
      str_detect(end_use, "Building") ~ recovered_same_sector,
      str_detect(end_use, "Civil") ~ coalesce(secondary_for_roads, 0),
      TRUE ~ 0
    ),
    recovered_material_Mt = replace_na(recovered_material_Mt, 0),
    production_Mt = pmax(0, production_Mt - recovered_material_Mt)
  ) |>
  dplyr::select(
    -recovered_same_sector,
    -recovered_to_roads,
    -secondary_for_roads,
    -Downcycling_Buildings,
    -Downcycling_Roads
  )

cat(
  "  Total global recovered minerals (SSP2, 2050):",
  round(
    sum(summary_raw$recovered_material_Mt[summary_raw$scenario == "SSP2" & summary_raw$year == 2050], na.rm = TRUE),
    1
  ),
  "Mt\n"
)


# Step 7: Save outputs ----------------------------------------------------------

cat("\nSTEP 7: Save outputs\n")

stock_traj_out <- summary_raw |> dplyr::select(scenario, region, end_use, year, total_stock_Mt)
write_csv(stock_traj_out, paste0("Results/stock_trajectory_", MATERIAL_KEY, ".csv"))
cat("  Saved: Results/stock_trajectory_", MATERIAL_KEY, ".csv (", nrow(stock_traj_out), " rows)\n", sep = "")

age_profile_out <- snapshots_raw |>
  dplyr::select(scenario, region, end_use, snapshot_year, cohort_year, cohort_age, surviving_stock_Mt)
write_csv(age_profile_out, paste0("Results/stock_age_profile_", MATERIAL_KEY, ".csv"))
cat("  Saved: Results/stock_age_profile_", MATERIAL_KEY, ".csv (", nrow(age_profile_out), " rows)\n", sep = "")

forecast_out <- summary_raw |>
  dplyr::select(
    scenario,
    region,
    end_use,
    year,
    replacement_Mt,
    new_additions_Mt,
    production_Mt,
    waste_Mt,
    recovered_material_Mt
  )
write_csv(forecast_out, paste0("Results/", MATERIAL_KEY, "_forecast.csv"))
cat("  Saved: Results/", MATERIAL_KEY, "_forecast.csv (", nrow(forecast_out), " rows)\n", sep = "")


# Step 8: Validation plots ------------------------------------------------------

cat("\nSTEP 8: Save validation plots to Figures/Forecast/\n")


# -- Plot A: Historical (1970-2024) + SSP forecast fan (2025-2060) by region ---

hist_traj <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE) |>
  rename(region = Region) |>
  mutate(end_use = ENDUSE_LABELS[end_use]) |>
  filter(material == MATERIAL_LABEL) |>
  group_by(region, end_use, year) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop")

forecast_ribbon <- stock_traj_out |>
  group_by(region, end_use, year) |>
  summarise(stock_min = min(total_stock_Mt), stock_max = max(total_stock_Mt), .groups = "drop")

forecast_ssp2 <- stock_traj_out |> filter(scenario == "SSP2")

global_hist <- hist_traj |>
  group_by(end_use, year) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(region = "Global")

global_ribbon <- stock_traj_out |>
  group_by(end_use, year, scenario) |>
  summarise(stock_Mt = sum(total_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  group_by(end_use, year) |>
  summarise(stock_min = min(stock_Mt), stock_max = max(stock_Mt), .groups = "drop") |>
  mutate(region = "Global")

global_ssp2 <- stock_traj_out |>
  filter(scenario == "SSP2") |>
  group_by(end_use, year) |>
  summarise(total_stock_Mt = sum(total_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(region = "Global")

forecast_ssp2_labelled <- bind_rows(
  forecast_ssp2 |> rename(stock_Mt = total_stock_Mt),
  global_ssp2 |> rename(stock_Mt = total_stock_Mt)
)

p_traj <- ggplot() +
  # SSP1-5 ribbon
  geom_ribbon(
    data = bind_rows(forecast_ribbon, global_ribbon),
    aes(x = year, ymin = stock_min / 1e3, ymax = stock_max / 1e3, fill = region),
    alpha = 0.15,
    show.legend = FALSE
  ) +
  # Historical lines (1970-2024)
  geom_line(
    data = bind_rows(hist_traj, global_hist),
    aes(x = year, y = stock_Mt / 1e3, colour = region),
    linewidth = 0.5
  ) +
  # SSP2 central forecast line with region label drawn along path
  geom_textline(
    data = forecast_ssp2_labelled,
    aes(x = year, y = stock_Mt / 1e3, colour = region, label = region),
    linewidth = 0.5,
    linetype = "dashed",
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(~end_use, scales = "free_y") +
  scale_colour_manual(values = c(PALETTE_REGIONS, "Global" = "black"), na.value = "#999999") +
  scale_fill_manual(values = c(PALETTE_REGIONS, "Global" = "black"), na.value = "#999999") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = paste(MATERIAL_LABEL, "— In-use stock trajectory 1970–2060"), x = "Year", y = "In-use stock (Gt)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_traj

# fmt: skip
ggsave(paste0("Figures/Stocks/", MATERIAL_KEY, "_stock_trajectory.png"),p_traj,units = "cm",dpi = 600,width = 8.7 * 2,height = 8.7 * 2)


# -- Plot B: Production 2025-2060 (SSP2), stacked replacement vs new, by region -

prod_plot <- forecast_out |>
  filter(scenario == "SSP2") |>
  filter(str_detect(end_use, "Building|Civil")) |>
  mutate(region = factor(region, levels = names(PALETTE_REGIONS))) |>
  dplyr::select(region, end_use, year, replacement_Mt, new_additions_Mt) |>
  pivot_longer(cols = c(replacement_Mt, new_additions_Mt), names_to = "component", values_to = "Mt") |>
  mutate(
    component = factor(
      component,
      levels = c("new_additions_Mt", "replacement_Mt"),
      labels = c("New additions", "Replacement")
    )
  )

p_prod <- prod_plot |>
  ggplot(aes(x = year, y = Mt, fill = component)) +
  geom_area(alpha = 0.85, colour = NA) +
  facet_grid(region ~ end_use, scales = "free_y") +
  scale_fill_manual(values = c("New additions" = "#2C7BB6", "Replacement" = "#D7191C"), name = NULL) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, NA)) +
  labs(
    title = paste(MATERIAL_LABEL, "— Production requirements 2025–2060 (SSP2)"),
    x = "Year",
    y = "Production (Mt/yr)"
  ) +
  theme_pb_large() +
  theme(legend.position = "bottom", plot.margin = margin(5.5, 10, 5.5, 5.5))


p_prod
# fmt: skip
ggsave(paste0("Figures/Stocks/", MATERIAL_KEY, "_production.png"),p_prod,units = "cm",dpi = 600,width = 8.7 * 2.5,height = 8.7 * 3)


# Step 9: Summary check --------------------------------------------------------

cat("\n── STEP 9: Summary check ────────────────────────────────────────────────────\n")

cat("  Scenarios processed:", paste(sort(unique(summary_raw$scenario)), collapse = ", "), "\n")
cat("  Regions processed:  ", paste(sort(unique(summary_raw$region)), collapse = ", "), "\n")
cat("  End-uses processed: ", paste(sort(unique(summary_raw$end_use)), collapse = ", "), "\n")
cat(
  "  Groups (region x end_use x scenario):",
  n_distinct(summary_raw |> dplyr::select(region, end_use, scenario)),
  "\n"
)

cat("\n  Global production (Mt) under SSP2:\n")
print(
  summary_raw |>
    filter(scenario == "SSP2", year %in% c(2030, 2050, 2060)) |>
    group_by(year) |>
    summarise(production_Mt = round(sum(production_Mt, na.rm = TRUE), 1), .groups = "drop")
)

if (nrow(missing_profile) > 0) {
  cat("\n  [WARN] Groups skipped due to missing 2024 age profile:\n")
  print(missing_profile |> distinct(region, end_use))
}

cat("\n  Year-group combos with production forced to 0 (stock overshoot):", overshoot_count, "\n")
cat("  Negative value checks -- stock:", neg_stock, "| production:", neg_prod, "| waste:", neg_waste, "\n")

cat(
  "\n  NA check -- total_stock_Mt:",
  sum(is.na(summary_raw$total_stock_Mt)),
  "| production_Mt:",
  sum(is.na(summary_raw$production_Mt)),
  "\n"
)

# EoF
