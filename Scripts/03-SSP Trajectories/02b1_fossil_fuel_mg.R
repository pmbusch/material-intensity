## =============================================================================
## 02b1_fossil_fuel_mg.R
## Computes M/G (material per unit GDP) intensity trajectories for fossil fuels
## by decomposing the Kaya identity: M/G = (P/G) × (E/P) × (M/E)
## where M/E = 1 (current assumption) so M/G = E/G.
##
## Run AFTER 02b (reads Results/fossil_fuel_forecast.csv and SSP drivers).
##
## Outputs:
##   Parameters/fossil_fuel_mg_trajectories.csv  -- M/G by region x fuel x year x scenario
##   Parameters/fossil_fuel_mg_assumptions.csv   -- template for FossilFuels Excel sheet
##                                                  (2024 base + SSP1-5 targets at TARGET_YEAR)
##   Results/fossil_fuel_mg_bounds.csv           -- bound (min/max) of the M/G target implied
##                                                  by any region x SSP1-5 combination at
##                                                  TARGET_YEAR -- cross-check against
##                                                  MC_Assumptions.xlsx "Intensity" sheet
##   Figures/Assumptions/FossilFuelMG_Decomposed.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8") # model_parameters.R: BASE_YEAR, TARGET_YEAR, SSP_LABELS, ASSUMPTIONS_FILE

FUEL_UNEP_MAP <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum")
FUEL_LABEL <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Oil")


# Step 1: Load pre-computed forecast (from 04b) --------------------------------

cat("STEP 1: Load fossil fuel forecast and SSP drivers\n")

forecast <- read_csv("Results/fossil_fuel_forecast.csv", show_col_types = FALSE)
cat("  Forecast rows:", nrow(forecast), "| scenarios:", paste(sort(unique(forecast$scenario)), collapse = ", "), "\n")

ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
df_gdp_base <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)

# Pop and GDP/P indices for decomposition
pop_idx <- ssp_drivers |>
  filter(variable == "Population", year >= BASE_YEAR, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, pop_index = index)

gdp_percap_idx <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= BASE_YEAR, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_percap_index = index)

# Absolute GDP (billion USD): needed to compute M/G in physical units
gdp_full <- ssp_drivers |>
  filter(variable == "GDP|PPP", year >= BASE_YEAR, year <= FORECAST_END) |>
  dplyr::select(scenario, region, year, gdp_index = index) |>
  left_join(df_gdp_base |> filter(year == BASE_YEAR) |> dplyr::select(region = Region, GDP_2015USD), by = "region") |>
  mutate(gdp_billion_usd = GDP_2015USD * gdp_index / 1e9)


# Step 2: Compute projected M/G from forecast / GDP ----------------------------

cat("\nSTEP 2: Compute projected M/G from forecast / GDP\n")

projected_mg <- forecast |>
  filter(year >= BASE_YEAR, year <= FORECAST_END) |>
  left_join(gdp_full |> dplyr::select(scenario, region, year, gdp_billion_usd), by = c("scenario", "region", "year")) |>
  mutate(mg_kgUSD = M_Mt / gdp_billion_usd) |>
  filter(!is.na(mg_kgUSD)) |>
  dplyr::select(scenario, region, fuel, year, mg_kgUSD)

cat("  Projected rows:", nrow(projected_mg), "\n")

write_csv(projected_mg, "Parameters/fossil_fuel_mg_trajectories.csv")


# Build assumptions CSV for Excel FossilFuels sheet --------------------
# Format mirrors MatIntensity_Assumptions.xlsx: Region, fuel, kg/USD 2024, SSP1..SSP5
# TARGET_YEAR value gives the endpoint for each SSP scenario.

cat("\nSTEP 3: Build assumptions template for Excel sheet\n")

mg_base <- projected_mg |>
  filter(year == BASE_YEAR) |>
  dplyr::select(region, fuel, mg_base_2024 = mg_kgUSD) |>
  distinct()

mg_targets <- projected_mg |>
  filter(year == TARGET_YEAR) |>
  dplyr::select(scenario, region, fuel, mg_target = mg_kgUSD) |>
  pivot_wider(names_from = scenario, values_from = mg_target)

# Ensure SSP1-5 columns are present (some combinations may be missing)
ssp_template <- setdiff(SSP_LABELS, names(mg_targets))
if (length(ssp_template) > 0) {
  mg_targets[ssp_template] <- NA_real_
  cat("  [WARN] Missing SSP columns (no data):", paste(ssp_template, collapse = ", "), "\n")
}

assumptions_template <- mg_base |>
  left_join(mg_targets, by = c("region", "fuel")) |>
  rename(`kg/USD 2024` = mg_base_2024) |>
  dplyr::select(Region = region, fuel, `kg/USD 2024`, all_of(SSP_LABELS)) |>
  arrange(fuel, Region)

# Round to two decimals
assumptions_rounded <- assumptions_template |>
  mutate(
    SSP1 = round(SSP1, 2),
    SSP2 = round(SSP2, 2),
    SSP3 = round(SSP3, 2),
    SSP4 = round(SSP4, 2),
    SSP5 = round(SSP5, 2)
  )

write_csv(assumptions_rounded, "Parameters/fossil_fuel_mg_assumptions.csv")
cat("  Saved: Parameters/fossil_fuel_mg_assumptions.csv\n")

# Manual paste target: FossilFuels sheet in MatIntensity_Assumptions.xlsx
assumptions_rounded %>% write.table('clipboard-16384', sep = '\t', row.names = FALSE)


# Step 4: Global (world-aggregate) M/G trajectory per SSP scenario ------------
# Region-level M and GDP summed first, then divided -- one size-weighted world
# M/G line per SSP (distinct from the per-region x SSP envelope in Step 5).

cat("\nSTEP 4: Global M/G trajectory per SSP scenario\n")

global_mg <- forecast |>
  filter(year >= BASE_YEAR, year <= FORECAST_END) |>
  group_by(scenario, fuel, year) |>
  summarise(M_Mt = sum(M_Mt), .groups = "drop") |>
  left_join(
    gdp_full |> group_by(scenario, year) |> summarise(gdp_billion_usd = sum(gdp_billion_usd), .groups = "drop"),
    by = c("scenario", "year")
  ) |>
  mutate(mg_kgUSD = M_Mt / gdp_billion_usd, fuel = FUEL_LABEL[fuel]) |>
  dplyr::select(scenario, fuel, year, mg_kgUSD)

cat("  Global M/G rows:", nrow(global_mg), "\n")


# Step 5: Bound of implied M/G targets -----------------------------------------
# mg_envelope: min/max across every region x SSP1-5 combination, by year --
#   drawn as the figure's shaded band (visual only, not saved).
# mg_bounds: same min/max but frozen at TARGET_YEAR -- the summary result,
#   directly comparable to MC_Assumptions.xlsx "Intensity" sheet min/max.

cat("\nSTEP 5: Bound of implied M/G targets across region x SSP\n")

mg_envelope <- projected_mg |>
  mutate(fuel = FUEL_LABEL[fuel]) |>
  group_by(fuel, year) |>
  summarise(mg_min = min(mg_kgUSD), mg_max = max(mg_kgUSD), .groups = "drop")

mg_bounds <- projected_mg |>
  filter(year == TARGET_YEAR) |>
  mutate(fuel = FUEL_LABEL[fuel]) |>
  group_by(fuel) |>
  summarise(
    mg_min = min(mg_kgUSD),
    region_min = region[which.min(mg_kgUSD)],
    scenario_min = scenario[which.min(mg_kgUSD)],
    mg_max = max(mg_kgUSD),
    region_max = region[which.max(mg_kgUSD)],
    scenario_max = scenario[which.max(mg_kgUSD)],
    .groups = "drop"
  ) |>
  arrange(fuel)

print(mg_bounds)

write_csv(mg_bounds, "Results/fossil_fuel_mg_bounds.csv")
cat("  Saved: Results/fossil_fuel_mg_bounds.csv\n")


# Step 6: Figure -- embedded M/G assumptions per fossil fuel -------------------
# Shaded band = full region x SSP spread; colored lines = global SSP trajectory;
# dotted marker + direct labels = the TARGET_YEAR bound saved above.

cat("\nSTEP 6: Build FossilFuelMG_Decomposed figure\n")

target_labels <- mg_bounds |>
  mutate(
    label_min = paste0("Min ", round(mg_min, 2), " (", scenario_min, ", ", region_min, ")"),
    label_max = paste0("Max ", round(mg_max, 2), " (", scenario_max, ", ", region_max, ")")
  )

ggplot() +
  geom_ribbon(data = mg_envelope, aes(year, ymin = mg_min, ymax = mg_max), fill = "grey50", alpha = 0.18) +
  geom_line(data = global_mg, aes(year, mg_kgUSD, col = scenario)) +
  geom_textline(
    data = global_mg |> filter(fuel == "Coal"),
    aes(year, mg_kgUSD, col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1,
    text_smoothing = 30,
    show.legend = FALSE,
    linewidth = NA
  ) +
  geom_vline(xintercept = TARGET_YEAR, linetype = "dotted", colour = "grey40", linewidth = 0.3) +
  geom_text(
    data = target_labels,
    aes(x = TARGET_YEAR, y = mg_max, label = label_max),
    hjust = -0.05, vjust = 0, size = pb_annot_size("largeFont"), colour = "grey30"
  ) +
  geom_text(
    data = target_labels,
    aes(x = TARGET_YEAR, y = mg_min, label = label_min),
    hjust = -0.05, vjust = 1, size = pb_annot_size("largeFont"), colour = "grey30"
  ) +
  facet_wrap(~fuel, scales = "free_y") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = "M / GDP (kg per USD)", col = "") +
  theme_pb_large() +
  theme(legend.position = "none", plot.margin = margin(5.5, 70, 5.5, 5.5))

# fmt: skip
ggsave("Figures/Assumptions/FossilFuelMG_Decomposed.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 3, height = 8.7 * 1.4)
cat("  Saved: Figures/Assumptions/FossilFuelMG_Decomposed.png\n")

# EoF
