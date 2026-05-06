## =============================================================================
## 04a_forecast_biomass.R
## Forecast biomass material consumption (Mt/yr) by region, category, and SSP
## scenario from 2024 to 2060 using the Kaya decomposition identity:
##   M = P × (GDP/P) × (M/G)
##
## Inputs:
##   Parameters/IIASA/ssp_drivers.csv              — Population and GDP/P indices (Script 03a)
##   Inputs/Driver_Assumptions_Biomass.xlsx        — M/G ratios (2024 base + 2050 targets by SSP)
##   Parameters/materials_region_DMC.csv            — UNEP domestic extraction for M_2024 anchor
##
## Output:
##   Results/biomass_forecast.csv
##     columns: scenario, region, category, year, M_Mt
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04_BaseAssumptions.R", encoding = "UTF-8")


# Step 1: Load SSP drivers ─────────────────────────────────────────────────────

cat("STEP 1: Load SSP drivers\n")

ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)

pop_index <- ssp_drivers |> filter(variable == "Population") |> dplyr::select(scenario, region, year, pop_index = index)

gdp_percap_index <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]") |>
  dplyr::select(scenario, region, year, gdp_percap_index = index)

cat("  Scenarios:", paste(sort(unique(ssp_drivers$scenario)), collapse = ", "), "\n")
cat("  Regions:", paste(sort(unique(ssp_drivers$region)), collapse = ", "), "\n")
cat("  Year range:", min(ssp_drivers$year), "-", max(ssp_drivers$year), "\n")


# Step 2: Load M/G driver assumptions from Excel ───────────────────────────────

cat("\nSTEP 2: Load M/G assumptions from Excel\n")

mg_raw <- read_excel("Inputs/MatIntensity_Assumptions.xlsx", sheet = "Biomass") |>
  dplyr::select(category = Category, region = Region, mg_2024 = `kg/USD 2024`, all_of(SSP_COLS))

cat("  Rows:", nrow(mg_raw), "| Categories:", paste(unique(mg_raw$category), collapse = ", "), "\n")
cat("  Regions:", paste(sort(unique(mg_raw$region)), collapse = ", "), "\n")


# Step 3: Build annual M/G index (linear 2024 → 2050, flat thereafter) ─────────
# The index is computed in absolute kg/USD space then normalised to 2024 = 1.

cat("\nSTEP 3: Build M/G ratio index (linear 2024 –", TARGET_YEAR, ", flat after)\n")

forecast_years <- seq(FORECAST_START, FORECAST_END)

mg_long <- mg_raw |> pivot_longer(cols = all_of(SSP_COLS), names_to = "scenario", values_to = "mg_2050")

mg_annual <- mg_long |>
  crossing(year = forecast_years) |>
  mutate(
    mg_ratio = case_when(
      year <= TARGET_YEAR ~ mg_2024 + (mg_2050 - mg_2024) * (year - BASE_YEAR) / (TARGET_YEAR - BASE_YEAR),
      TRUE ~ mg_2050
    ),
    mg_ratio_index = mg_ratio / mg_2024
  ) |>
  dplyr::select(scenario, region, category, year, mg_ratio, mg_ratio_index)

# Crop Residues follow the same M/G index trajectory as Crops
mg_annual <- mg_annual |> bind_rows(mg_annual |> filter(category == "Crops") |> mutate(category = "Crop Residues"))

cat("  Annual M/G rows:", nrow(mg_annual), "\n")
cat("  Categories:", paste(sort(unique(mg_annual$category)), collapse = ", "), "\n")


# Step 4: Load base-year material anchor from UNEP domestic extraction ─────────

cat("\nSTEP 4: Load M_2024 from UNEP DE data\n")

BIOMASS_CATEGORIES <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")

unep_dmc <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
cat("  UNEP DE rows:", nrow(unep_dmc), "| year range:", min(unep_dmc$year), "-", max(unep_dmc$year), "\n")

anchor_year <- max(unep_dmc$year[unep_dmc$year <= BASE_YEAR])
cat("  Using anchor year:", anchor_year, "\n")

m_2024 <- unep_dmc |>
  filter(year == anchor_year, material_category %in% BIOMASS_CATEGORIES) |>
  dplyr::select(region = Region, category = material_category, M_2024_Mt = DMC_Mt)

cat("  Anchor rows (region × category):", nrow(m_2024), "\n")
cat("  Categories found:", paste(sort(unique(m_2024$category)), collapse = ", "), "\n")
range(unep_dmc$year)

# Step 5: Kaya decomposition forecast ─────────────────────────────────────────
# M = P × (GDP/P) × (M/G)
# pop_index and gdp_percap_index are SSP indices (2024 = 1).
# mg_ratio_index captures efficiency change relative to 2024.

cat("\nSTEP 5: Kaya decomposition forecast (", FORECAST_START, "–", FORECAST_END, ")\n")

forecast <- mg_annual |>
  filter(year >= FORECAST_START, year <= FORECAST_END) |>
  left_join(pop_index, by = c("scenario", "region", "year")) |>
  left_join(gdp_percap_index, by = c("scenario", "region", "year")) |>
  left_join(m_2024, by = c("region", "category")) |>
  mutate(M_Mt = M_2024_Mt * pop_index * gdp_percap_index * mg_ratio_index) |>
  filter(!is.na(M_Mt))

cat("  Forecast rows:", nrow(forecast), "\n")
cat("  Scenarios:", paste(sort(unique(forecast$scenario)), collapse = ", "), "\n")
cat("  Regions:", paste(sort(unique(forecast$region)), collapse = ", "), "\n")
cat("  Categories:", paste(sort(unique(forecast$category)), collapse = ", "), "\n")


# Figure: M/G intensity index trajectory ──────────────────────────────────────

# Compute data first
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)

# 1. Compute historical M/G ratios (1970-2024)
historical_mg <- unep_dmc |>
  filter(material_category %in% BIOMASS_CATEGORIES) |>
  group_by(Region, year, material_category) |>
  summarise(M = sum(DMC_Mt), .groups = "drop") |>
  left_join(df_gdp, by = c("Region", "year")) |>
  mutate(
    mg_ratio = M / (GDP_2015USD / 1e9), # adjust units to match mg_annual
    mg_ratio_index = NA_real_,
    scenario = "Historical"
  ) |>
  rename(region = Region, category = material_category) # if needed — see note below

# 2. Get 2024 anchor values per region/category for indexing
anchor_2024 <- historical_mg |> filter(year == 2024) |> dplyr::select(region, mg_ratio_2024 = mg_ratio, category)

# 3. Apply index to forecast: actual M/G = index * 2024 actual M/G
mg_forecast <- mg_annual |> left_join(anchor_2024) |> mutate(mg_ratio = mg_ratio_index * mg_ratio_2024)

# 4. Bind historical + forecast
mg_full <- bind_rows(
  historical_mg |> dplyr::select(scenario, region, category, year, mg_ratio, mg_ratio_index),
  mg_forecast |> dplyr::select(scenario, region, category, year, mg_ratio, mg_ratio_index)
)

mg_full |>
  ggplot(aes(year, mg_ratio, col = scenario)) +
  geom_line() +
  geom_textline(
    data = ~ filter(.x, region == "Sub-Saharan Africa"),
    aes(col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1,
    text_smoothing = 30,
    show.legend = FALSE,
    linewidth = NA
  ) +
  facet_grid(category ~ region, scales = "free") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "DMC / GDP (kg per 2015 USD)", color = "") +
  theme_pb_large() +
  theme(legend.position = c(0.05, 0.9))

# fmt: skip
ggsave("Figures/Assumptions/BiomassIntensity.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 4.2, height = 8.7 * 2)


# Step 6: Save output ──────────────────────────────────────────────────────────

cat("\nSTEP 6: Save forecast\n")

forecast <- forecast |> dplyr::select(scenario, region, category, year, M_Mt)
write_csv(forecast, "Results/biomass_forecast.csv")
cat("  Saved: Results/biomass_forecast.csv (", nrow(forecast), "rows)\n")


# Step 7: Sanity check ─────────────────────────────────────────────────────────

cat("\n── Sanity check ────────────────────────────────────────────────────────────\n")
cat(
  "  Rows:",
  nrow(forecast),
  "| Year range:",
  min(forecast$year),
  "-",
  max(forecast$year),
  "| Scenarios:",
  paste(sort(unique(forecast$scenario)), collapse = ", "),
  "\n"
)
cat("  NA check — M_Mt:", sum(is.na(forecast$M_Mt)), "\n")

cat("\n  Total global biomass M (Mt) at selected years:\n")
print(
  forecast |>
    filter(year %in% c(2024, 2030, 2040, 2050, 2060)) |>
    group_by(scenario, category, year) |>
    summarise(M_Mt = round(sum(M_Mt, na.rm = TRUE), 1), .groups = "drop") |>
    pivot_wider(names_from = year, values_from = M_Mt) |>
    arrange(category, scenario)
)


# Figure: Global biomass forecast by category and scenario ─────────────────────

forecast |>
  group_by(scenario, category, year) |>
  reframe(M_Gt = sum(M_Mt) / 1e3) |>
  ggplot(aes(year, M_Gt)) +
  geom_line(aes(col = scenario)) +
  geom_textline(
    data = ~ filter(.x, category == "Crops"),
    aes(col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1,
    text_smoothing = 30,
    size = 3,
    show.legend = FALSE,
    linewidth = NA
  ) +
  facet_wrap(~category, scales = "free") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, NA)) +
  labs(x = "Year", y = "DMC [Gt]", color = "") +
  theme_pb_large() +
  theme(legend.position = "none", plot.margin = margin(5.5, 15, 5.5, 5.5))

# fmt: skip
ggsave("Figures/BiomassForecast.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2.5, height = 8.7)


# Figure: Biomass forecast by region (stacked area) ────────────────────────────

forecast_plot <- forecast |> group_by(scenario, category, region, year) |> reframe(M_Gt = sum(M_Mt) / 1e3)

region_order <- forecast_plot |>
  group_by(region) |>
  summarise(total = sum(M_Gt)) |>
  arrange(desc(total)) |>
  pull(region)

forecast_plot <- forecast_plot |> mutate(region = factor(region, levels = rev(region_order)))

label_data <- forecast_plot |>
  filter(scenario == "SSP1", category == "Crops") |>
  filter(year == max(year)) |>
  group_by(category, year) |>
  arrange(desc(region)) |>
  mutate(ymax = cumsum(M_Gt), ymin = lag(ymax, default = 0), y_mid = (ymin + ymax) / 2) |>
  ungroup()

ggplot(forecast_plot, aes(year, M_Gt, fill = region)) +
  geom_area(color = "black",linewidth=0.2, alpha = 0.85) +
  geom_text(
    data = label_data,
    aes(x = year, y = y_mid, label = region),
    hjust = 1, size = 2.5, fontface = "bold", show.legend = FALSE
  ) +
  facet_grid(category ~ scenario, scales = "free") +
  scale_fill_manual(values = PALETTE_REGIONS) +
  scale_color_manual(values = PALETTE_REGIONS) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, NA)) +
  labs(x = "Year", y = "DMC [Gt]") +
  theme_pb_large() +
  theme(legend.position = "none", plot.margin = margin(5.5, 15, 5.5, 5.5))

# fmt: skip
ggsave("Figures/BiomassForecast_Region.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 3, height = 8.7 * 4)

# EoF
