## =============================================================================
## 04b_forecast_fossil_fuels.R
## Forecast fossil fuel material consumption (Mt/yr) by region, fuel, and SSP
## scenario from 2024 to 2060 using the Kaya decomposition identity:
##   M = P × (GDP/P) × (E/GDP) × (M/E)
##
## Inputs:
##   Parameters/IIASA/ssp_drivers.rds               — GDP and population indices (Script 03a)
##   Inputs/IIASA_SSP/2018_Quantification/
##     primaryEnergy_SSP.csv                         — IIASA R5 primary energy by scenario
##   Parameters/materials_region_DE.csv              — UNEP domestic extraction for M_2024 anchor
##
## Output:
##   Results/fossil_fuel_forecast.csv
##     columns: scenario, region, fuel, year, M_Mt
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# ── Marker models: one per SSP scenario ───────────────────────────────────────
SSP_MARKERS <- c(
  "SSP1" = "IMAGE",
  "SSP2" = "MESSAGE-GLOBIOM",
  "SSP3" = "AIM/CGE",
  "SSP4" = "GCAM4",
  "SSP5" = "REMIND-MAGPIE"
)

# ── Fuel → IIASA energy variable name ─────────────────────────────────────────
FUEL_VARIABLES <- c("coal" = "Primary Energy|Coal", "gas" = "Primary Energy|Gas", "oil" = "Primary Energy|Oil")

# ── Fuel → UNEP material_category for base-year anchor (DE flow) ──────────────
FUEL_UNEP_MAP <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum")

# ── Europe & Russia blend weights (fuel-specific share of OECD & EU R5) ───────
# Weights reflect Russia's share of regional energy from historical UNEP data.
# (1 - w_oecd_eu) is the Reforming Economies weight.
# Based on UNEP data 2015-2024 weights
EUR_BLEND_WEIGHTS <- tribble(
  ~fuel  , ~w_oecd_eu ,
  "coal" , 0.76       , # Russia 24%
  "gas"  , 0.54       , # Russia 46%
  "oil"  , 0.67 # Russia 33%
)

# ── IIASA R5 region labels as they appear in the energy CSV ───────────────────
R5_OECD_EU <- "OECD & EU (R5)"
R5_ASIA <- "Asia (R5)"
R5_LAM <- "Latin America (R5)"
R5_MAF <- "Middle East & Africa (R5)"
R5_REF <- "Reforming Economies (R5)"

# ── R5 code → project region(s) mapping ──────────────────────────────────────
# One-to-many: R5 regions that cover multiple project regions share the same
# energy trajectory index (IIASA does not disaggregate at sub-R5 level).
R5_REGION_MAP <- tribble(
  ~r5_region , ~region                      ,
  "OECD_EU"  , "North America"              ,
  "OECD_EU"  , "Oceania"                    ,
  "Asia"     , "East Asia"                  ,
  "Asia"     , "South Asia"                 ,
  "LAM"      , "Latin America"              ,
  "MAF"      , "Middle East & North Africa" ,
  "MAF"      , "Sub-Saharan Africa"         ,
  "EUR"      , "Europe & Russia" # blended OECD_EU + REF trajectory
)

BASE_YEAR <- 2024L
FORECAST_START <- 2024L
FORECAST_END <- 2060L


# Step 1: Load IIASA primary energy data ──────────────────────────────────────

cat("STEP 1: Load IIASA primary energy data\n")

energy_raw <- read_csv("Inputs/IIASA_SSP/2018_Quantification/primaryEnergy_SSP.csv", show_col_types = FALSE)
cat("Energy raw:", nrow(energy_raw), "rows x", ncol(energy_raw), "cols\n")
cat("R5 regions in file:", paste(sort(unique(energy_raw$region)), collapse = ", "), "\n")
cat("Variables in file:", paste(sort(unique(energy_raw$variable)), collapse = ", "), "\n")
cat("Scenarios in file:", paste(sort(unique(energy_raw$scenario)), collapse = ", "), "\n")

yr_cols <- names(energy_raw)[grepl("^[0-9]{4}$", names(energy_raw))]
cat("Year columns:", paste(yr_cols, collapse = ", "), "\n")


# Step 2: Filter to marker scenarios and coal / oil / gas ─────────────────────

cat("\nSTEP 2: Filter to marker scenarios and fossil fuel variables\n")

model_to_ssp <- setNames(names(SSP_MARKERS), SSP_MARKERS)

energy_filtered <- energy_raw %>%
  filter(model %in% unname(SSP_MARKERS)) %>%
  filter(variable %in% unname(FUEL_VARIABLES)) %>%
  mutate(ssp = model_to_ssp[model], ssp_tag = str_extract(scenario, "^SSP[0-9]")) %>%
  filter(ssp == ssp_tag) %>%
  filter(str_detect(scenario, "Baseline")) |>
  # Add short fuel label
  mutate(fuel = names(FUEL_VARIABLES)[match(variable, unname(FUEL_VARIABLES))]) %>%
  select(ssp, r5_region = region, fuel, all_of(yr_cols))

cat("Rows after filter:", nrow(energy_filtered), "\n")
cat("SSPs:", paste(sort(unique(energy_filtered$ssp)), collapse = ", "), "\n")
cat("Fuels:", paste(sort(unique(energy_filtered$fuel)), collapse = ", "), "\n")
cat("R5 regions:", paste(sort(unique(energy_filtered$r5_region)), collapse = ", "), "\n")


# Step 3: Pivot years to long ─────────────────────────────────────────────────

cat("\nSTEP 3: Pivot year columns to long format\n")

energy_long <- energy_filtered %>%
  pivot_longer(cols = all_of(yr_cols), names_to = "year", values_to = "value_ej") %>%
  mutate(year = as.integer(year)) %>%
  arrange(ssp, r5_region, fuel, year)

cat("Long format rows:", nrow(energy_long), "\n")


# Step 4: PCHIP interpolation from irregular grid to annual resolution ─────────
# splinefun(method="monoH.FC") implements PCHIP (monotone cubic Hermite
# interpolation), which preserves monotonicity within each segment and avoids
# the spurious oscillations of standard cubic splines.

cat("\nSTEP 4: PCHIP interpolation to annual resolution\n")

interp_years <- seq(min(energy_long$year), max(energy_long$year))

energy_annual <- energy_long %>%
  group_by(ssp, r5_region, fuel) %>%
  nest() %>%
  mutate(
    annual_df = purrr::map(data, function(d) {
      d <- d %>% arrange(year)
      f <- splinefun(x = d$year, y = d$value_ej, method = "monoH.FC")
      tibble(year = as.integer(interp_years), value_ej = f(interp_years))
    })
  ) %>%
  select(-data) %>%
  unnest(annual_df) %>%
  ungroup()

cat("Annual rows:", nrow(energy_annual), "\n")
cat("Year range:", min(energy_annual$year), "-", max(energy_annual$year), "\n")


# Step 5: Build Europe & Russia blended trajectory ────────────────────────────
# Blend OECD & EU (R5) and Reforming Economies (R5) using fuel-specific weights
# before normalising to an index so the blend reflects absolute energy levels.

cat("\nSTEP 5: Blend OECD & EU + Reforming Economies for Europe & Russia\n")

energy_oecd_ref <- energy_annual %>%
  filter(r5_region %in% c(R5_OECD_EU, R5_REF)) %>%
  mutate(r5_code = if_else(r5_region == R5_OECD_EU, "OECD_EU", "REF")) %>%
  dplyr::select(-r5_region) |>
  pivot_wider(names_from = r5_code, values_from = value_ej, values_fill = 0) %>%
  left_join(EUR_BLEND_WEIGHTS, by = "fuel") %>%
  mutate(value_ej = w_oecd_eu * OECD_EU + (1 - w_oecd_eu) * REF, r5_region = "EUR") %>%
  select(ssp, r5_region, fuel, year, value_ej)

cat("  Europe & Russia blended rows:", nrow(energy_oecd_ref), "\n")

# Combine: original R5 regions (relabelled) + EUR blend
energy_r5 <- energy_annual %>%
  mutate(
    r5_region = case_when(
      r5_region == R5_ASIA ~ "Asia",
      r5_region == R5_OECD_EU ~ "OECD_EU",
      r5_region == R5_LAM ~ "LAM",
      r5_region == R5_MAF ~ "MAF",
      r5_region == R5_REF ~ "REF",
      TRUE ~ r5_region
    )
  ) %>%
  # Keep all five R5 codes plus the blended EUR series
  bind_rows(energy_oecd_ref)


# Step 6: Map R5 regions to project regions ───────────────────────────────────

cat("\nSTEP 6: Map R5 regions to project regions\n")

energy_7region <- energy_r5 %>%
  left_join(R5_REGION_MAP, by = "r5_region", relationship = "many-to-many") %>%
  filter(!is.na(region)) %>%
  select(scenario = ssp, region, fuel, year, value_ej)

cat("  7-region energy rows:", nrow(energy_7region), "\n")
cat("  Regions:", paste(sort(unique(energy_7region$region)), collapse = ", "), "\n")


# Step 7: Load SSP drivers and compute E/GDP intensity index ──────────────────

cat("\nSTEP 7: Load SSP drivers and compute E/GDP index\n")

ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)

pop_index <- ssp_drivers %>% filter(variable == "Population") %>% select(scenario, region, year, pop_index = index)

gdp_percap_index <- ssp_drivers %>%
  filter(variable == "GDP|PPP [per capita]") %>%
  select(scenario, region, year, gdp_percap_index = index)

gdp_value <- ssp_drivers %>% filter(variable == "GDP|PPP") %>% select(scenario, region, year, gdp_value = value)

cat("  Scenarios:", paste(sort(unique(ssp_drivers$scenario)), collapse = ", "), "\n")
cat(
  "  Driver rows — pop:",
  nrow(pop_index),
  "| gdp_percap:",
  nrow(gdp_percap_index),
  "| gdp_value:",
  nrow(gdp_value),
  "\n"
)

# E/GDP: fuel-level energy (EJ) over total regional GDP (billion USD), then
# indexed to 2024 = 1. Captures how each fuel's energy use scales with output.
e_gdp_raw <- energy_7region %>%
  left_join(gdp_value, by = c("scenario", "region", "year")) %>%
  mutate(e_gdp = value_ej / gdp_value)

e_gdp_base_vals <- e_gdp_raw %>% filter(year == BASE_YEAR) %>% select(scenario, region, fuel, e_gdp_base = e_gdp)

e_gdp_index <- e_gdp_raw %>%
  left_join(e_gdp_base_vals, by = c("scenario", "region", "fuel")) %>%
  mutate(e_gdp_index = if_else(!is.na(e_gdp_base) & e_gdp_base > 0, e_gdp / e_gdp_base, NA_real_)) %>%
  select(scenario, region, fuel, year, e_gdp_index)

cat("  E/GDP index rows:", nrow(e_gdp_index), "\n")

# Figure E/GDP trajectory
e_gdp_index |>
  mutate(fuel = FUEL_UNEP_MAP[fuel]) |>
  filter(year <= 2060) |>
  ggplot(aes(year, e_gdp_index)) +
  # Projections (2024–2060): colored by scenario
  geom_line(data = ~ filter(.x, year >= 2024), aes(col = scenario)) +
  # Labels only on East Asia facet
  geom_textline(
    data = ~ filter(.x, year >= 2024, region == "Sub-Saharan Africa"),
    aes(col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1, # above the line; negative = above
    text_smoothing = 30,
    show.legend = FALSE,
    linewidth = NA # suppress duplicate line (already drawn above)
  ) +
  facet_grid(fuel ~ region, scales = "free") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Year", y = "E/GDP (Base 2024 = 1)", color = "") +
  theme_pb_large() +
  theme(legend.position = c(0.05, 0.9))

# fmt: skip
ggsave("Figures/IIASA/PrimaryEnergy.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4.2, height = 8.7*1.6)

# Step 8: Extract base-year material anchor from UNEP domestic extraction ──────

cat("\nSTEP 8: Extract M_2024 from UNEP DE data\n")

unep_de <- read_csv("Parameters/materials_region_DE.csv", show_col_types = FALSE)
cat("  UNEP DE rows:", nrow(unep_de), "| year range:", min(unep_de$year), "-", max(unep_de$year), "\n")

# Map fuel labels to UNEP material_category names
fuel_unep_lookup <- tibble(fuel = names(FUEL_UNEP_MAP), material_category = unname(FUEL_UNEP_MAP))

# Use the last available year at or before BASE_YEAR as the anchor
anchor_year <- max(unep_de$year[unep_de$year <= BASE_YEAR])
cat("  Using UNEP DE anchor year:", anchor_year, "\n")

m_2024 <- unep_de %>%
  filter(year == anchor_year) %>%
  inner_join(fuel_unep_lookup, by = "material_category") %>%
  select(region = Region, fuel, M_2024_Mt = DE_Mt)

cat("  Anchor rows (region × fuel):", nrow(m_2024), "\n")
cat("  Regions in anchor:", paste(sort(unique(m_2024$region)), collapse = ", "), "\n")
unique(m_2024$fuel)

# Step 9: Kaya decomposition forecast ────────────────────────────────────────
# M = P × (GDP/P) × (E/GDP) × (M/E)
# All three driver terms are IIASA indices with base year 2024 = 1.
# M/E ratio held constant at 1 — placeholder for scenario-specific efficiency.

cat("\nSTEP 9: Kaya decomposition forecast (", FORECAST_START, "–", FORECAST_END, ")\n")

forecast <- e_gdp_index %>%
  filter(year >= FORECAST_START, year <= FORECAST_END) %>%
  left_join(pop_index, by = c("scenario", "region", "year")) %>%
  left_join(gdp_percap_index, by = c("scenario", "region", "year")) %>%
  left_join(m_2024, by = c("region", "fuel")) %>%
  mutate(
    me_ratio_index = 1, # placeholder — M/E ratio held constant
    M_Mt = M_2024_Mt * pop_index * gdp_percap_index * e_gdp_index * me_ratio_index
  ) %>%
  filter(!is.na(M_Mt)) %>%
  select(scenario, region, fuel, year, M_Mt)

cat("  Forecast rows:", nrow(forecast), "\n")
cat("  Scenarios:", paste(sort(unique(forecast$scenario)), collapse = ", "), "\n")
cat("  Regions:", paste(sort(unique(forecast$region)), collapse = ", "), "\n")
cat("  Fuels:", paste(sort(unique(forecast$fuel)), collapse = ", "), "\n")


# Step 10: Save output ────────────────────────────────────────────────────────

cat("\nSTEP 10: Save forecast\n")


write_csv(forecast, "Results/fossil_fuel_forecast.csv")
cat("  Saved: Results/fossil_fuel_forecast.csv (", nrow(forecast), "rows )\n")


# Step 11: Summary check ──────────────────────────────────────────────────────

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

cat("\n  Total global fossil fuel M (Mt) at selected years:\n")
print(
  forecast %>%
    filter(year %in% c(2024, 2030, 2040, 2050, 2060)) %>%
    group_by(scenario, fuel, year) %>%
    summarise(M_Mt = round(sum(M_Mt, na.rm = TRUE), 1), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = M_Mt) %>%
    arrange(fuel, scenario)
)

forecast |>
  mutate(fuel = FUEL_UNEP_MAP[fuel]) |>
  group_by(scenario, fuel, year) |>
  reframe(M_Gt = sum(M_Mt) / 1e3) |>
  ggplot(aes(year, M_Gt)) +
  # Projections (2024–2060): colored by scenario
  geom_line(aes(col = scenario)) +
  # Labels only on East Asia facet
  geom_textline(
    data = ~ filter(.x, fuel == "Coal"),
    aes(col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1, # above the line; negative = above
    text_smoothing = 30,
    size = 3,
    show.legend = FALSE,
    linewidth = NA # suppress duplicate line (already drawn above)
  ) +
  facet_wrap(~fuel, scales = "free") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(expand = F, clip = "off", ylim = c(0, NA)) +
  labs(x = "Year", y = "DMC [Gt]", color = "") +
  theme_pb_large() +
  theme(legend.position = "none", plot.margin = margin(5.5, 15, 5.5, 5.5))

# fmt: skip
ggsave("Figures/EnergyForecast.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


## By region ------------

forecast_plot <- forecast |>
  mutate(fuel = FUEL_UNEP_MAP[fuel]) |>
  group_by(scenario, fuel, region, year) |>
  reframe(M_Gt = sum(M_Mt) / 1e3)

# Compute region order by total volume (largest at bottom)
region_order <- forecast_plot |>
  group_by(region) |>
  summarise(total = sum(M_Gt)) |>
  arrange(desc(total)) |>
  pull(region)

forecast_plot <- forecast_plot |> mutate(region = factor(region, levels = rev(region_order)))

# Compute label positions explicitly matching ggplot stack order
label_data <- forecast_plot |>
  filter(scenario == "SSP1", fuel == "Natural Gas") |>
  filter(year == max(year)) |>
  group_by(fuel, year) |>
  arrange(desc(region)) |> # factor order = stack order
  mutate(ymax = cumsum(M_Gt), ymin = lag(ymax, default = 0), y_mid = (ymin + ymax) / 2) |>
  ungroup()

ggplot(forecast_plot, aes(year, M_Gt, fill = region)) +
  geom_area(color = NA, alpha = 0.85) +
  geom_text(
    data = label_data,
    aes(x = year, y = y_mid, label = region),
    hjust = 1, size = 2.5, fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(fuel ~ scenario, scales = "free") +
  scale_fill_manual(values = PALETTE_REGIONS) +
  scale_color_manual(values = PALETTE_REGIONS) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, NA)) +
  labs(x = "Year", y = "DMC [Gt]") +
  theme_pb_large() +
  theme(legend.position = "none", plot.margin = margin(5.5, 15, 5.5, 5.5))

# fmt: skip
ggsave("Figures/EnergyForecast_Region.png", ggplot2::last_plot(),units = "cm", dpi = 600, width = 8.7 * 3, height = 8.7*3)

# EoF
