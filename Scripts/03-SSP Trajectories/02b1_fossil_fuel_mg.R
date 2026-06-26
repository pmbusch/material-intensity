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

TARGET_YEAR
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
assumptions_template |>
  mutate(
    SSP1 = round(SSP1, 2),
    SSP2 = round(SSP2, 2),
    SSP3 = round(SSP3, 2),
    SSP4 = round(SSP4, 2),
    SSP5 = round(SSP5, 2)
  )

.Last.value %>% write.table('clipboard-16384', sep = '\t', row.names = FALSE)

# EoF
