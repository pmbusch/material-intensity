## =============================================================================
## 00-Parameters.R
## Model configuration: base (deterministic) settings -- temporal bounds, input
## file paths, end-use classification, DSM thresholds, lifetime anchors -- plus MC-specific run control and sampling settings.
## Sourced by every 04-Simulation script and by the figure scripts that need
## model constants.
##
## HARD RULE: no magic numbers live anywhere else in model logic code.
## =============================================================================

# -- Temporal ------------------------------------------------------------------
# FORECAST_END (the projection horizon) is defined in Scripts/00-CommonParameters.R
# -- sourced before this file in every entry point -- so it's a single global
# knob shared by every simulation and figure, not redefined here.
BASE_YEAR <- 2024L
TARGET_YEAR <- 2050L # intensity convergence: log-linear BASE_YEAR→TARGET_YEAR, flat after
SNAPSHOT_YEARS <- seq(2030L, FORECAST_END, by = 10L)

# -- Input file paths ----------------------------------------------------------
ASSUMPTIONS_FILE <- "Inputs/MatIntensity_Assumptions.xlsx"
RECYCLING_FILE <- "Inputs/Recycling_Assumptions.xlsx"

# -- End-use labels ------------------------------------------------------------
ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)

# -- Lifetime parameters (deterministic anchor; also used by 02b) --------------
lifetime_params <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes") |>
  dplyr::select(sub_use, super_category, mean_life, weibull_k)

# -- MC run control ------------------------------------------------------------
N_RUNS <- 1000L
# N_RUNS <- 200L # DEBUG
GLOBAL_SEED <- 12062026L
# Max relative departure of any SSP's share of runs (by ssp_label) from an
# equal split before 02-RunSimulations.R warns (0.5 = +/-50%)
SSP_SHARE_TOL <- 0.5

# -- MC: global scalar parameters (min, central, max) ---------------------------
# Read by parameter_name from the Parameters sheet; each becomes NAME_MIN,
# NAME_CENTRAL, NAME_MAX. LHS column = tolower(NAME) (01-Sampling.R). Sampled
# semi-uniformly: u < 0.5 -> min..central, u >= 0.5 -> central..max.
MC_PARAM_NAMES <- c(
  "RECYC_CONVERGENCE_YR",
  "RECYCLING_RATE_FE",
  "RECYCLING_RATE_NONFE",
  "DOWNCYCLING",
  "MAX_SECONDARY_BUILD_CIVIL",
  "MAX_SECONDARY_ROADS",
  "SHARE_CONCRETE_BUILDINGS",
  "SHARE_CONCRETE_CIVIL",
  "SHARE_AGG_CONCRETE",
  "SHARE_GRANULAR_ROAD",
  "GRADE_ORE_FE",
  "GRADE_ORE_NONFE",
  "TARGET_YEAR"
)
MC_PARAMS <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Parameters") |>
  dplyr::filter(parameter_name %in% MC_PARAM_NAMES) |>
  dplyr::transmute(parameter_name, col = tolower(parameter_name), min, central = central_value, max)
stopifnot(setequal(MC_PARAMS$parameter_name, MC_PARAM_NAMES))
for (i in seq_len(nrow(MC_PARAMS))) {
  assign(paste0(MC_PARAMS$parameter_name[i], "_MIN"), MC_PARAMS$min[i])
  assign(paste0(MC_PARAMS$parameter_name[i], "_CENTRAL"), MC_PARAMS$central[i])
  assign(paste0(MC_PARAMS$parameter_name[i], "_MAX"), MC_PARAMS$max[i])
}

# -- Stock-growth seam blend ----------------------------------------------------
# Years over which target-stock growth blends linearly from the historical 2024
# rate (per region x material x sub_use) into the Kaya-driven rate:
# weight on model growth = (year - 2024) / (N + 1) -> 0.25, 0.5, 0.75, then 1.
STOCK_GROWTH_BLEND_YRS <- 3L

# Stock-intensity ramp (02/02b STEP 5): starts at the slope that continues the
# 2024 historical stock growth (historical stock log-growth minus the run's
# 2025 GDP log-growth, per region x material x sub_use) instead of a flat start;
# still reaches the sampled endpoint at target_year with zero slope (cubic
# Hermite in log space; reduces to the smoothstep when that slope is 0).

# -- MC: lifetime sampling ranges (±20% around deterministic anchor) ----------
LIFETIME_MIN <- 0.3 # MC: minimum sampled lifetime (years)
LIFETIME_SAMPLE_PARAMS <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes")

# -- Power sector (explicit generation + storage stock) ------------------------
# Fixed coefficients only: nothing here is sampled, so the LHS design and the
# sensitivity parameter list are unchanged. Capacity follows the run's own
# fossil draws (01b-PowerSector.R builds the inputs, 02-RunSimulations.R runs it).
WANG_FILE <- "Inputs/Wang2023_material_intensity_technology.xlsx"
WANG_SHEET <- "Master - all intensity values"
BATTERY_FILE <- "Inputs/Battery_intensity.xlsx"
BATTERY_SHEET <- "Battery_Intensity"
CLIMATE_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-0e7dfab0-46ee-486b-b50d-4faf3de27da4.csv"
TEMP_MEDIAN_VAR <- "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICC v7.6.0a3]"

CAPACITY_ANCHOR_YEAR <- 2025L # ScenarioMIP "today" = the model's anchor stock (DSM start)

# Fossil-fuel energy densities (MJ/kg) -> 2024 energy shares of coal/gas/oil.
# PLACEHOLDER values to confirm: IPCC (2006) default NCVs, same as Figure 3.
ENERGY_DENSITY_MJ_PER_KG <- c(coal = 25.8, gas = 48.0, oil = 42.3)
if (anyNA(ENERGY_DENSITY_MJ_PER_KG) || length(ENERGY_DENSITY_MJ_PER_KG) != 3L) {
  stop("ENERGY_DENSITY_MJ_PER_KG (00-Parameters.R) is empty -- fill in coal/gas/oil MJ/kg")
}

# Lifetime classes (fixed lifetimes; k = 2.5 as for every long-lived sub_use).
# donor_sub_use: existing sub_use with the closest central lifetime -- its 2024
# cohort shape is the power class's 2025 age profile (CEM splice from the
# donor's lifetime, as for every other stock). Hydro/nuclear: no retirement
# before FORECAST_END, modelled as a very long mean lifetime.
POWER_LIFE_CLASSES <- tibble::tribble(
  ~sub_use              , ~label                   , ~mean_life , ~weibull_k , ~donor_sub_use      ,
  "power_thermal"       , "Power: thermal"         , 40         , 2.5        , "roads"             ,
  "power_solar_wind"    , "Power: solar & wind"    , 27         , 2.5        , "machinery_group"   ,
  "power_hydro_nuclear" , "Power: hydro & nuclear" , 1e4        , 2.5        , "civil_engineering" ,
  "power_battery"       , "Power: batteries"       , 15         , 2.5        , "vehicles_group"
)

# IAMC capacity variable -> tech key, Wang (2023) technology (no CCS), class.
# Capacity|Electricity|Fossil is NOT used (= Coal + Gas + Oil).
POWER_TECHS <- tibble::tribble(
  ~variable                            , ~tech           , ~wang_tech        , ~sub_use              ,
  "Capacity|Electricity|Coal"          , "coal"          , "Coal"            , "power_thermal"       ,
  "Capacity|Electricity|Gas"           , "gas"           , "Gas"             , "power_thermal"       ,
  "Capacity|Electricity|Oil"           , "oil"           , "Gas"             , "power_thermal"       ,
  "Capacity|Electricity|Hydrogen"      , "hydrogen"      , "Gas"             , "power_thermal"       ,
  "Capacity|Electricity|Other"         , "other"         , "Gas"             , "power_thermal"       ,
  "Capacity|Electricity|Biomass"       , "biomass"       , "Biomass"         , "power_thermal"       ,
  "Capacity|Electricity|Geothermal"    , "geothermal"    , "Geothermal"      , "power_thermal"       ,
  "Capacity|Electricity|Solar|PV"      , "solar_pv"      , "CSI_PV"          , "power_solar_wind"    ,
  "Capacity|Electricity|Solar|CSP"     , "solar_csp"     , "CSP"             , "power_solar_wind"    ,
  "Capacity|Electricity|Wind|Onshore"  , "wind_onshore"  , "Onshore_AG"      , "power_solar_wind"    ,
  "Capacity|Electricity|Wind|Offshore" , "wind_offshore" , "Offshore_DD_PMG" , "power_solar_wind"    ,
  "Capacity|Electricity|Hydro"         , "hydro"         , "Hydro"           , "power_hydro_nuclear" ,
  "Capacity|Electricity|Nuclear"       , "nuclear"       , "Nuclear"         , "power_hydro_nuclear"
)

# Stationary storage: GWh = hours x coverage x (PV + onshore + offshore GW)
BATTERY_HOURS <- 4
BATTERY_COVERAGE <- 0.1 # was 0.2; IEA 2025 stock ~110 GW x 3 h = 330 GWh (utility + behind-the-meter)
BATTERY_GWH_PER_GW <- BATTERY_HOURS * BATTERY_COVERAGE # 0.4
BATTERY_SOURCE_TECHS <- c("solar_pv", "wind_onshore", "wind_offshore")
BATTERY_CHEMISTRIES <- c("LFP", "NMC 811") # simple average, kg/kWh

# Material -> model material group (metals in metal mass; ore via sampled grades)
WANG_MATERIAL_GROUP <- c(
  "Steel" = "metal_fe",
  "Al" = "metal_nonfe",
  "Cu" = "metal_nonfe",
  "Si" = "nonmetallic_minerals", # ore is quartz
  "Glass" = "nonmetallic_minerals",
  "Cement" = "nonmetallic_minerals",
  "Aggregates" = "nonmetallic_minerals" # = AGGREGATE_PER_CEMENT x cement
)
AGGREGATE_PER_CEMENT <- 7 # t aggregates per t cement (Kane 2026; no water)
BATTERY_MATERIAL_GROUP <- c(
  "Steel" = "metal_fe",
  "Stainless steel" = "metal_fe",
  "Aluminum" = "metal_nonfe",
  "Cobalt" = "metal_nonfe",
  "Copper" = "metal_nonfe",
  "Lithium" = "metal_nonfe",
  "Manganese" = "metal_nonfe",
  "Nickel" = "metal_nonfe",
  "Graphite" = "nonmetallic_minerals"
)

# Carve-out: 2025 power-sector stock is removed from this 2024 base stock
POWER_CARVEOUT_SUB_USE <- "civil_engineering"
# Ratio form (anchor x scenario GW/GDP ratio) = scenario path x calibration
# factor (anchor / scenario's own 2025 GW). Tiny scenario bases blow this up
# (e.g. 0.07 GW anchor / 3e-7 GW base), so outside [1/X, X] the scenario's own
# level is used instead -- same threshold (5) as 04_summary_ratio.R's tiny-base fix.
POWER_CALIB_MAX <- 5
# Report (not drop) 2025->2030 world capacity jumps above this ratio
POWER_JUMP_REPORT <- 1.5

# SEE DISTRIBUTION
# {lifetime <- 120
# k <- 2.5
# scale <- lifetime / gamma(1 + 1 / k)
# x <- seq(0, 3 * lifetime, length.out = 1000)
# ggplot2::ggplot(data.frame(x = x, pdf = dweibull(x, shape = k, scale = scale)), aes(x, pdf)) +
#   geom_line(linewidth = 1) +
#   labs(
#     x = "Lifetime",
#     y = "Density",
#     title = "Weibull Distribution",
#     subtitle = paste("Mean =", lifetime, ", Shape =", k)
#   ) +
#   theme_minimal()}
