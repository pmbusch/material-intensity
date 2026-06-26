## =============================================================================
## model_parameters.R
## Deterministic model configuration: temporal bounds, SSP labels, material
## classification, sub-factors, and lifetime anchors.
## Sourced by 04_BaseAssumptions.R (and transitively by mc_parameters.R).
##
## HARD RULE: no magic numbers live anywhere else in logic code.
## MC-specific parameters live in mc_parameters.R, which sources this file.
## =============================================================================

# -- Temporal ------------------------------------------------------------------
BASE_YEAR <- 2024L
FORECAST_START <- 2024L # 04c/04d override to 2025L after sourcing
FORECAST_END <- 2060L
TARGET_YEAR <- 2050L # intensity convergence: linear BASE_YEAR→TARGET_YEAR, flat after
SNAPSHOT_YEARS <- c(2030L, 2040L, 2050L, 2060L)

# -- SSP -----------------------------------------------------------------------
SSP_LABELS <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
SSP_COLS <- SSP_LABELS # alias used in forecast scripts

# -- Input file paths ----------------------------------------------------------
ASSUMPTIONS_FILE <- "Inputs/MatIntensity_Assumptions.xlsx"
RECYCLING_FILE <- "Inputs/Recycling_Assumptions.xlsx"

# -- Material / end-use classification -----------------------------------------
FIXED_INTENSITY_ENDUSES <- c("machinery", "short_lived")

ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)

BIOMASS_CATEGORIES <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood", "Other biomass")

# -- Fossil fuel: SSP marker models --------------------------------------------
SSP_MARKERS <- c(
  "SSP1" = "IMAGE",
  "SSP2" = "MESSAGE-GLOBIOM",
  "SSP3" = "AIM/CGE",
  "SSP4" = "GCAM4",
  "SSP5" = "REMIND-MAGPIE"
)


# -- DSM numerical thresholds --------------------------------------------------
DSM_STOCK_MIN <- 1e-9 # minimum stock_Mt to carry a cohort forward
DSM_SURV_MIN <- 0.001 # survival ratio; below this cohort contribution is zeroed


# -- Downcycling construction parameters ------------------------------------
SUB_FACTOR_RECYCLING_SAME <- 0.7 # recycled concrete efficiency vs virgin (B→B)
SUB_FACTOR_DOWNCYCLING_ROADS <- 0.9 # recycled aggregate efficiency vs virgin (→ roads)
MAX_SECONDARY_ROADS <- 0.7 # max share of road demand met by secondary material


# -- Lifetime parameters (deterministic anchor; also used by Script 02b) -------
lifetime_params <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes") |>
  dplyr::select(sub_use, super_category, mean_life, weibull_k)
