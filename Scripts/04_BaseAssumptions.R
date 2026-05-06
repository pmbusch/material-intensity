## =============================================================================
## 04_BaseAssumptions.R
## Shared constants for all Script 04 forecast modules.
## Sourced at the top of each 04*.R script after 00-Libraries.R.
##
## Individual scripts may override FORECAST_START or other values after sourcing
## (e.g., 04c/04d set FORECAST_START <- 2025L for the forward DSM).
## =============================================================================

BASE_YEAR <- 2024L
TARGET_YEAR <- 2050L # intensity convergence year
FORECAST_START <- 2024L # override in DSM scripts: 2025L
FORECAST_END <- 2060L
SNAPSHOT_YEARS <- c(2030L, 2040L, 2050L, 2060L)

SSP_COLS <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

ASSUMPTIONS_FILE <- "Inputs/MatIntensity_Assumptions.xlsx"
FIXED_INTENSITY_ENDUSES <- c("machinery", "short_lived")

# Lifetime parameters shared by all DSM-based forecast scripts
# (identical to Script 02b; do not modify here without updating that script too)
lifetime_params <- tribble(
  ~end_use               , ~mean_life , ~weibull_k ,
  "buildings"            ,         70 , 2.5        ,
  "civil_infrastructure" ,         50 , 2.5        ,
  "machinery"            ,         20 , 2.5        ,
  "short_lived"          ,          4 , 1.5
)


ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)
