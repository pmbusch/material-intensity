## =============================================================================
## 00-Parameters.R
## Model configuration: base (deterministic) settings -- temporal bounds, input
## file paths, end-use classification, DSM thresholds, downcycling constants,
## lifetime anchors -- plus MC-specific run control and sampling settings.
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

# -- Downcycling construction parameters ------------------------------------
SUB_FACTOR_RECYCLING_SAME <- 0.7 # recycled concrete efficiency vs virgin (B→B)
SUB_FACTOR_DOWNCYCLING_ROADS <- 0.9 # recycled aggregate efficiency vs virgin (→ roads)
MAX_SECONDARY_ROADS <- 0.7 # max share of road demand met by secondary material

# -- Lifetime parameters (deterministic anchor; also used by 02b) --------------
lifetime_params <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes") |>
  dplyr::select(sub_use, super_category, mean_life, weibull_k)

# -- MC run control ------------------------------------------------------------
N_RUNS <- 1000L
# N_RUNS <- 200L # DEBUG
GLOBAL_SEED <- 12062026L

# READ ALL MONTECARLO PARAMETERS BOUNDS FROM ASSUMPTIONS
p <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Parameters")
p <- p |> dplyr::filter(parameter_name != "DOWNCYCLING_BLDG_TO_ROADS")
for (i in seq_len(nrow(p))) {
  assign(paste0(p$parameter_name[i], "_MIN"), p$min[i])
  assign(paste0(p$parameter_name[i], "_MAX"), p$max[i])
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
