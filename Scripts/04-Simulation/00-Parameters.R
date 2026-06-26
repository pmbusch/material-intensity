## =============================================================================
## 00-Parameters.R
## Monte Carlo configuration. Sources model_parameters.R for all base
## parameters; adds MC-specific run control and sampling settings.
##
## HARD RULE: no magic numbers live anywhere else in MC logic code.
## =============================================================================

source("Scripts/model_parameters.R", encoding = "UTF-8")

# -- MC run control ------------------------------------------------------------
# N_RUNS <- 10000L
N_RUNS <- 100L # DEBUG
GLOBAL_SEED <- 12062026L

# READ ALL MONTECARLO PARAMETERS BOUNDS FROM ASSUMPTIONS
p <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Parameters")
p <- p |> dplyr::filter(parameter_name != "DOWNCYCLING_BLDG_TO_ROADS")
for (i in seq_len(nrow(p))) {
  assign(paste0(p$parameter_name[i], "_MIN"), p$min[i])
  assign(paste0(p$parameter_name[i], "_MAX"), p$max[i])
}

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
