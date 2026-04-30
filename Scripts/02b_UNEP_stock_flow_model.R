## =============================================================================
## 03_UNEP_stock_flow_model.R
## Dynamic Stock-Flow Model (DSM): reconstruct in-use stock 1970-2024 from
## UNEP inflows using Weibull lifetime distributions, calibrate to MISO2
## stocks at 2016, and produce the 2024 age-structured stock.
##
## Input:
##   Parameters/Intermediate/UNEP_flows_enduse.csv  -- from Script 02
##   Parameters/MISO/MISO_stock_regional.csv        -- from Script 01
##
## Outputs:
##   Parameters/stock_2024_age_profile.csv   -- cohort-level stock at 2024
##   Parameters/stock_2024_total.csv         -- calibrated total stock at 2024
##   Figures/Stocks/stock_trajectory_1970_2024.png
##   Figures/Stocks/stock_trajectory_endUSE_1970_2024.png
##   Figures/Stocks/age_profile_2024.png
##   Figures/Stocks/outflow_trajectory.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# -- Lifetime parameters (defined inline -- do not read from file) -------------
# "machinery_vehicles" renamed to "machinery" to match the project's end_use
# naming convention (global convention: buildings, civil_infrastructure,
# machinery, short_lived).
lifetime_params <- tribble(
  ~end_use               , ~mean_life , ~weibull_k ,
  "buildings"            ,         70 , 2.5        ,
  "civil_infrastructure" ,         50 , 2.5        ,
  "machinery"            ,         20 , 2.5        ,
  "short_lived"          , NA         , NA # uses fixed 4-year lag instead
)

# Calibration tolerance for diagnostic flags (does not stop execution)
CAL_TOLERANCE <- 0.3


# -- Survival functions -------------------------------------------------------

# Weibull scale derived from mean: mean = lambda * Gamma(1 + 1/k)
weibull_survival <- function(age, mean_life, k) {
  lambda <- mean_life / gamma(1 + 1 / k)
  exp(-(age / lambda)^k)
}

# Dispatcher: short_lived uses a fixed 4-year lag (all mass exits after 4 years)
get_survival <- function(ages, mean_life, k) {
  if (is.na(mean_life)) {
    as.numeric(ages >= 0 & ages <= 4)
  } else {
    ifelse(ages < 0, 0, weibull_survival(pmax(ages, 0), mean_life, k))
  }
}


# -- DSM function: trajectory 1970-2024 for one (Region x material x end_use) -
#
# df            -- tibble: year (sorted), flow_Mt (calibrated inflows)
# ghost_cohorts -- tibble: cohort_year (5-yr bins 1900-1970), cohort_inflow_Mt
#                  (incremental MISO2 stock additions per bin); age-structured
#                  pre-1970 legacy stock replacing the earlier single-lump scalar
# mean_life, k  -- Weibull parameters (NA -> short_lived fixed lag)
#
# Returns tibble: year, stock_Mt, outflow_Mt, ghost_stock_Mt

run_dsm_trajectory <- function(df, ghost_cohorts, mean_life, k) {
  df <- df %>% arrange(year)
  years <- df$year
  n <- length(years)
  inflows <- df$flow_Mt

  # age_mat[i, j] = age of cohort i observed at simulation year j (years[j] - years[i])
  age_mat <- outer(years, years, function(cohort, sim) sim - cohort)
  surv_mat <- matrix(get_survival(as.vector(age_mat), mean_life, k), n, n)
  surv_mat[age_mat < 0] <- 0 # cohort i cannot contribute before year years[i]

  # Stock from each cohort: row i x inflows[i], column = simulation year
  cohort_stock_mat <- sweep(surv_mat, 1, inflows, "*")
  cohort_stock <- colSums(cohort_stock_mat)

  # Ghost cohorts: sum survival-weighted contributions from each pre-1970 cohort bin
  ghost_stock <- vapply(
    years,
    function(sim_yr) {
      sum(ghost_cohorts$cohort_inflow_Mt * get_survival(sim_yr - ghost_cohorts$cohort_year, mean_life, k))
    },
    numeric(1)
  )

  total_stock <- cohort_stock + ghost_stock

  # Mass balance: outflow(t) = stock(t-1) - stock(t) + inflow(t)
  # undefined at t=1970 because stock(1969) is unknown
  outflow <- c(NA_real_, total_stock[-n] - total_stock[-1] + inflows[-1])

  tibble(year = years, stock_Mt = total_stock, outflow_Mt = outflow, ghost_stock_Mt = ghost_stock)
}


# -- Age profile at 2024: one row per cohort for a single group ---------------

compute_age_profile_2024 <- function(df, ghost_cohorts, mean_life, k, lambda_cal) {
  df <- df %>% arrange(year)
  years <- df$year
  inflows <- df$flow_Mt * lambda_cal # scale to calibrated inflows

  ages_2024 <- 2024 - years
  surv_2024 <- get_survival(ages_2024, mean_life, k)
  cohort_stock_2024 <- inflows * surv_2024

  ghost_rows <- ghost_cohorts %>%
    mutate(
      surviving_stock_Mt = cohort_inflow_Mt * lambda_cal * get_survival(2024 - cohort_year, mean_life, k),
      cohort_age = as.integer(2024 - cohort_year),
      is_ghost = TRUE
    ) %>%
    select(cohort_year, surviving_stock_Mt, cohort_age, is_ghost)

  bind_rows(
    tibble(
      cohort_year = years,
      surviving_stock_Mt = cohort_stock_2024,
      cohort_age = as.integer(ages_2024),
      is_ghost = FALSE
    ),
    ghost_rows
  )
}


# Step 1: Load inputs ---------------------------------------------------------

cat("STEP 1: Load inputs\n")

unep_enduse <- read_csv("Parameters/Intermediate/UNEP_flows_enduse.csv", show_col_types = FALSE)
cat(
  "UNEP flows by end-use:",
  nrow(unep_enduse),
  "rows |",
  "years:",
  min(unep_enduse$year),
  "-",
  max(unep_enduse$year),
  "\n"
)

miso_stock <- read_csv("Parameters/MISO/MISO_stock_regional.csv", show_col_types = FALSE)
cat("MISO stock:", nrow(miso_stock), "rows |", "years:", min(miso_stock$year), "-", max(miso_stock$year), "\n")


# Step 2: Build pre-1970 ghost cohort table from MISO2 stock series -----------

cat("\nSTEP 2: Build pre-1970 ghost cohort table from MISO2 stock series (5-year bins)\n")

ghost_cohorts_nested <- miso_stock %>%
  filter(year >= 1900, year <= 1970, year %% 5 == 0) %>%
  arrange(Region, material, end_use, year) %>%
  group_by(Region, material, end_use) %>%
  mutate(cohort_inflow_Mt = pmax(value_Mt - lag(value_Mt, default = 0), 0)) %>%
  ungroup() %>%
  rename(cohort_year = year) %>%
  select(Region, material, end_use, cohort_year, cohort_inflow_Mt) %>%
  group_by(Region, material, end_use) %>%
  nest(ghost_cohorts = c(cohort_year, cohort_inflow_Mt)) %>%
  ungroup()

cat("  Ghost cohort groups:", nrow(ghost_cohorts_nested), "| bins per group: 1900, 1905, ..., 1970\n")


# Step 3: Build simulation input -- UNEP flows + lifetime params --------------

cat("\nSTEP 3: Build simulation input data frame\n")

# Ensure UNEP covers at least up to 2024; if not, warn
if (max(unep_enduse$year) < 2024) {
  warning(paste("UNEP flows only go to", max(unep_enduse$year), "-- extending last year by LOCF to 2024."))
  last_year_data <- unep_enduse %>% filter(year == max(year)) %>% select(-year)
  fill_years <- seq(max(unep_enduse$year) + 1, 2024)
  unep_enduse <- bind_rows(
    unep_enduse,
    expand_grid(last_year_data, year = fill_years) %>% select(Region, material, end_use, year, flow_Mt)
  )
}

sim_input <- unep_enduse %>% filter(year >= 1970, year <= 2024) %>% left_join(lifetime_params, by = "end_use")

cat("  Simulation input:", nrow(sim_input), "rows\n")
cat("  Groups (Region x material x end_use):", n_distinct(sim_input %>% select(Region, material, end_use)), "\n")


# Step 4: Nest and run DSM for each group ------------------------------------

cat("\nSTEP 4: Run DSM for each Region x material x end_use group\n")

dsm_nested <- sim_input %>%
  group_by(Region, material, end_use, mean_life, weibull_k) %>%
  nest() %>%
  ungroup() %>%
  left_join(ghost_cohorts_nested, by = c("Region", "material", "end_use")) %>%
  mutate(
    ghost_cohorts = purrr::map(ghost_cohorts, function(gc) {
      if (is.null(gc)) tibble(cohort_year = integer(), cohort_inflow_Mt = double()) else gc
    })
  )

# purrr::pmap runs run_dsm_trajectory for each row of the nested data frame
dsm_nested <- dsm_nested %>%
  mutate(sim = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k), run_dsm_trajectory))

dsm_results <- dsm_nested %>% select(-data, -ghost_cohorts) %>% unnest(sim)

cat("  DSM results:", nrow(dsm_results), "rows\n")


# Step 5: Calibrate to MISO2 stock at 2016 -----------------------------------

cat("\nSTEP 5: Calibrate simulated stocks to MISO2 at 2016\n")

miso_2016 <- miso_stock %>% filter(year == 2016) %>% select(Region, material, end_use, miso_stock_2016_Mt = value_Mt)

calibration_scalars <- dsm_results %>%
  filter(year == 2016) %>%
  select(Region, material, end_use, sim_stock_2016_Mt = stock_Mt) %>%
  left_join(miso_2016, by = c("Region", "material", "end_use")) %>%
  mutate(
    lambda_cal = case_when(
      is.na(miso_stock_2016_Mt) ~ 1.0, # no MISO reference -> no adjustment
      sim_stock_2016_Mt <= 0 ~ 1.0, # zero simulated stock -> no adjustment
      TRUE ~ miso_stock_2016_Mt / sim_stock_2016_Mt
    ),
    flag_large_adj = abs(lambda_cal - 1) > CAL_TOLERANCE
  )

# Diagnostic -- large adjustments indicate UNEP/MISO divergence (not an error)
flagged_cal <- calibration_scalars %>% filter(flag_large_adj)
if (nrow(flagged_cal) > 0) {
  cat(
    "[DIAGNOSTIC] lambda_cal outside [0.7, 1.3] for",
    nrow(flagged_cal),
    "groups (expected -- see Script 02 divergence check):\n"
  )
  print(flagged_cal %>% select(Region, material, end_use, lambda_cal) %>% arrange(desc(abs(lambda_cal - 1))), n = 20)
}
cat("  Calibration scalar summary (lambda_cal):\n")
print(summary(calibration_scalars$lambda_cal))


# Apply calibration: rescale entire time series uniformly
dsm_calibrated <- dsm_results %>%
  left_join(
    calibration_scalars %>% select(Region, material, end_use, lambda_cal),
    by = c("Region", "material", "end_use")
  ) %>%
  mutate(
    stock_Mt = stock_Mt * lambda_cal,
    outflow_Mt = outflow_Mt * lambda_cal,
    ghost_stock_Mt = ghost_stock_Mt * lambda_cal
  )


# Step 6: Compute 2024 age profile -------------------------------------------

cat("\nSTEP 6: Compute 2024 age-structured stock\n")

# Join calibration scalars back to the nested sim input
age_profile_nested <- dsm_nested %>%
  left_join(
    calibration_scalars %>% select(Region, material, end_use, lambda_cal),
    by = c("Region", "material", "end_use")
  ) %>%
  mutate(lambda_cal = replace_na(lambda_cal, 1.0))

age_profile_list <- age_profile_nested %>%
  mutate(
    profile = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k, lambda_cal), compute_age_profile_2024)
  ) %>%
  select(Region, material, end_use, profile) %>%
  unnest(profile)

cat("  Age profile rows:", nrow(age_profile_list), "\n")

write_csv(age_profile_list, "Parameters/stock_2024_age_profile.csv")
cat("  Saved: Parameters/stock_2024_age_profile.csv\n")

# Total calibrated stock at 2024 (no age detail)
stock_2024_total <- dsm_calibrated %>% filter(year == 2024) %>% select(Region, material, end_use, stock_Mt)

write_csv(stock_2024_total, "Parameters/stock_2024_total.csv")
cat("  Saved: Parameters/stock_2024_total.csv\n")


# Step 7: Validation plots ---------------------------------------------------

cat("\nSTEP 7: Save validation plots to Figures/Stocks/\n")


# -- Plot 1: Simulated vs MISO2 stock trajectory ------------------------------

miso_traj <- miso_stock %>% rename(stock_Mt = value_Mt)


library(geomtextpath)
p_traj <- dsm_calibrated %>%
  group_by(Region, material, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = stock_Mt / 1e3, colour = Region)) +
  geom_line(linewidth = 0.4) +
  geom_point(
    data = miso_traj %>%
      group_by(Region, material, year) %>%
      summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop"),
    aes(x = year, y = stock_Mt / 1e3),
    shape = 1, size = 0.5
  ) +
  geom_textline(
    # replaces geom_line; draws label along path
    aes(label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.95,
    offset = unit(3, "pt"), # lift text off the line
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(
      material = c("Metal ores", "Non-metallic minerals"),
      year     = 1905,
      stock_Gt = c(2, 50),
      label    = "MISO2\nstock estimates"
    ),
    aes(x = year, y = stock_Gt, label = label),
    inherit.aes = FALSE,
    colour = "black", size = 2.5, hjust = 0, fontface = "bold"
  ) +
  facet_wrap(~material, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Year", y = "In-use stock (Gt)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_traj

# fmt: skip
ggsave("Figures/Stocks/stock_trajectory_1970_2024.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)

p_traj2 <- dsm_calibrated %>%
  group_by(Region, material, year, end_use) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(end_use = ENDUSE_LABELS[end_use]) |>
  ggplot(aes(x = year, y = stock_Mt / 1e3, colour = Region)) +
  geom_line(linewidth = 0.4) +
  geom_point(
    data = miso_traj %>%
      group_by(Region, material, year, end_use) %>%
      mutate(end_use = ENDUSE_LABELS[end_use]) |>
      summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop"),
    aes(x = year, y = stock_Mt / 1e3),
    shape = 1, size = 0.5
  ) +
  geom_textline(
    # replaces geom_line; draws label along path
    aes(label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.95,
    offset = unit(3, "pt"), # lift text off the line
    show.legend = FALSE
  ) +
  facet_wrap(end_use ~ material, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Year", y = "In-use stock (Gt)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_traj2

# fmt: skip
ggsave("Figures/Stocks/stock_trajectory_endUSE_1970_2024.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*3)


# -- Plot 2: 2024 age distribution by end-use and material --------------------

gap <- 7 # in Gt, adjust to taste

pyramid_data <- age_profile_list %>%
  mutate(end_use = ENDUSE_LABELS[end_use]) |>
  filter(material %in% "Non-metallic minerals", end_use %in% c("Buildings", "Civil infrastructure")) %>%
  mutate(
    cohort_bin = cut(
      cohort_year,
      breaks = c(-Inf, seq(1970, 2025, by = 5)),
      labels = c("55+", paste0(2024 - seq(1974, 2024, by = 5), "-", 2024 - seq(1970, 2020, by = 5))),
      right = TRUE
    ),
    cohort_bin = fct_rev(cohort_bin),
  ) |>
  group_by(end_use, cohort_bin, Region) %>%
  summarise(stock_Gt = sum(surviving_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  # Compute cumulative stack positions per side
  group_by(end_use, cohort_bin) %>%
  arrange(Region) %>%
  mutate(cum_stock = cumsum(stock_Gt), cum_stock_lag = lag(cum_stock, default = 0)) %>%
  ungroup() %>%
  mutate(
    xmin = if_else(end_use == "Buildings", -(cum_stock) - gap, cum_stock_lag + gap),
    xmax = if_else(end_use == "Buildings", -(cum_stock_lag) - gap, cum_stock + gap)
  )

x_lim <- pyramid_data %>%
  group_by(end_use, cohort_bin) %>%
  summarise(stack_total = sum(stock_Gt), .groups = "drop") %>%
  pull(stack_total) %>%
  max() *
  1.05 +
  gap

age_ticks_df <- pyramid_data %>% distinct(cohort_bin)

p_pyramid <- pyramid_data %>%
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = as.numeric(cohort_bin) - 0.4,
      ymax = as.numeric(cohort_bin) + 0.4,
      fill = Region
    ),
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = age_ticks_df,
    aes(x = 0, y = cohort_bin, label = cohort_bin),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20"
  ) +
  # fmt: skip
  annotate("text", x = -(x_lim / 2), y = Inf, label = "Buildings",          hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x =  (x_lim / 2), y = Inf, label = "Civil infrastructure", hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x = 0,            y = Inf, label = "Age",                 hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  scale_x_continuous(
    limits = c(-x_lim, x_lim),
    breaks = c(-(gap + c(100, 50, 0)), gap + c(0, 50, 100)),
    labels = c(100, 50, 0, 0, 50, 100)
  ) +
  scale_y_discrete() +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "2024 Non-metallic minerals Stock (Gt)", y = NULL) +
  # manual X axis
  annotate("segment", x = gap, xend = x_lim, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  # fmt: skip
  annotate("segment", x = -gap, xend = -x_lim, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  theme_pb_large() +
  theme(
    legend.position = c(0.2, 0.8),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks.y = element_blank(),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "mm")
  ) +
  guides(fill = guide_legend(ncol = 1))

p_pyramid

# fmt: skip
ggsave("Figures/Stocks/age_profile_2024.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


# -- Plot 3: Annual outflow trajectory ----------------------------------------

p_out <- dsm_calibrated %>%
  filter(!is.na(outflow_Mt)) %>%
  group_by(material, end_use, year) %>%
  summarise(outflow_Mt = sum(outflow_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(end_use = ENDUSE_LABELS[end_use]) %>%
  ggplot(aes(x = year, y = outflow_Mt, colour = end_use, label = end_use)) +
  geom_line(linewidth = 0.7) +
  geom_textline(
    linewidth = 0,
    size = 2.5,
    fontface = "bold",
    hjust = 0.85,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(~material, scales = "free_y") +
  scale_colour_manual(values = PALETTE_ENDUSE) +
  labs(title = "Annual outflows (end-of-life) 1971–2025", x = "Year", y = "Outflow (Mt/year)") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_pb_large() +
  theme(legend.position = "none")
p_out

# fmt: skip
ggsave("Figures/Stocks/outflow_trajectory.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


# Step 8: Summary ------------------------------------------------------------

cat("\nSTEP 8: Summary -- 2024 total stock by material and Region\n")

summary_2024 <- stock_2024_total %>%
  group_by(material, Region) %>%
  summarise(stock_Gt = round(sum(stock_Mt, na.rm = TRUE) / 1e3, 3), .groups = "drop")

print(summary_2024 %>% arrange(material, Region), n = 50)

cat("\n-- Comparison: 2024 DSM stock vs MISO2 2016 stock (reference anchor) ------\n")

anchor <- bind_rows(
  stock_2024_total %>%
    group_by(material) %>%
    summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
    mutate(source = "DSM 2024 (calibrated)"),
  miso_stock %>%
    filter(year == 2016) %>%
    group_by(material) %>%
    summarise(stock_Gt = sum(value_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
    mutate(source = "MISO2 2016")
)
print(anchor %>% arrange(material, source))

cat("\n-- Sanity check ------------------------------------------------------------\n")
cat(
  "  DSM output rows:",
  nrow(dsm_calibrated),
  "| Year range:",
  min(dsm_calibrated$year),
  "-",
  max(dsm_calibrated$year),
  "\n"
)
cat("  Age profile rows:", nrow(age_profile_list), "\n")
cat("  Calibration scalars:", nrow(calibration_scalars), "| Flagged:", sum(calibration_scalars$flag_large_adj), "\n")

# EoF
