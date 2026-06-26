## =============================================================================
## 03_UNEP_stock_flow_model.R
## Dynamic Stock-Flow Model (DSM): reconstruct in-use stock 1970-2024 from
## UNEP inflows using Weibull lifetime distributions, calibrate to MISO2
## stocks at 2016, and produce the 2024 age-structured stock.
##
## Input:
##   Parameters/Intermediate/UNEP_flows_subenduse.parquet  -- from Script 01b (8 sub-uses)
##   Parameters/materials_region_DMC.csv                   -- UNEP raw DMC for Fe/NonFe split
##   Parameters/MISO/MISO_stock_regional.csv               -- calibration anchor (super-cat level)
##   Parameters/MISO/metal_grade_ore.csv                   -- ore→metal conversion factor g
##
## Outputs:
##   Parameters/Intermediate/stock_trajectory_subenduse.parquet  -- stock by sub-use + Fe/NonFe
##   Parameters/stock_2024_age_profile.csv   -- cohort-level stock at 2024
##   Parameters/stock_2024_total.csv         -- calibrated total stock at 2024
##   Figures/Stocks/stock_trajectory_1970_2024.png
##   Figures/Stocks/stock_trajectory_endUSE_1970_2024.png
##   Figures/Stocks/age_profile_2024.png
##   Figures/Stocks/outflow_trajectory.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# -- Lifetime parameters per sub-end-use (MISO mean, Weibull k) ----------------
# Super-categories from script 01 are disaggregated into 8 sub-uses via script 01b.
# Metal ores are further split into Metal_Fe / Metal_NonFe in Step 1 below.
lifetime_params <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes") |>
  dplyr::select(sub_use, super_category, mean_life, weibull_k)


# Sub-end-use display labels (used in plots) --------
SUBENDUSE_LABELS <- c(
  residential = "Residential",
  non_residential = "Non-residential",
  roads = "Roads",
  civil_engineering = "Civil engineering",
  machinery_group = "Machinery & equipment",
  vehicles_group = "Vehicles",
  durables = "Durables",
  packaging = "Packaging"
)

# Calibration tolerance for diagnostic flags (does not stop execution)
CAL_TOLERANCE <- 0.3


# -- Survival functions -------------------------------------------------------

# Weibull scale derived from mean: mean = lambda * Gamma(1 + 1/k)
weibull_survival <- function(age, mean_life, k) {
  lambda <- mean_life / gamma(1 + 1 / k)
  exp(-(age / lambda)^k)
}

get_survival <- function(ages, mean_life, k) {
  ifelse(ages < 0, 0, weibull_survival(pmax(ages, 0), mean_life, k))
}


# -- DSM function: trajectory 1970-2024 for one (Region x material x end_use) -
#
# df            -- tibble: year (sorted), flow_Mt (calibrated inflows)
# ghost_cohorts -- tibble: cohort_year (5-yr bins 1900-1970), cohort_inflow_Mt
#                  (incremental MISO2 stock additions per bin); age-structured
#                  pre-1970 legacy stock replacing the earlier single-lump scalar
# mean_life, k  -- Weibull parameters
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
    dplyr::select(cohort_year, surviving_stock_Mt, cohort_age, is_ghost)

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

# Sub-end-use UNEP flows from script 01b (still in ore mass for Metal ores)
unep_sub <- read.csv("Parameters/Intermediate/UNEP_flows_subenduse.csv")
cat(
  "UNEP sub-end-use flows:",
  nrow(unep_sub),
  "rows | years:",
  min(unep_sub$year),
  "-",
  max(unep_sub$year),
  "| materials:",
  paste(sort(unique(unep_sub$material)), collapse = ", "),
  "\n"
)

miso_stock <- read_csv("Parameters/MISO/MISO_stock_regional.csv", show_col_types = FALSE)
cat("MISO stock:", nrow(miso_stock), "rows |", "years:", min(miso_stock$year), "-", max(miso_stock$year), "\n")

# -- Fe/NonFe split and ore-to-metal conversion (Metal ores only) --------------
# UNEP DMC reports metals in ore mass; stocks are in metal mass.
# Multiply by ore grade g (from script 02) to convert ore → metal.
# Secondary flows are already in metal mass (no g correction needed).

unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

# Compute regional fe_share = Ferrous / (Ferrous + Non-ferrous) by year
fe_share_rt <- unep_raw %>%
  filter(material_category %in% c("Ferrous ores", "Non-ferrous ores")) %>%
  group_by(Region, year, material_category) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = material_category, values_from = DMC_Mt, values_fill = 0) %>%
  rename(fe_Mt = `Ferrous ores`, nonfe_Mt = `Non-ferrous ores`) %>%
  mutate(
    fe_share = case_when(
      fe_Mt < 0 & nonfe_Mt >= 0 ~ 0,
      nonfe_Mt < 0 & fe_Mt >= 0 ~ 1,
      fe_Mt + nonfe_Mt > 0 ~ fe_Mt / (fe_Mt + nonfe_Mt),
      TRUE ~ 0.9
    )
  )

# Ore grade g: year × group (Ferrous / Non-ferrous) → g value
grade_raw <- read_csv("Parameters/MISO/metal_grade_ore.csv", show_col_types = FALSE)

grade_wide <- grade_raw %>%
  pivot_wider(names_from = group, values_from = g) %>%
  rename(g_Fe = Ferrous, g_NonFe = `Non-ferrous`)

# Extend g backward and forward via LOCF to cover all UNEP years
yr_min <- min(unep_sub$year)
yr_max <- max(unep_sub$year)
grade_early <- grade_wide %>% filter(year == min(year)) %>% dplyr::select(-year)
grade_late <- grade_wide %>% filter(year == max(year)) %>% dplyr::select(-year)
if (min(grade_wide$year) > yr_min) {
  grade_wide <- bind_rows(expand_grid(grade_early, year = seq(yr_min, min(grade_wide$year) - 1)), grade_wide)
}
if (max(grade_wide$year) < yr_max) {
  grade_wide <- bind_rows(grade_wide, expand_grid(grade_late, year = seq(max(grade_wide$year) + 1, yr_max)))
}

# Split Metal ores rows into Metal_Fe and Metal_NonFe; apply g
metal_ore_flows <- unep_sub %>%
  filter(material == "Metal ores") %>%
  left_join(fe_share_rt %>% dplyr::select(Region, year, fe_share), by = c("Region", "year")) %>%
  mutate(fe_share = replace_na(fe_share, 0.9)) %>%
  left_join(grade_wide, by = "year")

metal_fe <- metal_ore_flows %>%
  mutate(material = "Metal_Fe", flow_Mt = flow_Mt * fe_share * g_Fe) %>%
  dplyr::select(-fe_share, -g_Fe, -g_NonFe)

metal_nonfe <- metal_ore_flows %>%
  mutate(material = "Metal_NonFe", flow_Mt = flow_Mt * (1 - fe_share) * g_NonFe) %>%
  dplyr::select(-fe_share, -g_Fe, -g_NonFe)

# Combine: metal mass flows for Fe/NonFe + unchanged non-metallic flows
unep_all <- bind_rows(metal_fe, metal_nonfe, unep_sub %>% filter(material != "Metal ores"))
unique(unep_all$material)
cat(sprintf(
  "  After Fe/NonFe split + g correction: %d rows | materials: %s\n",
  nrow(unep_all),
  paste(sort(unique(unep_all$material)), collapse = ", ")
))


# Step 2: Build pre-1970 ghost cohort table from MISO2 stock series -----------

cat("\nSTEP 2: Build pre-1970 ghost cohort table from MISO2 stock series (5-year bins)\n")

# Ghost cohorts built at super_category level (MISO has no sub-use detail),
# then scaled to sub-use level using the 1970 inflow shares from script 01b.

# Super-category ghost cohorts from MISO (Material ores in metal mass already)
ghost_super <- miso_stock %>%
  filter(year >= 1900, year <= 1970, year %% 5 == 0) %>%
  arrange(Region, material, end_use, year) %>%
  group_by(Region, material, end_use) %>%
  mutate(cohort_inflow_Mt = pmax(value_Mt - lag(value_Mt, default = 0), 0)) %>%
  ungroup() %>%
  rename(cohort_year = year, super_category = end_use) %>%
  dplyr::select(Region, material, super_category, cohort_year, cohort_inflow_Mt)

# 1970 inflow shares from 01b (global → same for all regions; take first region as proxy)
shares_1970_sub <- unep_sub %>%
  filter(year == 1970) %>%
  group_by(material, super_category, sub_use) %>%
  summarise(inflow_share = mean(inflow_share, na.rm = TRUE), .groups = "drop")

# 1970 regional fe_share for metal ghost cohort splitting
fe_share_1970 <- fe_share_rt %>% filter(year == 1970) %>% dplyr::select(Region, fe_share_1970 = fe_share)

# Scale super-category ghost cohorts to sub-use level using 1970 shares
ghost_cohorts_nested <- ghost_super %>%
  left_join(shares_1970_sub, by = c("material", "super_category"), relationship = "many-to-many") %>%
  mutate(cohort_inflow_Mt = cohort_inflow_Mt * inflow_share) %>%
  dplyr::select(Region, material, super_category, sub_use, cohort_year, cohort_inflow_Mt) %>%
  # Split Metal ores into Metal_Fe / Metal_NonFe using 1970 fe_share
  {
    metal_gc <- filter(., material == "Metal ores") %>%
      left_join(fe_share_1970, by = "Region") %>%
      mutate(fe_share_1970 = replace_na(fe_share_1970, 0.9))
    nonmetal_gc <- filter(., material != "Metal ores")
    bind_rows(
      mutate(metal_gc, material = "Metal_Fe", cohort_inflow_Mt = cohort_inflow_Mt * fe_share_1970) %>%
        dplyr::select(-fe_share_1970),
      mutate(metal_gc, material = "Metal_NonFe", cohort_inflow_Mt = cohort_inflow_Mt * (1 - fe_share_1970)) %>%
        dplyr::select(-fe_share_1970),
      nonmetal_gc
    )
  } %>%
  group_by(Region, material, super_category, sub_use) %>%
  nest(ghost_cohorts = c(cohort_year, cohort_inflow_Mt)) %>%
  ungroup()

cat("  Ghost cohort groups:", nrow(ghost_cohorts_nested), "| bins per group: 1900, 1905, ..., 1970\n")


# Step 3: Build simulation input -- UNEP flows + lifetime params --------------

cat("\nSTEP 3: Build simulation input data frame\n")

# Ensure coverage to 2024 (01b parquet should already extend via LOCF; warn if not)
if (max(unep_all$year) < 2024) {
  warning(paste("UNEP sub-use flows only go to", max(unep_all$year), "-- extending by LOCF to 2024."))
  last_yr_data <- unep_all %>% filter(year == max(year)) %>% dplyr::select(-year)
  fill_years <- seq(max(unep_all$year) + 1, 2024)
  unep_all <- bind_rows(
    unep_all,
    expand_grid(last_yr_data, year = fill_years) %>%
      dplyr::select(year, Region, material, super_category, sub_use, mean_life, inflow_share, flow_Mt)
  )
}

sim_input <- unep_all %>%
  filter(year >= 1970, year <= 2024) %>%
  dplyr::select(-mean_life, -inflow_share) %>% # drop 01b lognormal mean_life; Weibull params from lifetime_params
  left_join(
    lifetime_params %>% dplyr::select(sub_use, super_category, mean_life, weibull_k),
    by = c("sub_use", "super_category")
  )

cat("  Simulation input:", nrow(sim_input), "rows\n")
cat("  Groups (Region x material x sub_use):", n_distinct(sim_input %>% dplyr::select(Region, material, sub_use)), "\n")


# Step 4: Nest and run DSM for each group ------------------------------------

cat("\nSTEP 4: Run DSM for each Region x material x super_category x sub_use group\n")

dsm_nested <- sim_input %>%
  group_by(Region, material, super_category, sub_use, mean_life, weibull_k) %>%
  nest() %>%
  ungroup() %>%
  left_join(ghost_cohorts_nested, by = c("Region", "material", "super_category", "sub_use")) %>%
  mutate(
    ghost_cohorts = purrr::map(ghost_cohorts, function(gc) {
      if (is.null(gc)) tibble(cohort_year = integer(), cohort_inflow_Mt = double()) else gc
    })
  )

# purrr::pmap runs run_dsm_trajectory for each row of the nested data frame
dsm_nested <- dsm_nested %>%
  mutate(sim = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k), run_dsm_trajectory))

dsm_results <- dsm_nested %>% dplyr::select(-data, -ghost_cohorts) %>% unnest(sim)

cat("  DSM results:", nrow(dsm_results), "rows\n")


# Step 5: Calibrate to MISO2 stock at 2016 -----------------------------------
#
# Calibration is at the (Region, material, super_category) level because MISO
# only tracks the 4 original end-use groups.  λ is then inherited by all
# sub-uses within each super-category group.  For metals, Metal_Fe and
# Metal_NonFe share the λ computed against MISO "Metal ores" total.

cat("\nSTEP 5: Calibrate simulated stocks to MISO2 at 2016\n")

# MISO reference: super_category level (rename end_use → super_category)
miso_2016 <- miso_stock %>%
  filter(year == 2016) %>%
  rename(super_category = end_use) %>%
  dplyr::select(Region, material, super_category, miso_stock_2016_Mt = value_Mt)

# Aggregate DSM to (Region, material_cal, super_category) level for comparison
# Metal_Fe and Metal_NonFe combined back to "Metal ores" for calibration
dsm_2016_super <- dsm_results %>%
  filter(year == 2016) %>%
  mutate(material_cal = recode(material, Metal_Fe = "Metal ores", Metal_NonFe = "Metal ores")) %>%
  group_by(Region, material_cal, super_category) %>%
  summarise(sim_stock_2016_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  rename(material = material_cal)

calibration_scalars <- dsm_2016_super %>%
  left_join(miso_2016, by = c("Region", "material", "super_category")) %>%
  mutate(
    lambda_cal = case_when(
      is.na(miso_stock_2016_Mt) ~ 1.0,
      sim_stock_2016_Mt <= 0 ~ 1.0,
      TRUE ~ miso_stock_2016_Mt / sim_stock_2016_Mt
    ),
    flag_large_adj = abs(lambda_cal - 1) > CAL_TOLERANCE
  )

flagged_cal <- calibration_scalars %>% filter(flag_large_adj)
if (nrow(flagged_cal) > 0) {
  cat(
    "[DIAGNOSTIC] lambda_cal outside [0.7, 1.3] for",
    nrow(flagged_cal),
    "groups (expected -- see Script 01 divergence check):\n"
  )
  print(
    flagged_cal %>% dplyr::select(Region, material, super_category, lambda_cal) %>% arrange(desc(abs(lambda_cal - 1))),
    n = 20
  )
}
cat("  Calibration scalar summary (lambda_cal):\n")
print(summary(calibration_scalars$lambda_cal))

# Apply λ: join back by super_category (Metal_Fe and Metal_NonFe get the Metal ores λ)
lambda_join <- calibration_scalars %>%
  dplyr::select(Region, material, super_category, lambda_cal) %>%
  # duplicate Metal ores λ for both material_detail codes
  bind_rows(
    filter(., material == "Metal ores") %>% mutate(material = "Metal_Fe"),
    filter(., material == "Metal ores") %>% mutate(material = "Metal_NonFe")
  ) %>%
  filter(material != "Metal ores") # remove the original "Metal ores" row (not in dsm_results)

dsm_calibrated <- dsm_results %>%
  left_join(lambda_join, by = c("Region", "material", "super_category")) %>%
  mutate(
    stock_Mt = stock_Mt * lambda_cal,
    outflow_Mt = outflow_Mt * lambda_cal,
    ghost_stock_Mt = ghost_stock_Mt * lambda_cal
  )

write.csv(
  dsm_calibrated %>% dplyr::select(Region, material, super_category, sub_use, year, stock_Mt),
  "Parameters/Intermediate/stock_trajectory_subenduse.csv",
  row.names = F
)
cat("  Year range:", range(dsm_calibrated$year), "\n")

# Step 6: Compute 2024 age profile -------------------------------------------

cat("\nSTEP 6: Compute 2024 age-structured stock\n")

age_profile_nested <- dsm_nested %>%
  left_join(lambda_join, by = c("Region", "material", "super_category")) %>%
  mutate(lambda_cal = replace_na(lambda_cal, 1.0))

age_profile_list <- age_profile_nested %>%
  mutate(
    profile = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k, lambda_cal), compute_age_profile_2024)
  ) %>%
  dplyr::select(Region, material, super_category, sub_use, profile) %>%
  unnest(profile)

cat("  Age profile rows:", nrow(age_profile_list), "\n")

write_csv(age_profile_list, "Parameters/stock_2024_age_profile.csv")
cat("  Saved: Parameters/stock_2024_age_profile.csv\n")

stock_2024_total <- dsm_calibrated %>%
  filter(year == 2024) %>%
  dplyr::select(Region, material, super_category, sub_use, stock_Mt)

write_csv(stock_2024_total, "Parameters/stock_2024_total.csv")
cat("  Saved: Parameters/stock_2024_total.csv\n")


# Step 7: Validation plots ---------------------------------------------------

cat("\nSTEP 7: Save validation plots to Figures/Stocks/\n")


# -- Plot 1: Simulated vs MISO2 stock trajectory ------------------------------

miso_traj <- miso_stock %>% rename(stock_Mt = value_Mt)


library(geomtextpath)
p_traj <- dsm_calibrated %>%
  # NEED TO remove Fe and NonFe for stock comparison
  mutate(material = if_else(str_detect(material, "_Fe|NonFe"), "Metal ores", material)) |>
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

SUPER_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)

# For trajectory comparison plot, aggregate DSM to super_category for MISO comparison
miso_traj_super <- miso_traj %>% rename(super_category = end_use)

p_traj2 <- dsm_calibrated %>%
  # NEED TO remove Fe and NonFe for stock comparison
  mutate(material = if_else(str_detect(material, "_Fe|NonFe"), "Metal ores", material)) |>
  group_by(Region, material, year, super_category) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(super_label = SUPER_LABELS[super_category]) |>
  ggplot(aes(x = year, y = stock_Mt / 1e3, colour = Region)) +
  geom_line(linewidth = 0.4) +
  geom_point(
    data = miso_traj_super %>%
      group_by(Region, material, year, super_category) %>%
      summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
      mutate(super_label = SUPER_LABELS[super_category]),
    aes(x = year, y = stock_Mt / 1e3),
    shape = 1, size = 0.5
  ) +
  geom_textline(
    aes(label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.95,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(super_label ~ material, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Year", y = "In-use stock (Gt)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_traj2

# fmt: skip
ggsave("Figures/Stocks/stock_trajectory_endUSE_1970_2024.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*4)


# -- Plot 2: Annual outflow trajectory ----------------------------------------

p_out <- dsm_calibrated %>%
  filter(!is.na(outflow_Mt)) %>%
  group_by(material, sub_use, year) %>%
  summarise(outflow_Mt = sum(outflow_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    sub_use_label = SUBENDUSE_LABELS[sub_use],
    material_label = recode(material, Metal_Fe = "Metal (Fe)", Metal_NonFe = "Metal (non-Fe)")
  ) %>%
  ggplot(aes(x = year, y = outflow_Mt, colour = sub_use_label, label = sub_use_label)) +
  geom_line(linewidth = 0.7) +
  geom_textline(
    linewidth = 0,
    size = 2.5,
    fontface = "bold",
    hjust = 0.85,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(~material_label, scales = "free_y") +
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

cat("\n-- 2024 stock by sub-end-use (global total, Gt) ----------------------------\n")
stock_2024_total %>%
  group_by(material, super_category, sub_use) %>%
  summarise(stock_Gt = round(sum(stock_Mt, na.rm = TRUE) / 1e3, 3), .groups = "drop") %>%
  arrange(material, super_category, sub_use) %>%
  print(n = 40)

cat("\n-- Comparison: 2024 DSM stock vs MISO2 2016 stock (reference anchor) ------\n")

anchor <- bind_rows(
  stock_2024_total %>%
    mutate(material_cal = recode(material, Metal_Fe = "Metal ores", Metal_NonFe = "Metal ores")) %>%
    group_by(material_cal) %>%
    summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
    rename(material = material_cal) %>%
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
