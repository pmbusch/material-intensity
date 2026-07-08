## =============================================================================
## 03_UNEP_stock_flow_model.R
## Dynamic Stock-Flow Model (DSM): reconstruct in-use stock 1970-2024 from
## UNEP inflows using Weibull lifetime distributions, calibrate to MISO2
## stocks at 2016, and produce the 2024 age-structured stock.
##
## Metal inflows are scope-corrected by A = MISO/UNEP (script 02c): historical
## metal "inflow" is TOTAL metal entering use (primary + secondary + embodied
## trade), MISO-consistent. Historical primary ore for reporting stays raw
## UNEP DMC. Non-metallic minerals are NOT corrected.
##
## Input:
##   Parameters/Intermediate/UNEP_flows_subenduse.parquet  -- from Script 01b (8 sub-uses)
##   Parameters/materials_region_DMC.csv                   -- UNEP raw DMC for Fe/NonFe split
##   Parameters/MISO/MISO_stock_regional.csv               -- calibration anchor (super-cat level)
##   Parameters/MISO/metal_grade_ore.csv                   -- ore→metal conversion factor g (to 2016)
##   Inputs/MC_Assumptions.xlsx (sheet Parameters)          -- 2024 grade "now" target (ramp anchor)
##   Parameters/Intermediate/miso_unep_scope_factor_A.csv  -- from Script 02c
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
lifetime_params <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Lifetimes") |>
  dplyr::select(sub_use, super_category, mean_life, weibull_k)

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

CAL_TOLERANCE <- 0.3


# -- Survival functions -------------------------------------------------------

weibull_survival <- function(age, mean_life, k) {
  lambda <- mean_life / gamma(1 + 1 / k)
  exp(-(age / lambda)^k)
}

get_survival <- function(ages, mean_life, k) {
  ifelse(ages < 0, 0, weibull_survival(pmax(ages, 0), mean_life, k))
}


# -- DSM function: trajectory 1970-2024 for one (Region x material x end_use) -

run_dsm_trajectory <- function(df, ghost_cohorts, mean_life, k) {
  df <- df %>% arrange(year)
  years <- df$year
  n <- length(years)
  inflows <- df$flow_Mt

  age_mat <- outer(years, years, function(cohort, sim) sim - cohort)
  surv_mat <- matrix(get_survival(as.vector(age_mat), mean_life, k), n, n)
  surv_mat[age_mat < 0] <- 0

  cohort_stock_mat <- sweep(surv_mat, 1, inflows, "*")
  cohort_stock <- colSums(cohort_stock_mat)

  ghost_stock <- vapply(
    years,
    function(sim_yr) {
      sum(ghost_cohorts$cohort_inflow_Mt * get_survival(sim_yr - ghost_cohorts$cohort_year, mean_life, k))
    },
    numeric(1)
  )

  total_stock <- cohort_stock + ghost_stock
  outflow <- c(NA_real_, total_stock[-n] - total_stock[-1] + inflows[-1])

  tibble(year = years, stock_Mt = total_stock, outflow_Mt = outflow, ghost_stock_Mt = ghost_stock)
}


# -- Age profile at 2024 ------------------------------------------------------

compute_age_profile_2024 <- function(df, ghost_cohorts, mean_life, k, lambda_cal) {
  df <- df %>% arrange(year)
  years <- df$year
  inflows <- df$flow_Mt * lambda_cal

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

unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

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

grade_raw <- read_csv("Parameters/MISO/metal_grade_ore.csv", show_col_types = FALSE)

grade_wide <- grade_raw %>%
  pivot_wider(names_from = group, values_from = g) %>%
  rename(g_Fe = Ferrous, g_NonFe = `Non-ferrous`)

# MC_Assumptions "mid" (= (min+max)/2) grade -- same value used as the 2024
# "now" baseline for the MC ore-grade convergence ramp in
# Scripts/04-Simulation/02-RunSimulations.R (GRADE_ORE_FE_NOW / GRADE_ORE_NONFE_NOW).
grade_mc_bounds <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Parameters") %>%
  filter(parameter_name %in% c("GRADE_ORE_FE", "GRADE_ORE_NONFE"))
grade_mid_fe <- with(grade_mc_bounds, mean(c(min[parameter_name == "GRADE_ORE_FE"], max[parameter_name == "GRADE_ORE_FE"])))
grade_mid_nonfe <- with(
  grade_mc_bounds,
  mean(c(min[parameter_name == "GRADE_ORE_NONFE"], max[parameter_name == "GRADE_ORE_NONFE"]))
)

yr_min <- min(unep_sub$year)
yr_max <- max(unep_sub$year)
grade_early <- grade_wide %>% filter(year == min(year)) %>% dplyr::select(-year)
grade_last_yr <- max(grade_wide$year)
grade_last_row <- grade_wide %>% filter(year == grade_last_yr) %>% dplyr::select(-year)
if (min(grade_wide$year) > yr_min) {
  grade_wide <- bind_rows(expand_grid(grade_early, year = seq(yr_min, min(grade_wide$year) - 1)), grade_wide)
}
if (grade_last_yr < yr_max) {
  # Ramp linearly from the last observed grade (2016) to the MC "now" mid
  # value, landing at 2024; hold at the mid value for any years beyond 2024.
  ramp_years <- seq(grade_last_yr + 1, yr_max)
  ramp_alpha <- pmin(1, (ramp_years - grade_last_yr) / (2024L - grade_last_yr))
  grade_ramp <- tibble(
    year = ramp_years,
    g_Fe = grade_last_row$g_Fe + (grade_mid_fe - grade_last_row$g_Fe) * ramp_alpha,
    g_NonFe = grade_last_row$g_NonFe + (grade_mid_nonfe - grade_last_row$g_NonFe) * ramp_alpha
  )
  grade_wide <- bind_rows(grade_wide, grade_ramp)
}

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

unep_all <- bind_rows(metal_fe, metal_nonfe, unep_sub %>% filter(material != "Metal ores"))
cat(sprintf(
  "  After Fe/NonFe split + g correction: %d rows | materials: %s\n",
  nrow(unep_all),
  paste(sort(unique(unep_all$material)), collapse = ", ")
))


# Step 1c: Apply MISO/UNEP scope factor A to metal inflows --------------------
#
# A = MISO_inflow / UNEP_metal_inflow (Region x super_category x year), from
# script 02c. Corrects UNEP's scope gap (recycled scrap + embodied trade,
# undecomposed). After this, flow_Mt for Metal_Fe/Metal_NonFe = TOTAL metal
# entering use, MISO-consistent. Ghost cohorts NOT corrected (built from MISO
# stock, already MISO scope). Non-metallic minerals NOT corrected.

cat("\nSTEP 1c: Apply MISO/UNEP scope factor A to metal inflows\n")

A_factor <- read_csv("Parameters/Intermediate/miso_unep_scope_factor_A.csv", show_col_types = FALSE)

if (max(A_factor$year) < max(unep_all$year[unep_all$year <= 2024])) {
  warning("Scope factor A does not cover all years to 2024 -- uncovered years get A = 1.")
}

pre_metal_2016 <- unep_all %>%
  filter(material %in% c("Metal_Fe", "Metal_NonFe"), year == 2016) %>%
  summarise(Mt = sum(flow_Mt)) %>%
  pull(Mt)

unep_all <- unep_all %>%
  left_join(A_factor, by = c("Region", "super_category", "year")) %>%
  mutate(A = if_else(material %in% c("Metal_Fe", "Metal_NonFe"), replace_na(A, 1.0), 1.0), flow_Mt = flow_Mt * A) %>%
  dplyr::select(-A)

post_metal_2016 <- unep_all %>%
  filter(material %in% c("Metal_Fe", "Metal_NonFe"), year == 2016) %>%
  summarise(Mt = sum(flow_Mt)) %>%
  pull(Mt)

cat(sprintf(
  "  Global metal inflow 2016: %.0f Mt (UNEP) -> %.0f Mt (x A; should be near MISO ~1455 Mt)\n",
  pre_metal_2016,
  post_metal_2016
))


# Step 2: Build pre-1970 ghost cohort table from MISO2 stock series -----------

cat("\nSTEP 2: Build pre-1970 ghost cohort table from MISO2 stock series (5-year bins)\n")

ghost_super <- miso_stock %>%
  filter(year >= 1900, year <= 1970, year %% 5 == 0) %>%
  arrange(Region, material, end_use, year) %>%
  group_by(Region, material, end_use) %>%
  mutate(cohort_inflow_Mt = pmax(value_Mt - lag(value_Mt, default = 0), 0)) %>%
  ungroup() %>%
  rename(cohort_year = year, super_category = end_use) %>%
  dplyr::select(Region, material, super_category, cohort_year, cohort_inflow_Mt)

shares_1970_sub <- unep_sub %>%
  filter(year == 1970) %>%
  group_by(material, super_category, sub_use) %>%
  summarise(inflow_share = mean(inflow_share, na.rm = TRUE), .groups = "drop")

fe_share_1970 <- fe_share_rt %>% filter(year == 1970) %>% dplyr::select(Region, fe_share_1970 = fe_share)

ghost_cohorts_nested <- ghost_super %>%
  left_join(shares_1970_sub, by = c("material", "super_category"), relationship = "many-to-many") %>%
  mutate(cohort_inflow_Mt = cohort_inflow_Mt * inflow_share) %>%
  dplyr::select(Region, material, super_category, sub_use, cohort_year, cohort_inflow_Mt) %>%
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


# Step 3: Build simulation input -- corrected flows + lifetime params ---------

cat("\nSTEP 3: Build simulation input data frame\n")

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
  dplyr::select(-mean_life, -inflow_share) %>%
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

dsm_nested <- dsm_nested %>%
  mutate(sim = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k), run_dsm_trajectory))

dsm_results <- dsm_nested %>% dplyr::select(-data, -ghost_cohorts) %>% unnest(sim)

cat("  DSM results:", nrow(dsm_results), "rows\n")


# Step 5: Calibrate to MISO2 stock at 2016 -----------------------------------

cat("\nSTEP 5: Calibrate simulated stocks to MISO2 at 2016\n")

miso_2016 <- miso_stock %>%
  filter(year == 2016) %>%
  rename(super_category = end_use) %>%
  dplyr::select(Region, material, super_category, miso_stock_2016_Mt = value_Mt)

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
  cat("[DIAGNOSTIC] lambda_cal outside [0.7, 1.3] for", nrow(flagged_cal), "groups:\n")
  print(
    flagged_cal %>% dplyr::select(Region, material, super_category, lambda_cal) %>% arrange(desc(abs(lambda_cal - 1))),
    n = 20
  )
}
cat("  Calibration scalar summary (lambda_cal):\n")
print(summary(calibration_scalars$lambda_cal))

# -- Verify (2): with scope factor A applied, metal lambda_cal should collapse
# toward ~1 (residual = lifetime/shape mismatch only, not scope). If any metal
# group still has lambda > 1.5 or < 0.6, A and the calibration disagree there.
cat("[VERIFY 2] lambda_cal for Metal ores groups after scope correction (expect ~0.8-1.2):\n")
print(
  calibration_scalars %>%
    filter(material == "Metal ores") %>%
    summarise(
      min = min(lambda_cal),
      p25 = quantile(lambda_cal, .25),
      median = median(lambda_cal),
      p75 = quantile(lambda_cal, .75),
      max = max(lambda_cal)
    )
)
worst_metal <- calibration_scalars %>% filter(material == "Metal ores", lambda_cal > 1.5 | lambda_cal < 0.6)
if (nrow(worst_metal) > 0) {
  cat("[VERIFY 2 - FLAG] metal groups still far from 1:\n")
  print(worst_metal %>% dplyr::select(Region, super_category, lambda_cal) %>% arrange(desc(abs(lambda_cal - 1))))
}

lambda_join <- calibration_scalars %>%
  dplyr::select(Region, material, super_category, lambda_cal) %>%
  bind_rows(
    filter(., material == "Metal ores") %>% mutate(material = "Metal_Fe"),
    filter(., material == "Metal ores") %>% mutate(material = "Metal_NonFe")
  ) %>%
  filter(material != "Metal ores")

# Scale inflows and ghost cohorts by lambda_cal, then re-run DSM (linearity).
dsm_nested_cal <- dsm_nested %>%
  left_join(lambda_join, by = c("Region", "material", "super_category")) %>%
  mutate(
    lambda_cal = replace_na(lambda_cal, 1.0),
    data = purrr::map2(data, lambda_cal, ~ mutate(.x, flow_Mt = flow_Mt * .y)),
    ghost_cohorts = purrr::map2(ghost_cohorts, lambda_cal, ~ mutate(.x, cohort_inflow_Mt = cohort_inflow_Mt * .y))
  ) %>%
  dplyr::select(-lambda_cal) %>%
  mutate(sim = purrr::pmap(list(data, ghost_cohorts, mean_life, weibull_k), run_dsm_trajectory))

dsm_calibrated <- dsm_nested_cal %>% dplyr::select(-data, -ghost_cohorts) %>% unnest(sim)

write.csv(
  dsm_calibrated %>% dplyr::select(Region, material, super_category, sub_use, year, stock_Mt),
  "Parameters/Intermediate/stock_trajectory_subenduse.csv",
  row.names = F
)
cat("  Year range:", range(dsm_calibrated$year), "\n")

# -- Verify (1): calibrated stock@2016 == MISO@2016 ----------------------------
cal_check_2016 <- dsm_calibrated %>%
  filter(year == 2016) %>%
  mutate(material_cal = recode(material, Metal_Fe = "Metal ores", Metal_NonFe = "Metal ores")) %>%
  group_by(Region, material_cal, super_category) %>%
  summarise(cal_stock_2016_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  rename(material = material_cal) %>%
  left_join(miso_2016, by = c("Region", "material", "super_category")) %>%
  filter(!is.na(miso_stock_2016_Mt), miso_stock_2016_Mt > 0) %>%
  mutate(ratio = cal_stock_2016_Mt / miso_stock_2016_Mt)
cat("[VERIFY 1] Calibrated stock@2016 / MISO@2016 ratio (should be ~1.0):\n")
print(summary(cal_check_2016$ratio))

# -- Verify (4): global metal stock growth 2014-2024 ---------------------------
# THE seam-relevant number. Pre-correction: NonFe ~0.8%/yr (vs imposed ~2.7%
# at 2025 -> 2x jump). With MISO-scope inflows the slope should rise toward
# ~2-3%/yr, and the 2025 discontinuity in scripts 02/02b should mostly close.
cat("[VERIFY 4] Global metal stock growth (2014-2024, %/yr):\n")
print(
  dsm_calibrated %>%
    filter(material %in% c("Metal_Fe", "Metal_NonFe"), year >= 2013) %>%
    group_by(material, year) %>%
    summarise(S = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
    group_by(material) %>%
    arrange(year) %>%
    mutate(growth_pct = 100 * (S / lag(S) - 1)) %>%
    filter(year >= 2014),
  n = 30
)
# Step 5b: Compute 2024 non-primary share (Region x material) ----------------
#
# Anchor for the forward MC recycling trajectories (Scripts/04-Simulation/02
# and 02b): s_2024 = 1 - primary_metal_Mt / inflow_Mt, where inflow_Mt is the
# MISO-scope-corrected metal inflow (post scope factor A, post lambda_cal) and
# primary_metal_Mt is raw UNEP DMC (uncorrected -- true domestic mining) times
# 2024 ore grade. The gap between them is the same undecomposed scrap +
# embodied-trade wedge that A corrects for on the inflow side; this fixes it
# on the supply-split side. A itself is untouched.

cat("\nSTEP 5b: Compute 2024 non-primary share (Region x material)\n")

inflow_2024 <- dsm_nested_cal %>%
  dplyr::select(Region, material, data) %>%
  tidyr::unnest(data) %>%
  filter(year == 2024, material %in% c("Metal_Fe", "Metal_NonFe")) %>%
  group_by(Region, material) %>%
  summarise(inflow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop")

grade_2024 <- grade_wide %>% filter(year == 2024)

primary_2024 <- unep_raw %>%
  filter(material_category %in% c("Ferrous ores", "Non-ferrous ores"), year == 2024) %>%
  group_by(Region, material_category) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    material = if_else(material_category == "Ferrous ores", "Metal_Fe", "Metal_NonFe"),
    primary_metal_Mt = if_else(material == "Metal_Fe", DMC_Mt * grade_2024$g_Fe, DMC_Mt * grade_2024$g_NonFe)
  ) %>%
  dplyr::select(Region, material, primary_metal_Mt)

nonprimary_share_2024 <- inflow_2024 %>%
  full_join(primary_2024, by = c("Region", "material")) %>%
  mutate(
    s_2024 = case_when(
      !is.finite(inflow_Mt) | inflow_Mt <= 0 ~ NA_real_,
      !is.finite(primary_metal_Mt) | primary_metal_Mt < 0 ~ NA_real_,
      TRUE ~ 1 - primary_metal_Mt / inflow_Mt
    )
  ) %>%
  dplyr::select(Region, material, inflow_Mt, primary_metal_Mt, s_2024) %>%
  arrange(material, Region)

write.csv(nonprimary_share_2024, "Parameters/Intermediate/nonprimary_share_2024.csv", row.names = FALSE)
cat("  2024 non-primary share (s_2024 = 1 - primary_metal_Mt / inflow_Mt), by Region x material:\n")
print(nonprimary_share_2024, n = Inf)


# Step 6: Compute 2024 age profile -------------------------------------------

cat("\nSTEP 6: Compute 2024 age-structured stock\n")

age_profile_nested <- dsm_nested_cal %>% mutate(lambda_cal = 1.0)

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

miso_traj <- miso_stock %>% rename(stock_Mt = value_Mt)

library(geomtextpath)
p_traj <- dsm_calibrated %>%
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
    aes(label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.95,
    offset = unit(3, "pt"),
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

miso_traj_super <- miso_traj %>% rename(super_category = end_use)

p_traj2 <- dsm_calibrated %>%
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
