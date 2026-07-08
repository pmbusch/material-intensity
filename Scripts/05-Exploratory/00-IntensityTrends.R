## =============================================================================
## Scripts/QA/05-IntensityTrends.R
## Standalone QA: fit historical GLOBAL material intensity trends (2014–2023)
## per material grain, project to 2040 under two models, output CSV + plot.
## Output is for manual inspection only; no other script consumes it.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")

library(readxl)

# ── Constants ------------------------------------------------------------------

FIT_START <- 2014L
FIT_END   <- 2023L
PROJ_YEAR <- 2040L

BIOMASS_CATS_NAMED <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS_NAMED  <- c("Coal", "Natural Gas", "Petroleum")
STOCK_ENDUSE_HIST  <- c("buildings", "civil_infrastructure", "machinery", "short_lived")

# ── Output directories ---------------------------------------------------------

dir.create("Results/Intensity_trends", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/Intensity_trends",  showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# STEP 1: Load raw data
# =============================================================================

cat("STEP 1: Load raw data\n")

dmc_region      <- read_csv("Parameters/materials_region_DMC.csv",        show_col_types = FALSE)
gdp_region_raw  <- read_csv("Parameters/gdp_region.csv",                  show_col_types = FALSE)
stock_subenduse <- read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE)
intensity_excel <- read_excel("Inputs/MC_Assumptions.xlsx", sheet = "Intensity")

cat("  DMC rows:", nrow(dmc_region), "| GDP rows:", nrow(gdp_region_raw),
    "| Stock rows:", nrow(stock_subenduse), "\n")


# =============================================================================
# STEP 2: World GDP series (sum across regions, fit window only)
# =============================================================================

cat("STEP 2: World GDP\n")

gdp_world <- gdp_region_raw |>
  filter(year >= FIT_START, year <= FIT_END) |>
  group_by(year) |>
  summarise(GDP_2015USD = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop")


# =============================================================================
# STEP 3: Intensity floor values from MC_Assumptions Intensity sheet
# =============================================================================

cat("STEP 3: Intensity floors from Excel\n")

floor_biomass <- intensity_excel |>
  filter(Category == "Biomass") |>
  mutate(
    material_detail = case_when(
      str_detect(tolower(Detail), "residue") ~ "Crop Residues",
      str_detect(tolower(Detail), "grazed")  ~ "Grazed biomass and fodder crops",
      str_detect(tolower(Detail), "crop")    ~ "Crops",
      str_detect(tolower(Detail), "wood")    ~ "Wood",
      str_detect(tolower(Detail), "other")   ~ "Other biomass",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(material_detail)) |>
  select(material_detail, intensity_min_excel = min)

floor_fossil <- intensity_excel |>
  filter(str_detect(tolower(Category), "fossil")) |>
  mutate(
    material_detail = case_when(
      str_detect(tolower(Detail), "coal")          ~ "Coal",
      str_detect(tolower(Detail), "gas")           ~ "Natural Gas",
      str_detect(tolower(Detail), "petroleum|oil") ~ "Petroleum",
      str_detect(tolower(Detail), "other")         ~ "Other fossil fuels",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(material_detail)) |>
  select(material_detail, intensity_min_excel = min)

floor_metal <- intensity_excel |>
  filter(str_detect(tolower(Category), "metal ore")) |>
  mutate(
    super_category = case_when(
      str_detect(tolower(Detail), "build")  ~ "buildings",
      str_detect(tolower(Detail), "civil")  ~ "civil_infrastructure",
      str_detect(tolower(Detail), "short")  ~ "short_lived",
      str_detect(tolower(Detail), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(super_category)) |>
  select(super_category, intensity_min_excel = min)

floor_nonmet <- intensity_excel |>
  filter(str_detect(tolower(Category), "metalic")) |>
  mutate(
    super_category = case_when(
      str_detect(tolower(Detail), "build")  ~ "buildings",
      str_detect(tolower(Detail), "civil")  ~ "civil_infrastructure",
      str_detect(tolower(Detail), "short")  ~ "short_lived",
      str_detect(tolower(Detail), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(super_category)) |>
  select(super_category, intensity_min_excel = min)


# =============================================================================
# STEP 4: Build global intensity series
# =============================================================================

cat("STEP 4: Build global intensity series\n")

# Helper: ensure both grain columns exist before select
tidy_series <- function(df) {
  if (!"material_detail" %in% names(df)) df$material_detail <- NA_character_
  if (!"super_category"  %in% names(df)) df$super_category  <- NA_character_
  df |> select(year, material_group, material_detail, super_category, intensity)
}

# -- 4a: Biomass (global DMC flow) per material_detail ------------------------

biomass_series <- dmc_region |>
  filter(
    material_category %in% c(
      BIOMASS_CATS_NAMED,
      "Wild catch and harvest",
      "Non-wild animal products",
      "Products mainly from biomass nec."
    )
  ) |>
  mutate(
    material_detail = if_else(
      material_category %in% BIOMASS_CATS_NAMED,
      material_category,
      "Other biomass"
    )
  ) |>
  group_by(year, material_detail) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  filter(year >= FIT_START, year <= FIT_END) |>
  left_join(gdp_world, by = "year") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(intensity = DMC_Mt * 1e9 / GDP_2015USD, material_group = "Biomass") |>
  tidy_series()

# -- 4b: Fossil fuels (global DMC flow) per material_detail -------------------

fossil_series <- dmc_region |>
  filter(
    material_category %in% c(
      FOSSIL_CATS_NAMED,
      "Oil shale and tar sands",
      "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
      "Other products mainly from fossil fuels e.g. plastics"
    )
  ) |>
  mutate(
    material_detail = if_else(
      material_category %in% FOSSIL_CATS_NAMED,
      material_category,
      "Other fossil fuels"
    )
  ) |>
  group_by(year, material_detail) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  filter(year >= FIT_START, year <= FIT_END) |>
  left_join(gdp_world, by = "year") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(intensity = DMC_Mt * 1e9 / GDP_2015USD, material_group = "Fossil fuels") |>
  tidy_series()

# -- 4c: Metal ores (global stock, sum Fe + NonFe) per super_category ---------

metal_series <- stock_subenduse |>
  filter(material %in% c("Metal_Fe", "Metal_NonFe"), super_category %in% STOCK_ENDUSE_HIST) |>
  group_by(year, super_category) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  filter(year >= FIT_START, year <= FIT_END) |>
  left_join(gdp_world, by = "year") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(intensity = stock_Mt * 1e9 / GDP_2015USD, material_group = "Metal ores") |>
  tidy_series()

# -- 4d: Non-metallic minerals (global stock) per super_category --------------

nonmet_series <- stock_subenduse |>
  filter(material == "Non-metallic minerals", super_category %in% STOCK_ENDUSE_HIST) |>
  group_by(year, super_category) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  filter(year >= FIT_START, year <= FIT_END) |>
  left_join(gdp_world, by = "year") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(intensity = stock_Mt * 1e9 / GDP_2015USD, material_group = "Non-metallic minerals") |>
  tidy_series()

all_series <- bind_rows(biomass_series, fossil_series, metal_series, nonmet_series) |>
  mutate(region = "Global")

cat("  Total rows:", nrow(all_series),
    "| Panels:", n_distinct(paste(all_series$material_group,
                                   coalesce(all_series$material_detail, all_series$super_category))), "\n")


# =============================================================================
# STEP 5: Fit models per panel (material_group x grain)
# =============================================================================

cat("STEP 5: Fit models\n")

panel_keys <- c("material_group", "material_detail", "super_category")

fit_panel <- function(df) {
  yr <- df$year
  v  <- df$intensity

  # Linear-in-level
  fit_lin <- tryCatch(lm(v ~ yr), error = function(e) NULL)
  if (!is.null(fit_lin)) {
    s   <- summary(fit_lin)
    cm  <- coef(s)
    int_lin <- coef(fit_lin)[["(Intercept)"]]
    slp_lin <- cm["yr", "Estimate"]
    se_lin  <- cm["yr", "Std. Error"]
    pv_lin  <- cm["yr", "Pr(>|t|)"]
    r2_lin  <- s$r.squared
  } else {
    int_lin <- slp_lin <- se_lin <- pv_lin <- r2_lin <- NA_real_
  }

  # Log-linear (skip if fewer than 3 positive observations)
  pos <- !is.na(v) & v > 0
  if (sum(pos) >= 3) {
    yr_p   <- yr[pos]
    vlog_p <- log(v[pos])
    fit_log <- tryCatch(lm(vlog_p ~ yr_p), error = function(e) NULL)
  } else {
    fit_log <- NULL
  }
  if (!is.null(fit_log)) {
    s   <- summary(fit_log)
    cm  <- coef(s)
    int_log <- coef(fit_log)[["(Intercept)"]]
    slp_log <- cm["yr_p", "Estimate"]
    se_log  <- cm["yr_p", "Std. Error"]
    pv_log  <- cm["yr_p", "Pr(>|t|)"]
    r2_log  <- s$r.squared
  } else {
    int_log <- slp_log <- se_log <- pv_log <- r2_log <- NA_real_
  }

  tibble(
    n_obs                  = nrow(df),
    intensity_2014         = v[yr == FIT_START][1],
    intensity_2024         = v[yr == FIT_END][1],
    central_intensity_mean = mean(v, na.rm = TRUE),
    intercept_linear       = int_lin,
    slope_linear           = slp_lin,
    slope_se_linear        = se_lin,
    pvalue_linear          = pv_lin,
    r2_linear              = r2_lin,
    intercept_log          = int_log,
    slope_log              = slp_log,
    slope_se_log           = se_log,
    pvalue_log             = pv_log,
    r2_log                 = r2_log
  )
}

fits <- all_series |>
  group_by(across(all_of(panel_keys))) |>
  group_modify(~ fit_panel(.x)) |>
  ungroup() |>
  mutate(
    region      = "Global",
    sig_lin_p05 = !is.na(pvalue_linear) & pvalue_linear < 0.05,
    sig_lin_p10 = !is.na(pvalue_linear) & pvalue_linear < 0.10,
    sig_log_p05 = !is.na(pvalue_log)    & pvalue_log    < 0.05,
    sig_log_p10 = !is.na(pvalue_log)    & pvalue_log    < 0.10
  )

cat("  Panels fitted:", nrow(fits), "\n")


# =============================================================================
# STEP 6: Attach floors and project to 2040
# =============================================================================

cat("STEP 6: Project to 2040\n")

fits_floored <- fits |>
  left_join(
    bind_rows(
      floor_biomass |> mutate(material_group = "Biomass"),
      floor_fossil  |> mutate(material_group = "Fossil fuels")
    ),
    by = c("material_group", "material_detail")
  ) |>
  left_join(
    bind_rows(
      floor_metal  |> mutate(material_group = "Metal ores"),
      floor_nonmet |> mutate(material_group = "Non-metallic minerals")
    ),
    by = c("material_group", "super_category"),
    suffix = c("", "_stock")
  ) |>
  mutate(
    intensity_min_excel = coalesce(intensity_min_excel, intensity_min_excel_stock),
    floor_value = pmax(
      0.5 * replace_na(intensity_2014, 0),
      replace_na(intensity_min_excel, 0),
      na.rm = TRUE
    )
  ) |>
  select(-intensity_min_excel_stock)

fits_out <- fits_floored |>
  mutate(
    # unconstrained projections (to detect floor binding before clipping)
    raw_lin_p05 = if_else(sig_lin_p05,
      intercept_linear + slope_linear * PROJ_YEAR, central_intensity_mean),
    raw_lin_p10 = if_else(sig_lin_p10,
      intercept_linear + slope_linear * PROJ_YEAR, central_intensity_mean),
    raw_log_p05 = if_else(sig_log_p05,
      exp(intercept_log + slope_log * PROJ_YEAR), central_intensity_mean),
    raw_log_p10 = if_else(sig_log_p10,
      exp(intercept_log + slope_log * PROJ_YEAR), central_intensity_mean),
    floor_triggered_lin_p05 = !is.na(raw_lin_p05) & raw_lin_p05 < floor_value,
    floor_triggered_lin_p10 = !is.na(raw_lin_p10) & raw_lin_p10 < floor_value,
    floor_triggered_log_p05 = !is.na(raw_log_p05) & raw_log_p05 < floor_value,
    floor_triggered_log_p10 = !is.na(raw_log_p10) & raw_log_p10 < floor_value,
    proj_2040_lin_p05 = pmax(raw_lin_p05, floor_value, na.rm = TRUE),
    proj_2040_lin_p10 = pmax(raw_lin_p10, floor_value, na.rm = TRUE),
    proj_2040_log_p05 = pmax(raw_log_p05, floor_value, na.rm = TRUE),
    proj_2040_log_p10 = pmax(raw_log_p10, floor_value, na.rm = TRUE)
  ) |>
  select(-raw_lin_p05, -raw_lin_p10, -raw_log_p05, -raw_log_p10)


# =============================================================================
# STEP 7: Write CSV
# =============================================================================

cat("STEP 7: Write CSV\n")

csv_out <- fits_out |>
  select(
    region, material_group, material_detail, super_category,
    n_obs, intensity_2024, intensity_2014, central_intensity_mean,
    slope_linear, slope_se_linear, pvalue_linear, r2_linear, sig_lin_p05, sig_lin_p10,
    slope_log,    slope_se_log,    pvalue_log,    r2_log,    sig_log_p05, sig_log_p10,
    proj_2040_lin_p05, proj_2040_lin_p10, proj_2040_log_p05, proj_2040_log_p10,
    floor_value,
    floor_triggered_lin_p05, floor_triggered_lin_p10,
    floor_triggered_log_p05, floor_triggered_log_p10
  )

write_csv(csv_out, "Results/Intensity_trends/intensity_slopes_2014_2023.csv")
cat("  Saved: Results/Intensity_trends/intensity_slopes_2014_2023.csv (", nrow(csv_out), "rows)\n")


# =============================================================================
# STEP 8: Console summary per material_group
# =============================================================================

cat("\n-- Summary by material group -----------------------------------------------\n")

csv_out |>
  group_by(material_group) |>
  summarise(
    sig_lin_p05 = sum(sig_lin_p05,  na.rm = TRUE),
    sig_lin_p10 = sum(sig_lin_p10,  na.rm = TRUE),
    sig_log_p05 = sum(sig_log_p05,  na.rm = TRUE),
    sig_log_p10 = sum(sig_log_p10,  na.rm = TRUE),
    floor_any   = sum(
      (floor_triggered_lin_p05 | floor_triggered_lin_p10 |
       floor_triggered_log_p05 | floor_triggered_log_p10),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  pwalk(function(material_group, sig_lin_p05, sig_lin_p10, sig_log_p05, sig_log_p10, floor_any) {
    cat(sprintf(
      "  %-28s  lin p<.05=%d p<.10=%d  |  log p<.05=%d p<.10=%d  |  floor triggered=%d\n",
      material_group, sig_lin_p05, sig_lin_p10, sig_log_p05, sig_log_p10, floor_any
    ))
  })


# =============================================================================
# STEP 9: Diagnostic plot
# =============================================================================

cat("\nSTEP 9: Diagnostic plot\n")

hist_plot <- all_series |>
  mutate(panel_label = coalesce(material_detail, super_category))

# Projection lines 2023-2040 (p<0.10).
# At FIT_END, anchor to the actual historical value to ensure visual continuity.
proj_lines <- fits_out |>
  mutate(panel_label = coalesce(material_detail, super_category)) |>
  select(
    material_group, panel_label,
    intercept_linear, slope_linear, sig_lin_p10,
    intercept_log,    slope_log,    sig_log_p10,
    central_intensity_mean, floor_value, intensity_2024
  ) |>
  crossing(year = seq(FIT_END, PROJ_YEAR)) |>
  mutate(
    proj_lin = if_else(
      year == FIT_END,
      intensity_2024,
      pmax(
        if_else(sig_lin_p10,
          intercept_linear + slope_linear * year,
          central_intensity_mean),
        floor_value, na.rm = TRUE
      )
    ),
    proj_log = if_else(
      year == FIT_END,
      intensity_2024,
      pmax(
        if_else(sig_log_p10,
          exp(intercept_log + slope_log * year),
          central_intensity_mean),
        floor_value, na.rm = TRUE
      )
    )
  )

floor_ref <- fits_out |>
  mutate(panel_label = coalesce(material_detail, super_category)) |>
  select(material_group, panel_label, floor_value) |>
  distinct()

mat_colors <- PALETTE_MATERIAL_GROUPS[c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")]

ggplot() +
  geom_line(
    data = hist_plot,
    aes(x = year, y = intensity, colour = material_group),
    linewidth = 0.7
  ) +
  geom_point(
    data = hist_plot |> filter(year %in% c(FIT_START, FIT_END)),
    aes(x = year, y = intensity, colour = material_group),
    size = 1.2
  ) +
  geom_line(
    data = proj_lines,
    aes(x = year, y = proj_lin, colour = material_group),
    linetype = "dashed", linewidth = 0.6
  ) +
  geom_line(
    data = proj_lines,
    aes(x = year, y = proj_log, colour = material_group),
    linetype = "dotted", linewidth = 0.7
  ) +
  geom_hline(
    data = floor_ref,
    aes(yintercept = floor_value),
    colour = "grey40", linewidth = 0.3, linetype = "longdash"
  ) +
  geom_vline(xintercept = FIT_END, colour = "grey55", linewidth = 0.25, linetype = "dotted") +
  facet_wrap(material_group ~ panel_label, scales = "free_y", ncol = 5) +
  scale_colour_manual(values = mat_colors, guide = "none") +
  scale_x_continuous(breaks = c(2014, 2023, 2040)) +
  scale_y_continuous(labels = scales::scientific) +
  labs(
    title    = "Global material intensity trends 2014-2023 and projections to 2040",
    subtitle = paste0(
      "Solid = historical | Dashed = linear proj. (p<0.10) | ",
      "Dotted = log-linear (p<0.10) | Long-dash = floor"
    ),
    x = "Year",
    y = "Intensity (t / 2015 USD)"
  ) +
  theme_pb_large() +
  theme(
    strip.text  = element_text(size = 5.5),
    axis.text   = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "Figures/Intensity_trends/intensity_2014_2040.png",
  ggplot2::last_plot(),
  units = "cm", dpi = 300, width = 34, height = 26
)
cat("  Saved: Figures/Intensity_trends/intensity_2014_2040.png\n")

cat("\n=== Done ===\n")

# EoF
