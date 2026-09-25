## =============================================================================
## Figure 2 Detail - Region.R
## Region-level version of Figure 2 (see "Figure 2 - ProjectionMethod.R"):
## same eight-panel Kaya layout (population, GDP/cap, M/G biomass, M/G fossil,
## S/G metal, S/G non-metallic, total DMC, total stock), rebuilt per region
## using that region's own population/GDP as the denominator, instead of
## world totals. One PNG + SVG per region, saved to
## Figures/Figure 2 Detail/Region/<region>.png.
##
## Unlike Figure 2, direct-curve label angles are computed from the local
## slope only (no per-category manual angle/nudge tuning), since 8 regions
## produce 8 different curve shapes that hand-tuned offsets wouldn't fit.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")
library(patchwork)

# ── Constants ----------------------------------------------------------------

HIST_END <- 2024L
PROJ_END <- FORECAST_END
CI_LO <- 0.25
CI_HI <- 0.75

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood", "Other biomass")
BIOMASS_CATS_NAMED <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum", "Other fossil fuels")
FOSSIL_CATS_NAMED <- c("Coal", "Natural Gas", "Petroleum")
STOCK_ENDUSE_HIST <- c("buildings", "civil_infrastructure", "machinery", "short_lived")
METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

REGIONS <- names(PALETTE_REGIONS)

RIBBON_ALPHA_MINMAX <- 0.12
RIBBON_ALPHA_IQR <- 0.32
HIST_LW <- 0.65
PROJ_LW <- 0.65

LABEL_SZ <- 2.8
FONT_TITLE <- 11.5
FONT_AXIS_T <- 9.5
FONT_AXIS_L <- 9.0
FONT_BUMP <- theme(
  axis.text = element_text(size = FONT_AXIS_L),
  axis.title = element_text(size = FONT_AXIS_T),
  plot.title = element_text(size = FONT_TITLE)
)

LABEL_YEAR <- 1982L
STACK_LABEL_YEAR <- 2005L
HIST_LABEL_YEAR <- 2020L

SUB_USE_LABELS <- c(
  "residential" = "Residential Bldg",
  "non_residential" = "Non-residential Bldg",
  "roads" = "Roads",
  "civil_engineering" = "Civil eng.",
  "machinery_group" = "Machinery",
  "vehicles_group" = "Vehicles",
  "durables" = "Durables",
  "packaging" = "Packaging"
)

F_LABEL_ENDUSES <- c("Roads", "Civil eng.", "Residential Bldg", "Non-residential Bldg")

# ── Shared plot layers -------------------------------------------------------

present_line_top <- list(
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"),
  annotate(
    "text", x = HIST_END - 2, y = Inf, label = "Present (2024)",
    hjust = 1.05, vjust = 0.5, size = LABEL_SZ, colour = "grey45", angle = 90
  )
)
present_line_bot <- list(
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"),
  annotate(
    "text", x = HIST_END - 2, y = -Inf, label = "Present (2024)",
    hjust = -0.05, vjust = 0.5, size = LABEL_SZ, colour = "grey45", angle = 90
  )
)
present_line <- list(geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"))

x_sc <- scale_x_continuous(breaks = seq(1970, PROJ_END, 10), labels = function(x) {
  ifelse(x %in% seq(1970, 2050, 20), x, "")
})
y_sc <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
co <- coord_cartesian(xlim = c(1970, PROJ_END), ylim = c(0, NA), clip = "off", expand = FALSE)
X_RANGE_YR <- PROJ_END - 1970

panel_tag <- function(ltr, side = c("left", "right")) {
  side <- match.arg(side)
  annotate(
    "text",
    x = if (side == "left") -Inf else Inf,
    y = Inf,
    label = ltr,
    hjust = if (side == "left") -0.5 else 1.5,
    vjust = 1.5,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  )
}

# Compute Min/P25/P50/P75/Max envelope; group_cols is a character vector of column names
env_quantiles <- function(df, val_col, group_cols) {
  df |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      p_min = min(.data[[val_col]], na.rm = TRUE),
      p25 = quantile(.data[[val_col]], CI_LO, na.rm = TRUE),
      p50 = median(.data[[val_col]], na.rm = TRUE),
      p75 = quantile(.data[[val_col]], CI_HI, na.rm = TRUE),
      p_max = max(.data[[val_col]], na.rm = TRUE),
      .groups = "drop"
    )
}


# ── SECTION A: Load historical data (region-level) ---------------------------

cat("A: Loading historical data\n")

pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE) |>
  rename(region = Region)
gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE) |> rename(region = Region)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) |> rename(region = Region)
stock_hist_raw <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE) |>
  rename(region = Region)
stock_subenduse_hist <- read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE) |>
  rename(region = Region)


# ── SECTION B: Load MC results -----------------------------------------------

cat("B: Loading MC results\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))
cat("  Runs:", n_distinct(results$run_id), "| Years:", paste(range(results$year), collapse = "-"), "\n")

run_ssp <- results |>
  distinct(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo)


# ── SECTION C: 2024 regional GDP/population baselines ------------------------

cat("C: Regional 2024 baselines\n")

gdp_2024_region <- gdp_region_hist |> filter(year == HIST_END) |> transmute(region, gdp_2024 = GDP_2015USD)
pop_2024_region <- pop_region_hist |> filter(year == HIST_END) |> transmute(region, pop_2024 = population)


# ── SECTION C2: Per-run, per-region GDP & population from the continuous blend
# Each run blends two bracketing SSPs independently for population and for
# GDP-per-capita (see 02-RunSimulations.R STEP 3/4), already at region level;
# unlike Figure 2 these are kept per-region rather than summed to a world total.

cat("C2: Per-run regional GDP/population from continuous SSP blend\n")

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, pop_ssp_lo, pop_ssp_hi, pop_ssp_share_lo) |>
  left_join(pop_idx_region |> rename(pop_ssp_lo = scenario), by = "pop_ssp_lo", relationship = "many-to-many") |>
  left_join(
    pop_idx_region |> rename(pop_ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("pop_ssp_hi", "region", "year")
  ) |>
  mutate(pop_idx_blend = pop_ssp_share_lo * pop_idx + (1 - pop_ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, gdppc_ssp_lo, gdppc_ssp_hi, gdppc_ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(gdppc_ssp_lo = scenario), by = "gdppc_ssp_lo", relationship = "many-to-many") |>
  left_join(
    gdppc_idx_region |> rename(gdppc_ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("gdppc_ssp_hi", "region", "year")
  ) |>
  mutate(gdppc_idx_blend = gdppc_ssp_share_lo * gdppc_idx + (1 - gdppc_ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

pop_by_run_region <- pop_idx_blend |>
  left_join(pop_2024_region, by = "region") |>
  transmute(run_id, region, year, pop_run = pop_2024 * pop_idx_blend)

gdp_by_run_region <- pop_idx_blend |>
  left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  left_join(gdp_2024_region, by = "region") |>
  transmute(run_id, region, year, gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend)


# ── SECTION D: Historical M/G and S/G by region x sub-material / end-use -----

cat("D: Historical M/G and S/G by region\n")

biomass_hist <- dmc_hist |>
  filter(
    material_category %in%
      c(BIOMASS_CATS_NAMED, "Wild catch and harvest", "Non-wild animal products", "Products mainly from biomass nec.")
  ) |>
  mutate(material_category = if_else(material_category %in% BIOMASS_CATS_NAMED, material_category, "Other biomass")) |>
  group_by(region, year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

fossil_hist <- dmc_hist |>
  filter(
    material_category %in%
      c(
        FOSSIL_CATS_NAMED,
        "Oil shale and tar sands",
        "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
        "Other products mainly from fossil fuels e.g. plastics"
      )
  ) |>
  mutate(
    material_category = if_else(material_category %in% FOSSIL_CATS_NAMED, material_category, "Other fossil fuels")
  ) |>
  group_by(region, year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

metal_hist_eu <- stock_subenduse_hist |>
  filter(material %in% c("Metal_Fe", "Metal_NonFe")) |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(region, year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)

nonmet_hist_eu <- stock_subenduse_hist |>
  filter(material == "Non-metallic minerals") |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(region, year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)


# ── SECTION E: MC projection envelopes (P5 / P50 / P95), per region ---------

cat("E: MC projection envelopes by region\n")

pop_env <- pop_by_run_region |> mutate(v = pop_run / 1e9) |> env_quantiles("v", c("region", "year"))

gdpcap_env <- pop_by_run_region |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(gdpcap = gdp_run / pop_run / 1e3) |>
  env_quantiles("gdpcap", c("region", "year"))

biomass_env <- results |>
  filter(material_group == "biomass", material_key %in% BIOMASS_CATS) |>
  group_by(run_id, region, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = M_Mt * 1e9 / gdp_run) |>
  env_quantiles("mg", c("region", "material_key", "year"))

fossil_env <- results |>
  filter(material_group == "fossil_fuels", material_key %in% FOSSIL_CATS) |>
  group_by(run_id, region, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = M_Mt * 1e9 / gdp_run) |>
  env_quantiles("mg", c("region", "material_key", "year"))

metal_env <- results |>
  filter(material_group == "metal_ores") |>
  group_by(run_id, region, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = stock_Mt * 1e9 / gdp_run) |>
  rename(end_use_label = material_key) |>
  env_quantiles("mg", c("region", "end_use_label", "year"))

nonmet_env <- results |>
  filter(material_group == "nonmetallic_minerals") |>
  group_by(run_id, region, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = stock_Mt * 1e9 / gdp_run) |>
  rename(end_use_label = material_key) |>
  env_quantiles("mg", c("region", "end_use_label", "year"))

dmc_env <- results |>
  group_by(run_id, region, year) |>
  summarise(DMC_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  env_quantiles("DMC_Gt", c("region", "year"))

stock_env <- results |>
  filter(material_group %in% c("metal_ores", "nonmetallic_minerals")) |>
  group_by(run_id, region, year) |>
  summarise(stock_Gt = sum(in_use_stock_Mt, na.rm = TRUE) / 1e6, .groups = "drop") |>
  env_quantiles("stock_Gt", c("region", "year"))


# ── SECTION F: Historical stacked data for panels g/h, per region -----------

cat("F: Historical totals for g/h by region\n")

dmc_total_hist <- dmc_hist |>
  mutate(
    mat_group = case_when(
      material_category %in% BIOMASS_CATS ~ "Biomass",
      material_category %in% FOSSIL_CATS ~ "Fossil fuels",
      material_category %in% METAL_CATS ~ "Metal ores",
      material_category %in% NONMET_CATS ~ "Non-metallic minerals",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(mat_group), year <= HIST_END) |>
  group_by(region, year, mat_group) |>
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Stacking order fixed globally (world totals) so colour/order match Figure 2 across all regions
mat_order <- dmc_total_hist |>
  group_by(mat_group) |>
  summarise(total = sum(DMC_Gt), .groups = "drop") |>
  arrange(total) |>
  pull(mat_group)
dmc_total_hist <- dmc_total_hist |> mutate(mat_group = factor(mat_group, levels = mat_order))
dmc_total_hist_sum <- dmc_total_hist |> group_by(region, year) |> summarise(DMC_Gt = sum(DMC_Gt), .groups = "drop")

dmc_stack_labels <- dmc_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  group_by(region) |>
  arrange(desc(mat_group), .by_group = TRUE) |>
  mutate(cum_top = cumsum(DMC_Gt), cum_bot = dplyr::lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  ungroup() |>
  mutate(label = as.character(mat_group))

stock_total_hist <- stock_hist_raw |>
  filter(material %in% c("Metal ores", "Non-metallic minerals"), end_use %in% STOCK_ENDUSE_HIST) |>
  mutate(end_use_label = ENDUSE_LABELS[end_use]) |>
  group_by(region, year, end_use_label) |>
  summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e6, .groups = "drop") |>
  filter(year <= HIST_END)

enduse_order <- stock_total_hist |>
  group_by(end_use_label) |>
  summarise(total = sum(stock_Gt), .groups = "drop") |>
  arrange(total) |>
  pull(end_use_label)
stock_total_hist <- stock_total_hist |> mutate(end_use_label = factor(end_use_label, levels = enduse_order))
stock_total_hist_sum <- stock_total_hist |> group_by(region, year) |> summarise(stock_Gt = sum(stock_Gt), .groups = "drop")

stock_stack_labels <- stock_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  group_by(region) |>
  arrange(desc(end_use_label), .by_group = TRUE) |>
  mutate(cum_top = cumsum(stock_Gt), cum_bot = dplyr::lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  ungroup() |>
  filter(end_use_label %in% c("Buildings", "Civil infrastructure"))


# ── SECTION F2: Anchor MC envelopes to actual 2024 values, per region -------

cat("F2: Anchoring MC envelopes to 2024 historical values\n")

pop_env <- pop_env |>
  filter(year > HIST_END) |>
  bind_rows(
    pop_region_hist |>
      filter(year == HIST_END) |>
      transmute(region, year, p_min = population / 1e9, p25 = population / 1e9, p50 = population / 1e9,
                p75 = population / 1e9, p_max = population / 1e9)
  )

gdpcap_env <- gdpcap_env |>
  filter(year > HIST_END) |>
  bind_rows(
    gdp_region_hist |>
      left_join(pop_region_hist, by = c("region", "year")) |>
      filter(year == HIST_END) |>
      transmute(region, year, p_min = GDP_2015USD / population / 1e3, p25 = GDP_2015USD / population / 1e3,
                p50 = GDP_2015USD / population / 1e3, p75 = GDP_2015USD / population / 1e3,
                p_max = GDP_2015USD / population / 1e3)
  )

biomass_env <- biomass_env |>
  filter(year > HIST_END) |>
  bind_rows(
    biomass_hist |>
      filter(year == HIST_END) |>
      transmute(region, material_key = material_category, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

fossil_env <- fossil_env |>
  filter(year > HIST_END) |>
  bind_rows(
    fossil_hist |>
      filter(year == HIST_END) |>
      transmute(region, material_key = material_category, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

metal_env <- metal_env |>
  filter(year > HIST_END) |>
  bind_rows(
    metal_hist_eu |>
      filter(year == HIST_END) |>
      transmute(region, end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

nonmet_env <- nonmet_env |>
  filter(year > HIST_END) |>
  bind_rows(
    nonmet_hist_eu |>
      filter(year == HIST_END) |>
      transmute(region, end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

dmc_env <- dmc_env |>
  filter(year > HIST_END) |>
  bind_rows(
    dmc_total_hist_sum |>
      filter(year == HIST_END) |>
      transmute(region, year, p_min = DMC_Gt, p25 = DMC_Gt, p50 = DMC_Gt, p75 = DMC_Gt, p_max = DMC_Gt)
  )

stock_env <- stock_env |>
  filter(year > HIST_END) |>
  bind_rows(
    stock_total_hist_sum |>
      filter(year == HIST_END) |>
      transmute(region, year, p_min = stock_Gt, p25 = stock_Gt, p50 = stock_Gt, p75 = stock_Gt, p_max = stock_Gt)
  )


# ── SECTION G: Build + save one 8-panel figure per region --------------------

cat("G: Building region figures\n")

dir.create("Figures/Figure 2 Detail/Region", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures/SVG/Figure 2 Detail/Region", recursive = TRUE, showWarnings = FALSE)

bio_range_base <- expand_grid(material_key = BIOMASS_CATS) |>
  mutate(pos_x = case_when(
    material_key == "Crops" ~ 0, material_key == "Crop Residues" ~ 1, str_detect(material_key, "Grazed") ~ 2, T ~ 0
  ))
fossil_range_base <- expand_grid(material_key = FOSSIL_CATS) |>
  mutate(pos_x = case_when(
    material_key == "Coal" ~ 0, material_key == "Petroleum" ~ 1, str_detect(material_key, "Natural") ~ 2, T ~ 0
  ))
metal_range_base <- expand_grid(end_use_label = unique(metal_env$end_use_label)) |>
  mutate(pos_x = case_when(
    end_use_label == "Roads" ~ 0, str_detect(end_use_label, "Civil") ~ 1, end_use_label == "Machinery" ~ 2,
    end_use_label == "Residential Bldg" ~ 0, end_use_label == "Non-residential Bldg" ~ 1, end_use_label == "Vehicles" ~ 2,
    end_use_label == "Durables" ~ 0, end_use_label == "Packaging" ~ 1, T ~ 0
  ))
nonmet_range_base <- expand_grid(end_use_label = unique(nonmet_env$end_use_label)) |>
  mutate(pos_x = case_when(
    end_use_label == "Residential Bldg" ~ 0, end_use_label == "Non-residential Bldg" ~ 1, end_use_label == "Roads" ~ 2,
    str_detect(end_use_label, "Civil") ~ 3, T ~ 0
  ))

for (rg in REGIONS) {
  cat("  -", rg, "\n")
  region_color <- PALETTE_REGIONS[[rg]]

  pop_env_r <- pop_env |> filter(region == rg)
  gdpcap_env_r <- gdpcap_env |> filter(region == rg)
  biomass_env_r <- biomass_env |> filter(region == rg)
  fossil_env_r <- fossil_env |> filter(region == rg)
  metal_env_r <- metal_env |> filter(region == rg)
  nonmet_env_r <- nonmet_env |> filter(region == rg)
  dmc_env_r <- dmc_env |> filter(region == rg)
  stock_env_r <- stock_env |> filter(region == rg)

  pop_hist_r <- pop_region_hist |> filter(region == rg, year <= HIST_END) |> mutate(v = population / 1e9)
  gdp_cap_hist_r <- gdp_region_hist |>
    left_join(pop_region_hist, by = c("region", "year")) |>
    filter(region == rg, year <= HIST_END) |>
    mutate(v = GDP_2015USD / population / 1e3)
  biomass_hist_r <- biomass_hist |> filter(region == rg)
  fossil_hist_r <- fossil_hist |> filter(region == rg)
  metal_hist_eu_r <- metal_hist_eu |> filter(region == rg)
  nonmet_hist_eu_r <- nonmet_hist_eu |> filter(region == rg)
  dmc_total_hist_r <- dmc_total_hist |> filter(region == rg)
  dmc_total_hist_sum_r <- dmc_total_hist_sum |> filter(region == rg)
  dmc_stack_labels_r <- dmc_stack_labels |> filter(region == rg)
  stock_total_hist_r <- stock_total_hist |> filter(region == rg)
  stock_total_hist_sum_r <- stock_total_hist_sum |> filter(region == rg)
  stock_stack_labels_r <- stock_stack_labels |> filter(region == rg)

  # Direct-label angle = local tangent of the line at the label year (slope over
  # +-10 yrs, normalised by panel's y-range / x-range, atan()'d to degrees,
  # snapped to 0 within +-10deg of horizontal). No per-category overrides.
  bio_yrange <- max(biomass_env_r$p_max, biomass_hist_r$mg, na.rm = TRUE)
  bio_slope <- biomass_hist_r |>
    filter(year %in% c(LABEL_YEAR - 10, LABEL_YEAR + 10)) |>
    dplyr::select(material_category, year, mg) |>
    pivot_wider(names_from = year, values_from = mg, names_prefix = "yr") |>
    mutate(
      angle = atan(
        (.data[[paste0("yr", LABEL_YEAR + 10)]] - .data[[paste0("yr", LABEL_YEAR - 10)]]) / 20 / (bio_yrange / X_RANGE_YR)
      ) * 180 / pi,
      angle = if_else(abs(angle) <= 10, 0, angle)
    ) |>
    dplyr::select(material_category, angle)
  bio_labels <- biomass_hist_r |>
    filter(year == LABEL_YEAR) |>
    mutate(label_x = str_remove(material_category, " and fodder crops")) |>
    left_join(bio_slope, by = "material_category")

  fossil_yrange <- max(fossil_env_r$p_max, fossil_hist_r$mg, na.rm = TRUE)
  fossil_slope <- fossil_hist_r |>
    filter(year %in% c(LABEL_YEAR - 10, LABEL_YEAR + 10)) |>
    dplyr::select(material_category, year, mg) |>
    pivot_wider(names_from = year, values_from = mg, names_prefix = "yr") |>
    mutate(
      angle = atan(
        (.data[[paste0("yr", LABEL_YEAR + 10)]] - .data[[paste0("yr", LABEL_YEAR - 10)]]) / 20 / (fossil_yrange / X_RANGE_YR)
      ) * 180 / pi,
      angle = if_else(abs(angle) <= 10, 0, angle)
    ) |>
    dplyr::select(material_category, angle)
  fossil_labels <- fossil_hist_r |> filter(year == LABEL_YEAR) |> left_join(fossil_slope, by = "material_category")

  metal_yrange <- max(metal_env_r$p_max, metal_hist_eu_r$mg, na.rm = TRUE)
  metal_slope <- metal_hist_eu_r |>
    filter(year == HIST_LABEL_YEAR - 10) |>
    dplyr::select(end_use_label, mg_lo = mg) |>
    left_join(
      metal_env_r |> filter(year == HIST_LABEL_YEAR + 4) |> dplyr::select(end_use_label, mg_hi = p50),
      by = "end_use_label"
    ) |>
    mutate(
      angle = atan((mg_hi - mg_lo) / 20 / (metal_yrange / X_RANGE_YR)) * 180 / pi,
      angle = if_else(abs(angle) <= 10, 0, angle)
    ) |>
    dplyr::select(end_use_label, angle)
  metal_hist_labels <- metal_hist_eu_r |>
    filter(year == HIST_LABEL_YEAR) |>
    left_join(metal_slope, by = "end_use_label") |>
    mutate(aux_label = str_replace(end_use_label, "tial Bldg", "tial\nBldg"))

  nonmet_yrange <- max(nonmet_env_r$p_max, nonmet_hist_eu_r$mg, na.rm = TRUE)
  nonmet_slope <- nonmet_hist_eu_r |>
    filter(year == HIST_LABEL_YEAR - 10) |>
    dplyr::select(end_use_label, mg_lo = mg) |>
    left_join(
      nonmet_env_r |> filter(year == HIST_LABEL_YEAR + 4) |> dplyr::select(end_use_label, mg_hi = p50),
      by = "end_use_label"
    ) |>
    mutate(
      angle = atan((mg_hi - mg_lo) / 20 / (nonmet_yrange / X_RANGE_YR)) * 180 / pi,
      angle = if_else(abs(angle) <= 10, 0, angle)
    ) |>
    dplyr::select(end_use_label, angle)
  nonmet_hist_labels <- nonmet_hist_eu_r |>
    filter(year == HIST_LABEL_YEAR, end_use_label %in% F_LABEL_ENDUSES) |>
    left_join(nonmet_slope, by = "end_use_label")

  bio_range <- biomass_env_r |> filter(year == PROJ_END) |> left_join(bio_range_base, by = "material_key")
  fossil_range <- fossil_env_r |> filter(year == PROJ_END) |> left_join(fossil_range_base, by = "material_key")
  metal_range <- metal_env_r |> filter(year == PROJ_END) |> left_join(metal_range_base, by = "end_use_label")
  nonmet_range <- nonmet_env_r |> filter(year == PROJ_END) |> left_join(nonmet_range_base, by = "end_use_label")

  ## Panel (a) — Population, with a legend box scaled to this region's own range
  pop_ymax <- max(pop_env_r$p_max, pop_hist_r$v, na.rm = TRUE)
  leg_y <- pop_ymax * c(min = 0.68, lo = 0.755, mid = 0.83, hi = 0.905, max = 0.98)

  p1 <- ggplot() +
    geom_ribbon(data = pop_env_r, aes(x = year, ymin = p_min, ymax = p_max), fill = region_color, alpha = RIBBON_ALPHA_MINMAX) +
    geom_ribbon(data = pop_env_r, aes(x = year, ymin = p25, ymax = p75), fill = region_color, alpha = RIBBON_ALPHA_IQR) +
    geom_line(data = pop_hist_r, aes(x = year, y = v), colour = region_color, linewidth = HIST_LW) +
    geom_line(
      data = pop_env_r |> filter(year >= HIST_END), aes(x = year, y = p50),
      colour = region_color, linewidth = PROJ_LW, linetype = "dashed"
    ) +
    annotate("rect", xmin = 1978, xmax = 1989, ymin = leg_y[["min"]], ymax = leg_y[["max"]], fill = "grey50", alpha = RIBBON_ALPHA_MINMAX, colour = NA) +
    annotate("rect", xmin = 1978, xmax = 1989, ymin = leg_y[["lo"]], ymax = leg_y[["hi"]], fill = "grey50", alpha = RIBBON_ALPHA_IQR, colour = NA) +
    annotate("segment", x = 1977, xend = 1990, y = leg_y[["mid"]], yend = leg_y[["mid"]], colour = "grey35", linewidth = PROJ_LW, linetype = "dashed") +
    annotate("text", x = 1991, y = leg_y[["max"]], label = "Max", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
    annotate("text", x = 1991, y = leg_y[["hi"]], label = paste0(round(CI_HI * 100), "%ile"), hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
    annotate("text", x = 1991, y = leg_y[["mid"]], label = "median", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
    annotate("text", x = 1991, y = leg_y[["lo"]], label = paste0(round(CI_LO * 100), "%ile"), hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
    annotate("text", x = 1991, y = leg_y[["min"]], label = "Min", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
    present_line_bot +
    x_sc +
    y_sc +
    co +
    panel_tag("a") +
    labs(x = NULL, y = "Population (billion)", title = "Population") +
    theme_pb_large() +
    FONT_BUMP

  ## Panel (b) — GDP per capita
  p2 <- ggplot() +
    geom_ribbon(data = gdpcap_env_r, aes(x = year, ymin = p_min, ymax = p_max), fill = region_color, alpha = RIBBON_ALPHA_MINMAX) +
    geom_ribbon(data = gdpcap_env_r, aes(x = year, ymin = p25, ymax = p75), fill = region_color, alpha = RIBBON_ALPHA_IQR) +
    geom_line(data = gdp_cap_hist_r, aes(x = year, y = v), colour = region_color, linewidth = HIST_LW) +
    geom_line(
      data = gdpcap_env_r |> filter(year >= HIST_END), aes(x = year, y = p50),
      colour = region_color, linewidth = PROJ_LW, linetype = "dashed"
    ) +
    present_line_top +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    panel_tag("b") +
    labs(x = NULL, y = "GDP per capita ('000 $/p)", title = "GDP per capita") +
    theme_pb_large() +
    FONT_BUMP

  ## Panel (c) — M/G Biomass
  p3 <- ggplot() +
    geom_ribbon(data = biomass_env_r, aes(x = year, ymin = p_min, ymax = p_max, fill = material_key), alpha = RIBBON_ALPHA_MINMAX, show.legend = FALSE) +
    geom_ribbon(data = biomass_env_r, aes(x = year, ymin = p25, ymax = p75, fill = material_key), alpha = RIBBON_ALPHA_IQR, show.legend = FALSE) +
    geom_line(data = biomass_hist_r, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
    geom_line(data = biomass_env_r |> filter(year >= HIST_END), aes(x = year, y = p50, colour = material_key), linewidth = PROJ_LW, linetype = "dashed") +
    geom_rect(data = bio_range, aes(xmin = PROJ_END + pos_x + 1, xmax = PROJ_END + pos_x + 1.45, ymin = p_min, ymax = p_max, fill = material_key), color = NA) +
    geom_text(
      data = bio_labels, aes(x = LABEL_YEAR, y = mg, colour = material_category, label = label_x, angle = angle),
      size = LABEL_SZ, show.legend = FALSE, nudge_y = 0.04 * bio_yrange
    ) +
    present_line +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
    scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
    panel_tag("c", side = "right") +
    labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Biomass") +
    theme_pb_large() +
    FONT_BUMP

  ## Panel (d/e) — M/G Fossil fuels
  p4 <- ggplot() +
    geom_ribbon(data = fossil_env_r, aes(x = year, ymin = p_min, ymax = p_max, fill = material_key), alpha = RIBBON_ALPHA_MINMAX, show.legend = FALSE) +
    geom_ribbon(data = fossil_env_r, aes(x = year, ymin = p25, ymax = p75, fill = material_key), alpha = RIBBON_ALPHA_IQR, show.legend = FALSE) +
    geom_line(data = fossil_hist_r, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
    geom_line(data = fossil_env_r |> filter(year >= HIST_END), aes(x = year, y = p50, colour = material_key), linewidth = PROJ_LW, linetype = "dashed") +
    geom_rect(data = fossil_range, aes(xmin = PROJ_END + pos_x + 1, xmax = PROJ_END + pos_x + 1.45, ymin = p_min, ymax = p_max, fill = material_key), color = NA) +
    geom_text(
      data = fossil_labels, aes(x = LABEL_YEAR, y = mg, colour = material_category, label = material_category, angle = angle),
      size = LABEL_SZ, show.legend = FALSE, nudge_y = 0.04 * fossil_yrange, nudge_x = 2
    ) +
    present_line +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
    scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
    panel_tag("e", side = "right") +
    labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Fossil fuels") +
    theme_pb_large() +
    FONT_BUMP

  ## Panel (d) — S/G Metal ores by end-use
  p5 <- ggplot() +
    geom_ribbon(data = metal_env_r, aes(x = year, ymin = p_min, ymax = p_max, fill = end_use_label), alpha = RIBBON_ALPHA_MINMAX, show.legend = FALSE) +
    geom_ribbon(data = metal_env_r, aes(x = year, ymin = p25, ymax = p75, fill = end_use_label), alpha = RIBBON_ALPHA_IQR, show.legend = FALSE) +
    geom_line(data = metal_hist_eu_r, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
    geom_line(data = metal_env_r |> filter(year >= HIST_END), aes(x = year, y = p50, colour = end_use_label), linewidth = PROJ_LW, linetype = "dashed") +
    geom_rect(data = metal_range, aes(xmin = PROJ_END + pos_x + 1, xmax = PROJ_END + pos_x + 1.45, ymin = p_min, ymax = p_max, fill = end_use_label), color = NA) +
    geom_text(
      data = metal_hist_labels, aes(x = year, y = mg, label = aux_label, colour = end_use_label, angle = angle),
      nudge_y = 0.02 * metal_yrange, nudge_x = -2, hjust = 1, lineheight = 0.8, size = LABEL_SZ, show.legend = FALSE
    ) +
    present_line +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
    scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
    panel_tag("d", side = "right") +
    labs(x = "", y = "Stock per GDP (kg/$)", title = "S/G Metal ores") +
    theme_pb_large() +
    FONT_BUMP

  ## Panel (f) — S/G Non-metallic minerals by end-use
  p6 <- ggplot() +
    geom_ribbon(data = nonmet_env_r, aes(x = year, ymin = p_min, ymax = p_max, fill = end_use_label), alpha = RIBBON_ALPHA_MINMAX, show.legend = FALSE) +
    geom_ribbon(data = nonmet_env_r, aes(x = year, ymin = p25, ymax = p75, fill = end_use_label), alpha = RIBBON_ALPHA_IQR, show.legend = FALSE) +
    geom_line(data = nonmet_hist_eu_r, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
    geom_line(data = nonmet_env_r |> filter(year >= HIST_END), aes(x = year, y = p50, colour = end_use_label), linewidth = PROJ_LW, linetype = "dashed") +
    geom_rect(data = nonmet_range, aes(xmin = PROJ_END + pos_x + 1, xmax = PROJ_END + pos_x + 1.45, ymin = p_min, ymax = p_max, fill = end_use_label), color = NA) +
    geom_text(
      data = nonmet_hist_labels, aes(x = year, y = mg, label = end_use_label, colour = end_use_label, angle = angle),
      nudge_y = 0.02 * nonmet_yrange, nudge_x = -2, hjust = 1, size = LABEL_SZ, show.legend = FALSE
    ) +
    present_line +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
    scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
    panel_tag("f") +
    labs(x = "", y = "Stock per GDP (kg/$)", title = "S/G Non-metallic minerals") +
    theme_pb_large() +
    FONT_BUMP

  # Panels g/h: relative band thickness varies a lot by region (unlike the
  # world total), so a fixed-position direct label collides across bands.
  # Use a compact in-panel legend instead (still inside the plot, no overlap).
  stack_legend_theme <- theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = FONT_AXIS_L - 1),
    legend.title = element_blank()
  )

  ## Panel (g) — Total material consumption
  p7 <- ggplot() +
    geom_area(data = dmc_total_hist_r, aes(x = year, y = DMC_Gt, fill = mat_group), position = "stack", alpha = 0.5, colour = NA) +
    geom_line(data = dmc_total_hist_sum_r, aes(x = year, y = DMC_Gt), colour = "black", linewidth = HIST_LW) +
    geom_ribbon(data = dmc_env_r, aes(x = year, ymin = p25, ymax = p75), fill = "grey60", alpha = RIBBON_ALPHA_IQR) +
    geom_line(data = dmc_env_r |> filter(year >= HIST_END), aes(x = year, y = p50), colour = "black", linewidth = PROJ_LW, linetype = "dashed") +
    present_line +
    x_sc +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    co +
    scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, name = NULL) +
    panel_tag("g") +
    labs(x = NULL, y = "Material consumption (Gt)", title = "Total material consumption") +
    theme_pb_large() +
    FONT_BUMP +
    stack_legend_theme

  ## Panel (h) — Total in-use stock
  p8 <- ggplot() +
    geom_area(data = stock_total_hist_r, aes(x = year, y = stock_Gt, fill = end_use_label), position = "stack", alpha = 0.5, colour = NA) +
    geom_line(data = stock_total_hist_sum_r, aes(x = year, y = stock_Gt), colour = "black", linewidth = HIST_LW) +
    geom_ribbon(data = stock_env_r, aes(x = year, ymin = p25, ymax = p75), fill = "grey60", alpha = RIBBON_ALPHA_IQR) +
    geom_line(data = stock_env_r |> filter(year >= HIST_END), aes(x = year, y = p50), colour = "black", linewidth = PROJ_LW, linetype = "dashed") +
    present_line +
    x_sc +
    y_sc +
    co +
    scale_fill_manual(values = PALETTE_ENDUSE, name = NULL) +
    panel_tag("h") +
    labs(x = "", y = "In-use stock ('000 Gt)", title = "Total stock") +
    theme_pb_large() +
    FONT_BUMP +
    stack_legend_theme

  fig <- wrap_plots(p1, p2, p3, p5, p4, p6, p7, p8, ncol = 2) +
    plot_annotation(title = rg, theme = theme(plot.title = element_text(size = FONT_TITLE + 1, face = "bold", hjust = 0.5))) &
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(5.5, 5.5, 5.5, 5.5)
    )

  rg_file <- gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", rg))
  ggsave(paste0("Figures/Figure 2 Detail/Region/", rg_file, ".png"), fig, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 3)
  ggsave(paste0("Figures/SVG/Figure 2 Detail/Region/", rg_file, ".svg"), fig, units = "cm", width = 8.7 * 2, height = 8.7 * 3)
  group_svg_layers(paste0("Figures/SVG/Figure 2 Detail/Region/", rg_file, ".svg"))
}

cat("  Saved:", length(REGIONS), "region figures to Figures/Figure 2 Detail/Region/\n")

# EoF
