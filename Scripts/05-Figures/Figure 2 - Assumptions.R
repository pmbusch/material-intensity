## =============================================================================
## Figure 2 - Assumptions.R  -> Figures/Fig2 - Assumptions.png
## Eight-panel Kaya figure: historical (1970-2024) + MC projected (2025-FORECAST_END).
##   (a) Global population
##   (b) World GDP per capita
##   (c) M/G Biomass sub-materials
##   (d) M/G Fossil fuels sub-materials
##   (e) S/G Metal ores by end-use
##   (f) S/G Non-metallic minerals by end-use
##   (g) Total material consumption (Gt)
##   (h) Total in-use stock (Gt)
##
## Projection: MC median (dashed) + 90% CI band (P5–P95) from 10K runs.
## Pop/GDP: each run blends the two SSPs bracketing its single
## continuous ssp_u draw (see 02-RunSimulations.R); no
## discrete ssp_label exists in results, so world pop/GDP per run are
## reconstructed region-by-region from that blend (SECTION C2).
## M/G, S/G: derived post-hoc as DMC or stock / world GDP per run.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
library(patchwork)

# ── Constants ----------------------------------------------------------------

HIST_END <- 2024L
PROJ_END <- FORECAST_END # central projection horizon, Scripts/00-CommonParameters.R
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

POP_COLOR <- "#1F618D"
GDPCAP_COLOR <- "#117A65"
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
POWER_LABEL_YEAR <- 1990L # panel d: label on the historical power-sector line
SIDE_BAR_W <- 1.35 # panels g/h: composition bar width (3x the 0.45 range bars of panels c-f)
HIST_LABEL_X <- HIST_END + 2 # panels g/h: x of the rotated HIST_END share labels, just right of the historical stack

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
    "text",
    x = HIST_END - 2,
    y = Inf,
    label = "Present (2024)",
    hjust = 1.05,
    vjust = 0.5,
    size = LABEL_SZ,
    colour = "grey45",
    angle = 90
  )
)
present_line_bot <- list(
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"),
  annotate(
    "text",
    x = HIST_END - 2,
    y = -Inf,
    label = "Present (2024)",
    hjust = -0.05,
    vjust = 0.5,
    size = LABEL_SZ,
    colour = "grey45",
    angle = 90
  )
)
present_line_right <- list(
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"),
  annotate(
    "text",
    x = HIST_END + 2,
    y = Inf,
    label = "Present (2024)",
    hjust = 1.05,
    vjust = 0.5,
    size = LABEL_SZ,
    colour = "grey45",
    angle = 90
  )
)
present_line <- list(geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"))


x_sc <- scale_x_continuous(breaks = seq(1970, PROJ_END, 10), labels = function(x) {
  ifelse(x %in% seq(1970, 2050, 20), x, "")
})
y_sc <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
co <- coord_cartesian(xlim = c(1970, PROJ_END), ylim = c(0, NA), clip = "off", expand = FALSE)
X_RANGE_YR <- PROJ_END - 1970 # shared x-axis span, used to normalise label slope -> angle

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


# ── SECTION A: Load historical data -----------------------------------------

cat("A: Loading historical data\n")

pop_world_hist <- read_csv("Parameters/UN-Population/population_world_historical.csv", show_col_types = FALSE)
gdp_world_hist <- read_csv("Parameters/Worldbank-GDP/gdp_world.csv", show_col_types = FALSE)
pop_region_hist <- read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
stock_hist_raw <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE)
stock_subenduse_hist <- read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE)
hist_flows_model <- read_csv("Results/MC/hist_flows_model_basis.csv", show_col_types = FALSE)


# ── SECTION B: Load MC results -----------------------------------------------

cat("B: Loading MC results\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(
    material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group),
    # The four power-sector end uses (02-RunSimulations.R) shown as one "Power sector" end use
    material_key = ifelse(startsWith(material_key, "Power: "), "Power sector", material_key)
  )
cat("  Runs:", n_distinct(results$run_id), "| Years:", paste(range(results$year), collapse = "-"), "\n")

# One row per run with its continuous SSP blend (population & GDP/capita independent)
run_ssp <- results |>
  distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)


# ── SECTION C: 2024 regional GDP/population baselines ------------------------
# (region-level anchors for the continuous per-run SSP blend built in C2)

cat("C: Regional 2024 baselines\n")

gdp_2024_region <- gdp_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)


# ── SECTION C2: Per-run world GDP & population from the continuous blend ----
# Each run blends two bracketing SSPs independently for population and for
# GDP-per-capita (see 02-RunSimulations.R STEP 3/4). World GDP total is not
# itself one of the two draws -- it is reconstructed region-by-region as
# GDP_2024 * pop_idx_blend(region,t) * gdppc_idx_blend(region,t), since
# GDP_total_index = Population_index * GDP_per_capita_index exactly (see
# Scripts/03-SSP Trajectories/01_preprocess_ssp_drivers.R, Step 5).

cat("C2: Per-run world GDP/population from continuous SSP blend\n")

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(pop_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(
    pop_idx_region |> rename(ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  mutate(pop_idx_blend = ssp_share_lo * pop_idx + (1 - ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(
    gdppc_idx_region |> rename(ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  mutate(gdppc_idx_blend = ssp_share_lo * gdppc_idx + (1 - ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

world_pop_by_run <- pop_idx_blend |>
  left_join(pop_2024_region, by = "region") |>
  mutate(pop_run = pop_2024 * pop_idx_blend) |>
  group_by(run_id, year) |>
  summarise(world_pop = sum(pop_run, na.rm = TRUE), .groups = "drop")

world_gdp_by_run <- pop_idx_blend |>
  left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend) |>
  group_by(run_id, year) |>
  summarise(world_gdp = sum(gdp_run, na.rm = TRUE), .groups = "drop")


# ── SECTION D: Historical M/G and S/G by sub-material / end-use -------------

cat("D: Historical M/G and S/G\n")

biomass_hist <- dmc_hist |>
  filter(
    material_category %in%
      c(BIOMASS_CATS_NAMED, "Wild catch and harvest", "Non-wild animal products", "Products mainly from biomass nec.")
  ) |>
  mutate(material_category = if_else(material_category %in% BIOMASS_CATS_NAMED, material_category, "Other biomass")) |>
  group_by(year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
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
  group_by(year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

# Power sector history (panels d/f): EIA installed capacity per region, 1980-HIST_END
# (01-PrepareData/06-Aggregate_EIA_Capacity.R) x the model's static material
# intensities (power_material_intensity.csv, t/GW and t/GWh). EIA aggregates are
# split into model technologies with each region's 2025 model anchor mix (fossil
# -> coal/gas/oil, solar -> PV/CSP, wind -> onshore/offshore), held constant;
# batteries = BATTERY_GWH_PER_GW x (solar + wind) GW, as in the model. Pumped
# storage and tide/wave have no model technology.
eia_gw <- read_csv("Parameters/EIA-Capacity/capacity_region_historical.csv", show_col_types = FALSE) |>
  filter(year <= HIST_END)

EIA_SPLIT <- c(coal = "fossil", gas = "fossil", oil = "fossil", solar_pv = "solar", solar_csp = "solar", wind_onshore = "wind", wind_offshore = "wind")
power_split <- read_csv("Parameters/Simulation/power_capacity_anchor.csv", show_col_types = FALSE) |>
  mutate(eia_tech = coalesce(unname(EIA_SPLIT[tech]), tech)) |>
  group_by(region, eia_tech) |>
  mutate(split = if (sum(cap_2025_gw) > 0) cap_2025_gw / sum(cap_2025_gw) else 1 / n()) |>
  ungroup() |>
  dplyr::select(region, tech, eia_tech, split)

power_units_hist <- eia_gw |>
  inner_join(power_split, by = c("region", "eia_tech"), relationship = "many-to-many") |>
  transmute(region, year, unit_key = tech, units = gw * split)
power_units_hist <- bind_rows(
  power_units_hist,
  eia_gw |>
    filter(eia_tech %in% c("solar", "wind")) |>
    group_by(region, year) |>
    summarise(units = BATTERY_GWH_PER_GW * sum(gw), .groups = "drop") |>
    mutate(unit_key = "battery")
)

# Region x year x model material (Metal_Fe, Metal_NonFe, Non-metallic minerals), Mt
POWER_MATERIAL_LABEL <- c("metal_fe" = "Metal_Fe", "metal_nonfe" = "Metal_NonFe", "nonmetallic_minerals" = "Non-metallic minerals")
power_hist_region <- power_units_hist |>
  inner_join(read_csv("Parameters/Simulation/power_material_intensity.csv", show_col_types = FALSE), by = "unit_key", relationship = "many-to-many") |>
  mutate(material = unname(POWER_MATERIAL_LABEL[material_group])) |>
  group_by(Region = region, material, year) |>
  summarise(power_Mt = sum(units * t_per_unit) / 1e6, .groups = "drop") # GW x t/GW -> Mt

# Same carve-out as the model (01b-PowerSector.R): the power stock is removed from
# civil engineering, per region x material, in every historical year
stock_subenduse_hist <- stock_subenduse_hist |>
  left_join(power_hist_region |> mutate(sub_use = POWER_CARVEOUT_SUB_USE), by = c("Region", "material", "sub_use", "year")) |>
  mutate(stock_Mt = stock_Mt - coalesce(power_Mt, 0)) |>
  dplyr::select(-power_Mt)
neg_hist <- stock_subenduse_hist |> filter(sub_use == POWER_CARVEOUT_SUB_USE, stock_Mt < 0)
if (nrow(neg_hist) > 0) {
  print(neg_hist |> arrange(stock_Mt) |> head(20) |> as.data.frame())
  stop("Historical power carve-out makes ", POWER_CARVEOUT_SUB_USE, " stock negative (", nrow(neg_hist), " region x material x year cells) -- see table above")
}

power_hist_mg <- power_hist_region |>
  mutate(is_metal = material != "Non-metallic minerals") |>
  group_by(year, is_metal) |>
  summarise(stock_Mt = sum(power_Mt), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(end_use_label = "Power sector", mg = stock_Mt * 1e9 / GDP_2015USD)

# EIA starts in 1980: S/G extended back to 1970 (display only) with the average
# annual change over 1980-1990, linear from the 1980 value (floored at 0)
POWER_BACKCAST_FROM <- 1970L
power_slope <- power_hist_mg |>
  group_by(is_metal) |>
  summarise(mg_1980 = mg[year == 1980L], slope = (mg[year == 1990L] - mg[year == 1980L]) / 10, .groups = "drop")
power_hist_mg <- bind_rows(
  power_slope |>
    tidyr::crossing(year = seq(POWER_BACKCAST_FROM, 1979L)) |>
    transmute(year, is_metal, end_use_label = "Power sector", mg = pmax(0, mg_1980 - slope * (1980L - year))),
  power_hist_mg
) |>
  arrange(is_metal, year)

power_hist_metal <- power_hist_mg |> filter(is_metal)
power_hist_nonmet <- power_hist_mg |> filter(!is_metal)

cat(
  "  Power sector (EIA x model t/GW), world stock", HIST_END, ": metals", round(power_hist_metal$stock_Mt[power_hist_metal$year == HIST_END]),
  "Mt, minerals", round(power_hist_nonmet$stock_Mt[power_hist_nonmet$year == HIST_END]), "Mt (model 2025 carve-out:",
  round(sum(read_csv("Parameters/Simulation/power_carveout_2025.csv", show_col_types = FALSE) |> filter(material != "Non-metallic minerals") |> pull(carve_Mt))), "/",
  round(sum(read_csv("Parameters/Simulation/power_carveout_2025.csv", show_col_types = FALSE) |> filter(material == "Non-metallic minerals") |> pull(carve_Mt))), "Mt)\n"
)

# Panels e/f: 8 sub-end-use historical lines from sub-enduse stock file (civil engineering net of power)
metal_hist_eu <- stock_subenduse_hist |>
  filter(material %in% c("Metal_Fe", "Metal_NonFe")) |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)

nonmet_hist_eu <- stock_subenduse_hist |>
  filter(material == "Non-metallic minerals") |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)


# ── SECTION E: MC projection envelopes (P5 / P50 / P95) ---------------------

cat("E: MC projection envelopes\n")

## Pop: one value per (run, year) from the continuous SSP blend
pop_env <- world_pop_by_run |> mutate(v = world_pop / 1e9) |> env_quantiles("v", "year")

## GDP/cap: same
gdpcap_env <- world_pop_by_run |>
  left_join(world_gdp_by_run, by = c("run_id", "year")) |>
  mutate(gdpcap = world_gdp / world_pop / 1e3) |>
  env_quantiles("gdpcap", "year")

## Biomass M/G per run × sub-material × year
biomass_env <- results |>
  filter(material_group == "biomass", material_key %in% BIOMASS_CATS) |>
  group_by(run_id, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_by_run, by = c("run_id", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp) |>
  env_quantiles("mg", c("material_key", "year"))

## Fossil M/G per run × sub-material × year
fossil_env <- results |>
  filter(material_group == "fossil_fuels", material_key %in% FOSSIL_CATS) |>
  group_by(run_id, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_by_run, by = c("run_id", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp) |>
  env_quantiles("mg", c("material_key", "year"))

## Metal S/G per run × end-use × year
metal_env <- results |>
  filter(material_group == "metal_ores") |>
  group_by(run_id, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_by_run, by = c("run_id", "year")) |>
  mutate(mg = stock_Mt * 1e9 / world_gdp) |>
  rename(end_use_label = material_key) |>
  env_quantiles("mg", c("end_use_label", "year"))

## Non-metallic S/G per run × end-use × year
nonmet_env <- results |>
  filter(material_group == "nonmetallic_minerals") |>
  group_by(run_id, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_by_run, by = c("run_id", "year")) |>
  mutate(mg = stock_Mt * 1e9 / world_gdp) |>
  rename(end_use_label = material_key) |>
  env_quantiles("mg", c("end_use_label", "year"))

## Total primary DMC
dmc_env <- results |>
  group_by(run_id, year) |>
  summarise(DMC_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  env_quantiles("DMC_Gt", "year")

## Total in-use stock (metal + non-metallic only)
stock_env <- results |>
  filter(material_group %in% c("metal_ores", "nonmetallic_minerals")) |>
  group_by(run_id, year) |>
  summarise(stock_Gt = sum(in_use_stock_Mt, na.rm = TRUE) / 1e6, .groups = "drop") |>
  env_quantiles("stock_Gt", "year")

# Panel g: per-run total material consumption growth 2025 -> PROJ_END (ratio
# and %/yr); median and 90% interval (p05-p95) across runs
dmc_growth_g <- results |>
  filter(year %in% c(2025L, PROJ_END)) |>
  group_by(run_id, year) |>
  summarise(DMC_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = DMC_Gt, names_prefix = "M") |>
  mutate(
    ratio = .data[[paste0("M", PROJ_END)]] / M2025,
    cagr = 100 * (ratio^(1 / (PROJ_END - 2025L)) - 1)
  )
cat("  Panel g -- total material consumption growth 2025 ->", PROJ_END, "across runs:\n")
print(
  tibble::tibble(
    stat = c("p05", "median", "p95"),
    Gt_2025 = round(quantile(dmc_growth_g$M2025, c(0.05, 0.5, 0.95), names = FALSE), 1),
    Gt_end = round(quantile(dmc_growth_g[[paste0("M", PROJ_END)]], c(0.05, 0.5, 0.95), names = FALSE), 1),
    ratio = round(quantile(dmc_growth_g$ratio, c(0.05, 0.5, 0.95), names = FALSE), 2),
    pct_yr = round(quantile(dmc_growth_g$cagr, c(0.05, 0.5, 0.95), names = FALSE), 2)
  ) |>
    as.data.frame()
)


# ── SECTION F: Historical stacked data for panels g/h -----------------------

cat("F: Historical totals for g/h\n")

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
  group_by(year, mat_group) |>
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  # Metals/minerals: historical primary on the model basis (DSM inflow - secondary,
  # 02-RunSimulations.R STEP 8) instead of UNEP DMC, so history meets the projection
  filter(mat_group %in% c("Biomass", "Fossil fuels")) |>
  bind_rows(
    hist_flows_model |>
      filter(year <= HIST_END) |>
      mutate(mat_group = if_else(material == "Non-metallic minerals", "Non-metallic minerals", "Metal ores")) |>
      group_by(year, mat_group) |>
      summarise(DMC_Gt = sum(primary_Mt, na.rm = TRUE) / 1e3, .groups = "drop")
  )

mat_order <- dmc_total_hist |>
  group_by(mat_group) |>
  summarise(total = sum(DMC_Gt), .groups = "drop") |>
  arrange(total) |>
  pull(mat_group)
dmc_total_hist <- dmc_total_hist |> mutate(mat_group = factor(mat_group, levels = mat_order))
dmc_total_hist_sum <- dmc_total_hist |> group_by(year) |> summarise(DMC_Gt = sum(DMC_Gt), .groups = "drop")

dmc_stack_labels <- dmc_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  arrange(desc(mat_group)) |>
  mutate(
    cum_top = cumsum(DMC_Gt),
    cum_bot = dplyr::lag(cum_top, default = 0),
    label_y = (cum_top + cum_bot) / 2,
    label = as.character(mat_group)
  )

stock_total_hist <- stock_hist_raw |>
  filter(material %in% c("Metal ores", "Non-metallic minerals"), end_use %in% STOCK_ENDUSE_HIST) |>
  mutate(end_use_label = ENDUSE_LABELS[end_use]) |>
  group_by(year, end_use_label) |>
  summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e6, .groups = "drop") |> # to thousand Gt
  filter(year <= HIST_END)

enduse_order <- stock_total_hist |>
  group_by(end_use_label) |>
  summarise(total = sum(stock_Gt), .groups = "drop") |>
  arrange(total) |>
  pull(end_use_label)
stock_total_hist <- stock_total_hist |> mutate(end_use_label = factor(end_use_label, levels = enduse_order))
stock_total_hist_sum <- stock_total_hist |> group_by(year) |> summarise(stock_Gt = sum(stock_Gt), .groups = "drop")

stock_stack_labels <- stock_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  arrange(desc(end_use_label)) |>
  mutate(cum_top = cumsum(stock_Gt), cum_bot = dplyr::lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  filter(end_use_label %in% c("Buildings", "Civil infrastructure"))


# ── SECTION F2: Anchor MC envelopes to actual 2024 values --------------------
# The MC-reconstructed 2024 value (SSP blend / stock-flow init) does not
# exactly equal the historical series it's spliced onto. Swap each envelope's
# HIST_END row for the true historical value so the dashed projection and its
# band start exactly where the solid historical line ends.

cat("F2: Anchoring MC envelopes to 2024 historical values\n")

pop_env <- pop_env |>
  filter(year > HIST_END) |>
  bind_rows(
    pop_world_hist |>
      filter(year == HIST_END) |>
      transmute(
        year,
        p_min = population / 1e9,
        p25 = population / 1e9,
        p50 = population / 1e9,
        p75 = population / 1e9,
        p_max = population / 1e9
      )
  )

gdpcap_env <- gdpcap_env |>
  filter(year > HIST_END) |>
  bind_rows(
    gdp_world_hist |>
      left_join(pop_world_hist, by = "year") |>
      filter(year == HIST_END) |>
      transmute(
        year,
        p_min = GDP_2015USD / population / 1e3,
        p25 = GDP_2015USD / population / 1e3,
        p50 = GDP_2015USD / population / 1e3,
        p75 = GDP_2015USD / population / 1e3,
        p_max = GDP_2015USD / population / 1e3
      )
  )

biomass_env <- biomass_env |>
  filter(year > HIST_END) |>
  bind_rows(
    biomass_hist |>
      filter(year == HIST_END) |>
      transmute(material_key = material_category, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

fossil_env <- fossil_env |>
  filter(year > HIST_END) |>
  bind_rows(
    fossil_hist |>
      filter(year == HIST_END) |>
      transmute(material_key = material_category, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

metal_env <- metal_env |>
  filter(year > HIST_END) |>
  bind_rows(
    metal_hist_eu |>
      filter(year == HIST_END) |>
      transmute(end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

nonmet_env <- nonmet_env |>
  filter(year > HIST_END) |>
  bind_rows(
    nonmet_hist_eu |>
      filter(year == HIST_END) |>
      transmute(end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
  )

# Anchor the power projection to the EIA HIST_END value, as for every other end use
metal_env <- bind_rows(
  metal_env,
  power_hist_metal |> filter(year == HIST_END) |> transmute(end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
)
nonmet_env <- bind_rows(
  nonmet_env,
  power_hist_nonmet |> filter(year == HIST_END) |> transmute(end_use_label, year, p_min = mg, p25 = mg, p50 = mg, p75 = mg, p_max = mg)
)

dmc_env <- dmc_env |>
  filter(year > HIST_END) |>
  bind_rows(
    dmc_total_hist_sum |>
      filter(year == HIST_END) |>
      transmute(year, p_min = DMC_Gt, p25 = DMC_Gt, p50 = DMC_Gt, p75 = DMC_Gt, p_max = DMC_Gt)
  )

stock_env <- stock_env |>
  filter(year > HIST_END) |>
  bind_rows(
    stock_total_hist_sum |>
      filter(year == HIST_END) |>
      transmute(year, p_min = stock_Gt, p25 = stock_Gt, p50 = stock_Gt, p75 = stock_Gt, p_max = stock_Gt)
  )


# ── SECTION F3: Median-run composition bars for panels g/h --------------------
# One stacked bar right of each panel at PROJ_END: the run whose PROJ_END total
# is closest to the median across runs, split by material (g) or end use (h);
# stacked in the same order as the historical areas, labelled with % shares.

cat("F3: Median-run composition bars\n")

run_total_g <- results |>
  filter(year == PROJ_END) |>
  group_by(run_id) |>
  summarise(v = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop")
median_run_g <- run_total_g$run_id[which.min(abs(run_total_g$v - median(run_total_g$v)))]

# One bar at PROJ_END, outside the box (median run); the HIST_END composition is
# labelled directly on the historical stack (hist_lab_g / hist_lab_h below)
bar_g <- results |>
  filter(year == PROJ_END, run_id == median_run_g) |>
  mutate(
    mat_group = case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group == "metal_ores" ~ "Metal ores",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals"
    )
  ) |>
  group_by(year, mat_group) |>
  summarise(v = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(mat_group = factor(mat_group, levels = mat_order)) |>
  # last factor level at the bottom, as in the stacked historical area
  arrange(year, desc(mat_group)) |>
  group_by(year) |>
  mutate(ymax = cumsum(v), ymin = dplyr::lag(ymax, default = 0), share = v / sum(v)) |>
  ungroup() |>
  mutate(x0 = PROJ_END + 1)

run_total_h <- results |>
  filter(year == PROJ_END, material_group %in% c("metal_ores", "nonmetallic_minerals")) |>
  group_by(run_id) |>
  summarise(v = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop")
median_run_h <- run_total_h$run_id[which.min(abs(run_total_h$v - median(run_total_h$v)))]

bar_h <- results |>
  filter(year == PROJ_END, run_id == median_run_h, material_group %in% c("metal_ores", "nonmetallic_minerals")) |>
  mutate(
    end_use_label = case_when(
      material_key %in% c("Residential", "Non-residential") ~ "Buildings",
      material_key %in% c("Roads", "Civil engineering") ~ "Civil infrastructure",
      material_key %in% c("Machinery", "Vehicles") ~ "Machinery",
      material_key %in% c("Durables", "Packaging") ~ "Short-lived products",
      material_key == "Power sector" ~ "Power sector"
    )
  ) |>
  group_by(year, end_use_label) |>
  summarise(v = sum(in_use_stock_Mt, na.rm = TRUE) / 1e6, .groups = "drop") |>
  # Power sector (not in the historical stacked area) on top of the historical end-use order
  mutate(end_use_label = factor(end_use_label, levels = c("Power sector", enduse_order))) |>
  arrange(year, desc(end_use_label)) |>
  group_by(year) |>
  mutate(ymax = cumsum(v), ymin = dplyr::lag(ymax, default = 0), share = v / sum(v)) |>
  ungroup() |>
  mutate(x0 = PROJ_END + 1)
stopifnot(!anyNA(bar_g$mat_group), !anyNA(bar_h$end_use_label))

cat("  Median run (g):", median_run_g, "| (h):", median_run_h, "\n")

# HIST_END shares, written on the historical stacked areas (same stacking order)
hist_lab_g <- dmc_total_hist |>
  filter(year == HIST_END) |>
  arrange(desc(mat_group)) |>
  mutate(ymax = cumsum(DMC_Gt), ymin = dplyr::lag(ymax, default = 0), share = DMC_Gt / sum(DMC_Gt))
hist_lab_h <- stock_total_hist |>
  filter(year == HIST_END) |>
  arrange(desc(end_use_label)) |>
  mutate(ymax = cumsum(stock_Gt), ymin = dplyr::lag(ymax, default = 0), share = stock_Gt / sum(stock_Gt))


# ── SECTION G: Build panels --------------------------------------------------

cat("G: Building panels\n")

# Direct-label angle = local tangent of the line at the label year: slope from
# (year-10) to (year+10), normalised by the panel's y-range / x-range so the
# angle reflects the line's visual slope, then atan()'d into [-90, 90] degrees
# and snapped to 0 whenever it falls within +-10 degrees of horizontal.

# Historical line-label data for panels c/d
bio_yrange <- max(biomass_env$p_max, biomass_hist$mg, na.rm = TRUE)
bio_slope <- biomass_hist |>
  filter(year %in% c(LABEL_YEAR - 10, LABEL_YEAR + 10)) |>
  dplyr::select(material_category, year, mg) |>
  pivot_wider(names_from = year, values_from = mg, names_prefix = "yr") |>
  mutate(
    angle = atan(
      (.data[[paste0("yr", LABEL_YEAR + 10)]] - .data[[paste0("yr", LABEL_YEAR - 10)]]) / 20 / (bio_yrange / X_RANGE_YR)
    ) *
      180 /
      pi,
    angle = if_else(abs(angle) <= 10, 0, angle)
  ) |>
  dplyr::select(material_category, angle) |>
  mutate(angle = case_when(material_category == "Wood" ~ -20, str_detect(material_category, "Grazed") ~ -50, T ~ angle))

bio_labels <- biomass_hist |>
  filter(year == LABEL_YEAR) |>
  mutate(label_x = str_remove(material_category, " and fodder crops")) |>
  left_join(bio_slope, by = "material_category")

fossil_yrange <- max(fossil_env$p_max, fossil_hist$mg, na.rm = TRUE)
fossil_slope <- fossil_hist |>
  filter(year %in% c(LABEL_YEAR - 10, LABEL_YEAR + 10)) |>
  dplyr::select(material_category, year, mg) |>
  pivot_wider(names_from = year, values_from = mg, names_prefix = "yr") |>
  mutate(
    angle = atan(
      (.data[[paste0("yr", LABEL_YEAR + 10)]] - .data[[paste0("yr", LABEL_YEAR - 10)]]) /
        20 /
        (fossil_yrange / X_RANGE_YR)
    ) *
      180 /
      pi,
    angle = if_else(abs(angle) <= 10, 0, angle)
  ) |>
  dplyr::select(material_category, angle) |>
  mutate(angle = case_when(material_category == "Coal" ~ 0, T ~ angle))
fossil_labels <- fossil_hist |> filter(year == LABEL_YEAR) |> left_join(fossil_slope, by = "material_category")

# Direct historical labels for panels e/f (at HIST_LABEL_YEAR); year+10 falls in
# the projection period, so its value comes from the MC median (p50), matching
# the dashed median line the label sits on.
metal_yrange <- max(metal_env$p_max, metal_hist_eu$mg, na.rm = TRUE)
metal_slope <- metal_hist_eu |>
  filter(year == HIST_LABEL_YEAR - 10) |>
  dplyr::select(end_use_label, mg_lo = mg) |>
  left_join(
    metal_env |> filter(year == HIST_LABEL_YEAR + 4) |> dplyr::select(end_use_label, mg_hi = p50),
    by = "end_use_label"
  ) |>
  mutate(
    angle = atan((mg_hi - mg_lo) / 20 / (metal_yrange / X_RANGE_YR)) * 180 / pi,
    angle = if_else(abs(angle) <= 10, 0, angle)
  ) |>
  dplyr::select(end_use_label, angle) |>
  mutate(angle = case_when(str_detect(end_use_label, "Non-") ~ -15, end_use_label == "Civil eng." ~ 5, T ~ angle))
metal_hist_labels <- metal_hist_eu |>
  filter(year == HIST_LABEL_YEAR) |>
  left_join(metal_slope, by = "end_use_label") |>
  mutate(aux_label = str_replace(end_use_label, "tial Bldg", "tial\nBldg"))

nonmet_yrange <- max(nonmet_env$p_max, nonmet_hist_eu$mg, na.rm = TRUE)
nonmet_slope <- nonmet_hist_eu |>
  filter(year == HIST_LABEL_YEAR - 10) |>
  dplyr::select(end_use_label, mg_lo = mg) |>
  left_join(
    nonmet_env |> filter(year == HIST_LABEL_YEAR + 4) |> dplyr::select(end_use_label, mg_hi = p50),
    by = "end_use_label"
  ) |>
  mutate(
    angle = atan((mg_hi - mg_lo) / 20 / (nonmet_yrange / X_RANGE_YR)) * 180 / pi,
    angle = if_else(abs(angle) <= 10, 0, angle)
  ) |>
  dplyr::select(end_use_label, angle) |>
  mutate(angle = case_when(end_use_label == "Roads" ~ 0, end_use_label == "Residential Bldg" ~ 25, T ~ angle))

nonmet_hist_labels <- nonmet_hist_eu |>
  filter(year == HIST_LABEL_YEAR, end_use_label %in% F_LABEL_ENDUSES) |>
  left_join(nonmet_slope, by = "end_use_label")


# ranges
bio_range <- biomass_env |>
  filter(year == PROJ_END) |>
  mutate(
    pos_x = case_when(
      material_key == "Crops" ~ 0,
      material_key == "Crop Residues" ~ 1,
      str_detect(material_key, "Grazed") ~ 2,
      T ~ 0
    )
  )

fossil_range <- fossil_env |>
  filter(year == PROJ_END) |>
  mutate(
    pos_x = case_when(
      material_key == "Coal" ~ 0,
      material_key == "Petroleum" ~ 1,
      str_detect(material_key, "Natural") ~ 2,
      T ~ 0
    )
  )

metal_range <- metal_env |>
  filter(year == PROJ_END) |>
  mutate(
    pos_x = case_when(
      end_use_label == "Roads" ~ 0,
      str_detect(end_use_label, "Civil") ~ 1,
      end_use_label == "Machinery" ~ 2,
      end_use_label == "Residential" ~ 0,
      end_use_label == "Non-residential" ~ 1,
      end_use_label == "Vehicles" ~ 2,
      end_use_label == "Durables" ~ 0,
      end_use_label == "Packaging" ~ 1,
      end_use_label == "Power sector" ~ 3,
      T ~ 0
    )
  )

nonmet_range <- nonmet_env |>
  filter(year == PROJ_END) |>
  mutate(
    pos_x = case_when(
      end_use_label == "Residential" ~ 0,
      end_use_label == "Non-residential" ~ 1,
      end_use_label == "Roads" ~ 2,
      str_detect(end_use_label, "Civil") ~ 3,
      end_use_label == "Power sector" ~ 4,
      T ~ 0
    )
  )


## Panel (a) — Population -------------------------------------------------------

pop_hist <- pop_world_hist |> filter(year <= HIST_END) |> mutate(v = population / 1e9)

p1 <- ggplot() +
  geom_ribbon(
    data = pop_env,
    aes(x = year, ymin = p_min, ymax = p_max),
    fill = POP_COLOR,
    alpha = RIBBON_ALPHA_MINMAX
  ) +
  geom_ribbon(data = pop_env, aes(x = year, ymin = p25, ymax = p75), fill = POP_COLOR, alpha = RIBBON_ALPHA_IQR) +
  geom_line(data = pop_hist, aes(x = year, y = v), colour = POP_COLOR, linewidth = HIST_LW) +
  geom_line(
    data = pop_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = POP_COLOR,
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  # custom legend (upper-left, above all historical data): nested Min-Max / P25-P75 bands + median,
  # rows spaced 1.0 apart (was 0.5) for more vertical separation and legend height
  # fmt: skip
  annotate("rect",xmin = 1978,xmax = 1989,ymin = 6.7,ymax = 9.7,fill = "grey50",alpha = RIBBON_ALPHA_MINMAX,colour = NA) +
  # fmt: skip
  annotate("rect",xmin = 1978,xmax = 1989,ymin = 7.45,ymax = 8.95,fill = "grey50",alpha = RIBBON_ALPHA_IQR,colour = NA) +
  # fmt: skip
  annotate("segment", x = 1977, xend = 1990, y = 8.2, yend = 8.2, colour = "grey35", linewidth = PROJ_LW,linetype="dashed") +
  annotate("text", x = 1991, y = 9.7, label = "Max", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  # fmt: skip
  annotate("text", x = 1991, y = 8.95,  label = paste0(round(CI_HI * 100), "%ile") , hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  annotate("text", x = 1991, y = 8.2, label = "median", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  # fmt: skip
  annotate("text", x = 1991, y = 7.45,  label = paste0(round(CI_LO * 100), "%ile") , hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  annotate("text", x = 1991, y = 6.7, label = "Min", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  present_line_bot +
  x_sc +
  y_sc +
  co +
  panel_tag("a") +
  labs(x = NULL, y = "Population (billion)", title = "Population") +
  theme_pb_large() +
  FONT_BUMP


## Panel (b) — GDP per capita ---------------------------------------------------

gdp_cap_hist <- gdp_world_hist |>
  left_join(pop_world_hist, by = "year") |>
  filter(year <= HIST_END) |>
  mutate(v = GDP_2015USD / population / 1e3)

p2 <- ggplot() +
  geom_ribbon(
    data = gdpcap_env,
    aes(x = year, ymin = p_min, ymax = p_max),
    fill = GDPCAP_COLOR,
    alpha = RIBBON_ALPHA_MINMAX
  ) +
  geom_ribbon(data = gdpcap_env, aes(x = year, ymin = p25, ymax = p75), fill = GDPCAP_COLOR, alpha = RIBBON_ALPHA_IQR) +
  geom_line(data = gdp_cap_hist, aes(x = year, y = v), colour = GDPCAP_COLOR, linewidth = HIST_LW) +
  geom_line(
    data = gdpcap_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = GDPCAP_COLOR,
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  present_line_top +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 30, 5), labels = function(x) {
    ifelse(x %% 10 == 0, x, "")
  }) +
  co +
  panel_tag("b") +
  labs(x = NULL, y = "GDP per capita ('000 $/p)", title = "GDP per capita") +
  theme_pb_large() +
  FONT_BUMP


## Panel (c) — M/G Biomass -----------------------------------------------------

p3 <- ggplot() +
  geom_ribbon(
    data = biomass_env,
    aes(x = year, ymin = p_min, ymax = p_max, fill = material_key),
    alpha = RIBBON_ALPHA_MINMAX,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = biomass_env,
    aes(x = year, ymin = p25, ymax = p75, fill = material_key),
    alpha = RIBBON_ALPHA_IQR,
    show.legend = FALSE
  ) +
  geom_line(data = biomass_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(
    data = biomass_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = material_key),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_rect(
    data = bio_range,
    aes(
      xmin = PROJ_END + pos_x + 1,
      xmax = PROJ_END + pos_x + 1 + 0.45,
      ymin = p_min,
      ymax = p_max,
      fill = material_key
    ),
    color = NA
  ) +
  geom_text(
    data = bio_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = label_x, angle = angle),
    size = LABEL_SZ, show.legend = FALSE,
    nudge_y = c(0.01,0.01,0.018,0.01,0.01)
  ) +
  present_line +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 0.25, 0.05), labels = function(x) {
    ifelse(abs(x / 0.1 - round(x / 0.1)) < 1e-9, x, "") # tolerance: %% on decimals misses e.g. 0.3
  }) +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
  panel_tag("c", side = "right") +
  labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Biomass") +
  theme_pb_large() +
  FONT_BUMP


## Panel (d) — M/G Fossil fuels ------------------------------------------------

p4 <- ggplot() +
  geom_ribbon(
    data = fossil_env,
    aes(x = year, ymin = p_min, ymax = p_max, fill = material_key),
    alpha = RIBBON_ALPHA_MINMAX,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = fossil_env,
    aes(x = year, ymin = p25, ymax = p75, fill = material_key),
    alpha = RIBBON_ALPHA_IQR,
    show.legend = FALSE
  ) +
  geom_line(data = fossil_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(
    data = fossil_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = material_key),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_rect(
    data = fossil_range,
    aes(
      xmin = PROJ_END + pos_x + 1,
      xmax = PROJ_END + pos_x + 1 + 0.45,
      ymin = p_min,
      ymax = p_max,
      fill = material_key
    ),
    color = NA
  ) +
  geom_text(
    data = fossil_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = material_category, angle = angle),
    size = LABEL_SZ, show.legend = FALSE, nudge_y = c(0.008, 0.01, 0.008,-0.015),nudge_x=2,
  ) +
  present_line +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 0.16, 0.02), labels = function(x) {
    ifelse(abs(x / 0.04 - round(x / 0.04)) < 1e-9, x, "") # tolerance: 0.12 %% 0.04 != 0 in floating point
  }) +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
  panel_tag("e", side = "right") +
  labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Fossil fuels") +
  theme_pb_large() +
  FONT_BUMP


## Panel (e) — S/G Metal ores by end-use (8 sub-end-uses) ----------------------

p5 <- ggplot() +
  geom_ribbon(
    data = metal_env,
    aes(x = year, ymin = p_min, ymax = p_max, fill = end_use_label),
    alpha = RIBBON_ALPHA_MINMAX,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = metal_env,
    aes(x = year, ymin = p25, ymax = p75, fill = end_use_label),
    alpha = RIBBON_ALPHA_IQR,
    show.legend = FALSE
  ) +
  geom_line(data = metal_hist_eu, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(data = power_hist_metal, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(
    data = metal_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = end_use_label),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_rect(
    data = metal_range,
    aes(
      xmin = PROJ_END + pos_x + 1,
      xmax = PROJ_END + pos_x + 1 + 0.45,
      ymin = p_min,
      ymax = p_max,
      fill = end_use_label
    ),
    color = NA
  ) +
  geom_text(
    data = metal_hist_labels,
    aes(x = year, y = mg, label = aux_label, colour = end_use_label, angle = angle),
    nudge_y=c(-7.5,1,5.7,3.5,0.9,3,0,-1.5)*0.005,
    nudge_x = c(-15,3,-30,-30,3,-13,-2,-0.5),
    hjust = c(1,1,1,0.5,1,0.5,1,1),
     lineheight = 0.8,
    size = LABEL_SZ, show.legend = FALSE
  ) +
  # Power sector: labelled on its historical (EIA) line
  geom_text(
    data = power_hist_metal |> filter(year == POWER_LABEL_YEAR),
    aes(x = year, y = mg, label = end_use_label, colour = end_use_label),
    vjust = -0.4, size = LABEL_SZ, show.legend = FALSE
  ) +
  present_line +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 0.12, 0.02), labels = function(x) {
    ifelse(round(x * 100) %% 4 == 0, x, "")
  }) +
  co +
  scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  panel_tag("d", side = "right") +
  labs(x = "", y = "Metal stock per GDP (kg metal/$)", title = "S/G Metals") + # stock is metal mass, not ore
  theme_pb_large() +
  FONT_BUMP


## Panel (f) — S/G Non-metallic minerals by end-use (8 sub, 4 labeled) ---------

p6 <- ggplot() +
  geom_ribbon(
    data = nonmet_env,
    aes(x = year, ymin = p_min, ymax = p_max, fill = end_use_label),
    alpha = RIBBON_ALPHA_MINMAX,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = nonmet_env,
    aes(x = year, ymin = p25, ymax = p75, fill = end_use_label),
    alpha = RIBBON_ALPHA_IQR,
    show.legend = FALSE
  ) +
  geom_line(data = nonmet_hist_eu, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(data = power_hist_nonmet, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(
    data = nonmet_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = end_use_label),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_rect(
    data = nonmet_range,
    aes(
      xmin = PROJ_END + pos_x + 1,
      xmax = PROJ_END + pos_x + 1 + 0.45,
      ymin = p_min,
      ymax = p_max,
      fill = end_use_label
    ),
    color = NA
  ) +
  geom_text(
    data = nonmet_hist_labels,
    aes(x = year, y = mg, label = end_use_label, colour = end_use_label, angle = angle),
    nudge_y=c(-0.6,-0.6,-0.25,2),
    nudge_x = c(-34,-0.5,-8,-25),
    hjust = 1,size = LABEL_SZ, show.legend = FALSE
  ) +
  present_line +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 7), labels = function(x) {
    ifelse(x %% 2 == 0, x, "")
  }) +
  co +
  scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  panel_tag("f") +
  labs(x = "", y = "Stock per GDP (kg/$)", title = "S/G Non-metallic minerals") +
  theme_pb_large() +
  FONT_BUMP


## Panel (g) — Total material consumption --------------------------------------

p7 <- ggplot() +
  geom_area(
    data = dmc_total_hist,
    aes(x = year, y = DMC_Gt, fill = mat_group),
    position = "stack", alpha = 0.5, colour = NA
  ) +
  geom_line(data = dmc_total_hist_sum, aes(x = year, y = DMC_Gt), colour = "black", linewidth = HIST_LW) +
  geom_text(
    data = dmc_stack_labels,
    aes(x = STACK_LABEL_YEAR, y = label_y, label = label, colour = mat_group),
    angle = c(0, 15, 20, 10), nudge_x = c(0, 0, 0, -12),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = dmc_env, aes(x = year, ymin = p25, ymax = p75), fill = "grey60", alpha = RIBBON_ALPHA_IQR) +
  geom_line(
    data = dmc_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = "black",
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  # Median-run PROJ_END composition by material (SECTION F3)
  geom_rect(
    data = bar_g,
    aes(xmin = x0, xmax = x0 + SIDE_BAR_W, ymin = ymin, ymax = ymax, fill = mat_group),
    alpha = 0.5, colour = "black", linewidth = 0.15
  ) +
  geom_text(
    data = bar_g |> filter(share >= 0.05),
    aes(x = x0 + SIDE_BAR_W + 0.3, y = (ymin + ymax) / 2, label = scales::percent(share, accuracy = 1), colour = mat_group),
    angle = 90, hjust = 0.5, vjust = 1, size = LABEL_SZ # rotated, reading upward, right of the bar
  ) +
  # HIST_END shares on the historical stack (no bar)
  geom_text(
    data = hist_lab_g |> filter(share >= 0.05),
    aes(x = HIST_LABEL_X, y = (ymin + ymax) / 2, label = scales::percent(share, accuracy = 1), colour = mat_group),
    angle = 90, hjust = 0.5, vjust = 0.5, size = LABEL_SZ
  ) +
  present_line +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 300, 50), labels = function(x) {
    ifelse(x %% 100 == 0, x, "")
  }) +
  co +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  panel_tag("g") +
  labs(x = NULL, y = "Material consumption (Gt)", title = "Total material consumption") +
  theme_pb_large() +
  FONT_BUMP


## Panel (h) — Total in-use stock ----------------------------------------------

p8 <- ggplot() +
  geom_area(
    data = stock_total_hist,
    aes(x = year, y = stock_Gt, fill = end_use_label),
    position = "stack", alpha = 0.5, colour = NA
  ) +
  geom_line(data = stock_total_hist_sum, aes(x = year, y = stock_Gt), colour = "black", linewidth = HIST_LW) +
  geom_text(
    data = stock_stack_labels,
    aes(x = STACK_LABEL_YEAR, y = label_y, label = end_use_label, colour = end_use_label),
    angle = c(0, 15),nudge_x=c(2,10),nudge_y=c(0,0.2),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = stock_env, aes(x = year, ymin = p25, ymax = p75), fill = "grey60", alpha = RIBBON_ALPHA_IQR) +
  geom_line(
    data = stock_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = "black",
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  # Median-run PROJ_END composition by end use (SECTION F3)
  geom_rect(
    data = bar_h,
    aes(xmin = x0, xmax = x0 + SIDE_BAR_W, ymin = ymin, ymax = ymax, fill = end_use_label),
    alpha = 0.5, colour = "black", linewidth = 0.15
  ) +
  geom_text(
    data = bar_h |> filter(share >= 0.05),
    aes(x = x0 + SIDE_BAR_W + 0.3, y = (ymin + ymax) / 2, label = scales::percent(share, accuracy = 1), colour = end_use_label),
    angle = 90, hjust = 0.5, vjust = 1, size = LABEL_SZ # rotated, reading upward, right of the bar
  ) +
  # HIST_END shares on the historical stack (no bar)
  geom_text(
    data = hist_lab_h |> filter(share >= 0.05),
    aes(x = HIST_LABEL_X, y = (ymin + ymax) / 2, label = scales::percent(share, accuracy = 1), colour = end_use_label),
    angle = 90, hjust = 0.5, vjust = 0.5, size = LABEL_SZ
  ) +
  present_line +
  x_sc +
  y_sc +
  co +
  scale_fill_manual(values = PALETTE_ENDUSE, guide = "none") +
  scale_colour_manual(values = PALETTE_ENDUSE, guide = "none") +
  panel_tag("h") +
  labs(x = "", y = "In-use stock ('000 Gt)", title = "Total stock") +
  theme_pb_large() +
  FONT_BUMP


# ── SECTION H: Assemble and save --------------------------------------------

cat("H: Assembling figure\n")

fig <- wrap_plots(p1, p2, p3, p5, p4, p6, p7, p8, ncol = 2) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(5.5, 16, 5.5, 5.5) # right: room for the g/h composition-bar labels
  )

ggsave("Figures/Fig2 - Assumptions.png", fig, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 3)
ggsave("Figures/SVG/Fig2 - Assumptions.svg", fig, units = "cm", width = 8.7 * 2, height = 8.7 * 3)
group_svg_layers("Figures/SVG/Fig2 - Assumptions.svg")

cat("  Saved: Figures/Fig2 - Assumptions.png\n")

# EoF
