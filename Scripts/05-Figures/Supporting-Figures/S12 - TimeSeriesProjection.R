## S12 - TimeSeriesProjection.R  -> Supporting Figure S12
## 4x2 figure, one row per material group (Biomass, Fossil fuels, Metal ores,
## Non-metallic minerals):
##   left column  - spaghetti of primary consumption 1990-2060 (MC runs),
##                  historical 1990-2024 in the group colour, top/bottom 10%
##                  cumulative-consumption pathways highlighted
##   right column - scatter of the two MC input assumptions driving that
##                  group's intensity, same pathway colours, point size = GDP
##                  per-capita growth rate (2025-2060 CAGR)

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
library(arrow)
library(patchwork)

pb_set_geom_defaults("largeFont")

# Constants --------

HIST_END <- 2024L

# Pathway classification colours reuse the project's Absolute decoupling
# green (Scripts/00-CommonParameters.R PALETTE_DECOUPLING) for visual
# consistency with Figure 3; "High growth" reuses the old "Highest 10%" red.
PATH_CLASS_COLORS <- c("Absolute decoupling" = "#1B7837", "High growth" = "#B2182B", "Other runs" = "grey70")
PATH_CLASS_LEVELS <- c("Other runs", "High growth", "Absolute decoupling") # draw order: highlighted classes on top
PATH_CLASS_ALPHA <- c("Other runs" = 0.10, "High growth" = 0.45, "Absolute decoupling" = 0.45)
PATH_CLASS_LINEWIDTH <- c("Other runs" = 0.15, "High growth" = 0.35, "Absolute decoupling" = 0.35)

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum")
METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

# Shared spaghetti-panel scales/coords (Figure 2 pattern)
x_sc <- scale_x_continuous(breaks = seq(1990, 2060, 10))
y_sc <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
co <- coord_cartesian(xlim = c(1990, 2060), ylim = c(0, NA), clip = "off", expand = FALSE)

present_line <- list(geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"))
present_line_labeled <- list(
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted"),
  annotate(
    "text",
    x = HIST_END - 1,
    y = Inf,
    label = "Present (2024)",
    hjust = 1.05,
    vjust = 1.3,
    size = pb_annot_size("largeFont"),
    colour = "grey45",
    angle = 90
  )
)


# Load data --------

hist_dmc <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
mc_results <- arrow::read_parquet("Results/MC/mc_results.parquet")
mc_input_matrix <- read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
mc_decoupling <- arrow::read_parquet("Results/MC/mc_decoupling.parquet") # produced by Scripts/04-Simulation/04-Decoupling.R
stock_2024 <- read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE)
stock_hist <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
secondary_flows_hist <- read_csv("Parameters/Intermediate/historical_secondary_flows.csv", show_col_types = FALSE)


# Historical primary consumption by material group (1990-2024) --------

hist_4mat <- hist_dmc |>
  dplyr::filter(abs(DMC_Mt) > 0.1, material_category %in% c(BIOMASS_CATS, FOSSIL_CATS, METAL_CATS, NONMET_CATS)) |>
  dplyr::mutate(
    material_group = dplyr::case_when(
      material_category %in% BIOMASS_CATS ~ "Biomass",
      material_category %in% FOSSIL_CATS ~ "Fossil fuels",
      material_category %in% METAL_CATS ~ "Metal ores",
      material_category %in% NONMET_CATS ~ "Non-metallic minerals"
    )
  ) |>
  dplyr::filter(year >= 1990, year <= HIST_END) |>
  dplyr::group_by(material_group, year) |>
  dplyr::summarise(primary_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")


# Projected primary consumption by material group, run & year (2025-2060) --------

proj_4mat <- mc_results |>
  dplyr::mutate(
    material_group = dplyr::case_when(
      material_group %in% c("metal_fe", "metal_nonfe") ~ "Metal ores",
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals"
    )
  ) |>
  dplyr::filter(!is.na(material_group)) |>
  dplyr::group_by(material_group, run_id, year) |>
  dplyr::summarise(primary_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Classify each run's pathway, within its own material group -- same
# classification colours both panel columns. CAGR is computed on the same
# total (world) primary_Gt series the left panel plots, 2025-2060 -- NOT
# mc_decoupling's per-capita CAGR, which can disagree with the plotted total
# mass whenever population growth outpaces a per-capita decline (a run can
# have falling consumption per person while its total still rises).
#   Absolute decoupling - primary_Gt CAGR < 0 over 2025-2060 (net decrease)
#   High growth         - top 10% highest 2060 consumption, within the group
#                         (evaluated only for runs not already classified above)
run_class <- proj_4mat |>
  dplyr::filter(year %in% c(2025L, FORECAST_END)) |>
  dplyr::arrange(material_group, run_id, year) |>
  dplyr::group_by(material_group, run_id) |>
  dplyr::summarise(
    val_2025 = primary_Gt[year == 2025L],
    val_end = primary_Gt[year == FORECAST_END],
    mf_cagr = (val_end / val_2025)^(1 / (FORECAST_END - 2025L)) - 1,
    .groups = "drop"
  ) |>
  dplyr::group_by(material_group) |>
  dplyr::mutate(pctile_end = dplyr::percent_rank(val_end)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    path_class = dplyr::case_when(
      mf_cagr < 0 ~ "Absolute decoupling",
      pctile_end >= 0.90 ~ "High growth",
      TRUE ~ "Other runs"
    )
  ) |>
  dplyr::select(material_group, run_id, path_class)

proj_4mat <- proj_4mat |> dplyr::left_join(run_class, by = c("material_group", "run_id"))

# Anchor every run to the common 2024 historical value, so projected
# pathways connect continuously to history instead of jumping at 2025.
anchor_2024 <- run_class |>
  dplyr::left_join(
    hist_4mat |> dplyr::filter(year == HIST_END) |> dplyr::select(material_group, primary_Gt),
    by = "material_group"
  ) |>
  dplyr::mutate(year = HIST_END)

proj_4mat <- dplyr::bind_rows(proj_4mat, anchor_2024) |> dplyr::arrange(material_group, run_id, year)

# Light 3-year centered moving average for display only -- run_class above
# already classified pathways from the raw annual values.
proj_4mat <- proj_4mat |>
  dplyr::group_by(material_group, run_id) |>
  dplyr::mutate(
    primary_Gt_smooth = dplyr::coalesce(as.numeric(stats::filter(primary_Gt, rep(1 / 3, 3), sides = 2)), primary_Gt)
  ) |>
  dplyr::ungroup()


# Rescale MC input draws to physical units --------
# value = bound_min + u*(bound_max-bound_min), same formula 02-RunSimulations.R
# and Figure 4 - PrepareData.R use to turn [0,1] LHS draws into real units.

# World kg/$ bounds (2024-GDP-weighted regional endpoint bounds, flows averaged
# across SSPs) -- written by 01-Sampling.R; display only.
intensity_bounds <- readr::read_csv("Parameters/Simulation/intensity_world_bounds.csv", show_col_types = FALSE) |>
  dplyr::select(param, bound_min, bound_max)

crops_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_crops_global"]
crops_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_crops_global"]
grazed_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_grazed_biomass_global"]
grazed_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_grazed_biomass_global"]
coal_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_coal_global"]
coal_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_coal_global"]
gas_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_gas_global"]
gas_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_gas_global"]
oil_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_oil_global"]
oil_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_oil_global"]

# Stock (S/G) intensity bounds, per end-use, for the metal-ores & minerals
# scatter panels' Y/X axes.
bldg_metal_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_buildings_metalOres_global"]
bldg_metal_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_buildings_metalOres_global"]
civil_metal_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_civil_metalOres_global"]
civil_metal_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_civil_metalOres_global"]
machinery_metal_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_machinery_metalOres_global"]
machinery_metal_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_machinery_metalOres_global"]
sl_metal_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_sl_products_metalOres_global"]
sl_metal_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_sl_products_metalOres_global"]
bldg_nonmet_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_buildings_nonMetallic_global"]
bldg_nonmet_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_buildings_nonMetallic_global"]
civil_nonmet_min <- intensity_bounds$bound_min[intensity_bounds$param == "intensity_civil_nonMetallic_global"]
civil_nonmet_max <- intensity_bounds$bound_max[intensity_bounds$param == "intensity_civil_nonMetallic_global"]

# Lifetime bounds (buildings & civil infrastructure), sub-uses averaged to
# their super_category, same as Figure 4 - PrepareData.R
lifetime_bounds <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category %in% c("buildings", "civil_infrastructure")) |>
  dplyr::group_by(super_category) |>
  dplyr::summarise(mean_life_min = mean(mean_life_min), mean_life_max = mean(mean_life_max), .groups = "drop")

life_bldg_min <- lifetime_bounds$mean_life_min[lifetime_bounds$super_category == "buildings"]
life_bldg_max <- lifetime_bounds$mean_life_max[lifetime_bounds$super_category == "buildings"]
life_civil_min <- lifetime_bounds$mean_life_min[lifetime_bounds$super_category == "civil_infrastructure"]
life_civil_max <- lifetime_bounds$mean_life_max[lifetime_bounds$super_category == "civil_infrastructure"]

# 2024 stock weights (buildings vs civil infrastructure) for non-metallic
# minerals, used to blend the buildings/civil stock-intensity & lifetime draws
mineral_stock_weights <- stock_2024 |>
  dplyr::filter(material == "Non-metallic minerals", super_category %in% c("buildings", "civil_infrastructure")) |>
  dplyr::group_by(super_category) |>
  dplyr::summarise(stock_Mt = sum(stock_Mt), .groups = "drop")

w_bldg <- mineral_stock_weights$stock_Mt[mineral_stock_weights$super_category == "buildings"]
w_civil <- mineral_stock_weights$stock_Mt[mineral_stock_weights$super_category == "civil_infrastructure"]

# 2024 stock weights (Fe vs NonFe), used to combine the Fe/NonFe recycling
# rate draws into a single mass-weighted metal recycling index.
fe_nonfe_weights <- stock_2024 |>
  dplyr::filter(material %in% c("Metal_Fe", "Metal_NonFe")) |>
  dplyr::group_by(material) |>
  dplyr::summarise(stock_Mt = sum(stock_Mt), .groups = "drop")

w_fe <- fe_nonfe_weights$stock_Mt[fe_nonfe_weights$material == "Metal_Fe"]
w_nonfe <- fe_nonfe_weights$stock_Mt[fe_nonfe_weights$material == "Metal_NonFe"]

# GDP per-capita growth rate per run, 2025-2060 window (matches the figure's
# projection horizon); world quantity, same value across material groups.
gdp_growth <- mc_decoupling |>
  dplyr::filter(variant_id == "window_2025_2060", material_group == "Total") |>
  dplyr::select(run_id, gdp_pc_cagr = gdp_percap_cagr)

run_vars <- mc_input_matrix |>
  dplyr::transmute(
    run_id,
    crop_intensity = crops_min + intensity_crops_global * (crops_max - crops_min),
    grazed_intensity = grazed_min + intensity_grazed_biomass_global * (grazed_max - grazed_min),
    coal_intensity = coal_min + intensity_coal_global * (coal_max - coal_min),
    gas_intensity = gas_min + intensity_gas_global * (gas_max - gas_min),
    oil_intensity = oil_min + intensity_oil_global * (oil_max - oil_min),
    # Recycling endpoints: semi-uniform around the central value (as 02-RunSimulations.R)
    recycling_index = (w_fe *
      dplyr::if_else(
        recycling_rate_fe < 0.5,
        RECYCLING_RATE_FE_MIN + 2 * recycling_rate_fe * (RECYCLING_RATE_FE_CENTRAL - RECYCLING_RATE_FE_MIN),
        RECYCLING_RATE_FE_CENTRAL + 2 * (recycling_rate_fe - 0.5) * (RECYCLING_RATE_FE_MAX - RECYCLING_RATE_FE_CENTRAL)
      ) +
      w_nonfe *
        dplyr::if_else(
          recycling_rate_nonfe < 0.5,
          RECYCLING_RATE_NONFE_MIN + 2 * recycling_rate_nonfe * (RECYCLING_RATE_NONFE_CENTRAL - RECYCLING_RATE_NONFE_MIN),
          RECYCLING_RATE_NONFE_CENTRAL + 2 * (recycling_rate_nonfe - 0.5) * (RECYCLING_RATE_NONFE_MAX - RECYCLING_RATE_NONFE_CENTRAL)
        )) /
      (w_fe + w_nonfe),
    intensity_bldg_metal = bldg_metal_min + intensity_buildings_metalOres_global * (bldg_metal_max - bldg_metal_min),
    intensity_civil_metal = civil_metal_min + intensity_civil_metalOres_global * (civil_metal_max - civil_metal_min),
    intensity_machinery_metal = machinery_metal_min +
      intensity_machinery_metalOres_global * (machinery_metal_max - machinery_metal_min),
    intensity_sl_metal = sl_metal_min + intensity_sl_products_metalOres_global * (sl_metal_max - sl_metal_min),
    intensity_bldg_nonmet = bldg_nonmet_min +
      intensity_buildings_nonMetallic_global * (bldg_nonmet_max - bldg_nonmet_min),
    intensity_civil_nonmet = civil_nonmet_min +
      intensity_civil_nonMetallic_global * (civil_nonmet_max - civil_nonmet_min),
    lifetime_bldg = life_bldg_min + lifetime_mean_buildings * (life_bldg_max - life_bldg_min),
    lifetime_civil = life_civil_min + lifetime_mean_civil_infrastructure * (life_civil_max - life_civil_min)
  ) |>
  dplyr::mutate(
    oil_ng_intensity = oil_intensity + gas_intensity,
    stock_intensity_metal = intensity_bldg_metal +
      intensity_civil_metal +
      intensity_machinery_metal +
      intensity_sl_metal,
    stock_intensity_minerals = intensity_bldg_nonmet + intensity_civil_nonmet,
    lifetime_minerals = (w_bldg * lifetime_bldg + w_civil * lifetime_civil) / (w_bldg + w_civil)
  ) |>
  dplyr::left_join(gdp_growth, by = "run_id")

SIZE_RANGE <- range(run_vars$gdp_pc_cagr, na.rm = TRUE)


# "Historical" (2019-2024) reference star for the scatter panels -- a single
# point per panel marking where the real economy actually sat over the last
# five years (not itself an MC draw), replacing the old single-year-2024
# dashed lines. Biomass/fossil/metal stock/recycling all have real annual
# data through 2024 (materials_region_DMC, gdp_region, stock_trajectory_1970_
# 2024, historical_secondary_flows), so each star is the average of that
# quantity's own annual ratio (value_t / GDP_t, or recovered_t / waste_t)
# over 2019-2024 -- not a single-year snapshot, and not a ratio of period
# averages. Mineral lifetime has no real annual calibration anywhere in the
# pipeline -- LIFETIME_SAMPLE_PARAMS' own central "mean_life" is used as the
# model's deterministic anchor instead (same convention Figure 3 uses for its
# own lifetime/grade stars).

gdp_world_hist <- gdp_region_hist |>
  dplyr::filter(year %in% 2019:2024) |>
  dplyr::group_by(year) |>
  dplyr::summarise(gdp = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop")

biomass_hist_intensity <- hist_dmc |>
  dplyr::filter(year %in% 2019:2024, material_category %in% c("Crops", "Grazed biomass and fodder crops")) |>
  dplyr::mutate(mat_key = dplyr::if_else(material_category == "Crops", "crop", "grazed")) |>
  dplyr::group_by(mat_key, year) |>
  dplyr::summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(gdp_world_hist, by = "year") |>
  dplyr::mutate(kg_usd = DMC_Mt * 1e9 / gdp) |>
  dplyr::group_by(mat_key) |>
  dplyr::summarise(kg_usd = mean(kg_usd), .groups = "drop")

crop_intensity_hist <- biomass_hist_intensity$kg_usd[biomass_hist_intensity$mat_key == "crop"]
grazed_intensity_hist <- biomass_hist_intensity$kg_usd[biomass_hist_intensity$mat_key == "grazed"]

fossil_hist_intensity <- hist_dmc |>
  dplyr::filter(year %in% 2019:2024, material_category %in% c("Coal", "Natural Gas", "Petroleum")) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      material_category == "Coal" ~ "coal",
      material_category == "Natural Gas" ~ "gas",
      material_category == "Petroleum" ~ "oil"
    )
  ) |>
  dplyr::group_by(mat_key, year) |>
  dplyr::summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(gdp_world_hist, by = "year") |>
  dplyr::mutate(kg_usd = DMC_Mt * 1e9 / gdp) |>
  dplyr::group_by(mat_key) |>
  dplyr::summarise(kg_usd = mean(kg_usd), .groups = "drop")

coal_intensity_hist <- fossil_hist_intensity$kg_usd[fossil_hist_intensity$mat_key == "coal"]
gas_intensity_hist <- fossil_hist_intensity$kg_usd[fossil_hist_intensity$mat_key == "gas"]
oil_intensity_hist <- fossil_hist_intensity$kg_usd[fossil_hist_intensity$mat_key == "oil"]
oil_ng_intensity_hist <- oil_intensity_hist + gas_intensity_hist

stock_hist_intensity <- stock_hist |>
  dplyr::filter(year %in% 2019:2024, material %in% c("Metal ores", "Non-metallic minerals")) |>
  dplyr::group_by(material, year) |>
  dplyr::summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(gdp_world_hist, by = "year") |>
  dplyr::mutate(kg_usd = stock_Mt * 1e9 / gdp) |>
  dplyr::group_by(material) |>
  dplyr::summarise(kg_usd = mean(kg_usd), .groups = "drop")

stock_intensity_metal_hist <- stock_hist_intensity$kg_usd[stock_hist_intensity$material == "Metal ores"]
stock_intensity_minerals_hist <- stock_hist_intensity$kg_usd[stock_hist_intensity$material == "Non-metallic minerals"]

recycling_index_hist <- secondary_flows_hist |>
  dplyr::filter(year %in% 2019:2024, material_group == "metal_ores") |>
  dplyr::group_by(year) |>
  dplyr::summarise(rate = sum(recovered_Mt, na.rm = TRUE) / sum(waste_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::summarise(rate = mean(rate)) |>
  dplyr::pull(rate)

lifetime_bldg_2024 <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category == "buildings") |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)
lifetime_civil_2024 <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category == "civil_infrastructure") |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)
lifetime_minerals_2024 <- (w_bldg * lifetime_bldg_2024 + w_civil * lifetime_civil_2024) / (w_bldg + w_civil)


# Panel 1: Spaghetti plots (primary consumption, 2010-2060) --------

## Biomass --------

plot_biomass_ts <- proj_4mat |>
  dplyr::filter(material_group == "Biomass") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)
hist_biomass <- hist_4mat |> dplyr::filter(material_group == "Biomass")

# Direct labels for the 3 highlighted pathway classes, placed at the year of
# widest separation (end of horizon) instead of relying on the shared legend.
# group_by(path_class) on the (possibly empty) filtered data means a class
# with zero runs simply contributes no label row, rather than erroring.
label_biomass_pts <- plot_biomass_ts |>
  dplyr::filter(year == FORECAST_END, path_class != "Other runs") |>
  dplyr::group_by(path_class) |>
  dplyr::summarise(
    y_max = max(primary_Gt_smooth),
    y_min = min(primary_Gt_smooth),
    y_med = stats::median(primary_Gt_smooth),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    y = dplyr::case_when(
      path_class == "High growth" ~ y_max,
      path_class == "Absolute decoupling" ~ y_min,
      TRUE ~ y_med
    ),
    label_vjust = dplyr::case_when(
      path_class == "High growth" ~ -0.6,
      path_class == "Absolute decoupling" ~ 1.4,
      TRUE ~ 0.5
    )
  )

p_biomass_ts <- ggplot(plot_biomass_ts, aes(x = year, y = primary_Gt_smooth, group = run_id)) +
  geom_line(aes(colour = path_class, alpha = path_class, linewidth = path_class)) +
  geom_line(
    data = hist_biomass,
    aes(x = year, y = primary_Gt),
    inherit.aes = FALSE,
    colour = unname(PALETTE_MATERIAL_GROUPS[["Biomass"]]),
    linewidth = 0.9
  ) +
  present_line_labeled +
  geom_text(
    data = label_biomass_pts,
    aes(y = y, label = path_class, colour = path_class, vjust = label_vjust),
    x = FORECAST_END,
    hjust = 1,
    size = pb_annot_size("largeFont"),
    fontface = "bold",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_alpha_manual(values = PATH_CLASS_ALPHA, guide = "none") +
  scale_linewidth_manual(values = PATH_CLASS_LINEWIDTH, guide = "none") +
  x_sc +
  y_sc +
  co +
  labs(title = "Biomass", tag = "a", x = "Year", y = "Primary material consumption (Gt)") +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Biomass"]]), hjust = 0.5))

## Fossil fuels --------

plot_fossil_ts <- proj_4mat |>
  dplyr::filter(material_group == "Fossil fuels") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)
hist_fossil <- hist_4mat |> dplyr::filter(material_group == "Fossil fuels")

p_fossil_ts <- ggplot(plot_fossil_ts, aes(x = year, y = primary_Gt_smooth, group = run_id)) +
  geom_line(aes(colour = path_class, alpha = path_class, linewidth = path_class)) +
  geom_line(
    data = hist_fossil,
    aes(x = year, y = primary_Gt),
    inherit.aes = FALSE,
    colour = unname(PALETTE_MATERIAL_GROUPS[["Fossil fuels"]]),
    linewidth = 0.9
  ) +
  present_line +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_alpha_manual(values = PATH_CLASS_ALPHA, guide = "none") +
  scale_linewidth_manual(values = PATH_CLASS_LINEWIDTH, guide = "none") +
  x_sc +
  y_sc +
  co +
  labs(title = "Fossil fuels", tag = "c", x = "Year", y = "Primary material consumption (Gt)") +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Fossil fuels"]]), hjust = 0.5))

## Metal ores --------

plot_metal_ts <- proj_4mat |>
  dplyr::filter(material_group == "Metal ores") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)
hist_metal <- hist_4mat |> dplyr::filter(material_group == "Metal ores")

p_metal_ts <- ggplot(plot_metal_ts, aes(x = year, y = primary_Gt_smooth, group = run_id)) +
  geom_line(aes(colour = path_class, alpha = path_class, linewidth = path_class)) +
  geom_line(
    data = hist_metal,
    aes(x = year, y = primary_Gt),
    inherit.aes = FALSE,
    colour = unname(PALETTE_MATERIAL_GROUPS[["Metal ores"]]),
    linewidth = 0.9
  ) +
  present_line +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_alpha_manual(values = PATH_CLASS_ALPHA, guide = "none") +
  scale_linewidth_manual(values = PATH_CLASS_LINEWIDTH, guide = "none") +
  x_sc +
  y_sc +
  co +
  labs(title = "Metal ores", tag = "e", x = "Year", y = "Primary material consumption (Gt)") +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Metal ores"]]), hjust = 0.5))

## Non-metallic minerals --------

plot_mineral_ts <- proj_4mat |>
  dplyr::filter(material_group == "Non-metallic minerals") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)
hist_mineral <- hist_4mat |> dplyr::filter(material_group == "Non-metallic minerals")

p_mineral_ts <- ggplot(plot_mineral_ts, aes(x = year, y = primary_Gt_smooth, group = run_id)) +
  geom_line(aes(colour = path_class, alpha = path_class, linewidth = path_class)) +
  geom_line(
    data = hist_mineral,
    aes(x = year, y = primary_Gt),
    inherit.aes = FALSE,
    colour = unname(PALETTE_MATERIAL_GROUPS[["Non-metallic minerals"]]),
    linewidth = 0.9
  ) +
  present_line +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_alpha_manual(values = PATH_CLASS_ALPHA, guide = "none") +
  scale_linewidth_manual(values = PATH_CLASS_LINEWIDTH, guide = "none") +
  x_sc +
  y_sc +
  co +
  labs(title = "Non-metallic minerals", tag = "g", x = "Year", y = "Primary material consumption (Gt)") +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Non-metallic minerals"]]), hjust = 0.5))


# Panel 2: Scatter plots (MC input assumptions) --------

## Biomass: crop intensity vs grazed biomass intensity --------

scatter_biomass <- run_vars |>
  dplyr::inner_join(dplyr::filter(run_class, material_group == "Biomass"), by = "run_id") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)

p_biomass_sc <- ggplot(scatter_biomass, aes(x = crop_intensity, y = grazed_intensity)) +
  geom_point(aes(colour = path_class, size = gdp_pc_cagr), shape = 16, alpha = 0.45) +
  annotate("point", x = crop_intensity_hist, y = grazed_intensity_hist, shape = "★", size = 6, colour = "black") +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_radius(
    name = "GDP growth rate (%/yr)",
    range = c(0.3, 6),
    limits = SIZE_RANGE,
    labels = scales::percent_format(accuracy = 0.1),
    guide = "none"
  ) +
  labs(title = "Biomass", tag = "b", x = "Crop intensity (kg/USD)", y = "Grazed biomass intensity (kg/USD)") +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Biomass"]]), hjust = 0.5))

## Fossil fuels: oil+NG intensity vs coal intensity --------

scatter_fossil <- run_vars |>
  dplyr::inner_join(dplyr::filter(run_class, material_group == "Fossil fuels"), by = "run_id") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)

p_fossil_sc <- ggplot(scatter_fossil, aes(x = oil_ng_intensity, y = coal_intensity)) +
  geom_point(aes(colour = path_class, size = gdp_pc_cagr), shape = 16, alpha = 0.45) +
  annotate("point", x = oil_ng_intensity_hist, y = coal_intensity_hist, shape = "★", size = 6, colour = "black") +
  annotate(
    "text",
    x = oil_ng_intensity_hist,
    y = coal_intensity_hist,
    label = "Historical\n(2019-2024)",
    hjust = 1,
    vjust = -0.6,
    fontface = "bold",
    size = pb_annot_size("largeFont", pt = 11),
    colour = "black"
  ) +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_radius(
    name = "GDP growth rate (%/yr)",
    range = c(0.3, 6),
    limits = SIZE_RANGE,
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  guides(size = guide_legend(position = "inside")) +
  labs(title = "Fossil fuels", tag = "d", x = "Oil + natural gas intensity (kg/USD)", y = "Coal intensity (kg/USD)") +
  theme_pb_large() +
  theme(
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Fossil fuels"]]), hjust = 0.5),
    legend.position.inside = c(0.97, 0.03),
    legend.justification.inside = c(1, 0)
  )

## Metal ores: ferrous recycling rate vs ferrous ore grade --------

scatter_metal <- run_vars |>
  dplyr::inner_join(dplyr::filter(run_class, material_group == "Metal ores"), by = "run_id") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)

p_metal_sc <- ggplot(scatter_metal, aes(x = stock_intensity_metal, y = recycling_index)) +
  geom_point(aes(colour = path_class, size = gdp_pc_cagr), shape = 16, alpha = 0.45) +
  annotate("point", x = stock_intensity_metal_hist, y = recycling_index_hist, shape = "★", size = 6, colour = "black") +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_radius(
    name = "GDP growth rate (%/yr)",
    range = c(0.3, 6),
    limits = SIZE_RANGE,
    labels = scales::percent_format(accuracy = 0.1),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Metal ores",
    tag = "f",
    x = "Metal ore stock intensity (kg/USD)",
    y = "Recycling index - Fe & NonFe (%)"
  ) +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Metal ores"]]), hjust = 0.5))

## Non-metallic minerals: downcycling vs lifetime (stock-weighted) --------

scatter_mineral <- run_vars |>
  dplyr::inner_join(dplyr::filter(run_class, material_group == "Non-metallic minerals"), by = "run_id") |>
  dplyr::mutate(path_class = factor(path_class, levels = PATH_CLASS_LEVELS)) |>
  dplyr::arrange(path_class)

p_mineral_sc <- ggplot(scatter_mineral, aes(x = stock_intensity_minerals, y = lifetime_minerals)) +
  geom_point(aes(colour = path_class, size = gdp_pc_cagr), shape = 16, alpha = 0.45) +
  annotate(
    "point",
    x = stock_intensity_minerals_hist,
    y = lifetime_minerals_2024,
    shape = "★",
    size = 6,
    colour = "black"
  ) +
  scale_colour_manual(values = PATH_CLASS_COLORS, name = "Pathway", guide = "none") +
  scale_radius(
    name = "GDP growth rate (%/yr)",
    range = c(0.3, 6),
    limits = SIZE_RANGE,
    labels = scales::percent_format(accuracy = 0.1),
    guide = "none"
  ) +
  labs(
    title = "Non-metallic minerals",
    tag = "h",
    x = "Non-metallic minerals stock intensity (kg/USD)",
    y = "Average product lifetime (years)"
  ) +
  theme_pb_large() +
  theme(plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS[["Non-metallic minerals"]]), hjust = 0.5))


# Combine & save --------

fig5 <- patchwork::wrap_plots(
  p_biomass_ts,
  p_biomass_sc,
  p_fossil_ts,
  p_fossil_sc,
  p_metal_ts,
  p_metal_sc,
  p_mineral_ts,
  p_mineral_sc,
  ncol = 2,
  byrow = TRUE
) &
  theme(
    plot.tag = element_text(face = "bold", margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt")),
    plot.tag.location = "panel"
  )

ggsave("Figures/Supporting-Figures/S12_TimeSeriesProjection.png", fig5, units = "cm", dpi = 600, width = 18, height = 8.7 * 4)
ggsave("Figures/SVG/Supporting-Figures/S12_TimeSeriesProjection.svg", fig5, units = "cm", width = 18, height = 8.7 * 4)
group_svg_layers("Figures/SVG/Supporting-Figures/S12_TimeSeriesProjection.svg")

# EoF
