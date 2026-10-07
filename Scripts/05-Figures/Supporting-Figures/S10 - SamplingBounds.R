## =============================================================================
## S10 - SamplingBounds.R  -> Supporting Figure S10
## Summary of every Monte Carlo sampled variable's bounds, in the format of
## summary_ratio_2060_2025.png: horizontal min-max bars, regions as coloured
## lines, open circle = current (2024) value.
##
## Values are ABSOLUTE endpoints (2024 value x ratio bounds), not ratios.
## Rows (top to bottom):
##   Population / GDP per capita (range across SSPs in FORECAST_END)
##   Biomass flow intensities (kg/$): per SSP cluster x region (SSP_SAMPLED; SSP4 excluded)
##   Fossil fuel flow intensities (kg/$)
##   Metal ore stock intensities (4 panels, 2 rows)
##   Non-metallic mineral stock intensities
##   Recycling / downcycling endpoint rates (one shared bar, regional 2024 points) +
##     non-metallic secondary room shares + target years (central value marked)
##   Ore grade (Fe, non-Fe) + mean lifetime + Weibull shape + region legend
##
## Input:  Parameters/Simulation/flow_ratio_bounds.csv, stock_ratio_bounds.csv (01-Sampling.R),
##         ASSUMPTIONS_FILE (2024 intensities), RECYCLING_FILE, MC_Assumptions.xlsx,
##         Parameters/IIASA-Trajectories/ssp_drivers.csv
## Output: Figures/Simulation/SamplingBounds/sampling_bounds.png (+ SVG mirror)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
library(patchwork)

FIG_DIR <- "Figures/Simulation/SamplingBounds"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Regions sorted by decreasing 2024 GDP per capita (top to bottom in every panel and the legend)
ssp_drivers <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)
REGION_ORDER <- ssp_drivers |>
  dplyr::filter(variable == "GDP|PPP [per capita]", year == 2024L, region %in% names(PALETTE_REGIONS)) |>
  dplyr::group_by(region) |>
  dplyr::summarise(gdppc_2024 = mean(value), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(gdppc_2024)) |>
  dplyr::pull(region)
SSP_LEVELS <- SSP_SAMPLED # SSP4 excluded unless re-added in 00-CommonParameters.R
PALETTE_FIG <- c(PALETTE_REGIONS, "All regions" = "#000000")

pb_set_geom_defaults("wide")


# Step 1: 2024 intensities (value_2024) per region x material -----------------

cat("STEP 1: Load 2024 values and bounds\n")

int_biomass <- readxl::read_excel(ASSUMPTIONS_FILE, sheet = "Biomass") |>
  dplyr::transmute(
    region = Region,
    mat_key = dplyr::recode(
      Category,
      "Crops" = "crops",
      "Grazed biomass and fodder crops" = "grazed_biomass",
      "Wood" = "wood",
      .default = NA_character_
    ),
    int_2024 = `kg/USD 2024`
  )
int_fossil <- readxl::read_excel(ASSUMPTIONS_FILE, sheet = "FossilFuels") |>
  dplyr::transmute(region = Region, mat_key = fuel, int_2024 = `kg/USD 2024`)
int_metal <- readxl::read_excel(ASSUMPTIONS_FILE, sheet = "MetalOres", skip = 1) |>
  dplyr::transmute(region, End_use = end_use, material_group = "metal_ores", int_2024 = `kg/USD 2024`)
int_nonmet <- readxl::read_excel(ASSUMPTIONS_FILE, sheet = "NonMetallicMinerals", skip = 1) |>
  dplyr::transmute(region, End_use = end_use, material_group = "nonmetallic_minerals", int_2024 = `kg/USD 2024`)

int_stock <- dplyr::bind_rows(int_metal, int_nonmet) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      stringr::str_detect(tolower(End_use), "build") ~ "buildings",
      stringr::str_detect(tolower(End_use), "civil") ~ "civil",
      stringr::str_detect(tolower(End_use), "short") ~ "sl_products",
      stringr::str_detect(tolower(End_use), "machin") ~ "machinery",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(region), !is.na(mat_key)) |>
  dplyr::distinct(region, material_group, mat_key, .keep_all = TRUE)

flow_ratio_bounds <- readr::read_csv("Parameters/Simulation/flow_ratio_bounds.csv", show_col_types = FALSE)
stock_ratio_bounds <- readr::read_csv("Parameters/Simulation/stock_ratio_bounds.csv", show_col_types = FALSE)


# Step 2: Shared axis and theme conventions ------------------------------------

# x axis: starts at 0, zero expansion, tick at every break, number on every 2nd break
LAB_EVERY2_NUM <- \(b) ifelse(round(b / min(diff(b), na.rm = TRUE)) %% 2 == 0, scales::label_comma(drop0trailing = TRUE)(b), "")
LAB_EVERY2_PCT <- \(b) ifelse(round(b / min(diff(b), na.rm = TRUE)) %% 2 == 0, scales::label_percent(drop0trailing = TRUE)(b), "")

X_NUM <- scale_x_continuous(
  limits = c(0, NA),
  breaks = scales::breaks_extended(n = 8, Q = c(1, 5, 2, 2.5)),
  labels = LAB_EVERY2_NUM,
  expand = expansion(0)
)
X_PCT <- scale_x_continuous(
  limits = c(0, NA),
  breaks = scales::breaks_extended(n = 8, Q = c(1, 5, 2, 2.5)),
  labels = LAB_EVERY2_PCT,
  expand = expansion(0)
)

THEME_ROW <- theme_pb_wide() +
  theme(
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0),
    legend.position = "none"
  )


# Step 3: Flow intensities -- SSP clusters x regions ---------------------------

FLOW_LABELS <- c(
  "coal" = "Coal",
  "oil" = "Petroleum",
  "gas" = "Natural gas",
  "crops" = "Crops (and crop residues)",
  "grazed_biomass" = "Grazed biomass and fodder crops",
  "wood" = "Wood"
)

REGION_STEP <- 0.08 # cluster height = 7 * 0.08 = 0.56 -> 0.44 gap between SSPs

flow_data <- flow_ratio_bounds |>
  dplyr::inner_join(dplyr::bind_rows(int_biomass, int_fossil), by = c("region", "mat_key")) |>
  dplyr::mutate(
    lo = int_2024 * ratio_min,
    hi = int_2024 * ratio_max,
    var_label = factor(FLOW_LABELS[mat_key], levels = FLOW_LABELS),
    s = match(ssp, SSP_LEVELS),
    r = match(region, REGION_ORDER),
    y = s + (r - (length(REGION_ORDER) + 1) / 2) * REGION_STEP
  )

p_bio <- ggplot(flow_data |> dplyr::filter(mat_key %in% c("crops", "grazed_biomass", "wood"))) +
  geom_segment(aes(x = lo, xend = hi, y = y, yend = y, colour = region), linewidth = 0.6) +
  geom_point(aes(x = int_2024, y = y, colour = region), shape = 21, fill = "white", stroke = 0.4, size = 0.9) +
  scale_colour_manual(values = PALETTE_FIG, guide = "none") +
  X_NUM +
  scale_y_reverse(breaks = seq_along(SSP_LEVELS), labels = SSP_LEVELS) +
  facet_wrap(~var_label, ncol = 3, scales = "free_x", labeller = label_wrap_gen(width = 30)) +
  coord_cartesian(clip = "off") +
  labs(x = "kg/$ of GDP", y = NULL, title = "Biomass flow intensity in the target year") +
  THEME_ROW

p_fossil <- p_bio +
  (flow_data |> dplyr::filter(mat_key %in% c("coal", "oil", "gas"))) +
  labs(title = "Fossil fuel flow intensity in the target year")


# Step 4: Region-level variables -- one bar per region -------------------------

STOCK_LABELS <- c(
  "buildings" = "Buildings",
  "civil" = "Civil infrastructure",
  "machinery" = "Machinery",
  "sl_products" = "Short-lived products"
)
STOCK_KEYS <- c(
  paste0("metal_ores.", names(STOCK_LABELS)),
  "nonmetallic_minerals.buildings",
  "nonmetallic_minerals.civil"
)

stock_data <- stock_ratio_bounds |>
  dplyr::mutate(key = paste(material_group, mat_key, sep = ".")) |>
  dplyr::filter(key %in% STOCK_KEYS) |>
  dplyr::inner_join(int_stock |> dplyr::select(region, material_group, mat_key, int_2024), by = c("region", "material_group", "mat_key")) |>
  dplyr::transmute(
    material_group,
    var_label = factor(STOCK_LABELS[mat_key], levels = STOCK_LABELS),
    region = factor(region, levels = rev(REGION_ORDER)),
    lo = int_2024 * ratio_min,
    hi = int_2024 * ratio_max,
    now = int_2024
  )

p_metal <- ggplot(stock_data |> dplyr::filter(material_group == "metal_ores")) +
  geom_segment(aes(x = lo, xend = hi, y = region, yend = region, colour = region), linewidth = 0.8) +
  geom_point(aes(x = now, y = region, colour = region), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  scale_colour_manual(values = PALETTE_FIG, guide = "none") +
  X_NUM +
  scale_y_discrete(drop = TRUE) +
  facet_wrap(~var_label, ncol = 2, scales = "free_x") +
  coord_cartesian(clip = "off") +
  labs(x = "kg of stock per $ of GDP", y = NULL, title = "Metal ore stock intensity in the target year") +
  THEME_ROW +
  theme(axis.text.y = element_blank())

p_mineral <- p_metal +
  (stock_data |> dplyr::filter(material_group == "nonmetallic_minerals")) +
  labs(title = "Non-metallic mineral stock intensity in the target year")

# SSP drivers: range across the sampled SSPs in FORECAST_END; point = 2024 value
driver_data <- ssp_drivers |>
  dplyr::filter(variable %in% c("Population", "GDP|PPP [per capita]"), year %in% c(2024L, FORECAST_END), scenario %in% SSP_SAMPLED) |>
  dplyr::mutate(value = dplyr::if_else(variable == "Population", value, value / 1e3)) |>
  dplyr::group_by(variable, region) |>
  dplyr::summarise(
    lo = min(value[year == FORECAST_END]),
    hi = max(value[year == FORECAST_END]),
    now = mean(value[year == 2024L]),
    .groups = "drop"
  ) |>
  dplyr::mutate(region = factor(region, levels = rev(REGION_ORDER)))

p_pop <- ggplot(driver_data |> dplyr::filter(variable == "Population")) +
  geom_segment(aes(x = lo, xend = hi, y = region, yend = region, colour = region), linewidth = 0.8) +
  geom_point(aes(x = now, y = region, colour = region), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  scale_colour_manual(values = PALETTE_FIG, guide = "none") +
  X_NUM +
  scale_y_discrete(drop = TRUE) +
  coord_cartesian(clip = "off") +
  labs(x = "Million people", y = NULL, title = paste0("Population, ", FORECAST_END, " (range across SSPs)")) +
  THEME_ROW +
  theme(axis.text.y = element_blank())

p_gdp <- p_pop +
  (driver_data |> dplyr::filter(variable == "GDP|PPP [per capita]")) +
  labs(x = "Thousand $ per person (PPP)", title = paste0("GDP per capita, ", FORECAST_END, " (range across SSPs)"))


# Step 5: Recovery rates -- one panel, 3 rows ----------------------------------

# Target-year endpoint bounds shared by all regions (black bar, black tick = central);
# open circles = 2024 regional anchors on the same row
recycling_raw <- readxl::read_excel(RECYCLING_FILE, sheet = "Recycling_EOL") |>
  dplyr::filter(!stringr::str_detect(Region, "—"))
downcycling_raw <- readxl::read_excel(RECYCLING_FILE, sheet = "Downcycling") |>
  dplyr::filter(!stringr::str_detect(Region, "—"))

RATE_VARS <- tibble::tribble(
  ~var_label, ~lo, ~hi, ~central,
  "Recycling rate, Fe", RECYCLING_RATE_FE_MIN, RECYCLING_RATE_FE_MAX, RECYCLING_RATE_FE_CENTRAL,
  "Recycling rate, non-Fe", RECYCLING_RATE_NONFE_MIN, RECYCLING_RATE_NONFE_MAX, RECYCLING_RATE_NONFE_CENTRAL,
  "Downcycling rate, construction minerals", DOWNCYCLING_MIN, DOWNCYCLING_MAX, DOWNCYCLING_CENTRAL
)
RATE_LEVELS <- rev(RATE_VARS$var_label)

rate_now <- dplyr::bind_rows(
  recycling_raw |> dplyr::transmute(var_label = "Recycling rate, Fe", region = Region, now = Recycling_rate_Fe),
  recycling_raw |> dplyr::transmute(var_label = "Recycling rate, non-Fe", region = Region, now = Recycling_rate_NonFe),
  downcycling_raw |> dplyr::transmute(var_label = "Downcycling rate, construction minerals", region = Region, now = `Downcycling rate`)
) |>
  dplyr::mutate(var_label = factor(var_label, levels = RATE_LEVELS))

p_recyc <- ggplot() +
  geom_segment(
    data = RATE_VARS |> dplyr::mutate(var_label = factor(var_label, levels = RATE_LEVELS)),
    aes(x = lo, xend = hi, y = var_label, yend = var_label),
    colour = "black",
    linewidth = 0.8
  ) +
  geom_point(
    data = RATE_VARS |> dplyr::mutate(var_label = factor(var_label, levels = RATE_LEVELS)),
    aes(x = central, y = var_label),
    shape = "|",
    colour = "black",
    size = 2.5
  ) +
  geom_point(
    data = rate_now,
    aes(x = now, y = var_label, colour = region),
    shape = 21,
    fill = "white",
    stroke = 0.4,
    size = 1.0
  ) +
  scale_colour_manual(values = PALETTE_FIG, guide = "none") +
  X_PCT +
  scale_y_discrete(labels = scales::label_wrap(22)) +
  coord_cartesian(clip = "off") +
  labs(x = "% of end-of-life flow", y = NULL, title = "Recycling and downcycling rates") +
  THEME_ROW


# Step 6: Global scalars -------------------------------------------------------

# Global scalars (MC_PARAMS, 00-Parameters.R): bar = min-max, open circle = central value
scalar_data <- tibble::tribble(
  ~panel, ~item, ~lo, ~hi, ~now,
  "Target years", "Intensity target year", TARGET_YEAR_MIN, TARGET_YEAR_MAX, TARGET_YEAR_CENTRAL,
  "Target years", "Recycling convergence year", RECYC_CONVERGENCE_YR_MIN, RECYC_CONVERGENCE_YR_MAX, RECYC_CONVERGENCE_YR_CENTRAL,
  "Ore grade", "Fe", GRADE_ORE_FE_MIN, GRADE_ORE_FE_MAX, GRADE_ORE_FE_CENTRAL,
  "Ore grade", "Non-Fe", GRADE_ORE_NONFE_MIN, GRADE_ORE_NONFE_MAX, GRADE_ORE_NONFE_CENTRAL,
  "Secondary room", "Max secondary share, buildings and civil", MAX_SECONDARY_BUILD_CIVIL_MIN, MAX_SECONDARY_BUILD_CIVIL_MAX, MAX_SECONDARY_BUILD_CIVIL_CENTRAL,
  "Secondary room", "Max secondary share, roads", MAX_SECONDARY_ROADS_MIN, MAX_SECONDARY_ROADS_MAX, MAX_SECONDARY_ROADS_CENTRAL,
  "Secondary room", "Concrete share, buildings", SHARE_CONCRETE_BUILDINGS_MIN, SHARE_CONCRETE_BUILDINGS_MAX, SHARE_CONCRETE_BUILDINGS_CENTRAL,
  "Secondary room", "Concrete share, civil engineering", SHARE_CONCRETE_CIVIL_MIN, SHARE_CONCRETE_CIVIL_MAX, SHARE_CONCRETE_CIVIL_CENTRAL,
  "Secondary room", "Aggregate share of concrete", SHARE_AGG_CONCRETE_MIN, SHARE_AGG_CONCRETE_MAX, SHARE_AGG_CONCRETE_CENTRAL,
  "Secondary room", "Granular share, roads", SHARE_GRANULAR_ROAD_MIN, SHARE_GRANULAR_ROAD_MAX, SHARE_GRANULAR_ROAD_CENTRAL
) |>
  dplyr::mutate(item = factor(item, levels = rev(unique(item))))

p_second <- ggplot(scalar_data |> dplyr::filter(panel == "Secondary room", !is.na(item)) |> droplevels()) +
  geom_segment(aes(x = lo, xend = hi, y = item, yend = item), colour = "black", linewidth = 0.8) +
  geom_point(aes(x = now, y = item), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  X_PCT +
  scale_y_discrete(labels = scales::label_wrap(22)) +
  coord_cartesian(clip = "off") +
  labs(x = "Share of demand or material (%)", y = NULL, title = "Secondary room, non-metallic minerals") +
  THEME_ROW

# Years: axis cannot start at 0; round the range out to the nearest decade, label every 10 years
p_target <- ggplot(scalar_data |> dplyr::filter(panel == "Target years") |> droplevels()) +
  geom_segment(aes(x = lo, xend = hi, y = item, yend = item), colour = "black", linewidth = 0.8) +
  geom_point(aes(x = now, y = item), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  scale_x_continuous(
    limits = c(
      floor(min(TARGET_YEAR_MIN, RECYC_CONVERGENCE_YR_MIN) / 10) * 10,
      ceiling(max(TARGET_YEAR_MAX, RECYC_CONVERGENCE_YR_MAX) / 10) * 10
    ),
    breaks = scales::breaks_width(5),
    labels = \(b) ifelse(b %% 10 == 0, b, ""),
    expand = expansion(0)
  ) +
  scale_y_discrete(labels = scales::label_wrap(16)) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = NULL, title = "Target years") +
  THEME_ROW

p_ore_fe <- ggplot(scalar_data |> dplyr::filter(panel == "Ore grade", item == "Fe")) +
  geom_segment(aes(x = lo, xend = hi, y = item, yend = item), colour = "black", linewidth = 0.8) +
  geom_point(aes(x = now, y = item), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  X_PCT +
  coord_cartesian(clip = "off") +
  labs(x = "% metal content", y = NULL, title = "Ore grade, Fe") +
  THEME_ROW +
  theme(axis.text.y = element_blank())

p_ore_nonfe <- p_ore_fe +
  (scalar_data |> dplyr::filter(panel == "Ore grade", item == "Non-Fe")) +
  labs(title = "Ore grade, non-Fe")

SUB_USE_LABELS <- c(
  "residential" = "Residential",
  "non_residential" = "Non-residential",
  "roads" = "Roads",
  "civil_engineering" = "Civil engineering",
  "machinery_group" = "Machinery",
  "vehicles_group" = "Vehicles",
  "durables" = "Durables",
  "packaging" = "Packaging"
)

life_data <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::transmute(
    item = factor(SUB_USE_LABELS[sub_use], levels = rev(SUB_USE_LABELS)),
    lo = mean_life_min,
    hi = mean_life_max,
    now = mean_life,
    k_lo = k_min,
    k_hi = k_max,
    k_now = weibull_k
  )

p_life <- ggplot(life_data) +
  geom_segment(aes(x = lo, xend = hi, y = item, yend = item), colour = "black", linewidth = 0.8) +
  geom_point(aes(x = now, y = item), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  # Short-lived sub-uses: bars too short to read, print bounds to the right
  geom_text(
    data = life_data |> dplyr::filter(item %in% c("Machinery", "Vehicles", "Durables", "Packaging")),
    aes(x = hi, y = item, label = paste0(round(lo, 1), "–", round(hi, 1))),
    hjust = 0,
    nudge_x = 5,
    size = pb_annot_size("wide", 7)
  ) +
  X_NUM +
  coord_cartesian(clip = "off") +
  labs(x = "Years", y = NULL, title = "Mean lifetime") +
  THEME_ROW

p_shape <- ggplot(life_data) +
  geom_segment(aes(x = k_lo, xend = k_hi, y = item, yend = item), colour = "black", linewidth = 0.8) +
  geom_point(aes(x = k_now, y = item), shape = 21, fill = "white", stroke = 0.4, size = 1.0) +
  X_NUM +
  coord_cartesian(clip = "off") +
  labs(x = "Weibull shape k (–)", y = NULL, title = "Lifetime shape") +
  THEME_ROW +
  theme(axis.text.y = element_blank())

# Region legend as its own panel (invisible data, legend only)
p_legend <- ggplot(tibble::tibble(region = factor(c(REGION_ORDER, "All regions"), levels = c(REGION_ORDER, "All regions")), x = 0)) +
  geom_segment(aes(x = x, xend = x, y = x, yend = x, colour = region), alpha = 0) +
  geom_point(aes(x = x, y = x, colour = region), shape = 21, fill = "white", stroke = 0.4, size = 1.0, alpha = 0) +
  scale_colour_manual(values = PALETTE_FIG, name = NULL) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(alpha = 1, linewidth = 1.2))) +
  theme_void() +
  theme(
    legend.position = c(0, 0.5),
    legend.justification = c(0, 0.5),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.key.spacing.y = unit(0, "pt")
  )


# Step 7: Assemble and save ----------------------------------------------------

cat("STEP 7: Assemble and save\n")

row_drivers <- p_pop + p_gdp
row_rates <- p_recyc + p_second + p_target
row_life <- wrap_plots(p_ore_fe / p_ore_nonfe, p_life, p_shape, p_legend, nrow = 1, widths = c(1, 1.2, 1, 1))

wrap_plots(
  row_drivers,
  p_bio,
  p_fossil,
  p_metal,
  p_mineral,
  wrap_elements(full = row_rates),
  wrap_elements(full = row_life),
  ncol = 1,
  heights = c(0.45, 1, 1, 0.9, 0.45, 1.6, 1.6)
) +
  plot_annotation(
    caption = paste0(
      "Bars: sampled min–max of the target-year value (one LHS draw per variable, shared by all regions, mapped into each\n",
      "region's own bounds); black = one range for all regions. Open circles: current 2024 value (scalars and target years:\n",
      "central value; lifetimes: historical central value); black tick on recovery rates: central endpoint. Global scalars are\n",
      "sampled with half of the draws on each side of the central value. Flow intensities: ScenarioMIP R10 2060/2025 ratios,\n",
      "GDP-weighted to model regions; SSP4 excluded (single ScenarioMIP run)."
    ),
    theme = theme(plot.caption = element_text(size = 7, hjust = 0, colour = "#666666"))
  )

ggsave("Figures/Supporting-Figures/S10_SamplingBounds.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 29)
ggsave("Figures/SVG/Supporting-Figures/S10_SamplingBounds.svg", ggplot2::last_plot(), units = "cm", width = 17, height = 29)
group_svg_layers("Figures/SVG/Supporting-Figures/S10_SamplingBounds.svg")

cat("  Saved", "Figures/Supporting-Figures/S10_SamplingBounds.png", "\n")

# EoF
