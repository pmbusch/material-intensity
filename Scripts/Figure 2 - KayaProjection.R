## =============================================================================
## Figure 2 - KayaProjection.R
## Eight-panel figure: historical (1970–2024) + projected (2024–2060) Kaya drivers
##   (a) Global population (billions)
##   (b) World GDP per capita (2015 USD)
##   (c) M/G Biomass   — Crops, Crop Residues, Grazed biomass, Wood
##   (d) M/G Fossil fuels — Coal, Natural Gas, Petroleum
##   (e) S/G Metal ores      (buildings + civil infrastructure end-uses)
##   (f) S/G Non-metallic minerals  (buildings + civil infrastructure end-uses)
##   (g) Total material consumption (Gt) — biomass + fossil + metal + non-metallic
##   (h) Total in-use stock (Gt) — metal ores + non-metallic minerals, by end-use
##
## Projection: SSP2 central (dashed); SSP1–5 shaded band per sub-material.
## M/G from Results/: M_Mt or stock_Mt summed globally / projected world GDP.
## Requires running 04a–04d first to produce Results/ files.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04_BaseAssumptions.R", encoding = "UTF-8")
library(patchwork)

# ── Constants --------------------------------

HIST_END <- 2024L
CENTRAL_SSP <- "SSP2"

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum")
FUEL_LABEL <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum")
STOCK_ENDUSE_PROJ <- c("Buildings", "Civil infrastructure")
STOCK_ENDUSE_HIST <- c("buildings", "civil_infrastructure")

METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)
ALL_ENDUSE_HIST <- c("buildings", "civil_infrastructure", "machinery", "short_lived")
ALL_ENDUSE_PROJ <- c("Buildings", "Civil infrastructure", "Machinery", "Short-lived products")

# Colors for single-line panels (muted, opaque)
POP_COLOR <- "#1F618D" # dark steel blue
GDPCAP_COLOR <- "#117A65" # dark teal

# Sizes
RIBBON_ALPHA <- 0.22
HIST_LW <- 0.65
PROJ_LW <- 0.65

# Font size parameters — adjust here to rescale all panels at once
LABEL_SZ <- 2.8 # geom_text size (ggplot units)
FONT_TITLE <- 11.5 # panel title
FONT_AXIS_T <- 9.5 # axis title
FONT_AXIS_L <- 9.0 # axis tick labels

FONT_BUMP <- theme(
  axis.text = element_text(size = FONT_AXIS_L),
  axis.title = element_text(size = FONT_AXIS_T),
  plot.title = element_text(size = FONT_TITLE)
)

LABEL_YEAR <- 1982L # year for sub-material line labels (panels c/d)
STACK_LABEL_YEAR <- 2005L # year for stacked area segment labels (panels g/h)


# ── Shared plot layers --------------------------------

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

x_sc <- scale_x_continuous(breaks = seq(1970, 2060, 20))
y_sc <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

co <- coord_cartesian(xlim = c(1970, 2060), ylim = c(0, NA), clip = "off", expand = F)

panel_tag <- function(ltr) {
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = ltr,
    hjust = -0.5,
    vjust = 1.5,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  )
}


# ── SECTION A: Load data --------------------------------

cat("A: Loading data\n")

pop_world_hist <- read_csv("Parameters/population_world_historical.csv", show_col_types = FALSE)
gdp_world_hist <- read_csv("Parameters/gdp_world.csv", show_col_types = FALSE)
pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
stock_hist_raw <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE)

# Forecast results (run 04a–04d first)
biomass_fc <- read_csv("Results/biomass_forecast.csv", show_col_types = FALSE)
fossil_fc <- read_csv("Results/fossil_fuel_forecast.csv", show_col_types = FALSE)
stock_metal_fc <- read_csv("Results/stock_trajectory_metal_ores.csv", show_col_types = FALSE)
stock_nonmet_fc <- read_csv("Results/stock_trajectory_nonmetallic_minerals.csv", show_col_types = FALSE)
metal_fc <- read_csv("Results/metal_ores_forecast.csv", show_col_types = FALSE)
nonmet_flow_fc <- read_csv("Results/nonmetallic_minerals_forecast.csv", show_col_types = FALSE)


# ── SECTION B: World pop + GDP projections --------------------------------

cat("B: World projections\n")

gdp_2024_region <- gdp_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

world_pop_proj <- ssp_drivers |>
  filter(variable == "Population") |>
  dplyr::select(scenario, region, year, idx = index) |>
  left_join(pop_2024_region, by = "region") |>
  mutate(pop = pop_2024 * idx) |>
  group_by(scenario, year) |>
  summarise(world_pop = sum(pop, na.rm = TRUE), .groups = "drop")

world_gdp_proj <- ssp_drivers |>
  filter(variable == "GDP|PPP") |>
  dplyr::select(scenario, region, year, idx = index) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(gdp = gdp_2024 * idx) |>
  group_by(scenario, year) |>
  summarise(world_gdp = sum(gdp, na.rm = TRUE), .groups = "drop")

world_gdpcap_proj <- world_pop_proj |>
  left_join(world_gdp_proj, by = c("scenario", "year")) |>
  mutate(gdp_cap = world_gdp / world_pop)


# ── SECTION C: Historical M/G for each material group --------------------------------

cat("C: Historical M/G\n")

biomass_hist <- dmc_hist |>
  filter(material_category %in% BIOMASS_CATS) |>
  group_by(year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

fossil_hist <- dmc_hist |>
  filter(material_category %in% FOSSIL_CATS) |>
  group_by(year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

metal_hist <- stock_hist_raw |>
  filter(material == "Metal ores", end_use %in% STOCK_ENDUSE_HIST) |>
  group_by(year) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)

nonmet_hist <- stock_hist_raw |>
  filter(material == "Non-metallic minerals", end_use %in% STOCK_ENDUSE_HIST) |>
  group_by(year) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_hist, by = "year") |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  mutate(mg = stock_Mt * 1e9 / GDP_2015USD)


# ── SECTION D: Projected M/G from Results files --------------------------------

cat("D: Projected M/G (from Results/)\n")

## Biomass
biomass_proj_all <- biomass_fc |>
  group_by(scenario, category, year) |>
  summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_proj, by = c("scenario", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp)

biomass_ssp2 <- biomass_proj_all |> filter(scenario == CENTRAL_SSP, year >= HIST_END)
biomass_range <- biomass_proj_all |>
  filter(year >= HIST_END) |>
  group_by(year, category) |>
  summarise(ymin = min(mg), ymax = max(mg), .groups = "drop")

## Fossil fuels
fossil_proj_all <- fossil_fc |>
  mutate(material_category = FUEL_LABEL[fuel]) |>
  group_by(scenario, material_category, year) |>
  summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_proj, by = c("scenario", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp)

fossil_ssp2 <- fossil_proj_all |> filter(scenario == CENTRAL_SSP, year >= HIST_END)
fossil_range <- fossil_proj_all |>
  filter(year >= HIST_END) |>
  group_by(year, material_category) |>
  summarise(ymin = min(mg), ymax = max(mg), .groups = "drop")

## Metal ores (buildings + civil only)
metal_proj_all <- stock_metal_fc |>
  filter(end_use %in% STOCK_ENDUSE_PROJ) |>
  group_by(scenario, year) |>
  summarise(stock_Mt = sum(total_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_proj, by = c("scenario", "year")) |>
  mutate(mg = stock_Mt * 1e9 / world_gdp)

metal_ssp2 <- metal_proj_all |> filter(scenario == CENTRAL_SSP, year >= HIST_END)
metal_range <- metal_proj_all |>
  filter(year >= HIST_END) |>
  group_by(year) |>
  summarise(ymin = min(mg), ymax = max(mg), .groups = "drop")

## Non-metallic minerals (buildings + civil only)
nonmet_proj_all <- stock_nonmet_fc |>
  filter(end_use %in% STOCK_ENDUSE_PROJ) |>
  group_by(scenario, year) |>
  summarise(stock_Mt = sum(total_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(world_gdp_proj, by = c("scenario", "year")) |>
  mutate(mg = stock_Mt * 1e9 / world_gdp)

nonmet_ssp2 <- nonmet_proj_all |> filter(scenario == CENTRAL_SSP, year >= HIST_END)
nonmet_range <- nonmet_proj_all |>
  filter(year >= HIST_END) |>
  group_by(year) |>
  summarise(ymin = min(mg), ymax = max(mg), .groups = "drop")


# ── SECTION E: Label data (geom_text at LABEL_YEAR for historical lines) --------------------------------

cat("E: Label data\n")

# Panels c & d: label each sub-material at LABEL_YEAR along the historical line
bio_labels <- biomass_hist |>
  filter(year == LABEL_YEAR) |>
  mutate(label_x = str_remove(material_category, " and fodder crops"))

fossil_labels <- fossil_hist |> filter(year == LABEL_YEAR)

# Panels a, b, e, f: label SSP2 projection line at a mid-projection year
SSP2_LABEL_YEAR <- 2044L

pop_ssp2_label <- world_pop_proj |>
  filter(scenario == CENTRAL_SSP, year == SSP2_LABEL_YEAR) |>
  mutate(v = world_pop / 1e9, label = "SSP2")
gdpcap_ssp2_label <- world_gdpcap_proj |>
  filter(scenario == CENTRAL_SSP, year == SSP2_LABEL_YEAR) |>
  mutate(v = gdp_cap, label = "SSP2")
metal_ssp2_label <- metal_ssp2 |> filter(year == SSP2_LABEL_YEAR) |> mutate(label = "SSP2")
nonmet_ssp2_label <- nonmet_ssp2 |> filter(year == SSP2_LABEL_YEAR) |> mutate(label = "SSP2")


# ── SECTION F: Build panels --------------------------------

cat("F: Building panels\n")

## Panel (a) — Population --------------------------------

pop_hist <- pop_world_hist |> filter(year <= HIST_END, year <= 2060) |> mutate(v = population / 1e9)
pop_ssp2 <- world_pop_proj |>
  filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= 2060) |>
  mutate(v = world_pop / 1e9)
pop_range <- world_pop_proj |>
  filter(year >= HIST_END, year <= 2060) |>
  group_by(year) |>
  summarise(ymin = min(world_pop) / 1e9, ymax = max(world_pop) / 1e9, .groups = "drop")

p1 <- ggplot() +
  geom_ribbon(data = pop_range, aes(x = year, ymin = ymin, ymax = ymax), fill = "grey70", alpha = RIBBON_ALPHA) +
  geom_line(data = pop_hist, aes(x = year, y = v), colour = POP_COLOR, linewidth = HIST_LW) +
  geom_line(data = pop_ssp2, aes(x = year, y = v), colour = POP_COLOR, linewidth = PROJ_LW, linetype = "dashed") +
  geom_text(data = pop_ssp2_label,
    aes(x = year, y = v), label = "SSP2", colour = POP_COLOR,
    vjust = -0.6, size = LABEL_SZ, angle = 0) +
  present_line_bot +
  x_sc +
  y_sc +
  co +
  panel_tag("a") +
  labs(x = NULL, y = "Population (billion)", title = "Population") +
  theme_pb_large() +
  FONT_BUMP


## Panel (b) — GDP per capita --------------------------------

gdp_cap_hist <- gdp_world_hist |>
  left_join(pop_world_hist, by = "year") |>
  filter(year <= HIST_END, year <= 2060) |>
  mutate(v = GDP_2015USD / population)

gdp_cap_ssp2 <- world_gdpcap_proj |>
  filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= 2060) |>
  mutate(v = gdp_cap)

gdp_cap_range <- world_gdpcap_proj |>
  filter(year >= HIST_END, year <= 2060) |>
  group_by(year) |>
  summarise(ymin = min(gdp_cap), ymax = max(gdp_cap), .groups = "drop")

p2 <- ggplot() +
  geom_ribbon(data = gdp_cap_range, aes(x = year, ymin = ymin, ymax = ymax), fill = "grey70", alpha = RIBBON_ALPHA) +
  geom_line(data = gdp_cap_hist, aes(x = year, y = v), colour = GDPCAP_COLOR, linewidth = HIST_LW) +
  geom_line(
    data = gdp_cap_ssp2,
    aes(x = year, y = v),
    colour = GDPCAP_COLOR,
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(data = gdpcap_ssp2_label,
    aes(x = year, y = v), label = "SSP2", colour = GDPCAP_COLOR,
    vjust = -0.6, size = LABEL_SZ, angle = 0) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  panel_tag("b") +
  labs(x = NULL, y = "GDP per capita (2015 USD)", title = "GDP per capita") +
  theme_pb_large() +
  FONT_BUMP


## Panel (c) — M/G Biomass --------------------------------

p3 <- ggplot() +
  geom_ribbon(
    data = biomass_range,
    aes(x = year, ymin = ymin, ymax = ymax, fill = category),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = biomass_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(data = biomass_ssp2, aes(x = year, y = mg, colour = category), linewidth = PROJ_LW, linetype = "dashed") +
  geom_text(
    data = bio_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = label_x),
    size = LABEL_SZ, angle = c(-30,0,-60,-15), show.legend = FALSE,nudge_y=0.015
  ) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS) +
  panel_tag("c") +
  labs(x = NULL, y = "M / GDP (kg per 2015 USD)", title = "M/G Biomass") +
  theme_pb_large() +
  FONT_BUMP


## Panel (d) — M/G Fossil fuels --------------------------------

p4 <- ggplot() +
  geom_ribbon(
    data = fossil_range,
    aes(x = year, ymin = ymin, ymax = ymax, fill = material_category),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = fossil_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(
    data = fossil_ssp2,
    aes(x = year, y = mg, colour = material_category),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(
    data = fossil_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = material_category),
    size = LABEL_SZ, angle = 0, show.legend = FALSE,nudge_y=c(0.01,0.01,-0.015)
  ) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS) +
  panel_tag("d") +
  labs(x = NULL, y = "M / GDP (kg per 2015 USD)", title = "M/G Fossil fuels") +
  theme_pb_large() +
  FONT_BUMP


## Panel (e) — S/G Metal ores --------------------------------

METAL_COL <- PALETTE_MATERIAL_GROUPS[["Metal ores"]]

p5 <- ggplot() +
  geom_ribbon(data = metal_range, aes(x = year, ymin = ymin, ymax = ymax), fill = METAL_COL, alpha = RIBBON_ALPHA) +
  geom_line(data = metal_hist, aes(x = year, y = mg), colour = METAL_COL, linewidth = HIST_LW) +
  geom_line(data = metal_ssp2, aes(x = year, y = mg), colour = METAL_COL, linewidth = PROJ_LW, linetype = "dashed") +
  geom_text(data = metal_ssp2_label,
    aes(x = year, y = mg), label = "SSP2", colour = METAL_COL,
    vjust = -0.6, size = LABEL_SZ, angle = 0) +
  present_line_bot +
  x_sc +
  y_sc +
  co +
  panel_tag("e") +
  labs(x = "Year", y = "Stock / GDP (kg per 2015 USD)", title = "S/G Metal ores") +
  theme_pb_large() +
  FONT_BUMP


## Panel (f) — S/G Non-metallic minerals --------------------------------

NONMET_COL <- PALETTE_MATERIAL_GROUPS[["Non-metallic minerals"]]

p6 <- ggplot() +
  geom_ribbon(data = nonmet_range, aes(x = year, ymin = ymin, ymax = ymax), fill = NONMET_COL, alpha = RIBBON_ALPHA) +
  geom_line(data = nonmet_hist, aes(x = year, y = mg), colour = NONMET_COL, linewidth = HIST_LW) +
  geom_line(data = nonmet_ssp2, aes(x = year, y = mg), colour = NONMET_COL, linewidth = PROJ_LW, linetype = "dashed") +
  geom_text(data = nonmet_ssp2_label,
    aes(x = year, y = mg), label = "SSP2", colour = NONMET_COL,
    vjust = -0.6, size = LABEL_SZ, angle = 0) +
  present_line_bot +
  x_sc +
  y_sc +
  co +
  panel_tag("f") +
  labs(x = "Year", y = "Stock / GDP (kg per 2015 USD)", title = "S/G Non-metallic minerals") +
  theme_pb_large() +
  FONT_BUMP


## Data for panels (g) and (h) --------------------------------

cat("F2: Panel g/h data\n")

# Panel g: total material consumption in Gt
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
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Dynamic ordering: biggest cumulative total at bottom of stack
mat_order <- dmc_total_hist |>
  group_by(mat_group) |>
  summarise(total = sum(DMC_Gt), .groups = "drop") |>
  arrange((total)) |>
  pull(mat_group)

dmc_total_hist <- dmc_total_hist |> mutate(mat_group = factor(mat_group, levels = mat_order))

dmc_total_hist_sum <- dmc_total_hist |> group_by(year) |> summarise(DMC_Gt = sum(DMC_Gt), .groups = "drop")

# Label midpoints: cumulative position of each segment at STACK_LABEL_YEAR
dmc_stack_labels <- dmc_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  arrange(desc(mat_group)) |>
  mutate(
    cum_top = cumsum(DMC_Gt),
    cum_bot = dplyr::lag(cum_top, default = 0),
    label_y = (cum_top + cum_bot) / 2,
    label = as.character(mat_group)
  )

dmc_total_proj_all <- bind_rows(
  biomass_fc |>
    group_by(scenario, year) |>
    summarise(M_Gt = sum(M_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
    mutate(mat_group = "Biomass"),
  fossil_fc |>
    group_by(scenario, year) |>
    summarise(M_Gt = sum(M_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
    mutate(mat_group = "Fossil fuels"),
  metal_fc |>
    group_by(scenario, year) |>
    summarise(M_Gt = sum(production_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
    mutate(mat_group = "Metal ores"),
  nonmet_flow_fc |>
    group_by(scenario, year) |>
    summarise(M_Gt = sum(production_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
    mutate(mat_group = "Non-metallic minerals")
) |>
  filter(year >= HIST_END)

# metal_fc and nonmet_flow_fc start at 2025 (DSM scripts override FORECAST_START to 2025L).
# Root cause fix: prepend 2024 historical values for those two groups so the
# projection line starts at the same point as the historical line endpoint.
dmc_2024_bridge <- dmc_total_hist |>
  filter(year == HIST_END, as.character(mat_group) %in% c("Metal ores", "Non-metallic minerals")) |>
  mutate(mat_group = as.character(mat_group)) |>
  dplyr::select(mat_group, M_Gt = DMC_Gt) |>
  crossing(scenario = SSP_COLS) |>
  mutate(year = HIST_END)

dmc_total_proj_all <- bind_rows(dmc_total_proj_all, dmc_2024_bridge)

dmc_ssp2 <- dmc_total_proj_all |>
  filter(scenario == CENTRAL_SSP) |>
  group_by(year) |>
  summarise(DMC_Gt = sum(M_Gt, na.rm = TRUE), .groups = "drop")

dmc_proj_range <- dmc_total_proj_all |>
  group_by(scenario, year) |>
  summarise(total_Gt = sum(M_Gt, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  summarise(ymin = min(total_Gt), ymax = max(total_Gt), .groups = "drop")

dmc_ssp2_label <- dmc_ssp2 |> filter(year == SSP2_LABEL_YEAR)

# Diagnostics: root-cause check for historical vs. forecast connection at 2024
cat("  [Diag g] Historical total at 2024:", filter(dmc_total_hist_sum, year == HIST_END)$DMC_Gt, "Gt\n")
cat(
  "  [Diag g] SSP2 forecast at 2024   :",
  {
    x <- filter(dmc_ssp2, year == HIST_END)
    if (nrow(x) > 0) x$DMC_Gt else "no row"
  },
  "Gt\n"
)
cat("  [Diag g] metal_fc years          :", paste(range(metal_fc$year), collapse = "-"), "\n")
cat("  [Diag g] nonmet_flow_fc years    :", paste(range(nonmet_flow_fc$year), collapse = "-"), "\n")

# Panel h: total in-use stock (metal + non-metallic) by end-use in Gt
stock_total_hist <- stock_hist_raw |>
  filter(material %in% c("Metal ores", "Non-metallic minerals"), end_use %in% ALL_ENDUSE_HIST) |>
  mutate(end_use_label = ENDUSE_LABELS[end_use]) |>
  group_by(year, end_use_label) |>
  summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  filter(year <= HIST_END)

# Dynamic ordering: biggest cumulative total at bottom of stack
enduse_order <- stock_total_hist |>
  group_by(end_use_label) |>
  summarise(total = sum(stock_Gt), .groups = "drop") |>
  arrange((total)) |>
  pull(end_use_label)

stock_total_hist <- stock_total_hist |> mutate(end_use_label = factor(end_use_label, levels = enduse_order))

stock_total_hist_sum <- stock_total_hist |> group_by(year) |> summarise(stock_Gt = sum(stock_Gt), .groups = "drop")

# Compute cumsum for all end-uses, then keep only the two visible segments for labels
stock_stack_labels <- stock_total_hist |>
  filter(year == STACK_LABEL_YEAR) |>
  arrange(desc(end_use_label)) |>
  mutate(cum_top = cumsum(stock_Gt), cum_bot = dplyr::lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  filter(end_use_label %in% c("Buildings", "Civil infrastructure"))

stock_total_proj_all <- bind_rows(
  stock_metal_fc |> mutate(material = "Metal ores"),
  stock_nonmet_fc |> mutate(material = "Non-metallic minerals")
) |>
  group_by(scenario, year) |>
  summarise(stock_Gt = sum(total_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  filter(year >= HIST_END)

stock_ssp2_total <- stock_total_proj_all |> filter(scenario == CENTRAL_SSP)
stock_proj_range <- stock_total_proj_all |>
  group_by(year) |>
  summarise(ymin = min(stock_Gt), ymax = max(stock_Gt), .groups = "drop")
stock_ssp2_label <- stock_ssp2_total |> filter(year == SSP2_LABEL_YEAR)


## Panel (g) — Total material consumption --------------------------------

p7 <- ggplot() +
  geom_area(
    data = dmc_total_hist,
    aes(x = year, y = DMC_Gt, fill = mat_group),
    position = "stack", alpha = 0.5, colour = NA
  ) +
  geom_line(data = dmc_total_hist_sum, aes(x = year, y = DMC_Gt), colour = "black", linewidth = HIST_LW) +
  geom_text(
    data = dmc_stack_labels,angle=c(0,15,30,0),nudge_x=c(0,0,0,-11),
    aes(x = STACK_LABEL_YEAR, y = label_y, label = label, colour = mat_group),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = dmc_proj_range, aes(x = year, ymin = ymin, ymax = ymax), fill = "grey60", alpha = RIBBON_ALPHA) +
  geom_line(data = dmc_ssp2, aes(x = year, y = DMC_Gt), colour = "black", linewidth = PROJ_LW, linetype = "dashed") +
  geom_text(data = dmc_ssp2_label,
    aes(x = year, y = DMC_Gt), label = "SSP2",
    colour = "black", vjust = -0.6, size = LABEL_SZ, angle = 0) +
  present_line_right +
  x_sc +
  y_sc +
  co +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  panel_tag("g") +
  labs(x = NULL, y = "Material consumption (Gt)", title = "Total material consumption") +
  theme_pb_large() +
  FONT_BUMP


## Panel (h) — Total in-use stock --------------------------------

p8 <- ggplot() +
  geom_area(
    data = stock_total_hist,
    aes(x = year, y = stock_Gt, fill = end_use_label),
    position = "stack", alpha = 0.5, colour = NA
  ) +
  geom_line(data = stock_total_hist_sum, aes(x = year, y = stock_Gt), colour = "black", linewidth = HIST_LW) +
  geom_text(
    data = stock_stack_labels,angle=c(0,15),
    aes(x = STACK_LABEL_YEAR, y = label_y, label = end_use_label, colour = end_use_label),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = stock_proj_range, aes(x = year, ymin = ymin, ymax = ymax), fill = "grey60", alpha = RIBBON_ALPHA) +
  geom_line(
    data = stock_ssp2_total,
    aes(x = year, y = stock_Gt),
    colour = "black",
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(data = stock_ssp2_label,
    aes(x = year, y = stock_Gt), label = "SSP2",
    colour = "black", vjust = -0.6, size = LABEL_SZ, angle = 30) +
  present_line_right +
  x_sc +
  y_sc +
  co +
  scale_fill_manual(values = PALETTE_ENDUSE, guide = "none") +
  scale_colour_manual(values = PALETTE_ENDUSE, guide = "none") +
  panel_tag("h") +
  labs(x = "Year", y = "In-use stock (Gt)", title = "Total stock (metal + non-metallic)") +
  theme_pb_large() +
  FONT_BUMP


# ── SECTION G: Assemble and save --------------------------------

cat("G: Assembling figure\n")

fig <- ((p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | plot_spacer())) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("Figures/Fig2.png", fig, units = "cm", dpi = 600, width = 8.7 * 3, height = 8.7 * 3)

ggsave("Figures/SVG/Fig2.svg", fig, units = "cm", width = 8.7 * 3, height = 8.7 * 3)
clean_svg("Figures/SVG/Fig2.svg")

cat("  Saved: Figures/Fig2.png\n")

## Save figure data ----------------------------------------------------------
dir.create("Results/Data-Figures/", showWarnings = FALSE, recursive = TRUE)

bind_rows(
  pop_hist |> mutate(type = "historical") |> dplyr::select(year, type, value = v),
  pop_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, value = v)
) |>
  mutate(variable = "population_billion") |>
  write_csv("Results/Data-Figures/fig2a.csv")

bind_rows(
  gdp_cap_hist |> mutate(type = "historical") |> dplyr::select(year, type, value = v),
  gdp_cap_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, value = v)
) |>
  mutate(variable = "gdp_per_capita_2015USD") |>
  write_csv("Results/Data-Figures/fig2b.csv")

bind_rows(
  biomass_hist |> mutate(type = "historical") |> dplyr::select(year, type, material = material_category, mg),
  biomass_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, material = category, mg)
) |>
  write_csv("Results/Data-Figures/fig2c.csv")

bind_rows(
  fossil_hist |> mutate(type = "historical") |> dplyr::select(year, type, material = material_category, mg),
  fossil_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, material = material_category, mg)
) |>
  write_csv("Results/Data-Figures/fig2d.csv")

bind_rows(
  metal_hist |> mutate(type = "historical") |> dplyr::select(year, type, mg),
  metal_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, mg)
) |>
  write_csv("Results/Data-Figures/fig2e.csv")

bind_rows(
  nonmet_hist |> mutate(type = "historical") |> dplyr::select(year, type, mg),
  nonmet_ssp2 |> mutate(type = "SSP2") |> dplyr::select(year, type, mg)
) |>
  write_csv("Results/Data-Figures/fig2f.csv")

bind_rows(
  dmc_total_hist |> mutate(type = "historical") |> dplyr::select(year, type, mat_group, DMC_Gt),
  dmc_ssp2 |> mutate(type = "SSP2", mat_group = "Total") |> dplyr::select(year, type, mat_group, DMC_Gt)
) |>
  write_csv("Results/Data-Figures/fig2g.csv")

bind_rows(
  stock_total_hist |> mutate(type = "historical") |> dplyr::select(year, type, end_use = end_use_label, stock_Gt),
  stock_ssp2_total |> mutate(type = "SSP2", end_use = "Total") |> dplyr::select(year, type, end_use, stock_Gt)
) |>
  write_csv("Results/Data-Figures/fig2h.csv")

cat("  Saved: Results/Data-Figures/fig2a-h.csv\n")

# EoF
