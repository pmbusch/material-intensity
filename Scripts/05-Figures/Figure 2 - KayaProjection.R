## =============================================================================
## Figure 2 - KayaProjection.R  (MC edition)
## Eight-panel Kaya figure: historical (1970–2024) + MC projected (2025–2060).
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
## Pop/GDP: determined by SSP drawn for each run (ssp_label in results).
## M/G, S/G: derived post-hoc as DMC or stock / world GDP per run.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")
library(patchwork)

# ── Constants ----------------------------------------------------------------

HIST_END <- 2024L
PROJ_END <- 2060L
CI_LO <- 0.05
CI_HI <- 0.95

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
RIBBON_ALPHA <- 0.22
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
co <- coord_cartesian(xlim = c(1970, PROJ_END), ylim = c(0, NA), clip = "off", expand = FALSE)

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

# Compute P5/P50/P95 envelope; group_cols is a character vector of column names
env_quantiles <- function(df, val_col, group_cols) {
  df |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      p05 = quantile(.data[[val_col]], CI_LO, na.rm = TRUE),
      p50 = median(.data[[val_col]], na.rm = TRUE),
      p95 = quantile(.data[[val_col]], CI_HI, na.rm = TRUE),
      .groups = "drop"
    )
}


# ── SECTION A: Load historical data -----------------------------------------

cat("A: Loading historical data\n")

pop_world_hist <- read_csv("Parameters/population_world_historical.csv", show_col_types = FALSE)
gdp_world_hist <- read_csv("Parameters/gdp_world.csv", show_col_types = FALSE)
pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)
gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
stock_hist_raw <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE)
stock_subenduse_hist <- read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE)


# ── SECTION B: Load MC results -----------------------------------------------

cat("B: Loading MC results\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))
cat("  Runs:", n_distinct(results$run_id), "| Years:", paste(range(results$year), collapse = "-"), "\n")

# One row per run with its drawn SSP
run_ssp <- results |> distinct(run_id, ssp_label)


# ── SECTION C: SSP-based world GDP and population trajectories ---------------

cat("C: SSP trajectories\n")

gdp_2024_region <- gdp_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  filter(year == HIST_END) |>
  rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

# World GDP per SSP × year (include 2024 so band anchors to historical endpoint)
gdp_world_proj <- ssp_drivers |>
  filter(variable == "GDP|PPP", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(ssp_label = scenario, region, year, idx = index) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(world_gdp = gdp_2024 * idx) |>
  group_by(ssp_label, year) |>
  summarise(world_gdp = sum(world_gdp, na.rm = TRUE), .groups = "drop")

pop_world_proj <- ssp_drivers |>
  filter(variable == "Population", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(ssp_label = scenario, region, year, idx = index) |>
  left_join(pop_2024_region, by = "region") |>
  mutate(world_pop = pop_2024 * idx) |>
  group_by(ssp_label, year) |>
  summarise(world_pop = sum(world_pop, na.rm = TRUE), .groups = "drop")

gdpcap_world_proj <- pop_world_proj |>
  left_join(gdp_world_proj, by = c("ssp_label", "year")) |>
  mutate(gdpcap = world_gdp / world_pop / 1e3)


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

# Panels e/f: 8 sub-end-use historical lines from sub-enduse stock file
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

## Pop: one value per (run, year) fully determined by SSP
pop_env <- run_ssp |>
  left_join(pop_world_proj, by = "ssp_label") |>
  mutate(v = world_pop / 1e9) |>
  env_quantiles("v", "year")

## GDP/cap: same
gdpcap_env <- run_ssp |> left_join(gdpcap_world_proj, by = "ssp_label") |> env_quantiles("gdpcap", "year")

## Biomass M/G per run × sub-material × year
biomass_env <- results |>
  filter(material_group == "biomass", material_key %in% BIOMASS_CATS) |>
  group_by(run_id, ssp_label, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_proj, by = c("ssp_label", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp) |>
  env_quantiles("mg", c("material_key", "year"))

## Fossil M/G per run × sub-material × year
fossil_env <- results |>
  filter(material_group == "fossil_fuels", material_key %in% FOSSIL_CATS) |>
  group_by(run_id, ssp_label, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_proj, by = c("ssp_label", "year")) |>
  mutate(mg = M_Mt * 1e9 / world_gdp) |>
  env_quantiles("mg", c("material_key", "year"))

## Metal S/G per run × end-use × year
metal_env <- results |>
  filter(material_group == "metal_ores") |>
  group_by(run_id, ssp_label, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_proj, by = c("ssp_label", "year")) |>
  mutate(mg = stock_Mt * 1e9 / world_gdp) |>
  rename(end_use_label = material_key) |>
  env_quantiles("mg", c("end_use_label", "year"))

## Non-metallic S/G per run × end-use × year
nonmet_env <- results |>
  filter(material_group == "nonmetallic_minerals") |>
  group_by(run_id, ssp_label, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_world_proj, by = c("ssp_label", "year")) |>
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
  summarise(stock_Gt = sum(in_use_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  env_quantiles("stock_Gt", "year")


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
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

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
  summarise(stock_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
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


# ── SECTION G: Build panels --------------------------------------------------

cat("G: Building panels\n")

# Historical line-label data for panels c/d
bio_labels <- biomass_hist |>
  filter(year == LABEL_YEAR) |>
  mutate(label_x = str_remove(material_category, " and fodder crops"))
fossil_labels <- fossil_hist |> filter(year == LABEL_YEAR)

# Direct historical labels for panels e/f (at HIST_LABEL_YEAR)
metal_hist_labels <- metal_hist_eu |> filter(year == HIST_LABEL_YEAR)
nonmet_hist_labels <- nonmet_hist_eu |> filter(year == HIST_LABEL_YEAR, end_use_label %in% F_LABEL_ENDUSES)


## Panel (a) — Population -------------------------------------------------------

pop_hist <- pop_world_hist |> filter(year <= HIST_END) |> mutate(v = population / 1e9)

p1 <- ggplot() +
  geom_ribbon(data = pop_env, aes(x = year, ymin = p05, ymax = p95), fill = POP_COLOR, alpha = RIBBON_ALPHA) +
  geom_line(data = pop_hist, aes(x = year, y = v), colour = POP_COLOR, linewidth = HIST_LW) +
  geom_line(
    data = pop_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = POP_COLOR,
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  # custom legend (upper-left, above all historical data)
  # fmt: skip
  annotate("rect",xmin = 1978,xmax = 1989,ymin = 9,ymax = 10.0,fill = "grey50",alpha = RIBBON_ALPHA,colour = NA) +
  # fmt: skip
  annotate("segment", x = 1977, xend = 1990, y = 9.5, yend = 9.5, colour = "grey35", linewidth = PROJ_LW,linetype="dashed") +
  # fmt: skip
  annotate("text", x = 1991, y = 10.0,  label = paste0(round(CI_HI * 100), "%ile") , hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  annotate("text", x = 1991, y = 9.5, label = "median", hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
  # fmt: skip
  annotate("text", x = 1991, y = 9,  label = paste0(round(CI_LO * 100), "%ile") , hjust = 0, vjust = 0.5, size = LABEL_SZ, colour = "grey30") +
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
  geom_ribbon(data = gdpcap_env, aes(x = year, ymin = p05, ymax = p95), fill = GDPCAP_COLOR, alpha = RIBBON_ALPHA) +
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
  y_sc +
  co +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  panel_tag("b") +
  labs(x = NULL, y = "GDP per capita (thousands USD 2015)", title = "GDP per capita") +
  theme_pb_large() +
  FONT_BUMP


## Panel (c) — M/G Biomass -----------------------------------------------------

p3 <- ggplot() +
  geom_ribbon(
    data = biomass_env,
    aes(x = year, ymin = p05, ymax = p95, fill = material_key),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = biomass_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(
    data = biomass_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = material_key),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(
    data = bio_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = label_x),
    size = LABEL_SZ, angle = c(-25, 0, -60, 0, -15), show.legend = FALSE, 
    nudge_y = c(0.01,0.01,0.018,0.01,0.01)
  ) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
  panel_tag("c") +
  labs(x = NULL, y = "M / GDP (kg per 2015 USD)", title = "M/G Biomass") +
  theme_pb_large() +
  FONT_BUMP


## Panel (d) — M/G Fossil fuels ------------------------------------------------

p4 <- ggplot() +
  geom_ribbon(
    data = fossil_env,
    aes(x = year, ymin = p05, ymax = p95, fill = material_key),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = fossil_hist, aes(x = year, y = mg, colour = material_category), linewidth = HIST_LW) +
  geom_line(
    data = fossil_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = material_key),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(
    data = fossil_labels,
    aes(x = LABEL_YEAR, y = mg, colour = material_category, label = material_category),
    size = LABEL_SZ, angle = 0, show.legend = FALSE, nudge_y = c(0.008, 0.01, 0.008,-0.015),nudge_x=2,
  ) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_fill_manual(values = PALETTE_MATERIALS, guide = "none") +
  panel_tag("d") +
  labs(x = NULL, y = "M / GDP (kg per 2015 USD)", title = "M/G Fossil fuels") +
  theme_pb_large() +
  FONT_BUMP


## Panel (e) — S/G Metal ores by end-use (8 sub-end-uses) ----------------------

p5 <- ggplot() +
  geom_ribbon(
    data = metal_env,
    aes(x = year, ymin = p05, ymax = p95, fill = end_use_label),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = metal_hist_eu, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(
    data = metal_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = end_use_label),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(
    data = metal_hist_labels,
    aes(x = year, y = mg, label = end_use_label, colour = end_use_label),
    nudge_y=c(-3,1,16,-2,1,7,1.1,-1)*0.005,
    nudge_x = c(-20,-0.5,-36,2,-0.5,-21,0,-0.5),
    angle=c(-10,0,35,-25,0,-30,0,0), 
    hjust = 1, 
    size = LABEL_SZ, show.legend = FALSE
  ) +
  present_line_top +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  panel_tag("e") +
  labs(x = "Year", y = "Stock / GDP (kg per 2015 USD)", title = "S/G Metal ores") +
  theme_pb_large() +
  FONT_BUMP


## Panel (f) — S/G Non-metallic minerals by end-use (8 sub, 4 labeled) ---------

p6 <- ggplot() +
  geom_ribbon(
    data = nonmet_env,
    aes(x = year, ymin = p05, ymax = p95, fill = end_use_label),
    alpha = RIBBON_ALPHA,
    show.legend = FALSE
  ) +
  geom_line(data = nonmet_hist_eu, aes(x = year, y = mg, colour = end_use_label), linewidth = HIST_LW) +
  geom_line(
    data = nonmet_env |> filter(year >= HIST_END),
    aes(x = year, y = p50, colour = end_use_label),
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
  geom_text(
    data = nonmet_hist_labels,
    aes(x = year, y = mg, label = end_use_label, colour = end_use_label),
    nudge_y=c(-0.6,-0.3,-0.55,1.1),
    nudge_x = c(-34,-0.5,-8,-25), 
    angle=c(30,35,35,0),
    hjust = 1,size = LABEL_SZ, show.legend = FALSE
  ) +
  present_line_bot +
  x_sc +
  y_sc +
  co +
  scale_colour_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  scale_fill_manual(values = PALETTE_SUBENDUSE, guide = "none") +
  panel_tag("f") +
  labs(x = "Year", y = "Stock / GDP (kg per 2015 USD)", title = "S/G Non-metallic minerals") +
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
    angle = c(0, 15, 25, 0), nudge_x = c(0, 0, 0, -12),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = dmc_env, aes(x = year, ymin = p05, ymax = p95), fill = "grey60", alpha = RIBBON_ALPHA) +
  geom_line(
    data = dmc_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = "black",
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
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
    angle = c(0, 15),
    size = LABEL_SZ, hjust = 0.5, vjust = 0.5, fontface = "bold", show.legend = FALSE
  ) +
  geom_ribbon(data = stock_env, aes(x = year, ymin = p05, ymax = p95), fill = "grey60", alpha = RIBBON_ALPHA) +
  geom_line(
    data = stock_env |> filter(year >= HIST_END),
    aes(x = year, y = p50),
    colour = "black",
    linewidth = PROJ_LW,
    linetype = "dashed"
  ) +
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


# ── SECTION H: Assemble and save --------------------------------------------

cat("H: Assembling figure\n")

fig <- wrap_plots(p1, p2, p3, p4, p5, p6, p7, p8, plot_spacer(), ncol = 3) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

ggsave("Figures/Fig2.png", fig, units = "cm", dpi = 600, width = 8.7 * 3, height = 8.7 * 3)
ggsave("Figures/SVG/Fig2.svg", fig, units = "cm", width = 8.7 * 3, height = 8.7 * 3)
clean_svg("Figures/SVG/Fig2.svg")

cat("  Saved: Figures/Fig2.png\n")

# EoF
