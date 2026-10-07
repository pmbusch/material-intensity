## =============================================================================
## S01-S02 - CumulativeIntensityCurve.R  -> Supporting Figure S01 (two panels)
## Step curves, each region rectangle stacked by material group (2024):
##   a) regions sorted high→low by DMC per capita, X = cumulative population
##   b) regions sorted high→low by DMC per GDP,    X = cumulative GDP
## (Former S02 is now panel b.)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

pb_set_geom_defaults("wide")

YEAR <- 2024

# ── Data ──────────────────────────────────────────────────────────────────────
dmc <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE) |> filter(year == YEAR)

pop <- read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE) |>
  filter(year == YEAR) |>
  select(Region, population)

dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") |>
  select(Material_22, Material_group)

# ── Aggregate ─────────────────────────────────────────────────────────────────
BIG4 <- c("Fossil fuels", "Biomass", "Metal ores", "Non-metallic minerals")

dmc_grp <- dmc |>
  left_join(dict_mat, by = c("material_category" = "Material_22")) |>
  filter(Material_group %in% BIG4) |>
  group_by(Region, Material_group) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Region totals → sort order + x positions (billions of people)
region_meta <- dmc_grp |>
  group_by(Region) |>
  summarise(DMC_Mt_total = sum(DMC_Mt), .groups = "drop") |>
  left_join(pop, by = "Region") |>
  mutate(dmc_pc = DMC_Mt_total * 1e6 / population) |> # tonnes per person
  arrange(desc(dmc_pc)) |>
  mutate(pop_B = population / 1e9, x_max = cumsum(pop_B), x_min = lag(x_max, default = 0))

# Rectangle coords: stack material groups bottom→top
STACK_ORDER <- c("Non-metallic minerals", "Biomass", "Metal ores", "Fossil fuels")

df_rect <- dmc_grp |>
  left_join(region_meta |> select(Region, population, x_min, x_max), by = "Region") |>
  mutate(dmc_pc = DMC_Mt * 1e6 / population, Material_group = factor(Material_group, levels = STACK_ORDER)) |>
  arrange(Region, Material_group) |>
  group_by(Region) |>
  mutate(y_max = cumsum(dmc_pc), y_min = lag(y_max, default = 0)) |>
  ungroup()

# Region labels at top of each stacked rectangle (with total in Gt)
region_labels <- region_meta |>
  left_join(df_rect |> group_by(Region) |> summarise(y_top = max(y_max), .groups = "drop"), by = "Region") |>
  mutate(
    x_label = (x_min + x_max) / 2,
    angle = 50,
    label = paste0(
      dplyr::recode(Region, "Sub-Saharan Africa" = "SS Africa", "Middle East & North Africa" = "MENA"),
      " (", sprintf("%.1f Gt", DMC_Mt_total / 1e3), ")"
    )
  )

# Direct material group labels inside East Asia bars
east_asia_labels <- df_rect |>
  filter(Region == "East Asia") |>
  mutate(x_label = (x_min + x_max) / 2, y_label = (y_min + y_max) / 2)

# ── Panel a: per capita ───────────────────────────────────────────────────────
p_pop <- ggplot(df_rect) +
  geom_rect(
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = Material_group),
    alpha = 0.5,
    colour = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = region_labels,
    aes(x = x_label, y = y_top, label = label,angle=angle),
    size = pb_annot_size("wide", 7), vjust = 0, hjust = 0, nudge_y = 0.3
  ) +
  geom_text(
    data = east_asia_labels,
    aes(x = x_label, y = y_label,
      #  colour = Material_group,
       label = Material_group),
    size = pb_annot_size("wide", 7), fontface = "bold"
  ) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, breaks = STACK_ORDER, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(
    name = "Population (billions)",
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    name = NULL,
    expand = expansion(mult = c(0, 0.45))
  ) +
  labs(title = "2024 material consumption per capita (t/person)") +
  coord_cartesian(clip = "off") +
  theme_pb_wide() +
  theme(plot.margin = margin(t = 4, r = 22, b = 4, l = 4))


# ── Panel b: per GDP ──────────────────────────────────────────────────────────

gdp <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |>
  filter(year == YEAR) |>
  select(Region, GDP_2015USD)

# Region totals → sort by material intensity (DMC / GDP), x = cumulative GDP
region_meta_gdp <- dmc_grp |>
  group_by(Region) |>
  summarise(DMC_Mt_total = sum(DMC_Mt), .groups = "drop") |>
  left_join(gdp, by = "Region") |>
  mutate(dmc_gdp = DMC_Mt_total * 1e9 / GDP_2015USD) |> # tonnes per $1,000 GDP
  arrange(desc(dmc_gdp)) |>
  mutate(gdp_T = GDP_2015USD / 1e12, x_max = cumsum(gdp_T), x_min = lag(x_max, default = 0))

df_rect_gdp <- dmc_grp |>
  left_join(region_meta_gdp |> select(Region, GDP_2015USD, x_min, x_max), by = "Region") |>
  mutate(dmc_gdp = DMC_Mt * 1e9 / GDP_2015USD, Material_group = factor(Material_group, levels = STACK_ORDER)) |>
  arrange(Region, Material_group) |>
  group_by(Region) |>
  mutate(y_max = cumsum(dmc_gdp), y_min = lag(y_max, default = 0)) |>
  ungroup()

region_labels_gdp <- region_meta_gdp |>
  left_join(df_rect_gdp |> group_by(Region) |> summarise(y_top = max(y_max), .groups = "drop"), by = "Region") |>
  mutate(
    x_label = (x_min + x_max) / 2,
    angle = 50,
    label = paste0(
      dplyr::recode(Region, "Sub-Saharan Africa" = "SS Africa", "Middle East & North Africa" = "MENA"),
      " (", sprintf("%.1f Gt", DMC_Mt_total / 1e3), ")"
    )
  )

east_asia_labels_gdp <- df_rect_gdp |>
  filter(Region == "East Asia") |>
  mutate(x_label = (x_min + x_max) / 2, y_label = (y_min + y_max) / 2)

p_gdp <- ggplot(df_rect_gdp) +
  geom_rect(
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = Material_group),
    alpha = 0.5,
    colour = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = region_labels_gdp,
    aes(x = x_label, y = y_top, label = label, angle = angle),
    size = pb_annot_size("wide", 7), vjust = 0, hjust = 0, nudge_y = 0.03
  ) +
  geom_text(
    data = east_asia_labels_gdp,
    aes(x = x_label, y = y_label, label = Material_group),
    size = pb_annot_size("wide", 7), fontface = "bold"
  ) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, breaks = STACK_ORDER, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(
    name = "GDP (trillion $ 2015)",
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    name = NULL,
    expand = expansion(mult = c(0, 0.45))
  ) +
  labs(title = "2024 material consumption per $1,000 GDP (t)") +
  coord_cartesian(clip = "off") +
  theme_pb_wide() +
  theme(plot.margin = margin(t = 4, r = 22, b = 4, l = 4))


# ── Combine: S01 with panels a/b ──────────────────────────────────────────────
p_pop + p_gdp + patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("Figures/Supporting-Figures/S01_CumulativeIntensityCurve.png", ggplot2::last_plot(), width = 17, height = 8.7, units = "cm", dpi = 600)
ggsave("Figures/SVG/Supporting-Figures/S01_CumulativeIntensityCurve.svg", ggplot2::last_plot(), width = 17, height = 8.7, units = "cm")
clean_svg("Figures/SVG/Supporting-Figures/S01_CumulativeIntensityCurve.svg")
