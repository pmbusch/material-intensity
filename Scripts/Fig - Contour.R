## =============================================================================
## Fig - Contour.R
## Phase-space trajectories: Mat/GDP (x) vs GDP/Person (y) by region, 1970–2023
## Each path traces one region every 5 years; iso-lines show constant Mat/capita.
## X axis: linear (Mat/GDP).  Y axis: log (GDP/capita).
## Iso-lines y = k/x appear as hyperbolas on this semi-log layout.
## Three figures:
##   A — all materials summed
##   B — faceted by material group  (6 groups, same classification as Fig 1C)
##   C — faceted by all 22 material categories
## Reads from Parameters/ and Inputs/; saves figures to Figures/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

## Load data -------------------------------------------------------------------
# Pre-aggregated regional files produced by 01a/01b/01c scripts.
df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) %>% rename(Analysis_group = Region)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS          : 9 analysis groups (regions)
# PALETTE_MATERIALS        : 21 material categories
# PALETTE_MATERIAL_GROUPS  : 6 material groups

## Material group dictionary ---------------------------------------------------

dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  select(Material_22, Material_group)

grp_mat_order <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Material_group)) %>%
  group_by(Material_group) %>%
  summarise(total = sum(DMC_Mt), .groups = "drop") %>%
  arrange(total)

years_5yr <- seq(1970, 2024, by = 1)

## Helper: build iso-line dataframe -------------------------------------------
# Mat/cap = Mat/GDP × GDP/cap  →  y = iso_level_kg / x  (hyperbola)
# Log-spaced x gives smooth curves (denser at small x where the hyperbola bends sharply).
# y_min / y_max clip the output so iso-lines don't expand the facet axis limits.

make_isolines <- function(iso_levels_t, x_min, x_max, y_min = -Inf, y_max = Inf, n = 200) {
  x_min <- max(x_min, 1e-10, na.rm = TRUE)
  x_max <- max(x_max, x_min * 2, na.rm = TRUE)
  x_seq <- 10^seq(log10(x_min), log10(x_max), length.out = n)
  expand.grid(iso_t = iso_levels_t, x = x_seq) %>%
    mutate(
      y = (iso_t * 1e3) / x, # t/cap → kg/cap;  y = level_kg / x
      iso_label = paste0(iso_t, " t/cap")
    ) %>%
    filter(y >= y_min, y <= y_max)
}

## Helper: auto-pick iso-line levels that bracket a mat/cap range --------------
# Selects up to n_max "nice" log-spaced values (1/2/5 series) spanning from
# just below the data minimum to just above the data maximum.

NICE_LEVELS_T <- sort(c(outer(c(1, 2, 3, 4, 5), 10^(-3:3)))) # 0.001 … 5000 t/cap

pick_iso_levels <- function(mat_pc_vals_kg, n_max = 5) {
  min_t <- min(mat_pc_vals_kg, na.rm = TRUE) / 1000 # kg → t
  max_t <- max(mat_pc_vals_kg, na.rm = TRUE) / 1000
  # Keep levels that span from below the min to above the max
  in_range <- NICE_LEVELS_T[NICE_LEVELS_T >= min_t * 0.4 & NICE_LEVELS_T <= max_t * 2.5]
  if (length(in_range) == 0) {
    return(c(min_t, max_t))
  }
  if (length(in_range) > n_max) {
    idx <- round(seq(1, length(in_range), length.out = n_max))
    in_range <- in_range[idx]
  }
  in_range
}

## FIGURE A: All materials summed ----------------------------------------------

cat("\n── Figure Contour A: all materials ──\n")

# Data already at regional level — join GDP and population directly by region-year
region_all <- df %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_gdp, by = c("Analysis_group" = "Region", "year")) %>%
  left_join(df_pop, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(GDP_PPP_2017USD), !is.na(population)) %>%
  mutate(
    GDP = GDP_PPP_2017USD,
    pop = population,
    mat_gdp = DMC_kg / GDP_PPP_2017USD,
    gdp_pc = GDP_PPP_2017USD / population
  ) %>%
  filter(year %in% years_5yr, !is.na(mat_gdp), !is.na(gdp_pc)) %>%
  arrange(Analysis_group, year)

# Region-year econ totals reused as denominator in Figures B and C
region_econ <- region_all %>% select(year, Analysis_group, GDP, pop)

# World average: sum across all regions
world_all <- region_all %>%
  group_by(year) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

iso_a <- make_isolines(
  iso_levels_t = c(0.5, 1, 2, 5, 10, 20, 30, 40),
  x_min = min(region_all$mat_gdp),
  x_max = max(region_all$mat_gdp),
  y_min = min(region_all$gdp_pc),
  y_max = max(region_all$gdp_pc)
)

ggplot(region_all, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_a,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.25,
    size = 2.8,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last"),
    linewidth = 0.6
  ) +
  # World average — black line on top
  geom_path(
    data = world_all,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_all, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 2.4, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
  data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
  shape = 16, size = 2, colour = "white", alpha = 1
) +
  geom_point(
      data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
      aes(alpha = year),
      shape = 16, size = 2
    ) +
  # Direct region labels at 2024 endpoint
  geom_text_repel(
    data = filter(region_all, year == 2024),
    aes(label = Analysis_group),
    size = 2.2,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.3,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_all, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 2.4,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.3,
    segment.colour = "grey40"
  ) +
  # Year labels for East Asia at start (1970) and end (2024)
  geom_text(
    data = filter(world_all, year %in% c(1970, 2024)),
    aes(label = year,vjust=c(1.5, 0),hjust=c(-0.2, 1.5)),
    fontface="bold",
    colour = "black", size = 2.5,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(breaks = c(0.5, 2, 4, 6)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.2, colour = "grey50") +
  coord_cartesian(clip = "off", expand = F, xlim = c(0.3, 7.4), ylim = c(NA, 100e3)) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2017 USD PPP)", y = "GDP per Capita (2017 USD PPP)") +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/FigContour_A_all.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave("Figures/SVG/FigContour_A_all.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7 * 2)


## FIGURE B: Faceted by material group -----------------------------------------

cat("\n── Figure Contour B: by material group ──\n")

# DMC numerator only — restricted to the consistent country base so the
# denominator (region_econ) is shared with region_all.
region_grp <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Analysis_group), !is.na(Material_group)) %>%
  group_by(year, Analysis_group, Material_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(region_econ, by = c("year", "Analysis_group")) %>%
  filter(!is.na(GDP), year %in% years_5yr) %>%
  filter(!(Material_group %in% c("Mixed", "Waste"))) |>
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop) %>%
  mutate(Material_group = factor(Material_group, levels = grp_mat_order$Material_group)) %>%
  arrange(Analysis_group, Material_group, year)

# World average per material group
world_grp <- region_grp %>%
  group_by(year, Material_group) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

# Per-group iso-lines: auto-pick levels that bracket actual mat/cap range,
# and clip x/y to the data range so the facet axes aren't expanded by the lines.
iso_b <- region_grp %>%
  group_by(Material_group) %>%
  summarise(
    x_min = min(mat_gdp),
    x_max = max(mat_gdp),
    y_min = min(gdp_pc),
    y_max = max(gdp_pc),
    iso_levels = list(pick_iso_levels(mat_gdp * gdp_pc)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(iso_df = list(make_isolines(iso_levels, x_min, x_max, y_min = y_min, y_max = y_max))) %>%
  select(Material_group, iso_df) %>%
  unnest(iso_df)

ggplot(region_grp, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_b,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = 2.2,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed", ends = "last"),
    linewidth = 0.5
  ) +
  # World average per group — black line
  geom_path(
    data = world_grp,
    aes(x = mat_gdp, y = gdp_pc, group = Material_group),
    colour = "black",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_grp, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 2, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
    data = filter(region_grp, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.8, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_grp, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year),
    shape = 16, size = 1.8
  ) +
  # Direct labels at 2024 endpoint
  geom_text_repel(
    data = filter(region_grp, year == 2024),
    aes(label = Analysis_group),
    size = 1.8,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.25,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_grp, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 1.9,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.25,
    segment.colour = "grey40"
  ) +
  # Year labels for world avg at start (1970) and end (2024)
  geom_text(
    data = filter(world_grp, year %in% c(1970, 2024)),
    aes(x = mat_gdp, y = gdp_pc, label = year,
        vjust = ifelse(year == 1970, 1.5, 0),
        hjust = ifelse(year == 1970, -0.2, 1.5)),
    fontface = "bold",
    colour = "black", size = 2,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  facet_wrap(~Material_group, nrow = 2, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.18, colour = "grey50") +
  coord_cartesian(clip = "off", expand = F, ylim = c(NA, 100e3)) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2017 USD PPP)", y = "GDP per Capita (2017 USD PPP)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

# fmt: skip
ggsave("Figures/FigContour_B_material_group.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 3, height = 8.7 * 2)
ggsave(
  "Figures/SVG/FigContour_B_material_group.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 3,
  height = 8.7 * 2
)


## FIGURE C: Faceted by all 22 material categories -----------------------------

cat("\n── Figure Contour C: 22 material categories ──\n")


# DMC numerator only — same consistent country base and region_econ denominator
# as region_all, so sum of all 22 materials' mat/cap equals the total.
region_mat <- df %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group, material_category) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(region_econ, by = c("year", "Analysis_group")) %>%
  filter(!is.na(GDP), year %in% years_5yr) %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

# Facet order: same as Fig 1F (descending total DMC)
mat_order_c <- region_mat %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_kg), .groups = "drop") %>%
  arrange(desc(total))

region_mat <- region_mat %>%
  mutate(
    mat_label = str_wrap(material_category, width = 30),
    mat_label = factor(mat_label, levels = str_wrap(mat_order_c$material_category, width = 30))
  ) %>%
  arrange(Analysis_group, material_category, year)

# World average per material category
world_mat <- region_mat %>%
  group_by(year, material_category, mat_label) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

# Per-material iso-lines: auto-pick levels that bracket actual mat/cap range,
# and clip x/y to the data range so facet axes aren't expanded by the lines.
iso_c <- region_mat %>%
  group_by(mat_label) %>%
  summarise(
    x_min = min(mat_gdp),
    x_max = max(mat_gdp),
    y_min = min(gdp_pc),
    y_max = max(gdp_pc),
    iso_levels = list(pick_iso_levels(mat_gdp * gdp_pc)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(iso_df = list(make_isolines(iso_levels, x_min, x_max, y_min = y_min, y_max = y_max))) %>%
  select(mat_label, iso_df) %>%
  unnest(iso_df)

ggplot(region_mat, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_c,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.18,
    size = 1.8,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.08, "cm"), type = "closed", ends = "last"),
    linewidth = 0.38
  ) +
  # World average per material — black line
  geom_path(
    data = world_mat,
    aes(x = mat_gdp, y = gdp_pc, group = material_category),
    colour = "black",
    linewidth = 0.7,
    arrow = arrow(length = unit(0.08, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_mat, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 1.4, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
    data = filter(region_mat, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.3, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_mat, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year),
    shape = 16, size = 1.3
  ) +
  # Direct labels at 2024 endpoint (skip to avoid clutter in 22-panel plot)
  geom_text_repel(
    data = filter(region_mat, year == 2024),
    aes(label = Analysis_group),
    size = 1.5,
    show.legend = FALSE,
    max.overlaps = 5,
    seed = 42,
    segment.size = 0.2,
    segment.colour = "grey60",
    min.segment.length = 0.3
  ) +
  facet_wrap(~mat_label, nrow = 4, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.15, colour = "grey50") +
  theme_pb_large() +
  coord_cartesian(clip = "off", expand = F, ylim = c(NA, 100e3)) +
  labs(x = "Material Intensity (kg per 2017 USD PPP)", y = "GDP per Capita (2017 USD PPP)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5))

# fmt: skip
ggsave("Figures/FigContour_C_22_materials.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 4, height = 8.7 * 3)
ggsave(
  "Figures/SVG/FigContour_C_22_materials.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 4,
  height = 8.7 * 3
)

# EoF
