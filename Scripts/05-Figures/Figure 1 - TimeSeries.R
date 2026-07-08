## =============================================================================
## Figure 1 - TimeSeries.R
## Stacked area charts from pre-aggregated regional UNEP/Pop/GDP data.
## Reads from Parameters/; saves figures to Figures/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Materials (DMC) — already aggregated to Region by 01a-Aggregate_UNEP.R
# Columns: Region, year, material_category, DMC_Mt
df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")
cat("Regions:", paste(sort(unique(df$Region)), collapse = " | "), "\n")
cat("Material categories:", paste(sort(unique(df$material_category)), collapse = " | "), "\n")

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS          : 8 regions
# PALETTE_MATERIALS        : 21 material categories
# PALETTE_MATERIAL_GROUPS  : 6 material groups

# Vertical event lines reused across figures
event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40")
)


# FIGURES ---------------

# Figure 1A old: Global DMC by material group ------------------------------------

cat("── Figure 1A ──\n")

dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group)

global_grp_mat <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Material_group)) %>%
  group_by(year, Material_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

grp_mat_totals_1c <- global_grp_mat %>%
  group_by(Material_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

global_grp_mat <- global_grp_mat %>%
  mutate(Material_group = factor(Material_group, levels = grp_mat_totals_1c$Material_group))

labels_1c <- global_grp_mat %>%
  filter(year == 2008) %>%
  arrange(desc(Material_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) |>
  mutate(Material_group_label = if_else(DMC_Gt > 0.5, Material_group, ""))

ggplot(global_grp_mat, aes(x = year, y = DMC_Gt, fill = Material_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_text(
    data = labels_1c,
    aes(x = 2008, y = mid_y, label = Material_group_label),
    hjust = 0, size = 1.8, inherit.aes = FALSE
  ) +
  # event_lines +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material group", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1A_global_DMC_by_material_group.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
ggsave(
  "Figures/SVG/Fig1A_global_DMC_by_material_group.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 2,
  height = 8.7
)


# Figure 1A: Global DMC by material category ----------------------------------

cat("\n── Figure 1B ──\n")


global_mat <- df %>%
  filter(DMC_Mt > 0) |>
  filter(!str_detect(material_category, "Waste for ")) |>
  mutate(
    material_group = case_when(
      material_category %in%
        c(
          "Coal",
          "Natural Gas",
          "Petroleum",
          "Oil shale and tar sands",
          "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
          "Other products mainly from fossil fuels e.g. plastics"
        ) ~ "Fossil fuels",
      material_category %in%
        c(
          "Crops",
          "Crop Residues",
          "Wood",
          "Grazed biomass and fodder crops",
          "Non-wild animal products",
          "Wild catch and harvest",
          "Products mainly from biomass nec.",
          "Mixed / complex products nec."
        ) ~ "Biomass",
      material_category %in% c("Ferrous ores", "Non-ferrous ores", "Products mainly from metals nec.") ~ "Metal ores",
      material_category %in%
        c(
          "Non-metallic minerals - construction dominant",
          "Non-metallic minerals - industrial or agricultural dominant",
          "Products mainly from non-metallic minerals"
        ) ~ "Non-metallic minerals",

      TRUE ~ NA_character_ # flags anything unclassified
    )
  ) |>
  mutate(
    material_category_plot = case_when(
      material_category == "Coal" ~ "Coal",
      material_category == "Natural Gas" ~ "Natural Gas",
      material_category == "Petroleum" ~ "Petroleum",
      material_category == "Crops" ~ "Crops",
      material_category == "Crop Residues" ~ "Crop Residues",
      material_category == "Wood" ~ "Wood",
      material_category == "Grazed biomass and fodder crops" ~ "Grazed biomass",
      material_category == "Ferrous ores" ~ "Ferrous ores",
      material_category == "Non-ferrous ores" ~ "Non-ferrous ores",
      material_category == "Non-metallic minerals - construction dominant" ~ "Construction minerals",
      material_category == "Non-metallic minerals - industrial or agricultural dominant" ~ "Industrial minerals",
      material_group == "Fossil fuels" ~ "Other - Fossil fuels",
      material_group == "Biomass" ~ "Other - Biomass",
      material_group == "Metal ores" ~ "Other - Metal ores",
      material_group == "Non-metallic minerals" ~ "Other - Non-metallic minerals",
      TRUE ~ "Other"
    )
  ) |>
  group_by(year, material_group, material_category_plot) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(
    material_group = factor(
      material_group,
      levels = rev(c("Non-metallic minerals", "Biomass", "Fossil fuels", "Metal ores"))
    )
  )

PALETTE_MATERIALS <- c(
  # Fossil fuels — brown family
  "Coal" = "#3E2723",
  "Natural Gas" = "#6D4C41",
  "Petroleum" = "#A1887F",
  "Other - Fossil fuels" = "#D7CCC8",
  # Biomass — green family
  "Crops" = "#1B5E20",
  "Crop Residues" = "#388E3C",
  "Wood" = "#558B2F",
  "Grazed biomass" = "#8BC34A",
  "Other - Biomass" = "#DCEDC8",
  # Metal ores — red family
  "Ferrous ores" = "#B71C1C",
  "Non-ferrous ores" = "#E57373",
  "Other - Metal ores" = "#FFCDD2",
  # Non-metallic minerals — grey-blue family
  "Construction minerals" = "#455A64",
  "Industrial minerals" = "#90A4AE",
  "Other - Non-metallic minerals" = "#CFD8DC"
)

mat_totals_1a <- global_mat %>%
  group_by(material_group, material_category_plot) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(material_group, total)

global_mat <- global_mat %>%
  mutate(material_category_plot = factor(material_category_plot, levels = mat_totals_1a$material_category_plot))

labels_1a <- global_mat %>%
  filter(year == 1995) %>%
  arrange(desc(material_category_plot)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  mutate(material_category_label = if_else(DMC_Gt > 0.5, material_category_plot, NA_character_))

group_boundaries <- global_mat %>%
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt)) %>%
  group_by(year, material_group) %>%
  summarise(boundary_y = max(top), .groups = "drop") %>%
  filter(material_group != last(levels(global_mat$material_category_plot))) # drop top

group_labels <- group_boundaries %>%
  arrange(year, desc(material_group)) %>%
  filter(year == max(year)) %>%
  mutate(bot = lag(boundary_y, default = 0), mid_y = (boundary_y + bot) / 2) |>
  mutate(
    material_group_label = case_when(
      material_group == "Fossil fuels" ~ "Fossil\nfuels",
      material_group == "Metal ores" ~ "Metal\nores",
      TRUE ~ material_group
    )
  )

p_material <- ggplot(global_mat, aes(x = year, y = DMC_Gt, fill = material_category_plot)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_text(
    data = labels_1a |>
      filter(
        !material_category_plot %in%
          c("Coal", "Crops", "Industrial minerals", "Construction minerals", "Ferrous ores", "Other - Fossil fuels")
      ),
    aes(x = year, y = mid_y, label = material_category_label),
    colour = "black",
    angle = 30,
    hjust = 0.5,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |>
      filter(material_category_plot %in% c("Coal", "Crops", "Industrial minerals")),
    aes(x = year, y = mid_y, label = material_category_label),
    colour = "white",
    angle = 30,
    hjust = 0.5,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Construction minerals"),
    aes(x = year, y = mid_y, label = material_category_label),
    colour = "white",
    angle = 0,
    hjust = 0.5,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Ferrous ores"),
    aes(x = year, y = mid_y, label = material_category_label),
    colour = PALETTE_MATERIALS["Ferrous ores"],
    angle = 30,
    hjust = 0.5,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Other - Fossil fuels"),
    aes(x = year, y = mid_y, label = material_category_label),
    colour = PALETTE_MATERIALS["Other - Fossil fuels"],
    angle = 30,
    hjust = 0.5,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = group_boundaries,
    aes(x = year, y = boundary_y, group = material_group),
    colour = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
  data = group_labels,
  aes(x = 2025, y = mid_y, label = material_group_label,color=material_group),
  inherit.aes = FALSE, lineheight = 0.8,
  fontface = "bold", size = 2.2,
  angle = 90, hjust = 0.5,
  clip = "off",vjust=0.9
) +
  # event_lines +
  # fmt: skip
  annotate("text",x = -Inf, y = Inf,label = "b",hjust = -1, vjust = 1,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_MATERIALS, name = NULL) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off", xlim = c(1970, 2024)) +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    plot.margin = margin(t = 5, r = 15, b = 0, l = 5, unit = "pt")
  )
p_material

# fmt: skip
ggsave("Figures/Fig1A_global_DMC_by_material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# fmt: skip
ggsave("Figures/SVG/Fig1A_global_DMC_by_material.svg",ggplot2::last_plot(),units = 'cm',width = 8.7,height = 8.7)
clean_svg("Figures/SVG/Fig1A_global_DMC_by_material.svg")


#  Figure 1B: Global DMC by region --------------------------------------------

cat("── Figure 1B ──\n")

# Data is already at region level — just rename for plot aesthetics
global_grp <- df %>%
  filter(DMC_Mt > 0) |>
  filter(!str_detect(material_category, "Waste for ")) |>
  rename(analysis_group = Region) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

grp_totals_1b <- global_grp %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

global_grp <- global_grp %>% mutate(analysis_group = factor(analysis_group, levels = grp_totals_1b$analysis_group))

labels_1b <- global_grp %>%
  filter(year == 1995) %>%
  arrange(desc(analysis_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  mutate(label = if_else(DMC_Gt > 0.5, analysis_group, NA_character_))

# Thicker black boundary between each region (regions have no further sub-grouping,
# so every region gets the separator treatment analogous to the material-group
# boundaries in panel b)
region_boundaries <- global_grp %>%
  arrange(year, desc(analysis_group)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt)) %>%
  group_by(year, analysis_group) %>%
  summarise(boundary_y = max(top), .groups = "drop")

p_region <- ggplot(global_grp, aes(x = year, y = DMC_Gt, fill = analysis_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_line(
    data = region_boundaries,
    aes(x = year, y = boundary_y, group = analysis_group),
    colour = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(!analysis_group %in% c("Oceania", "North America", "East Asia")),
    aes(x = year, y = mid_y, label = label),
    colour = "black",
    angle = 30,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "East Asia"),
    aes(x = year, y = mid_y, label = label),
    colour = "black",
    angle = 0,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "North America"),
    aes(x = year, y = mid_y, label = label),
    colour = "white",
    angle = 30,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "Oceania"),
    aes(x = year, y = mid_y, label = label),
    colour = PALETTE_REGIONS["Oceania"],
    angle = 30,
    size = 2.0,
    inherit.aes = FALSE
  ) +
  # event_lines +
  # fmt: skip
  annotate("text",x = -Inf, y = Inf,label = "a",hjust = -1, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Region", x = "Year", y = "Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    plot.margin = margin(t = 5, r = 10, b = 0, l = 5, unit = "pt")
  )
p_region

# fmt: skip
ggsave("Figures/Fig1B_global_DMC_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
ggsave("Figures/SVG/Fig1B_global_DMC_by_region.svg", ggplot2::last_plot(), units = 'cm', width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig1B_global_DMC_by_region.svg")

# Panels c & d: phase-space contours (Mat/GDP vs GDP/capita), 1970-2024 ------
# Historical only (no SSP projection). Layout/logic ported from
# "Figure 3 - Contour.R" (FIGURE A and the Fig3-panel-b 9-material section),
# resized down to an 8.7 x 8.7 cm quadrant. Column alignment: c (regions)
# sits under a (regions), d (9 materials) sits under b (material).

cat("── Figure 1C/D (contour panels) ──\n")

HIST_END <- 2024L

df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)

## Helper: build iso-line dataframe -------------------------------------------
# Mat/cap = Mat/GDP × GDP/cap  →  y = iso_level_kg / x  (hyperbola)
make_isolines <- function(iso_levels_t, x_min, x_max, y_min = -Inf, y_max = Inf, n = 200) {
  x_min <- max(x_min, 1e-10, na.rm = TRUE)
  x_max <- max(x_max, x_min * 2, na.rm = TRUE)
  x_seq <- 10^seq(log10(x_min), log10(x_max), length.out = n)
  expand.grid(iso_t = iso_levels_t, x = x_seq) %>%
    mutate(y = (iso_t * 1e3) / x, iso_label = paste0(iso_t, " t/cap")) %>%
    filter(y >= y_min, y <= y_max)
}

## Helper: auto-pick iso-line levels that bracket a mat/cap range --------------
NICE_LEVELS_T <- sort(c(outer(c(1, 2, 3, 4, 5), 10^(-3:3))))

pick_iso_levels <- function(mat_pc_vals_kg, n_max = 5) {
  min_t <- min(mat_pc_vals_kg, na.rm = TRUE) / 1000
  max_t <- max(mat_pc_vals_kg, na.rm = TRUE) / 1000
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

## LOWESS smoothing: avoid annual noise while preserving endpoints ------------
smooth_lowess <- function(df, group_cols = NULL, frac = 0.18181818, year_col = "year") {
  split_df <- if (is.null(group_cols)) list(df) else split(df, df[group_cols], drop = TRUE)
  out <- lapply(split_df, function(d) {
    x <- d[[year_col]]
    for (col in c("mat_gdp", "gdp_pc")) {
      y <- d[[col]]
      d[[paste0(col, "_raw")]] <- y
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) < 5) {
        d[[col]] <- y
        next
      }
      fit <- tryCatch(loess(y[ok] ~ x[ok], span = frac, na.action = na.exclude), error = function(e) NULL)
      if (is.null(fit)) {
        d[[col]] <- y
      } else {
        pred <- tryCatch(predict(fit, newdata = x), error = function(e) rep(NA_real_, length(x)))
        d[[col]] <- pred
      }
    }
    d
  })
  do.call(rbind, out)
}

## Panel c data: regions, all materials summed --------------------------------

region_all <- df %>%
  rename(Analysis_group = Region) %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_gdp, by = c("Analysis_group" = "Region", "year")) %>%
  left_join(df_pop, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(GDP_2015USD), !is.na(population)) %>%
  mutate(GDP = GDP_2015USD, pop = population, mat_gdp = DMC_kg / GDP_2015USD, gdp_pc = GDP_2015USD / population) %>%
  filter(year >= 1970, year <= HIST_END, !is.na(mat_gdp), !is.na(gdp_pc)) %>%
  arrange(Analysis_group, year)

region_all <- smooth_lowess(region_all, group_cols = c("Analysis_group"))

region_econ <- region_all %>% dplyr::select(year, Analysis_group, GDP, pop)

world_all <- region_all %>%
  group_by(year) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)
world_all <- smooth_lowess(world_all)

## Panel d data: 9 material groups, world average -----------------------------
# Computed here (ahead of both panels' plots) so panels c & d can share one
# common Y range (GDP per capita) below.

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum")
METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

world_gdp_pop <- region_econ %>% group_by(year) %>% summarise(GDP = sum(GDP), pop = sum(pop), .groups = "drop")

world_9mat_hist <- df %>%
  filter(material_category %in% c(BIOMASS_CATS, FOSSIL_CATS, METAL_CATS, NONMET_CATS)) %>%
  mutate(
    mat9 = case_when(
      material_category %in% METAL_CATS ~ "Metal ores",
      material_category %in% NONMET_CATS ~ "Non-metallic minerals",
      material_category == "Grazed biomass and fodder crops" ~ "Grazed biomass",
      TRUE ~ material_category
    )
  ) %>%
  group_by(year, mat9) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(world_gdp_pop, by = "year") %>%
  filter(!is.na(GDP), year >= 1970, year <= HIST_END) %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

world_9mat_hist <- smooth_lowess(world_9mat_hist, group_cols = c("mat9"))

# Palette matching panel b's local PALETTE_MATERIALS (same hex per category)
pal_9mat <- c(
  "Coal" = "#3E2723",
  "Natural Gas" = "#6D4C41",
  "Petroleum" = "#A1887F",
  "Crops" = "#1B5E20",
  "Crop Residues" = "#388E3C",
  "Wood" = "#558B2F",
  "Grazed biomass" = "#8BC34A",
  "Metal ores" = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]),
  "Non-metallic minerals" = unname(PALETTE_MATERIAL_GROUPS["Non-metallic minerals"])
)

## Shared axis bounds for panels c & d -----------------------------------------
# Same scale as "Figure 3 - Contour.R" FIGURE A for x; Y range (min AND max)
# is shared identically between c and d, capped at the project-standard 100,000.
x_lim_c <- c(0.3, 7.4)
x_lim_d <- range(world_9mat_hist$mat_gdp, na.rm = TRUE) * c(0.95, 1.05)
y_lim_cd <- c(min(c(region_all$gdp_pc, world_9mat_hist$gdp_pc), na.rm = TRUE), 100e3)

# Iso-lines computed across the FULL displayed panel (not just the tight data
# range) so the dashed hyperbolas span edge-to-edge instead of stopping short.
iso_c <- make_isolines(
  iso_levels_t = c(0.5, 1, 2, 5, 10, 20, 30, 40),
  x_min = x_lim_c[1],
  x_max = x_lim_c[2],
  y_min = y_lim_cd[1],
  y_max = y_lim_cd[2]
)

iso_d <- make_isolines(
  # Drop levels that clutter panel d without adding readable separation
  iso_levels_t = setdiff(
    pick_iso_levels(world_9mat_hist$mat_gdp * world_9mat_hist$gdp_pc, n_max = 12),
    c(0.3, 0.4, 3, 4)
  ),
  x_min = x_lim_d[1],
  x_max = x_lim_d[2],
  y_min = y_lim_cd[1],
  y_max = y_lim_cd[2]
)

pFig1c <- ggplot(region_all, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_c,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = 2.0,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.09, "cm"), type = "closed", ends = "last"),
    linewidth = 0.45
  ) +
  geom_path(
    data = world_all,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(world_all, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 1.9, inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.6, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year), shape = 16, size = 1.6
  ) +
  geom_text_repel(
    data = filter(region_all, year == HIST_END),
    aes(label = Analysis_group),
    size = 1.9,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.25,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_all, year == HIST_END),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 2.0,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.25,
    segment.colour = "grey40"
  ) +
  geom_text(
    data = filter(world_all, year %in% c(1970, HIST_END)),
    aes(label = year, vjust = c(1.5, 0), hjust = c(-0.2, 1.5)),
    fontface = "bold", colour = "black", size = 2.0, show.legend = FALSE
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "c",hjust = 1.5, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(breaks = c(0.5, 2, 4, 6)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.18, colour = "grey50") +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_c, ylim = y_lim_cd) +
  theme_pb_large() +
  labs(title = " ", x = "Material Intensity (kg/$)", y = "GDP per Capita  ($/p)") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 6),
    plot.margin = margin(t = 0, r = 10, b = 5, l = 5, unit = "pt")
  )
pFig1c

## Panel d plot: 9 material groups, world average ------------------------------
# (data computed above, alongside panel c, so the shared Y range could be built)

# Year labels anchored on the Non-metallic minerals line (least cluttered)
lbl_nonmet_1970 <- filter(world_9mat_hist, year == 1970, mat9 == "Non-metallic minerals")
lbl_nonmet_end <- filter(world_9mat_hist, year == HIST_END, mat9 == "Non-metallic minerals")

pFig1d <- ggplot() +
  geom_textline(
    data = iso_d,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.2,
    size = 2.0,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    data = world_9mat_hist,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, group = mat9),
    linewidth = 0.6,
    arrow = arrow(length = unit(0.09, "cm"), type = "closed", ends = "last")
  ) +
  geom_point(
    data = filter(world_9mat_hist, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, alpha = year),
    shape = 16, size = 1.6
  ) +
  geom_text_repel(
    data = filter(world_9mat_hist, year == HIST_END),
    aes(x = mat_gdp, y = gdp_pc, label = mat9, colour = mat9),
    size = 1.9,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.2,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text(
    data = lbl_nonmet_1970,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, label = year),
    vjust = 1.5, hjust = -0.2, fontface = "bold", size = 2.0, show.legend = FALSE
  ) +
  geom_text(
    data = lbl_nonmet_end,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, label = year),
    vjust = 0, hjust = 1.5, fontface = "bold", size = 2.0, show.legend = FALSE
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "d",hjust = 1.5, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_colour_manual(values = pal_9mat, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.18, colour = "grey50") +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_d, ylim = y_lim_cd) +
  theme_pb_large() +
  labs(title = " ", x = "Material Intensity (kg/$)", y = "GDP per Capita ($/p)") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 6),
    plot.margin = margin(t = 0, r = 10, b = 5, l = 5, unit = "pt")
  )
pFig1d


# Merge: final 2x2 figure (a=regions, b=material, c=regions contour, d=material contour)

library(cowplot)
# align="hv" kept for clean column alignment. Panels c/d get a blank title below
# (matching a/b's plot.title row) so cowplot doesn't pad them to compensate for
# a mismatched gtable structure, which otherwise widens the row gap.
fig1 <- plot_grid(p_region, p_material, pFig1c, pFig1d, ncol = 2, nrow = 2, align = "hv")
ggsave("Figures/Fig1.png", fig1, units = 'cm', dpi = 600, width = 17.4, height = 17.4)
ggsave("Figures/SVG/Fig1.svg", fig1, units = 'cm', width = 17.4, height = 17.4)
# clean_svg("Figures/SVG/Fig1.svg")
group_svg_layers("Figures/SVG/Fig1.svg", flat = TRUE)


#  Figure 1D OLD: Global extraction by region -------------------------------------

cat("── Figure 1D ──\n")

df2 <- read_csv("Parameters/materials_region_DE.csv", show_col_types = FALSE)
df2 <- df2 |> filter(abs(DE_Mt) > 0.1)

global_de <- df2 %>%
  rename(analysis_group = Region) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DE_Gt = sum(DE_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

grp_totals_1d <- global_de %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DE_Gt), .groups = "drop") %>%
  arrange(total)

global_de <- global_de %>% mutate(analysis_group = factor(analysis_group, levels = grp_totals_1d$analysis_group))

labels_1d <- global_de %>%
  filter(year == 2008) %>%
  arrange(desc(analysis_group)) %>%
  mutate(top = cumsum(DE_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_de, aes(x = year, y = DE_Gt, fill = analysis_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_text(data = labels_1d,
            aes(x = 2008, y = mid_y, label = analysis_group),
            hjust = 0, size = 1.8, inherit.aes = FALSE) +
  # event_lines +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Extraction by regions", x = "Year", y = "Domestic Extraction (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1D_global_DE_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
ggsave("Figures/SVG/Fig1D_global_DE_by_region.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7)
clean_svg("Figures/SVG/Fig1D_global_DE_by_region.svg")

## Save figure data ----------------------------------------------------------
dir.create("Results/Data-Figures/", showWarnings = FALSE, recursive = TRUE)
write_csv(
  global_mat |> dplyr::select(year, material_group, material = material_category_plot, DMC_Gt),
  "Results/Data-Figures/fig1a.csv"
)
write_csv(global_grp |> dplyr::select(year, analysis_group, DMC_Gt), "Results/Data-Figures/fig1b.csv")
cat("  Saved: Results/Data-Figures/fig1a-b.csv\n")


# Stacked 100% figures ---------------

# Figure 1A — 100% stacked area by material category -------------------------

cat("\n── Figure 1A stacked 100% ──\n")

global_mat_pct <- global_mat %>%
  group_by(year) %>%
  mutate(total_Gt = sum(DMC_Gt)) %>%
  ungroup() %>%
  mutate(DMC_pct = DMC_Gt / total_Gt * 100)

labels_1a_pct <- global_mat_pct %>%
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_pct), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup() %>%
  mutate(
    material_category_label = if_else(
      DMC_pct > 0 & !str_detect(material_category_plot, "Other"),
      material_category_plot,
      NA_character_
    )
  )

group_boundaries_pct <- global_mat_pct %>%
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_pct)) %>%
  group_by(year, material_group) %>%
  summarise(boundary_y = max(top), .groups = "drop") %>%
  filter(material_group != last(levels(global_mat$material_category_plot)))

group_labels_pct <- group_boundaries_pct %>%
  arrange(year, desc(material_group)) %>%
  filter(year == max(year)) %>%
  mutate(bot = lag(boundary_y, default = 0), mid_y = (boundary_y + bot) / 2) %>%
  mutate(
    material_group_label = case_when(
      material_group == "Fossil fuels" ~ "Fossil\nfuels",
      material_group == "Metal ores" ~ "Metal\nores",
      TRUE ~ material_group
    )
  )

p_material_pct <- ggplot(global_mat_pct, aes(x = year, y = DMC_pct, fill = material_category_plot)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_textpath(
    data = labels_1a_pct |>
      filter(
        !material_category_plot %in%
          c("Coal", "Crops", "Industrial minerals", "Construction minerals", "Ferrous ores", "Other - Fossil fuels")
      ),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = "black",
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1a_pct |>
      filter(material_category_plot %in% c("Coal", "Crops", "Industrial minerals", "Construction minerals")),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = "white",
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1a_pct |> filter(material_category_plot == "Ferrous ores"),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = PALETTE_MATERIALS["Ferrous ores"],
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1a_pct |> filter(material_category_plot == "Other - Fossil fuels"),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = PALETTE_MATERIALS["Other - Fossil fuels"],
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = group_boundaries_pct,
    aes(x = year, y = boundary_y, group = material_group),
    colour = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = group_labels_pct,
    aes(x = 2025, y = mid_y, label = material_group_label, color = material_group),
    inherit.aes = FALSE, lineheight = 0.8, fontface = "bold", size = 2.2,
    angle = 90, hjust = 0.5, vjust = 0.9
  ) +
  # event_lines +
  # fmt: skip
  annotate("text", x = -Inf, y = Inf, label = "a", hjust = -1, vjust = 1, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black") +
  scale_fill_manual(values = PALETTE_MATERIALS, name = NULL) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off", xlim = c(1970, 2024)) +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Share of Material Consumption (%)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  )
p_material_pct


# Figure 1B — 100% stacked area by region ------------------------------------

cat("── Figure 1B stacked 100% ──\n")

global_grp_pct <- global_grp %>%
  group_by(year) %>%
  mutate(total_Gt = sum(DMC_Gt)) %>%
  ungroup() %>%
  mutate(DMC_pct = DMC_Gt / total_Gt * 100)

labels_1b_pct <- global_grp_pct %>%
  arrange(year, desc(analysis_group)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_pct), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup() %>%
  mutate(label = if_else(DMC_pct > 0, analysis_group, NA_character_))

p_region_pct <- ggplot(global_grp_pct, aes(x = year, y = DMC_pct, fill = analysis_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_textpath(
    data = labels_1b_pct |> filter(analysis_group != "Oceania"),
    aes(x = year, y = mid_y, label = label, group = analysis_group),
    colour = "black",
    linewidth = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1b_pct |> filter(analysis_group == "Oceania"),
    aes(x = year, y = mid_y, label = label, group = analysis_group),
    colour = PALETTE_REGIONS["Oceania"],
    linewidth = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  # event_lines +
  # fmt: skip
  annotate("text", x = -Inf, y = Inf, label = "b", hjust = -1, vjust = 1.2, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black") +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Regions", x = "Year", y = "Share of Domestic Material Consumption (%)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")
  )

p_region_pct

# Merge stacked panels

plot_grid(p_material_pct, p_region_pct, ncol = 2)

ggsave("Figures/Fig1_stacked.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/svg/Fig1_stacked.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
clean_svg("Figures/svg/Fig1_stacked.svg")

# EoF
