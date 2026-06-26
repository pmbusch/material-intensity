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
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup() %>%
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
  geom_textpath(
    data = labels_1a |>
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
    data = labels_1a |>
      filter(material_category_plot %in% c("Coal", "Crops", "Industrial minerals", "Construction minerals")),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = "white",
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1a |> filter(material_category_plot == "Ferrous ores"),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = PALETTE_MATERIALS["Ferrous ores"],
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1a |> filter(material_category_plot == "Other - Fossil fuels"),
    aes(x = year, y = mid_y, label = material_category_label, group = material_category_plot),
    colour = PALETTE_MATERIALS["Other - Fossil fuels"],
    linewidth = 0,
    hjust = 0.5,
    size = 1.8,
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
  annotate("text",x = -Inf, y = Inf,label = "a",hjust = -1, vjust = 1,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_MATERIALS, name = NULL) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off", xlim = c(1970, 2024)) +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
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
  arrange(year, desc(analysis_group)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup() %>%
  mutate(label = if_else(DMC_Gt > 0.5, analysis_group, NA_character_))

p_region <- ggplot(global_grp, aes(x = year, y = DMC_Gt, fill = analysis_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_textpath(
    data = labels_1b |> filter(analysis_group != "Oceania"),
    aes(x = year, y = mid_y, label = label, group = analysis_group),
    colour = "black",
    linewidth = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  geom_textpath(
    data = labels_1b |> filter(analysis_group == "Oceania"),
    aes(x = year, y = mid_y, label = label, group = analysis_group),
    colour = PALETTE_REGIONS["Oceania"],
    linewidth = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  # event_lines +
  # fmt: skip
  annotate("text",x = -Inf, y = Inf,label = "b",hjust = -1, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Regions", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")
  )
p_region

# fmt: skip
ggsave("Figures/Fig1B_global_DMC_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
ggsave("Figures/SVG/Fig1B_global_DMC_by_region.svg", ggplot2::last_plot(), units = 'cm', width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig1B_global_DMC_by_region.svg")

# Merge --
library(cowplot)
plot_grid(p_material, p_region, ncol = 2)
ggsave("Figures/Fig1.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/SVG/Fig1.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7)
clean_svg("Figures/SVG/Fig1.svg")


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
