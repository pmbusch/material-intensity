## =============================================================================
## Figure 1 - TimeSeries.R
## Stacked area charts from df UNEP/Pop/GDP data
## Reads from Parameters/; saves figures to Figures/
## =============================================================================

## Load data ----------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Materials (DMC)
df <- read_csv("Parameters/materials_DMC.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")
cat("Material categories:", paste(sort(unique(df$material_category)), collapse = " | "), "\n")

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS          : 9 analysis groups (regions)
# PALETTE_MATERIALS        : 21 material categories
# PALETTE_MATERIAL_GROUPS  : 6 material groups

# Analysis_group lives in the material file; join it into pop and GDP frames.
iso_region <- df %>% distinct(ISO3, Analysis_group)

# Vertical event lines reused across figures
event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40")
)


# FIGURES ---------------

# Figure 1A: Global DMC by material group ------------------------------------

cat("── Figure 1A ──\n")

# Load material group classification (22 materials → 6 groups)
dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  select(Material_22, Material_group)

# Join group to panel data via material_category (assumed to match Material_22)
global_grp_mat <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Material_group)) %>%
  group_by(year, Material_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Sort groups by total DMC ascending (smallest at bottom of stack)
grp_mat_totals_1c <- global_grp_mat %>%
  group_by(Material_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

global_grp_mat <- global_grp_mat %>%
  mutate(Material_group = factor(Material_group, levels = grp_mat_totals_1c$Material_group))

# 6-colour palette for material groups
n_mat_groups <- nrow(grp_mat_totals_1c)

# Direct label positions at year 2015
labels_1c <- global_grp_mat %>%
  filter(year == 2008) %>%
  arrange(desc(Material_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) |>
  mutate(Material_group_label = if_else(DMC_Gt > 0.5, Material_group, ""))

ggplot(global_grp_mat, aes(x = year, y = DMC_Gt, fill = Material_group)) +
  geom_area(colour = "black",linewidth=0.05, alpha = 0.9) +
  geom_text(
    data = labels_1c,
    aes(x = 2008, y = mid_y, label = Material_group_label),
    hjust = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  event_lines +
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


# Figure 1B: Global DMC by material category ----------------------------------

cat("\n── Figure 1B ──\n")

global_mat <- df %>%
  group_by(year, material_category) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") # Mt → Gt

global_total <- global_mat %>% group_by(year) %>% summarise(total_Gt = sum(DMC_Gt, na.rm = TRUE), .groups = "drop")

# Sort materials by total DMC descending (largest at bottom of stack)
mat_totals_1a <- global_mat %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange((total))

global_mat <- global_mat %>%
  mutate(material_category = factor(material_category, levels = mat_totals_1a$material_category))


# Direct label positions at year 2020
labels_1a <- global_mat %>%
  filter(year == 2008) %>%
  arrange(desc(material_category)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) |>
  mutate(material_category_label = if_else(DMC_Gt > 0.5, material_category, ""))

ggplot(global_mat, aes(x = year, y = DMC_Gt, fill = material_category)) +
  geom_area(colour = "black",linewidth=0.05, alpha = 0.9) +
  geom_text(
    data = labels_1a,
    aes(x = 2008, y = mid_y, label = material_category_label),
    hjust = 0,
    size = 1.8,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_fill_manual(values = PALETTE_MATERIALS, name = NULL) +
  scale_colour_manual(values = PALETTE_MATERIALS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 70, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1B_global_DMC_by_material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

#  Figure 1C: Global DMC by analysis_group ----------------------------------------

cat("── Figure 1C ──\n")

global_grp <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

global_total_b <- global_grp %>% group_by(year) %>% summarise(total_Gt = sum(DMC_Gt, na.rm = TRUE), .groups = "drop")

# Sort regions by total DMC ascending (smallest at bottom of stack)
grp_totals_1b <- global_grp %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)
global_grp <- global_grp %>% mutate(analysis_group = factor(analysis_group, levels = grp_totals_1b$analysis_group))

# Direct label positions at year 2020
labels_1b <- global_grp %>%
  filter(year == 2008) %>%
  arrange(desc(analysis_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_grp, aes(x = year, y = DMC_Gt, fill = analysis_group)) +
  geom_area(colour = "black",linewidth=0.05, alpha = 0.9) +
  geom_text(data = labels_1b,
            aes(x = 2008, y = mid_y, label = analysis_group),
            hjust = 0, size = 1.8, inherit.aes = FALSE) +
  event_lines +
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
    plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1C_global_DMC_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

#  Figure 1D: Global extraction by region ----------------------------------------

cat("── Figure 1D ──\n")

df2 <- read_csv("Parameters/materials_DE.csv", show_col_types = FALSE)
df2 <- df2 |> filter(abs(DE_Mt) > 0.1)

global_de <- df2 %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DE_Gt = sum(DE_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

global_total_b <- global_de %>% group_by(year) %>% summarise(total_Gt = sum(DE_Gt, na.rm = TRUE), .groups = "drop")

# Sort regions by total DE ascending (smallest at bottom of stack)
grp_totals_1b <- global_de %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DE_Gt), .groups = "drop") %>%
  arrange(total)
global_de <- global_de %>% mutate(analysis_group = factor(analysis_group, levels = grp_totals_1b$analysis_group))

# Direct label positions at year 2020
labels_1d <- global_de %>%
  filter(year == 2008) %>%
  arrange(desc(analysis_group)) %>%
  mutate(top = cumsum(DE_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_de, aes(x = year, y = DE_Gt, fill = analysis_group)) +
  geom_area(colour = "black",linewidth=0.05, alpha = 0.9) +
  geom_text(data = labels_1d,
            aes(x = 2008, y = mid_y, label = analysis_group),
            hjust = 0, size = 1.8, inherit.aes = FALSE) +
  event_lines +
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

# EoF
