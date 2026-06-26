## =============================================================================
## Figure 2 - Drivers.R
## Line charts of DMC per capita and material intensity by region.
## Reads pre-aggregated regional files from Parameters/; saves to Figures/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Load pre-aggregated regional data produced by 01a/01b/01c scripts.
# Columns:
#   materials_region_DMC : Region, year, material_category, DMC_Mt
#   population_region    : Region, year, population
#   gdp_region           : Region, year, GDP_2015USD
df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)

# World-level files for the global average reference line
df_world_mat <- read_csv("Parameters/materials_world_DMC.csv", show_col_types = FALSE)
df_world_pop <- read_csv("Parameters/population_world_historical.csv", show_col_types = FALSE)
df_world_gdp <- read_csv("Parameters/gdp_world.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")
cat("Regions:", paste(sort(unique(df$Region)), collapse = " | "), "\n")

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS         : 8 regions
# PALETTE_MATERIALS       : 21 material categories
# PALETTE_MATERIAL_GROUPS : 6 material groups

# Vertical event lines reused across figures
event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40")
)


# FIGURES ---------------

## Figure 2A: DMC per capita by region (line chart) --------------------------

cat("── Figure 2A ──\n")
library(geomtextpath)

# Total DMC per region-year across all materials
dmc_region <- df %>% group_by(Region, year) %>% summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Join population and GDP; keep only region-years with all three
region_econ <- dmc_region %>%
  left_join(df_pop, by = c("Region", "year")) %>%
  left_join(df_gdp, by = c("Region", "year")) %>%
  filter(!is.na(population), !is.na(GDP_2015USD)) %>%
  mutate(
    DMC_pc_tonnes = DMC_Mt_total * 1e6 / population, # Mt → t, then per capita
    intensity_kg_USD = DMC_Mt_total * 1e9 / GDP_2015USD # Mt → kg, then per USD
  ) %>%
  rename(analysis_group = Region)

# World average — from world-level files (consistent universe)
world_econ <- df_world_mat %>%
  group_by(year) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_world_pop, by = "year") %>%
  left_join(df_world_gdp, by = "year") %>%
  filter(!is.na(population), !is.na(GDP_2015USD)) %>%
  mutate(
    DMC_pc_tonnes = DMC_Mt_total * 1e6 / population,
    intensity_kg_USD = DMC_Mt_total * 1e9 / GDP_2015USD,
    analysis_group = "World avg"
  )

# Staggered label positions (avoids overlap): sort by total, cycle through years
label_pos_yrs <- c(2000, 2010, 2005, 2015, 2020)
year_min <- min(region_econ$year)
year_max <- max(region_econ$year)

pc_grp <- region_econ %>% dplyr::select(year, analysis_group, DMC_pc_tonnes)

hjust_2a <- bind_rows(
  pc_grp %>% group_by(analysis_group) %>% summarise(total = sum(DMC_pc_tonnes), .groups = "drop"),
  tibble(analysis_group = "World avg", total = sum(world_econ$DMC_pc_tonnes))
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min) / (year_max - year_min)
  )

pc_grp_h <- left_join(pc_grp, dplyr::select(hjust_2a, analysis_group, hjust_val), by = "analysis_group")
world_pc_h <- left_join(
  world_econ %>% dplyr::select(year, analysis_group, DMC_pc_tonnes),
  dplyr::select(hjust_2a, analysis_group, hjust_val),
  by = "analysis_group"
)

ggplot(pc_grp_h, aes(x = year, y = DMC_pc_tonnes, colour = analysis_group, label = analysis_group, hjust = hjust_val)) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  geom_textline(
    data = world_pc_h,
    aes(x = year, y = DMC_pc_tonnes, label = analysis_group, hjust = hjust_val),
    colour = "black",
    linewidth = 1.2,
    size = 2.2,
    vjust = -0.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_pb_large() +
  labs(title = "DMC per capita", x = "Year", y = "DMC / Pop (tonnes per capita)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2A_DMC_per_capita_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*1.5)
ggsave(
  "Figures/SVG/Fig2A_DMC_per_capita_by_region.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 2,
  height = 8.7 * 1.5
)


##  Figure 2B: Material intensity by region (line chart) ---------------------

cat("── Figure 2B ──\n")

intensity_grp <- region_econ %>% dplyr::select(year, analysis_group, intensity_kg_USD)

hjust_2b <- bind_rows(
  intensity_grp %>% group_by(analysis_group) %>% summarise(total = sum(intensity_kg_USD), .groups = "drop"),
  tibble(analysis_group = "World avg", total = sum(world_econ$intensity_kg_USD))
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min) / (year_max - year_min)
  )

int_grp_h <- left_join(intensity_grp, dplyr::select(hjust_2b, analysis_group, hjust_val), by = "analysis_group")
world_int_h <- left_join(
  world_econ %>% dplyr::select(year, analysis_group, intensity_kg_USD),
  dplyr::select(hjust_2b, analysis_group, hjust_val),
  by = "analysis_group"
)

ggplot(
  int_grp_h,
  aes(x = year, y = intensity_kg_USD, colour = analysis_group, label = analysis_group, hjust = hjust_val)
) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  geom_textline(
    data = world_int_h,
    aes(x = year, y = intensity_kg_USD, label = analysis_group, hjust = hjust_val),
    colour = "black",
    linewidth = 1.2,
    size = 2.2,
    vjust = -0.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material intensity", x = "Year", y = "DMC / GDP (kg per 2015 USD)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2B_material_intensity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*1.5)
# fmt: skip
ggsave("Figures/SVG/Fig2B_material_intensity.svg",ggplot2::last_plot(),units = 'cm',width = 8.7 * 2,height = 8.7 * 1.5)


###################
#  BY MATERIAL -----------------------

# Panel order: largest total material (descending)
mat_totals <- df %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_Mt), .groups = "drop") %>%
  arrange(desc(total))

selected_materials <- mat_totals |> filter(total > 22e3) |> pull(material_category)


## Figure 2C: DMC per capita by region and material --------------------------

cat("── Figure 2C ──\n")

dmc_mat_region <- df %>%
  filter(material_category %in% selected_materials) %>%
  group_by(Region, year, material_category) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Join pop and GDP; region-years must have all three
region_econ_mat <- dmc_mat_region %>%
  left_join(df_pop, by = c("Region", "year")) %>%
  left_join(df_gdp, by = c("Region", "year")) %>%
  filter(!is.na(population), !is.na(GDP_2015USD)) %>%
  mutate(DMC_pc_tonnes = DMC_Mt_total * 1e6 / population, intensity_kg_USD = DMC_Mt_total * 1e9 / GDP_2015USD) %>%
  rename(analysis_group = Region)

# World average per material
world_econ_mat <- df_world_mat %>%
  filter(material_category %in% selected_materials) %>%
  left_join(df_world_pop, by = "year") %>%
  left_join(df_world_gdp, by = "year") %>%
  filter(!is.na(population), !is.na(GDP_2015USD)) %>%
  mutate(
    DMC_pc_tonnes = DMC_Mt * 1e6 / population,
    intensity_kg_USD = DMC_Mt * 1e9 / GDP_2015USD,
    analysis_group = "World avg"
  )

pc_mat <- region_econ_mat %>% dplyr::select(year, analysis_group, material_category, DMC_pc_tonnes)

hjust_2c <- bind_rows(
  pc_mat %>% group_by(analysis_group, material_category) %>% summarise(total = sum(DMC_pc_tonnes), .groups = "drop"),
  world_econ_mat %>%
    group_by(material_category) %>%
    summarise(total = sum(DMC_pc_tonnes), .groups = "drop") %>%
    mutate(analysis_group = "World avg")
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min) / (year_max - year_min)
  )

pc_mat_h <- left_join(
  pc_mat,
  dplyr::select(hjust_2c, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))

world_pc_mat_h <- left_join(
  world_econ_mat %>% dplyr::select(year, analysis_group, material_category, DMC_pc_tonnes),
  dplyr::select(hjust_2c, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))

ggplot(pc_mat_h, aes(x = year, y = DMC_pc_tonnes, colour = analysis_group, label = analysis_group, hjust = hjust_val)) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
  geom_textline(
    data = world_pc_mat_h,
    aes(x = year, y = DMC_pc_tonnes, label = analysis_group, hjust = hjust_val),
    colour = "black",
    linewidth = 1.2,
    size = 2.2,
    vjust = -0.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_pb_large() +
  labs(title = "DMC per capita", x = "Year", y = "DMC / Pop (tonnes per capita)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2C_DMC_per_capita_by_material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*4)
ggsave(
  "Figures/SVG/Fig2C_DMC_per_capita_by_material.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 4,
  height = 8.7 * 4
)


##  Figure 2D: Material intensity by region and material --------------------

cat("── Figure 2D ──\n")

# material intesity at 2024 for EXCEL BOUNDARIES
dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group) |>
  rename(material_category = Material_22)
data_2024 <- df %>%
  left_join(dict_mat) |>
  mutate(
    material_category = if_else(
      material_category %in% selected_materials,
      material_category,
      paste0("Rest of ", Material_group)
    )
  ) %>%
  group_by(Region, year, Material_group, material_category) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  # Join pop and GDP; region-years must have all three
  left_join(df_pop, by = c("Region", "year")) %>%
  left_join(df_gdp, by = c("Region", "year")) %>%
  filter(!is.na(population), !is.na(GDP_2015USD)) %>%
  filter(year == 2024)
data_2024 |>
  mutate(DMC_pc_tonnes = DMC_Mt_total * 1e6 / population, intensity_kg_USD = DMC_Mt_total * 1e9 / GDP_2015USD) %>%
  dplyr::select(Region, Material_group, material_category, intensity_kg_USD) |>
  arrange(Material_group, material_category, Region)
# pivot_wider(names_from = Region, values_from = intensity_kg_USD)

.Last.value %>% write.table('clipboard', sep = '\t', row.names = FALSE)

# world avg
data_2024 |>
  group_by(Material_group, material_category) |>
  summarise(
    DMC_Mt_total = sum(DMC_Mt_total, na.rm = TRUE),
    population = sum(population),
    GDP_2015USD = sum(GDP_2015USD),
    ,
    .groups = "drop"
  ) |>
  mutate(DMC_pc_tonnes = DMC_Mt_total * 1e6 / population, intensity_kg_USD = DMC_Mt_total * 1e9 / GDP_2015USD) %>%
  dplyr::select(Material_group, material_category, intensity_kg_USD) |>
  arrange(Material_group, material_category) |>
  mutate(Region = "World")

.Last.value %>% write.table('clipboard', sep = '\t', row.names = FALSE)

int_mat <- region_econ_mat %>% dplyr::select(year, analysis_group, material_category, intensity_kg_USD)

hjust_2d <- bind_rows(
  int_mat %>%
    group_by(analysis_group, material_category) %>%
    summarise(total = sum(intensity_kg_USD), .groups = "drop"),
  world_econ_mat %>%
    group_by(material_category) %>%
    summarise(total = sum(intensity_kg_USD), .groups = "drop") %>%
    mutate(analysis_group = "World avg")
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min) / (year_max - year_min)
  )

int_mat_h <- left_join(
  int_mat,
  dplyr::select(hjust_2d, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))

world_int_mat_h <- left_join(
  world_econ_mat %>% dplyr::select(year, analysis_group, material_category, intensity_kg_USD),
  dplyr::select(hjust_2d, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))

ggplot(
  int_mat_h,
  aes(x = year, y = intensity_kg_USD, colour = analysis_group, label = analysis_group, hjust = hjust_val)
) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
  geom_textline(
    data = world_int_mat_h,
    aes(x = year, y = intensity_kg_USD, label = analysis_group, hjust = hjust_val),
    colour = "black",
    linewidth = 1.2,
    size = 2.2,
    vjust = -0.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material intensity", x = "Year", y = "DMC / GDP (kg per 2015 USD)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2D_material_intensity_byMaterial.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*4)
ggsave(
  "Figures/SVG/Fig2D_material_intensity_byMaterial.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 4,
  height = 8.7 * 4
)


## Figure 2 aux: Small multiples by material category and region -------------

cat("── Figure 2 aux ──\n")

mat_region_stacked <- df %>%
  filter(material_category %in% selected_materials) %>%
  rename(analysis_group = Region) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, material_category, analysis_group) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

mat_totals_w <- mat_totals %>% mutate(material_category_w = str_wrap(material_category, width = 30))

grp_totals <- df %>%
  rename(analysis_group = Region) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

mat_region_stacked <- mat_region_stacked %>%
  mutate(
    material_category = str_wrap(material_category, width = 30),
    material_category = factor(material_category, levels = mat_totals_w$material_category_w),
    analysis_group = factor(analysis_group, levels = grp_totals$analysis_group)
  )

ggplot(mat_region_stacked, aes(x = year, y = DMC_Mt, fill = analysis_group)) +
  geom_area(colour = NA, alpha = 0.9) +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = c(1970, 1990, 2010), expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Year", y = "Domestic Material Consumption (Mt)") +
  theme_pb_large() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5)
  )

# fmt: skip
ggsave("Figures/Fig2F_small_multiples_material_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*3)
ggsave(
  "Figures/SVG/Fig2F_small_multiples_material_region.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 4,
  height = 8.7 * 3
)

# EoF
