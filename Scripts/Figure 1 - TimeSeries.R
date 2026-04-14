## =============================================================================
## Figure 1 - TimeSeries.R
## Stacked area charts and line charts from df UNEP/Pop/GDP data
## Reads from Parameters/; saves figures to Figures/
## =============================================================================

## Load data ----------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
df <- read_csv("Parameters/panel_data.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1) # filter out zero
cat("Rows:", nrow(df), "\n")
cat("Material categories:", paste(sort(unique(df$material_category)), collapse = " | "), "\n")


##  Colour palettes

# 21-colour palette for material categories — tab20 + 2 extras, assigned alphabetically
# fmt: skip
TAB20 <- c("#1f77b4","#aec7e8","#ff7f0e","#ffbb78","#2ca02c","#98df8a","#d62728",
"#ff9896","#9467bd","#8c564b","#c49c94","#e377c2","#f7b6d2","#7f7f7f",
"#c7c7c7","#bcbd22","#dbdb8d","#17becf","#9edae5","#393b79","#637939")

mats_sorted <- sort(unique(df$material_category))
n_mats <- length(mats_sorted)
cat("Number of material categories:", n_mats, "\n")

# Trim or extend palette to match number of categories
palette_mats <- setNames(TAB20[seq_len(n_mats)], mats_sorted)

# Analysis group palette — qualitative (up to 12)
groups_sorted <- sort(unique(df$Analysis_group[!is.na(df$Analysis_group)]))
n_groups <- length(groups_sorted)
cat("Number of analysis groups:", n_groups, "\n")

# Use RColorBrewer Set3 for groups (up to 15)
# fmt: skip
group_palette_raw <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69",
"#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f","#e41a1c","#377eb8","#4daf4a")

palette_groups <- setNames(group_palette_raw[seq_len(n_groups)], groups_sorted)

# Vertical event lines reused across figures
event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.5),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.5),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = -0.1, size = 3, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = -0.1, size = 3, colour = "grey40")
)


# FIGURES ---------------

## Figure 1A: Global DMC by material category ----------------------------------

cat("\n── Figure 1A ──\n")

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
  filter(year == 2015) %>%
  arrange(desc(material_category)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_mat, aes(x = year, y = DMC_Gt, fill = material_category)) +
  geom_area(colour = NA, alpha = 0.9) +
  geom_text(
    data = labels_1a,
    aes(x = 2015, y = mid_y, label = material_category),
    hjust = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_fill_manual(values = palette_mats, name = NULL) +
  scale_colour_manual(values = palette_mats, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(suffix = " Gt"), expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 70, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1A_global_DMC_by_material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

##  Figure 1B: Global DMC by analysis_group ----------------------------------------

cat("── Figure 1B ──\n")

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
  filter(year == 2015) %>%
  arrange(desc(analysis_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_grp, aes(x = year, y = DMC_Gt, fill = analysis_group)) +
  geom_area(colour = NA, alpha = 0.9) +
  geom_text(data = labels_1b,
            aes(x = 2015, y = mid_y, label = analysis_group),
            hjust = 0, size = 1.8, inherit.aes = FALSE) +
  event_lines +
  scale_fill_manual(values = palette_groups, name = NULL) +
  scale_colour_manual(values = palette_groups, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(suffix = " Gt"), expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Regions", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1B_global_DMC_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


## Figure 1C: Global DMC by material group ------------------------------------

cat("── Figure 1C ──\n")

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
# fmt: skip
palette_mat_groups_raw <- c("#e41a1c","#377eb8","#4daf4a","#ff7f00","#984ea3","#a65628")
palette_mat_groups <- setNames(palette_mat_groups_raw[seq_len(n_mat_groups)], grp_mat_totals_1c$Material_group)

# Direct label positions at year 2015
labels_1c <- global_grp_mat %>%
  filter(year == 2015) %>%
  arrange(desc(Material_group)) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2)

ggplot(global_grp_mat, aes(x = year, y = DMC_Gt, fill = Material_group)) +
  geom_area(colour = NA, alpha = 0.9) +
  geom_text(
    data = labels_1c,
    aes(x = 2015, y = mid_y, label = Material_group),
    hjust = 0,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  event_lines +
  scale_fill_manual(values = palette_mat_groups, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(suffix = " Gt"), expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material group", x = "Year", y = "Domestic Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Fig1C_global_DMC_by_material_group.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


## Figure 1D: DMC per capita by analysis_group (line chart) ------------------

cat("── Figure 1D ──\n")
library(geomtextpath)

# Deduplicated country-year population and GDP (one row per country-year)
pop_cy <- df %>% distinct(ISO3, year, Analysis_group, population) %>% filter(!is.na(population))
gdp_cy <- df %>% distinct(ISO3, year, Analysis_group, GDP_PPP_2017USD) %>% filter(!is.na(GDP_PPP_2017USD))

# DMC summed across all materials per country-year-group
dmc_grp <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group), !is.na(DMC_Mt)) %>%
  group_by(ISO3, year, analysis_group) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Consistent country base: must have DMC + population + GDP.
# Shared for both Fig 1D and 1E so both figures use the identical country set.
country_base_1d1e <- dmc_grp %>%
  left_join(pop_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  left_join(gdp_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  filter(!is.na(population), !is.na(GDP_PPP_2017USD))

# Region-year totals computed once from the consistent base
region_econ_1d1e <- country_base_1d1e %>%
  group_by(year, analysis_group) %>%
  summarise(
    total_DMC_t  = sum(DMC_Mt_total * 1e6, na.rm = TRUE), # Mt → tonnes
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE), # Mt → kg
    total_pop    = sum(population, na.rm = TRUE),
    total_GDP    = sum(GDP_PPP_2017USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DMC_pc_tonnes    = total_DMC_t  / total_pop,
    intensity_kg_USD = total_DMC_kg / total_GDP
  )

pc_grp <- region_econ_1d1e %>% select(year, analysis_group, DMC_pc_tonnes)

# World average per capita (same consistent base)
world_pc <- country_base_1d1e %>%
  group_by(year) %>%
  summarise(
    total_DMC = sum(DMC_Mt_total * 1e6, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DMC_pc_tonnes = total_DMC / total_pop, analysis_group = "World avg")


# Stagger label positions to avoid overlap: sort all lines by total, assign
# cycling label years 2000 → 2005 → 2010 → 2015 → 2020 → repeat
label_pos_yrs <- c(2000, 2005, 2010, 2015, 2020)
year_min_d <- min(pc_grp$year)
year_max_d <- max(pc_grp$year)

hjust_1d <- bind_rows(
  pc_grp %>% group_by(analysis_group) %>% summarise(total = sum(DMC_pc_tonnes), .groups = "drop"),
  tibble(analysis_group = "World avg", total = sum(world_pc$DMC_pc_tonnes))
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min_d) / (year_max_d - year_min_d)
  )

pc_grp_h <- left_join(pc_grp, select(hjust_1d, analysis_group, hjust_val), by = "analysis_group")
world_pc_h <- left_join(world_pc, select(hjust_1d, analysis_group, hjust_val), by = "analysis_group")

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
  scale_colour_manual(values = palette_groups, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_number(suffix = " t")) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "per capita", x = "Year", y = "Domestic Material Consumption per capita (tonnes per person)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig1D_DMC_per_capita_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


##  Figure 1E: Material intensity by region (line chart, no dual axis) --------

cat("── Figure 1E ──\n")

# Reuse the consistent country base from Fig 1D — same country set, same GDP denominator.
intensity_gdp <- region_econ_1d1e %>% select(year, analysis_group, intensity_kg_USD)

# World average intensity
world_intensity <- country_base_1d1e %>%
  group_by(year) %>%
  summarise(
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE),
    total_GDP    = sum(GDP_PPP_2017USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(intensity_kg_USD = total_DMC_kg / total_GDP, analysis_group = "World avg")

# Same staggered-label approach as 1D, sorted by total intensity
year_min_e <- min(intensity_gdp$year)
year_max_e <- max(intensity_gdp$year)

hjust_1e <- bind_rows(
  intensity_gdp %>% group_by(analysis_group) %>% summarise(total = sum(intensity_kg_USD), .groups = "drop"),
  tibble(analysis_group = "World avg", total = sum(world_intensity$intensity_kg_USD))
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min_e) / (year_max_e - year_min_e)
  )

intensity_gdp_h <- left_join(intensity_gdp, select(hjust_1e, analysis_group, hjust_val), by = "analysis_group")
world_int_h <- left_join(world_intensity, select(hjust_1e, analysis_group, hjust_val), by = "analysis_group")

ggplot(
  intensity_gdp_h,
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
  scale_colour_manual(values = palette_groups, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = " kg/$")) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material intensity", x = "Year", y = "DMC / GDP (kg per 2017 USD PPP)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig1E_material_intensity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


## Figure 1F: Small multiples by material category ---------

cat("── Figure 1F ──\n")

mat_gdp <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, material_category, analysis_group) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Panel order: largest total material (descending) — consistent across panels
mat_totals_1e <- mat_gdp %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_Mt), .groups = "drop") %>%
  arrange(desc(total))

# Fill order: same as Fig 1B (ascending total DMC — smallest at bottom of stack)
# Wrap long facet titles at 40 characters to avoid truncation
mat_totals_1e <- mat_totals_1e %>% mutate(material_category_w = str_wrap(material_category, width = 30))

mat_gdp <- mat_gdp %>%
  mutate(
    material_category = str_wrap(material_category, width = 30),
    material_category = factor(material_category, levels = mat_totals_1e$material_category_w),
    analysis_group = factor(analysis_group, levels = grp_totals_1b$analysis_group)
  )

ggplot(mat_gdp, aes(x = year, y = DMC_Mt, fill = analysis_group)) +
  geom_area(colour = NA, alpha = 0.9) +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
  scale_fill_manual(values = palette_groups, name = NULL) +
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
ggsave("Figures/Fig1F_small_multiples_material_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*3)

# EoF
