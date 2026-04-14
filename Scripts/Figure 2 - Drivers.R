## =============================================================================
## Figure 2 - Driver.R
## Line charts from df UNEP/Pop/GDP data
## Reads from Parameters/; saves figures to Figures/
## =============================================================================

## Load data ----------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Materials (DMC), GDP and population from separate panel files.
# Join GDP/pop only where needed — prevents accidental aggregation over materials.
df <- read_csv("Parameters/materials_DMC.csv", show_col_types = FALSE)
df_gdp <- read_csv("Parameters/gdp.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_historical.csv", show_col_types = FALSE)

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

## Figure 2A: DMC per capita by analysis_group (line chart) ------------------

cat("── Figure 2A ──\n")
library(geomtextpath)

# Deduplicated country-year population and GDP (one row per country-year).
# Analysis_group comes from the material file via iso_region.
pop_cy <- df_pop %>% left_join(iso_region, by = "ISO3") %>% filter(!is.na(population))

gdp_cy <- df_gdp %>% left_join(iso_region, by = "ISO3") %>% filter(!is.na(GDP_PPP_2017USD))

# DMC summed across all materials per country-year-group
dmc_grp <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group), !is.na(DMC_Mt)) %>%
  group_by(ISO3, year, analysis_group) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Consistent country base: must have DMC + population + GDP.
# Shared for both Fig 1D and 1E so both figures use the identical country set.
country_base_2 <- dmc_grp %>%
  left_join(pop_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  left_join(gdp_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  filter(!is.na(population), !is.na(GDP_PPP_2017USD))

# Region-year totals computed once from the consistent base
region_econ_2 <- country_base_2 %>%
  group_by(year, analysis_group) %>%
  summarise(
    total_DMC_t = sum(DMC_Mt_total * 1e6, na.rm = TRUE), # Mt → tonnes
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE), # Mt → kg
    total_pop = sum(population, na.rm = TRUE),
    total_GDP = sum(GDP_PPP_2017USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DMC_pc_tonnes = total_DMC_t / total_pop, intensity_kg_USD = total_DMC_kg / total_GDP)

pc_grp <- region_econ_2 %>% select(year, analysis_group, DMC_pc_tonnes)

# World average per capita (same consistent base)
world_pc <- country_base_2 %>%
  group_by(year) %>%
  summarise(
    total_DMC = sum(DMC_Mt_total * 1e6, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DMC_pc_tonnes = total_DMC / total_pop, analysis_group = "World avg")


# Stagger label positions to avoid overlap: sort all lines by total, assign
# cycling label years 2000 → 2005 → 2010 → 2015 → 2020 → repeat
label_pos_yrs <- c(2000, 2010, 2005, 2015, 2020)
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
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off", expand = F) +
  theme_pb_large() +
  labs(title = "DMC per capita", x = "Year", y = "DMC / Pop (tonnes per capita)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2A_DMC_per_capita_by_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


##  Figure 2B: Material intensity by region (line chart, no dual axis) --------

cat("── Figure 1B ──\n")

# Reuse the consistent country base from Fig 1D — same country set, same GDP denominator.
intensity_gdp <- region_econ_2 %>% select(year, analysis_group, intensity_kg_USD)

# World average intensity
world_intensity <- country_base_2 %>%
  group_by(year) %>%
  summarise(
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE),
    total_GDP = sum(GDP_PPP_2017USD, na.rm = TRUE),
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
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0.01, 0)) +
  scale_y_continuous() +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material intensity", x = "Year", y = "DMC / GDP (kg per 2017 USD PPP)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2B_material_intensity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


###################
#  BY MATERIAL -----------------------
# Panel order: largest total material (descending) — consistent across panels
mat_totals_1e <- df %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_Mt), .groups = "drop") %>%
  arrange(desc(total))

selected_materials <- mat_totals_1e |> filter(total > 22e3) |> pull(material_category)

## Figure 2C: DMC per capita ------------------

cat("── Figure 2C ──\n")

# DMC summed across all materials per country-year-group
dmc_grp <- df %>%
  filter(material_category %in% selected_materials) %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group), !is.na(DMC_Mt)) %>%
  group_by(ISO3, year, analysis_group, material_category) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")
unique(dmc_grp$material_category)

# Consistent country base: must have DMC + population + GDP.
# Shared for both Fig 1D and 1E so both figures use the identical country set.
country_base_2 <- dmc_grp %>%
  left_join(pop_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  left_join(gdp_cy %>% rename(analysis_group = Analysis_group), by = c("ISO3", "year", "analysis_group")) %>%
  filter(!is.na(population), !is.na(GDP_PPP_2017USD))

# Region-year totals computed once from the consistent base
region_econ_2 <- country_base_2 %>%
  group_by(year, material_category, analysis_group) %>%
  summarise(
    total_DMC_t = sum(DMC_Mt_total * 1e6, na.rm = TRUE), # Mt → tonnes
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE), # Mt → kg
    total_pop = sum(population, na.rm = TRUE),
    total_GDP = sum(GDP_PPP_2017USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DMC_pc_tonnes = total_DMC_t / total_pop, intensity_kg_USD = total_DMC_kg / total_GDP)

pc_grp <- region_econ_2 %>% select(year, analysis_group, DMC_pc_tonnes, material_category)

# World average per capita (same consistent base)
world_pc <- country_base_2 %>%
  group_by(year, material_category) %>%
  summarise(
    total_DMC = sum(DMC_Mt_total * 1e6, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DMC_pc_tonnes = total_DMC / total_pop, analysis_group = "World avg")


# Stagger label positions to avoid overlap: sort all lines by total, assign
# cycling label years 2000 → 2005 → 2010 → 2015 → 2020 → repeat
label_pos_yrs <- c(2000, 2010, 2005, 2015, 2020)
year_min_d <- min(pc_grp$year)
year_max_d <- max(pc_grp$year)

hjust_1d <- bind_rows(
  pc_grp %>% group_by(analysis_group, material_category) %>% summarise(total = sum(DMC_pc_tonnes), .groups = "drop"),
  pc_grp %>%
    group_by(material_category) %>%
    summarise(total = sum(DMC_pc_tonnes), .groups = "drop") |>
    mutate(analysis_group = "World avg")
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min_d) / (year_max_d - year_min_d)
  )

pc_grp_h <- left_join(
  pc_grp,
  select(hjust_1d, analysis_group, hjust_val, material_category),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))
world_pc_h <- left_join(
  world_pc,
  select(hjust_1d, analysis_group, hjust_val, material_category),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))


ggplot(pc_grp_h, aes(x = year, y = DMC_pc_tonnes, colour = analysis_group, label = analysis_group, hjust = hjust_val)) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
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
  coord_cartesian(clip = "off", expand = F) +
  theme_pb_large() +
  labs(title = "DMC per capita", x = "Year", y = "DMC / Pop (tonnes per capita)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2C_DMC_per_capita_by_material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*4)


##  Figure 2D: Material intensity by material --------

cat("── Figure 2b ──\n")

# Reuse the consistent country base from Fig 2B — same country set, same GDP denominator.
intensity_gdp <- region_econ_2 %>% select(year, analysis_group, material_category, intensity_kg_USD)

# World average intensity
world_intensity <- country_base_2 %>%
  group_by(year, material_category) %>%
  summarise(
    total_DMC_kg = sum(DMC_Mt_total * 1e9, na.rm = TRUE),
    total_GDP = sum(GDP_PPP_2017USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(intensity_kg_USD = total_DMC_kg / total_GDP, analysis_group = "World avg")

# Same staggered-label approach as 1D, sorted by total intensity
year_min_e <- min(intensity_gdp$year)
year_max_e <- max(intensity_gdp$year)

hjust_1e <- bind_rows(
  intensity_gdp %>%
    group_by(analysis_group, material_category) %>%
    summarise(total = sum(intensity_kg_USD), .groups = "drop"),
  intensity_gdp |>
    group_by(material_category) %>%
    summarise(total = sum(intensity_kg_USD), .groups = "drop") |>
    mutate(analysis_group = "World avg")
) %>%
  arrange(total) %>%
  mutate(
    label_year = label_pos_yrs[(row_number() - 1L) %% length(label_pos_yrs) + 1L],
    hjust_val = (label_year - year_min_e) / (year_max_e - year_min_e)
  )

intensity_gdp_h <- left_join(
  intensity_gdp,
  select(hjust_1e, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))
world_int_h <- left_join(
  world_intensity,
  select(hjust_1e, analysis_group, material_category, hjust_val),
  by = c("analysis_group", "material_category")
) |>
  mutate(material_category = factor(material_category, levels = selected_materials))

ggplot(
  intensity_gdp_h,
  aes(x = year, y = intensity_kg_USD, colour = analysis_group, label = analysis_group, hjust = hjust_val)
) +
  geom_textline(linewidth = 0.8, size = 2.2, vjust = -0.3, fontface = "bold") +
  facet_wrap(~material_category, nrow = 4, scales = "free_y") +
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
  labs(title = "Material intensity", x = "Year", y = "DMC / GDP (kg per 2017 USD PPP)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# fmt: skip
ggsave("Figures/Fig2D_material_intensity_byMaterial.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4, height = 8.7*4)


## Figure 2 aux: Small multiples by material category ---------

cat("── Figure 2 aux ──\n")

mat_gdp <- df %>%
  filter(material_category %in% selected_materials) %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, material_category, analysis_group) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")


# Fill order: same as Fig 1B (ascending total DMC — smallest at bottom of stack)
# Wrap long facet titles at 40 characters to avoid truncation
mat_totals_1e <- mat_totals_1e %>% mutate(material_category_w = str_wrap(material_category, width = 30))


# Sort regions by total DMC ascending (smallest at bottom of stack)
grp_totals <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

mat_gdp <- mat_gdp %>%
  mutate(
    material_category = str_wrap(material_category, width = 30),
    material_category = factor(material_category, levels = mat_totals_1e$material_category_w),
    analysis_group = factor(analysis_group, levels = grp_totals$analysis_group)
  )

ggplot(mat_gdp, aes(x = year, y = DMC_Mt, fill = analysis_group)) +
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

# EoF
