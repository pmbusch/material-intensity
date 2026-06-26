## =============================================================================
## 04_stock_intensity_analysis.R
## Material stock intensity: in-use stock per unit of GDP.
##   stock_intensity = Σ stock (kg) / GDP (constant 2015 USD)
##
## The DSM stock model (Script 02b) tracks two material groups:
##   Metal ores  |  Non-metallic minerals
## and four end-uses:
##   buildings  |  civil_infrastructure  |  machinery  |  short_lived
##
## Hierarchy used in figures:
##   material_group  = material  (already at L1 group level in DSM output)
##   material_detail = end_use   (finest available dimension)
##
## Inputs:
##   Parameters/Intermediate/stock_trajectory_1970_2024.csv  -- from Script 02b
##   Parameters/gdp_region.csv  -- Region x year x GDP_2015USD
##
## Outputs:
##   Figures/Stocks/stock_intensity_global_region.png
##   Figures/Stocks/stock_intensity_by_material_group.png
##   Figures/Stocks/stock_intensity_by_material_detail.png
##   Figures/Stocks/stock_intensity_grid_group_x_detail.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)


# Step 1: Load stock trajectory -----------------------------------------------

cat("STEP 1: Load stock trajectory\n")

TRAJ_PATH <- "Parameters/Intermediate/stock_trajectory_1970_2024.csv"
stock_traj <- read_csv(TRAJ_PATH, show_col_types = FALSE) %>% dplyr::select(Region, material, end_use, year, stock_Mt)

cat("  Rows:", nrow(stock_traj), "| years:", min(stock_traj$year), "-", max(stock_traj$year), "\n")
cat("  Materials:", paste(sort(unique(stock_traj$material)), collapse = ", "), "\n")
cat("  End-uses: ", paste(sort(unique(stock_traj$end_use)), collapse = ", "), "\n")
cat("  Regions:  ", paste(sort(unique(stock_traj$Region)), collapse = ", "), "\n")


# Step 2: Load GDP, add material hierarchy, join ------------------------------

cat("\nSTEP 2: Load GDP and add material hierarchy\n")

gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
cat("  GDP units: constant 2015 USD (GDP_2015USD)\n")
cat("  GDP rows:", nrow(gdp), "| years:", min(gdp$year), "-", max(gdp$year), "\n")

# The material column in the stock trajectory is already at the material-group
# (L1) level. PALETTE_MATERIAL_GROUPS holds the canonical set of group names.
# Warn for any value not found in that palette.
known_groups <- names(PALETTE_MATERIAL_GROUPS)
stock_traj <- stock_traj %>% mutate(material_group = material, material_detail = ENDUSE_LABELS[end_use])

# GDP join check
stock_gdp <- stock_traj %>% left_join(gdp, by = c("Region", "year"))

n_missing_gdp <- stock_gdp %>% filter(is.na(GDP_2015USD)) %>% nrow()
if (n_missing_gdp > 0) {
  missing_combos <- stock_gdp %>% filter(is.na(GDP_2015USD)) %>% distinct(Region, year) %>% arrange(Region, year)
  warning(paste(n_missing_gdp, "stock rows have no GDP match"))
  cat("  [WARN] Missing GDP for", nrow(missing_combos), "Region x year combos:\n")
  print(missing_combos, n = 20)
}


# Step 3: Compute stock intensity datasets ------------------------------------

cat("\nSTEP 3: Compute stock intensity (kg / constant 2015 USD)\n")

# World GDP by year — used as denominator for global intensity
world_gdp <- gdp %>% group_by(year) %>% summarise(GDP_2015USD = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop")

# Helper: given a stock summary with columns Region + grouping_vars + year + stock_Mt,
# return per-region and world intensity rows.
build_intensity <- function(stock_summary, gdp_df, world_gdp_df, grouping_vars = character(0)) {
  reg <- stock_summary %>%
    left_join(gdp_df, by = c("Region", "year")) %>%
    filter(!is.na(GDP_2015USD)) %>%
    mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD) # to kg per USD

  group_keys <- c(grouping_vars, "year")
  wld <- stock_summary %>%
    group_by(across(all_of(group_keys))) %>%
    summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
    left_join(world_gdp_df, by = "year") %>%
    filter(!is.na(GDP_2015USD)) %>%
    mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD, Region = "World")

  bind_rows(reg, wld)
}

# -- 3A: All materials and end-uses summed ------------------------------------
stock_A <- stock_traj %>% group_by(Region, year) %>% summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop")

intensity_A <- build_intensity(stock_A, gdp, world_gdp)

# Sanity check: global stock intensity in 2016 ~ 10 kg per USD
sanity_val <- intensity_A %>% filter(Region == "World", year == 2016) %>% pull(stock_intensity)
cat("  Sanity check — global stock intensity 2016:", round(sanity_val, 4), "kg / constant 2015 USD\n")
if (length(sanity_val) == 0 || sanity_val > 500 || sanity_val < 0.001) {
  warning(paste(
    "Global 2016 stock intensity =",
    ifelse(length(sanity_val) == 0, "NA", round(sanity_val, 4)),
    "kg/USD — check units or GDP year coverage."
  ))
}

# -- 3B: By material_group ----------------------------------------------------
stock_B <- stock_traj %>%
  group_by(Region, material_group, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop")

intensity_B <- build_intensity(stock_B, gdp, world_gdp, grouping_vars = "material_group")

# -- 3C: By material_detail (end-use) -----------------------------------------
stock_C <- stock_traj %>%
  group_by(Region, material_detail, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop")

intensity_C <- build_intensity(stock_C, gdp, world_gdp, grouping_vars = "material_detail")

# -- 3D: By material_group x material_detail (no aggregation across either) ---
stock_D <- stock_traj %>%
  group_by(Region, material_group, material_detail, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop")

intensity_D <- build_intensity(stock_D, gdp, world_gdp, grouping_vars = c("material_group", "material_detail"))


# Step 4: Figure A – Global + by-Region, all materials -----------------------

cat("\nSTEP 4: Figure A — global + regional stock intensity\n")

region_A <- intensity_A %>% filter(Region != "World")
world_A <- intensity_A %>% filter(Region == "World")

p_A <- ggplot() +
  geom_line(data = region_A, aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_textline(
    data = region_A,
    aes(x = year, y = stock_intensity, colour = Region, label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  geom_line(data = world_A, aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  geom_textline(
    data = world_A,
    aes(x = year, y = stock_intensity, label = "World"),
    colour = "black",
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 10)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Material stock intensity (kg / constant 2015 USD)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_A

# fmt: skip
ggsave("Figures/Stocks/stock_intensity_global_region.png", p_A,units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)


# Step 5: Figure B – By material_group ----------------------------------------

cat("\nSTEP 5: Figure B — by material group\n")

region_B <- intensity_B %>% filter(Region != "World")
world_B <- intensity_B %>% filter(Region == "World")

p_B <- ggplot() +
  geom_line(data = region_B, aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_textline(
    data = region_B,
    aes(x = year, y = stock_intensity, colour = Region, label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  geom_line(data = world_B, aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  geom_textline(
    data = world_B,
    aes(x = year, y = stock_intensity, colour = Region, , label = "World"),
    color = "black",
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(~material_group, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 10)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Material stock intensity (kg / constant 2015 USD)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_B
# fmt: skip
ggsave("Figures/Stocks/stock_intensity_by_material_group.png", p_B,units = "cm", dpi = 600,width = 8.7 * 2, height = 8.7)


# Step 6: Figure C – By material_detail (end-use) -----------------------------

cat("\nSTEP 6: Figure C — by material detail (end-use)\n")

region_C <- intensity_C %>% filter(Region != "World")
world_C <- intensity_C %>% filter(Region == "World")

p_C <- ggplot() +
  geom_line(data = region_C, aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_textline(
    data = region_C,
    aes(x = year, y = stock_intensity, colour = Region, label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  geom_line(data = world_C, aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  geom_textline(
    data = world_C,
    aes(x = year, y = stock_intensity, colour = Region, , label = "World"),
    color = "black",
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  facet_wrap(~material_detail, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 10)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Material stock intensity (kg / constant 2015 USD)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_C

# fmt: skip
ggsave("Figures/Stocks/stock_intensity_by_material_detail.png", p_C,units = "cm", dpi = 600,width = 8.7 * 2, height = 8.7 *2)


# Step 7: Figure D – facet_grid(material_group ~ material_detail) -------------

cat("\nSTEP 7: Figure D — grid of material group x end-use\n")

region_D <- intensity_D %>% filter(Region != "World")
world_D <- intensity_D %>% filter(Region == "World")

p_D <- ggplot() +
  geom_line(data = region_D, aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_textline(
    data = region_D,
    aes(x = year, y = stock_intensity, colour = Region, label = Region),
    linewidth = 0,
    size = 2,
    fontface = "bold",
    hjust = 0.9,
    offset = unit(3, "pt"),
    show.legend = FALSE
  ) +
  geom_line(data = world_D, aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  facet_grid(material_group ~ material_detail, scales = "free_y") +
  # facet_wrap(material_group ~ material_detail, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 20)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Material stock intensity (kg / constant 2015 USD)") +
  theme_pb_large() +
  theme(legend.position = "none")
p_D

# fmt: skip
ggsave("Figures/Stocks/stock_intensity_grid_group_x_detail.png", p_D,units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)

# at 2024
intensity_D |> filter(year == 2024) |> dplyr::select(Region, year, material_group, material_detail, stock_intensity)
.Last.value %>% write.table('clipboard', sep = '\t', row.names = FALSE)

# Step 8: Summary check -------------------------------------------------------

cat("\nSTEP 8: Summary\n")

cat(
  "  Year range:",
  min(stock_traj$year),
  "-",
  max(stock_traj$year),
  "\n",
  " Distinct regions:        ",
  n_distinct(stock_traj$Region),
  "\n",
  " Distinct material groups:",
  n_distinct(stock_traj$material_group),
  "—",
  paste(sort(unique(stock_traj$material_group)), collapse = ", "),
  "\n",
  " Distinct material detail:",
  n_distinct(stock_traj$material_detail),
  "—",
  paste(sort(unique(stock_traj$material_detail)), collapse = ", "),
  "\n"
)

for (yr in c(1970, 2000, 2024)) {
  val <- intensity_A %>% filter(Region == "World", year == yr) %>% pull(stock_intensity)
  if (length(val) == 0) {
    cat(sprintf("  Global stock intensity %d: NA (year not in data)\n", yr))
  } else {
    cat(sprintf("  Global stock intensity %d: %.4f kg / constant 2015 USD\n", yr, val))
  }
}


# Step 9: Load sub-end-use trajectory (material × super_category × sub_use) ---

cat("\nSTEP 9: Load sub-end-use stock trajectory\n")

SUBUSE_PATH <- "Parameters/Intermediate/stock_trajectory_subenduse.csv"
stock_sub <- read_csv(SUBUSE_PATH, show_col_types = FALSE)

cat("  Rows:", nrow(stock_sub), "| Materials:", paste(sort(unique(stock_sub$material)), collapse = ", "), "\n")

MATERIAL_LABELS <- c(
  "Metal_Fe"                = "Iron & Steel (Fe)",
  "Metal_NonFe"             = "Non-ferrous Metals",
  "Non-metallic minerals"   = "Non-metallic Minerals"
)
SUPER_CAT_LABELS <- c(
  "buildings"            = "Buildings",
  "civil_infrastructure" = "Civil Infra.",
  "machinery"            = "Machinery",
  "short_lived"          = "Short-lived"
)
SUB_USE_LABELS_FIG <- c(
  "residential"      = "Residential",
  "non_residential"  = "Non-residential",
  "roads"            = "Roads",
  "civil_engineering" = "Civil Eng.",
  "machinery_group"  = "Machinery",
  "vehicles_group"   = "Vehicles",
  "durables"         = "Durables",
  "packaging"        = "Packaging"
)

stock_sub <- stock_sub %>%
  mutate(
    material_label   = MATERIAL_LABELS[material],
    super_cat_label  = SUPER_CAT_LABELS[super_category],
    sub_use_label    = SUB_USE_LABELS_FIG[sub_use]
  )


# Step 10: Figure E – facet_grid(material × super_category) -------------------

cat("\nSTEP 10: Figure E — material × super_category intensity\n")

stock_E_reg <- stock_sub %>%
  group_by(Region, material_label, super_cat_label, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(gdp, by = c("Region", "year")) %>%
  filter(!is.na(GDP_2015USD)) %>%
  mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD)

stock_E_wld <- stock_sub %>%
  group_by(material_label, super_cat_label, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(world_gdp, by = "year") %>%
  filter(!is.na(GDP_2015USD)) %>%
  mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD, Region = "World")

mat_order_E <- c("Iron & Steel (Fe)", "Non-ferrous Metals", "Non-metallic Minerals")
sc_order_E  <- c("Buildings", "Civil Infra.", "Machinery", "Short-lived")

stock_E_reg <- stock_E_reg %>%
  mutate(
    material_label  = factor(material_label,  levels = mat_order_E),
    super_cat_label = factor(super_cat_label, levels = sc_order_E)
  )
stock_E_wld <- stock_E_wld %>%
  mutate(
    material_label  = factor(material_label,  levels = mat_order_E),
    super_cat_label = factor(super_cat_label, levels = sc_order_E)
  )

p_E <- ggplot() +
  geom_line(data = stock_E_reg %>% filter(Region != "World"),
            aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_line(data = stock_E_wld,
            aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  facet_grid(material_label ~ super_cat_label, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 20)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    x = "Year",
    y = "Stock intensity (kg / constant 2015 USD)",
    colour = NULL
  ) +
  theme_pb_large() +
  theme(legend.position = "none", strip.text = element_text(size = 6))
p_E

# fmt: skip
ggsave("Figures/Stocks/stock_intensity_material_x_supercategory.png", p_E,
       units = "cm", dpi = 600, width = 17, height = 13)


# Step 11: Figure F – facet_grid(material × sub_use) -------------------------

cat("\nSTEP 11: Figure F — material × sub_use intensity\n")

stock_F_reg <- stock_sub %>%
  group_by(Region, material_label, sub_use_label, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(gdp, by = c("Region", "year")) %>%
  filter(!is.na(GDP_2015USD)) %>%
  mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD)

stock_F_wld <- stock_sub %>%
  group_by(material_label, sub_use_label, year) %>%
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(world_gdp, by = "year") %>%
  filter(!is.na(GDP_2015USD)) %>%
  mutate(stock_intensity = stock_Mt * 1e9 / GDP_2015USD, Region = "World")

sub_order_F <- unname(SUB_USE_LABELS_FIG)

stock_F_reg <- stock_F_reg %>%
  mutate(
    material_label = factor(material_label, levels = mat_order_E),
    sub_use_label  = factor(sub_use_label,  levels = sub_order_F)
  )
stock_F_wld <- stock_F_wld %>%
  mutate(
    material_label = factor(material_label, levels = mat_order_E),
    sub_use_label  = factor(sub_use_label,  levels = sub_order_F)
  )

p_F <- ggplot() +
  geom_line(data = stock_F_reg %>% filter(Region != "World"),
            aes(x = year, y = stock_intensity, colour = Region), linewidth = 0.4) +
  geom_line(data = stock_F_wld,
            aes(x = year, y = stock_intensity), colour = "black", linewidth = 0.8) +
  facet_grid(material_label ~ sub_use_label, scales = "free_y") +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  scale_x_continuous(breaks = seq(1970, 2024, 20)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    x = "Year",
    y = "Stock intensity (kg / constant 2015 USD)",
    colour = NULL
  ) +
  theme_pb_large() +
  theme(legend.position = "none", strip.text = element_text(size = 5.5))
p_F

# fmt: skip
ggsave("Figures/Stocks/stock_intensity_material_x_subuse.png", p_F,
       units = "cm", dpi = 600, width = 17, height = 13)


# EoF
