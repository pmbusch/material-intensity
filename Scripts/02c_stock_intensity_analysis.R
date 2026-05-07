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
world_gdp <- gdp %>%
  group_by(year) %>%
  summarise(GDP_2015USD = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop")

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

cat(
  "  Rows with no GDP match:",
  n_missing_gdp,
  "\n",
  " Materials with no PALETTE match:",
  if (length(unmatched_mat) == 0) "none" else paste(unmatched_mat, collapse = ", "),
  "\n"
)

# EoF
