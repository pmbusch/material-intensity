## =============================================================================
## S27-S28 - RegionalTimeseries.R
## SI companions to Figure 1a/1b (historical DMC, UNEP, 1970-2024):
##   S27: one panel per region, DMC stacked by material group (free y per region:
##        compares composition and trend, not level). Same material-group mapping,
##        colours and stacked-area style as Figure 1b.
##   S28: 10-year moving-average growth of DMC, (DMC_t / DMC_t-10)^(1/10) - 1,
##        (a) by region + world, (b) by material group (world).
## Reads Parameters/UNEP-Materials/materials_region_DMC.csv; saves to
## Figures/Supporting-Figures/ (+ SVG).
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
pb_set_geom_defaults("largeFont")

MA_YEARS <- 10L # moving-average window for S28 (years)
MAT_GROUP_LEVELS <- c("Non-metallic minerals", "Biomass", "Fossil fuels", "Metal ores") # bottom -> top, as in Figure 1b


# Load data and map to material groups (same mapping as Figure 1b) --------------

df <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE) |>
  filter(abs(DMC_Mt) > 0.1, DMC_Mt > 0) |>
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
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(material_group))

# Region panels ordered by 2024 GDP per capita (highest first)
region_levels <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |>
  filter(year == 2024, Region %in% unique(df$Region)) |>
  inner_join(
    read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE) |> filter(year == 2024),
    by = c("Region", "year")
  ) |>
  arrange(desc(GDP_2015USD / population)) |>
  pull(Region)

region_mat <- df |>
  group_by(Region, year, material_group) |>
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(
    Region = factor(Region, levels = region_levels),
    material_group = factor(material_group, levels = rev(MAT_GROUP_LEVELS))
  )


# S27: DMC by region, stacked by material group ----------------------------------

# Thick black boundary on top of each material group (as in Figure 1b)
boundary_s27 <- region_mat |>
  arrange(Region, year, desc(material_group)) |>
  group_by(Region, year) |>
  mutate(top = cumsum(DMC_Gt)) |>
  ungroup()

# Direct labels: colour-coded key in the empty upper-left corner of the largest
# region's panel (its early years are low), stacked in the same order as the areas
LABEL_REGION <- region_mat |>
  group_by(Region) |>
  summarise(total = sum(DMC_Gt), .groups = "drop") |>
  slice_max(total, n = 1) |>
  pull(Region) |>
  as.character()
y_top_s27 <- max(boundary_s27$top[boundary_s27$Region == LABEL_REGION])
labels_s27 <- tibble::tibble(
  Region = factor(LABEL_REGION, levels = region_levels),
  material_group = rev(MAT_GROUP_LEVELS), # top of the stack first
  year = 1972,
  mid_y = y_top_s27 * (0.95 - 0.1 * (seq_along(MAT_GROUP_LEVELS) - 1))
)

ggplot(region_mat, aes(x = year, y = DMC_Gt, fill = material_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_line(data = boundary_s27, aes(x = year, y = top, group = material_group), colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_text(
    data = labels_s27, aes(x = year, y = mid_y, label = sub("Non-metallic ", "Non-metallic\n", material_group), colour = material_group),
    inherit.aes = FALSE, hjust = 0, vjust = 1, lineheight = 0.85, fontface = "bold", size = pb_annot_size("largeFont", 8)
  ) +
  facet_wrap(~Region, ncol = 4, scales = "free_y", labeller = label_wrap_gen(width = 18)) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2020, 25), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) + # precision from breaks (Oceania: 0.5 Gt steps)
  coord_cartesian(clip = "off") +
  labs(title = "Material consumption by region and material group (Gt)", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(panel.spacing = unit(0.8, "lines"), strip.text = element_text(face = "bold"))

ggsave("Figures/Supporting-Figures/S27_RegionalTimeseries.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 1.4)
ggsave("Figures/SVG/Supporting-Figures/S27_RegionalTimeseries.svg", ggplot2::last_plot(), units = "cm", width = 8.7 * 2, height = 8.7 * 1.4)
clean_svg("Figures/SVG/Supporting-Figures/S27_RegionalTimeseries.svg")
cat("  Saved: Figures/Supporting-Figures/S27_RegionalTimeseries.png\n")


# S28: 10-year moving-average growth of DMC --------------------------------------
# Growth over the trailing MA_YEARS window, annualised: (DMC_t / DMC_t-10)^(1/10) - 1,
# first value at 1970 + MA_YEARS.

growth_region <- df |>
  group_by(series = Region, year) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  bind_rows(
    df |>
      group_by(year) |>
      summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
      mutate(series = "World")
  ) |>
  arrange(series, year) |>
  group_by(series) |>
  mutate(growth = (DMC_Mt / dplyr::lag(DMC_Mt, MA_YEARS))^(1 / MA_YEARS) - 1) |>
  ungroup() |>
  filter(!is.na(growth))

growth_mat <- df |>
  group_by(series = material_group, year) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  arrange(series, year) |>
  group_by(series) |>
  mutate(growth = (DMC_Mt / dplyr::lag(DMC_Mt, MA_YEARS))^(1 / MA_YEARS) - 1) |>
  ungroup() |>
  filter(!is.na(growth))

# Shared y range so the two panels compare fairly
y_rng_s28 <- range(c(growth_region$growth, growth_mat$growth, 0))
y_lim_s28 <- y_rng_s28 + c(-0.05, 0.05) * diff(y_rng_s28)
x_lim_s28 <- c(1970L + MA_YEARS, max(df$year))

p_s28a <- ggplot(growth_region, aes(x = year, y = growth, colour = series)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_line(data = growth_region |> filter(series != "World"), linewidth = 0.45) +
  geom_line(data = growth_region |> filter(series == "World"), linewidth = 0.9) +
  geom_text_repel(
    data = growth_region |> filter(year == max(year)),
    aes(label = series, fontface = if_else(series == "World", "bold", "plain")),
    hjust = 0, direction = "y", nudge_x = 1.5, xlim = c(x_lim_s28[2] + 1, Inf),
    segment.size = 0.2, segment.colour = "grey60", min.segment.length = 0,
    size = pb_annot_size("largeFont", 8), show.legend = FALSE, seed = 42
  ) +
  annotate("text", x = -Inf, y = Inf, label = "a", hjust = -0.6, vjust = 1.3, fontface = "bold", size = pb_annot_size("largeFont", 11)) +
  scale_colour_manual(values = c(PALETTE_REGIONS, "World" = "black"), guide = "none") +
  scale_x_continuous(breaks = seq(1980, 2020, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(xlim = x_lim_s28, ylim = y_lim_s28, expand = FALSE, clip = "off") +
  labs(title = "By region", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(plot.margin = margin(t = 4, r = 125, b = 4, l = 4, unit = "pt")) # r: room for the direct labels

p_s28b <- ggplot(growth_mat, aes(x = year, y = growth, colour = series)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  geom_text_repel(
    data = growth_mat |> filter(year == max(year)),
    aes(label = series),
    hjust = 0, direction = "y", nudge_x = 1.5, xlim = c(x_lim_s28[2] + 1, Inf),
    segment.size = 0.2, segment.colour = "grey60", min.segment.length = 0,
    size = pb_annot_size("largeFont", 8), show.legend = FALSE, seed = 42
  ) +
  annotate("text", x = -Inf, y = Inf, label = "b", hjust = -0.6, vjust = 1.3, fontface = "bold", size = pb_annot_size("largeFont", 11)) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1980, 2020, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(xlim = x_lim_s28, ylim = y_lim_s28, expand = FALSE, clip = "off") +
  labs(title = "By material group (world)", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(plot.margin = margin(t = 4, r = 125, b = 4, l = 4, unit = "pt"))

fig_s28 <- patchwork::wrap_plots(p_s28a, p_s28b, ncol = 1) + # two rows: a (regions) above b (material groups)
  patchwork::plot_annotation(
    title = paste0("Material consumption growth, ", MA_YEARS, "-year moving average (%/yr)"),
    theme = theme_pb_large()
  )

ggsave("Figures/Supporting-Figures/S28_DMC_Growth_MA10.png", fig_s28, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 1.8)
ggsave("Figures/SVG/Supporting-Figures/S28_DMC_Growth_MA10.svg", fig_s28, units = "cm", width = 8.7 * 2, height = 8.7 * 1.8)
clean_svg("Figures/SVG/Supporting-Figures/S28_DMC_Growth_MA10.svg")
cat("  Saved: Figures/Supporting-Figures/S28_DMC_Growth_MA10.png\n")

# EoF
