## =============================================================================
## Figure 2 Detail - Region Summary.R
## 2024 region-comparison snapshot of M/G (all four material groups) and S/G
## (metal ores + non-metallic minerals), split out of
## "Figure 2 Detail - Region.R" so it doesn't have to wait on that script's
## slow 8-region x 8-panel loop. Only loads the historical data needed for
## the single year 2024 (no SSP/GDP blending, no envelopes, no MC results).
## Metal ores / Non-metallic minerals use the same 4 end-uses (buildings,
## civil infrastructure, machinery, short-lived products) for both M/G and
## S/G, sourced from UNEP_flows_enduse.csv (flow) and
## stock_trajectory_1970_2024.csv (stock).
## Three figures, each PNG + SVG, saved to Figures/Figure 2 Detail/:
##   - "Region 2024 Intensity - Stacked.png"     6-panel stacked bars by region
##   - "Region 2024 Intensity - Scatter MG.png"  M/G scatter, all 4 groups
##   - "Region 2024 Intensity - Scatter SG.png"  S/G scatter, metal + nonmet
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/model_parameters.R", encoding = "UTF-8")
library(patchwork)

# ── Constants ----------------------------------------------------------------

HIST_END <- 2024L

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood", "Other biomass")
BIOMASS_CATS_NAMED <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum", "Other fossil fuels")
FOSSIL_CATS_NAMED <- c("Coal", "Natural Gas", "Petroleum")

REGIONS <- names(PALETTE_REGIONS)

FONT_TITLE <- 11.5
FONT_AXIS_T <- 9.5
FONT_AXIS_L <- 9.0
FONT_BUMP <- theme(
  axis.text = element_text(size = FONT_AXIS_L),
  axis.title = element_text(size = FONT_AXIS_T),
  plot.title = element_text(size = FONT_TITLE)
)

# ── SECTION A: Load 2024 historical data --------------------------------------

cat("A: Loading historical data\n")

gdp_region_hist <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE) |> rename(region = Region)
pop_region_hist <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE) |>
  rename(region = Region)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) |> rename(region = Region)
flows_enduse_hist <- read_csv("Parameters/Intermediate/UNEP_flows_enduse.csv", show_col_types = FALSE) |>
  rename(region = Region)
stock_enduse_hist <- read_csv("Parameters/Intermediate/stock_trajectory_1970_2024.csv", show_col_types = FALSE) |>
  rename(region = Region)

gdp_2024_region <- gdp_region_hist |> filter(year == HIST_END) |> transmute(region, gdp_2024 = GDP_2015USD)

# Region display order for the stacked-bar figure: ascending GDP/cap, so the
# highest-GDP/cap region ends up at the top (last ggplot discrete-scale level
# = topmost row); "Legend" is prepended so it sits below the lowest region.
gdpcap_2024_region <- gdp_2024_region |>
  left_join(pop_region_hist |> filter(year == HIST_END) |> transmute(region, population), by = "region") |>
  mutate(gdpcap_2024 = gdp_2024 / population)
REGION_ORDER_ASC <- gdpcap_2024_region |> arrange(gdpcap_2024) |> pull(region)
REGION_LEVELS <- c("Legend", REGION_ORDER_ASC)


# ── SECTION C: Historical M/G (biomass, fossil) -------------------------------

cat("C: Historical M/G by region (biomass, fossil)\n")

biomass_hist <- dmc_hist |>
  filter(
    year == HIST_END,
    material_category %in%
      c(BIOMASS_CATS_NAMED, "Wild catch and harvest", "Non-wild animal products", "Products mainly from biomass nec.")
  ) |>
  mutate(material_category = if_else(material_category %in% BIOMASS_CATS_NAMED, material_category, "Other biomass")) |>
  group_by(region, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist |> filter(year == HIST_END), by = "region") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)

fossil_hist <- dmc_hist |>
  filter(
    year == HIST_END,
    material_category %in%
      c(
        FOSSIL_CATS_NAMED,
        "Oil shale and tar sands",
        "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
        "Other products mainly from fossil fuels e.g. plastics"
      )
  ) |>
  mutate(
    material_category = if_else(material_category %in% FOSSIL_CATS_NAMED, material_category, "Other fossil fuels")
  ) |>
  group_by(region, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist |> filter(year == HIST_END), by = "region") |>
  filter(!is.na(GDP_2015USD)) |>
  mutate(mg = DMC_Mt * 1e9 / GDP_2015USD)


# ── SECTION D: Metal ores / Non-metallic minerals, M/G and S/G, by end-use --
# Both M/G and S/G use the same 4 end-uses (buildings, civil_infrastructure,
# machinery, short_lived -> ENDUSE_LABELS/PALETTE_ENDUSE, already defined in
# model_parameters.R / 00-CommonParameters.R): M/G from UNEP_flows_enduse.csv
# (flow_Mt), S/G from stock_trajectory_1970_2024.csv (stock_Mt).

cat("D: 2024 M/G and S/G for metal ores / non-metallic minerals by end-use\n")

metal_mg_2024 <- flows_enduse_hist |>
  filter(year == HIST_END, material == "Metal ores") |>
  mutate(detail = ENDUSE_LABELS[end_use]) |>
  group_by(region, detail) |>
  summarise(flow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_2024_region, by = "region") |>
  filter(!is.na(gdp_2024)) |>
  transmute(region, detail, value = flow_Mt * 1e9 / gdp_2024)

nonmet_mg_2024 <- flows_enduse_hist |>
  filter(year == HIST_END, material == "Non-metallic minerals") |>
  mutate(detail = ENDUSE_LABELS[end_use]) |>
  group_by(region, detail) |>
  summarise(flow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_2024_region, by = "region") |>
  filter(!is.na(gdp_2024)) |>
  transmute(region, detail, value = flow_Mt * 1e9 / gdp_2024)

metal_sg_2024 <- stock_enduse_hist |>
  filter(year == HIST_END, material == "Metal ores") |>
  mutate(detail = ENDUSE_LABELS[end_use]) |>
  group_by(region, detail) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_2024_region, by = "region") |>
  filter(!is.na(gdp_2024)) |>
  transmute(region, detail, value = stock_Mt * 1e9 / gdp_2024)

nonmet_sg_2024 <- stock_enduse_hist |>
  filter(year == HIST_END, material == "Non-metallic minerals") |>
  mutate(detail = ENDUSE_LABELS[end_use]) |>
  group_by(region, detail) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_2024_region, by = "region") |>
  filter(!is.na(gdp_2024)) |>
  transmute(region, detail, value = stock_Mt * 1e9 / gdp_2024)


# ── SECTION E: Combine into one 2024 snapshot --------------------------------

cat("E: Building 2024 M/G / S/G snapshot by region\n")

snapshot_2024 <- bind_rows(
  biomass_hist |> transmute(region, detail = material_category, value = mg, mat_group = "Biomass", metric = "M/G"),
  fossil_hist |> transmute(region, detail = material_category, value = mg, mat_group = "Fossil fuels", metric = "M/G"),
  metal_mg_2024 |> mutate(mat_group = "Metal ores", metric = "M/G"),
  nonmet_mg_2024 |> mutate(mat_group = "Non-metallic minerals", metric = "M/G"),
  metal_sg_2024 |> mutate(mat_group = "Metal ores", metric = "S/G"),
  nonmet_sg_2024 |> mutate(mat_group = "Non-metallic minerals", metric = "S/G")
)

MATGROUP_ORDER <- c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")

# Stacking/legend/row order = world total (sum across regions), largest to
# smallest, fixed per group so colour position is consistent across every
# region's bar. Metal ores and Non-metallic minerals share the same 4
# end-use categories across BOTH M/G and S/G, so their world total is pooled
# across all four (metal M/G, nonmet M/G, metal S/G, nonmet S/G) -- one
# consistent order/colour slot for e.g. "Civil infrastructure" everywhere.
biomass_order <- snapshot_2024 |>
  filter(mat_group == "Biomass") |>
  group_by(detail) |>
  summarise(world_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(world_total)) |>
  pull(detail)

fossil_order <- snapshot_2024 |>
  filter(mat_group == "Fossil fuels") |>
  group_by(detail) |>
  summarise(world_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(world_total)) |>
  pull(detail)

enduse_order <- snapshot_2024 |>
  filter(mat_group %in% c("Metal ores", "Non-metallic minerals")) |>
  group_by(detail) |>
  summarise(world_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(world_total)) |>
  pull(detail)

DETAIL_ORDER <- c(biomass_order, fossil_order, enduse_order)

snapshot_2024 <- snapshot_2024 |>
  mutate(
    mat_group = factor(mat_group, levels = MATGROUP_ORDER),
    detail = factor(detail, levels = unique(DETAIL_ORDER)),
    region = factor(region, levels = REGION_LEVELS)
  )

dir.create("Figures/Figure 2 Detail", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures/SVG/Figure 2 Detail", recursive = TRUE, showWarnings = FALSE)


# ── SECTION F: Fig — 6 stacked-bar panels, regions x material detail --------
# One ggplot per (material group, metric) panel -- not facet_wrap -- so each
# can carry its own independently-scaled x-axis and its own x-axis title, and
# combine via patchwork into a 2-col x 3-row grid: Biomass/Fossil M/G on row
# 1, Metal/Nonmet M/G on row 2, Metal/Nonmet S/G on row 3. No legend:
# colours are direct-labelled in a blank row below the lowest region -- no
# bar, no tick, no axis text (via scale_y_discrete breaks=), just white
# space, all labels at the same height, positioned at each segment's mid-x
# (equal-width segments spanning the panel's real x-range, not real
# proportions -- proportional widths leave some segments too narrow for
# their label).

cat("F: Building 2024 stacked-bar summary panels (M/G all groups, S/G metal+nonmet)\n")

fill_palette_all <- c(PALETTE_MATERIALS, PALETTE_ENDUSE)

PANEL_SPECS <- list(
  list(mat_group = "Biomass", metric = "M/G", x_title = "Material consumption per GDP (kg/$)"),
  list(mat_group = "Fossil fuels", metric = "M/G", x_title = "Material consumption per GDP (kg/$)"),
  list(mat_group = "Metal ores", metric = "M/G", x_title = "Material consumption per GDP (kg/$)"),
  list(mat_group = "Non-metallic minerals", metric = "M/G", x_title = "Material consumption per GDP (kg/$)"),
  list(mat_group = "Metal ores", metric = "S/G", x_title = "Stock per GDP (kg/$)"),
  list(mat_group = "Non-metallic minerals", metric = "S/G", x_title = "Stock per GDP (kg/$)")
)

legend_y <- match("Legend", REGION_LEVELS)

stack_panels <- list()
for (i in seq_along(PANEL_SPECS)) {
  spec <- PANEL_SPECS[[i]]
  panel_data <- snapshot_2024 |> filter(mat_group == spec$mat_group, metric == spec$metric, region != "Legend")

  xmax <- panel_data |> group_by(region) |> summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |> pull(total) |> max()
  legend_row <- panel_data |>
    distinct(detail) |>
    arrange(desc(detail)) |>
    mutate(
      seg_width = xmax / n(),
      cum_top = seg_width * row_number(),
      cum_bot = dplyr::lag(cum_top, default = 0),
      mid_x = (cum_top + cum_bot) / 2,
      value = seg_width,
      region = factor("Legend", levels = REGION_LEVELS)
    )

  stack_panels[[i]] <- ggplot(panel_data, aes(x = value, y = region, fill = detail)) +
    geom_col(position = "stack", colour = "black", linewidth = 0.15, width = 0.75) +
    geom_text(
      data = legend_row, aes(x = mid_x, y = legend_y, label = detail, colour = detail),
      inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("largeFont", 8)
    ) +
    scale_fill_manual(values = fill_palette_all, guide = "none") +
    scale_colour_manual(values = fill_palette_all, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_discrete(limits = REGION_LEVELS, breaks = REGION_ORDER_ASC) +
    coord_cartesian(clip = "off") +
    labs(x = spec$x_title, y = NULL, title = spec$mat_group) +
    theme_pb_large() +
    FONT_BUMP +
    theme(plot.margin = margin(t = 4, r = 6, b = 14, l = 4))
}

p_stack <- wrap_plots(stack_panels, ncol = 2) +
  plot_annotation(
    title = "Material and stock intensity by region, 2024",
    theme = theme(plot.title = element_text(size = FONT_TITLE + 1, face = "bold", hjust = 0.5))
  ) &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(
  "Figures/Figure 2 Detail/Region 2024 Intensity - Stacked.png", p_stack,
  units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2.1
)
ggsave(
  "Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Stacked.svg", p_stack,
  units = "cm", width = 8.7 * 2, height = 8.7 * 2.1
)
group_svg_layers("Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Stacked.svg")


# ── SECTION G: Fig — M/G scatter, all 4 material groups ----------------------
# Rows: each material group's total (bold, larger point) followed by its
# subgroups (regular), in sequence for Biomass, Fossil fuels, Metal ores,
# Non-metallic minerals. Points = one region each, coloured by region.
# Metal ores and Non-metallic minerals both use the same 4 end-use details,
# so rows are keyed by (mat_group, detail), not detail alone -- otherwise
# the two groups' shared end-use names collide as duplicate factor levels.

cat("G: Building 2024 M/G scatter (all material groups)\n")

mg_data <- snapshot_2024 |> filter(metric == "M/G", region != "Legend")

mg_total <- mg_data |>
  group_by(region, mat_group) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  transmute(region, mat_group, row_label = as.character(mat_group), is_total = TRUE, value)

mg_detail <- mg_data |>
  transmute(region, mat_group, row_label = as.character(detail), is_total = FALSE, value)

mg_scatter_data <- bind_rows(mg_total, mg_detail) |>
  mutate(row_id = paste(mat_group, row_label, sep = "__"))

mg_row_id_order <- c()
for (mg in MATGROUP_ORDER) {
  labels_mg <- c(mg, DETAIL_ORDER[DETAIL_ORDER %in% unique(mg_detail$row_label[mg_detail$mat_group == mg])])
  mg_row_id_order <- c(mg_row_id_order, paste(mg, labels_mg, sep = "__"))
}
mg_scatter_data <- mg_scatter_data |> mutate(row_id = factor(row_id, levels = rev(mg_row_id_order)))
mg_scatter_sub <- mg_scatter_data |> filter(!is_total)
mg_scatter_tot <- mg_scatter_data |> filter(is_total)

mg_row_labels <- mg_scatter_data |> distinct(row_id, row_label, is_total)

p_mg_scatter <- ggplot(mg_scatter_data, aes(x = value, y = row_id, colour = region)) +
  geom_point(data = mg_scatter_sub, size = 1.7, alpha = 0.85) +
  geom_point(data = mg_scatter_tot, size = 3.0, alpha = 0.9) +
  geom_text(
    data = mg_row_labels |> filter(!is_total), aes(x = -Inf, y = row_id, label = row_label),
    inherit.aes = FALSE, hjust = 1.05, fontface = "plain", size = pb_annot_size("largeFont", 8), colour = "#222222"
  ) +
  geom_text(
    data = mg_row_labels |> filter(is_total), aes(x = -Inf, y = row_id, label = row_label),
    inherit.aes = FALSE, hjust = 1.05, fontface = "bold", size = pb_annot_size("largeFont", 9.5), colour = "#222222"
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(limits = rev(mg_row_id_order)) +
  coord_cartesian(clip = "off") +
  labs(x = "Material consumption per GDP (kg/$)", y = NULL, title = "M/G by region and material detail, 2024") +
  theme_pb_large() +
  FONT_BUMP +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.5),
    legend.justification = c(1, 0.5),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = FONT_AXIS_L - 1),
    legend.title = element_blank(),
    plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 130)
  ) +
  guides(colour = guide_legend(ncol = 2, override.aes = list(size = 2)))

ggsave(
  "Figures/Figure 2 Detail/Region 2024 Intensity - Scatter MG.png", p_mg_scatter,
  units = "cm", dpi = 600, width = 8.7 * 2, height = 12
)
ggsave(
  "Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Scatter MG.svg", p_mg_scatter,
  units = "cm", width = 8.7 * 2, height = 12
)
group_svg_layers("Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Scatter MG.svg")


# ── SECTION H: Fig — S/G scatter, split into 2 panels (metal, nonmet) -------
# Each material gets its own panel (stacked as 2 rows via patchwork), so no
# (mat_group, detail) row-key trick is needed here -- unlike the combined M/G
# plot, each panel's data only has one group's end-use names. Independent
# x-axis per panel: non-metallic minerals' stock/GDP runs much higher than
# metal ores', so a shared scale would flatten metal ores to a line at 0.

cat("H: Building 2024 S/G scatter (metal ores + non-metallic minerals, 2 panels)\n")

sg_data <- snapshot_2024 |> filter(metric == "S/G", region != "Legend")

sg_panels <- list()
sg_groups <- c("Metal ores", "Non-metallic minerals")
sg_group_rows <- c()
for (i in seq_along(sg_groups)) {
  mg <- sg_groups[i]
  grp_data <- sg_data |> filter(mat_group == mg)

  grp_total <- grp_data |>
    group_by(region) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    transmute(region, row_label = mg, is_total = TRUE, value)

  grp_detail <- grp_data |> transmute(region, row_label = as.character(detail), is_total = FALSE, value)

  grp_scatter <- bind_rows(grp_total, grp_detail)

  row_order <- c(mg, DETAIL_ORDER[DETAIL_ORDER %in% unique(grp_detail$row_label)])
  sg_group_rows <- c(sg_group_rows, length(row_order))
  grp_scatter <- grp_scatter |> mutate(row_label = factor(row_label, levels = rev(row_order)))
  grp_sub <- grp_scatter |> filter(!is_total)
  grp_tot <- grp_scatter |> filter(is_total)
  grp_labels <- grp_scatter |> distinct(row_label, is_total)

  sg_panels[[i]] <- ggplot(grp_scatter, aes(x = value, y = row_label, colour = region)) +
    geom_point(data = grp_sub, size = 1.7, alpha = 0.85) +
    geom_point(data = grp_tot, size = 3.0, alpha = 0.9) +
    geom_text(
      data = grp_labels |> filter(!is_total), aes(x = -Inf, y = row_label, label = row_label),
      inherit.aes = FALSE, hjust = 1.05, fontface = "plain", size = pb_annot_size("largeFont", 8), colour = "#222222"
    ) +
    geom_text(
      data = grp_labels |> filter(is_total), aes(x = -Inf, y = row_label, label = row_label),
      inherit.aes = FALSE, hjust = 1.05, fontface = "bold", size = pb_annot_size("largeFont", 9.5), colour = "#222222"
    ) +
    scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_discrete(limits = rev(row_order)) +
    coord_cartesian(clip = "off") +
    labs(x = "Stock per GDP (kg/$)", y = NULL, title = mg) +
    theme_pb_large() +
    FONT_BUMP +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.5),
      legend.justification = c(1, 0.5),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = FONT_AXIS_L - 1),
      legend.title = element_blank(),
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 95)
    ) +
    guides(colour = guide_legend(ncol = 2, override.aes = list(size = 2)))
}

p_sg_scatter <- wrap_plots(sg_panels, ncol = 1, heights = sg_group_rows) +
  plot_annotation(
    title = "S/G by region and material detail, 2024",
    theme = theme(plot.title = element_text(size = FONT_TITLE + 1, face = "bold", hjust = 0.5))
  ) &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(
  "Figures/Figure 2 Detail/Region 2024 Intensity - Scatter SG.png", p_sg_scatter,
  units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 1.6
)
ggsave(
  "Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Scatter SG.svg", p_sg_scatter,
  units = "cm", width = 8.7 * 2, height = 8.7 * 1.6
)
group_svg_layers("Figures/SVG/Figure 2 Detail/Region 2024 Intensity - Scatter SG.svg")

cat("  Saved 2024 region-summary figures (stacked + M/G scatter + S/G scatter)\n")

# EoF
