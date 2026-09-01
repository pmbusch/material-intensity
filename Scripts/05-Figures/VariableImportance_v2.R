## =============================================================================
## VariableImportance_v2.R
## Deprecated original, superseded by "Figure 4 - VariableImportance.R".
## Top: flipped parameter-importance panel from 04-VariableImportance.R (SHAP
##   on DMC 2050, parameter contribution % vs consumption) next to a GDP/capita
##   vs consumption scatter, coloured by the 3-class CAGR-based decoupling
##   classification from 19-Decoupling.R (absolute / relative / no decoupling,
##   Total material group, 2025-2050, actual population weights -- matches
##   this panel's 2050 snapshot, see STEP 3 note).
## Bottom: 14-LowHighConsumptionParameters.R's mirrored parameter-conditions
##   bar chart for that same subset, re-keyed on the average value of the
##   selected subset's draws (instead of the share of draws above 0.5).
##
## No modelling here -- all inputs are cached by "Figure 4 - PrepareData.R"
## in Parameters/Intermediate/, so this script only loads CSVs and plots.
##
## Output: Figures/Fig_VariableImportance_v2.png / Figures/SVG/Fig_VariableImportance_v2.svg
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

pb_set_geom_defaults("largeFont") # floor size for every geom_text/label unless overridden via pb_annot_size()

cat("=== Variable Importance & Decoupling (v2, deprecated) ===\n\n")

GDPCAP_COLOR <- "#117A65"
vline_x <- 106.3 # 2024 actual primary consumption (Gt), see 05-Exploratory/04-VariableImportance.R -- panel a only
DECOUPLE_PLOT_COLORS <- c(PALETTE_DECOUPLING, "Not classified" = "grey70", "Top 10%" = "black")
ALPHA_SIG <- 0.05 # matches Figure 4 - PrepareData.R's significance threshold (t-test of subset mean vs 0.5)


# STEP 1: Load prepared data -----------------------------------------------------

cat("STEP 1: Load prepared data\n")

run_data <- read_csv("Parameters/Intermediate/Figure3_RunScatter.csv", show_col_types = FALSE) |>
  mutate(
    decoupling_class = factor(
      dplyr::coalesce(decoupling_class, "Not classified"),
      levels = c(names(PALETTE_DECOUPLING), "Not classified")
    )
  )
plot_df <- read_csv("Parameters/Intermediate/Figure3_ParamImportance.csv", show_col_types = FALSE)
label_df <- read_csv("Parameters/Intermediate/Figure3_ParamLabels.csv", show_col_types = FALSE)
sel_stats <- read_csv("Parameters/Intermediate/Figure3_ParamBars.csv", show_col_types = FALSE)

# Rebuild the stacking order/legend factor that PrepareData determined (top
# parameters by SHAP importance), and the fill palette straight from the
# per-row hex codes it saved -- no need to redo the SHAP ranking here.
label_order <- plot_df |> distinct(display_label, stack_order) |> arrange(stack_order) |> pull(display_label)
plot_df <- plot_df |> mutate(display_label = factor(display_label, levels = label_order))
label_df <- label_df |> mutate(display_label = factor(display_label, levels = label_order))
fill_vals <- plot_df |> distinct(display_label, fill_hex) |> tibble::deframe()

x_range_dmc <- range(plot_df$x_gt, na.rm = TRUE)
x_breaks_dmc <- pretty(x_range_dmc, n = 6)


# STEP 2: Top-left panel - flipped parameter-importance stacked area ------------

cat("STEP 2: Flipped parameter-importance panel\n")

p_left_flipped <- ggplot(plot_df, aes(x = x_gt, y = pct, fill = display_label)) +
  geom_area(colour = "black", linewidth = 0.15, position = "stack") +
  geom_vline(xintercept = vline_x, linetype = "dashed", color = "grey70", linewidth = 0.3) +
  annotate(
    "text",
    x = vline_x - 10,
    y = 5,
    label = "2024 Consumption",
    angle = 0,
    color = "grey70",
    size = pb_annot_size("largeFont", 8),
    hjust = 0,
    vjust = -0.3
  ) +
  geom_text(
    data = label_df |> filter(show_label),
    aes(x = mid_x_gt, y = mid_y, label = as.character(display_label), color = text_col),
    size = pb_annot_size("largeFont", 8), hjust = 0.5, angle = 90, inherit.aes = FALSE
  ) +
  coord_flip(xlim = x_range_dmc, expand = FALSE) +
  scale_x_continuous(breaks = x_breaks_dmc, labels = x_breaks_dmc, position = "top") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
  scale_fill_manual(values = fill_vals, name = NULL, guide = "none") +
  scale_color_identity() +
  labs(title = "Parameter importance", x = "", y = "Relative contribution (%)") +
  theme_pb_large() +
  theme(
    axis.title.y = element_text(angle = 270), # right-side title: undo ggplot's stuck-at-90 default
    plot.margin = margin(t = 4, r = 2, b = 4, l = 4, unit = "pt") # tightened right margin: closer to panel b
  )


# STEP 3: Top-right panel - GDP/capita vs consumption growth, coloured by decoupling class ---
# Both axes are per-capita CAGR, 2025-2050 (main_2050 variant), read straight
# from mat_pc_cagr/gdp_pc_cagr (Figure 4 - PrepareData.R, sourced from
# 19-Decoupling.R) -- growth is now the PRIMARY axis (previously a derived
# sec_axis() bolted onto level axes, and the old y-axis was TOTAL consumption
# growth, not per-capita, so the two growth rates weren't actually comparable).
# The level framing (GDP/capita 2050, '000 USD) is kept only as small
# reference numbers near the x-axis, not a full continuous secondary scale.
# Point colour comes from 19-Decoupling.R's rigorous CAGR classification (Total
# material group, "main_2050" variant: window 2025-2050, actual population
# weights) -- chosen specifically to end at 2050, matching this panel's
# snapshot year (see Figure 4 - PrepareData.R STEP 3 for why a window ending
# at 2060 instead would let colour and position disagree).
# The percentile-rank `selected` subset (top 10% high GDP/capita & low
# consumption, same one the bar panel below is conditioned on) is layered on
# top as a black outline, not the fill colour.

cat("STEP 3: GDP/capita vs consumption growth scatter, coloured by decoupling class\n")

x_range_growth <- range(run_data$gdp_pc_cagr, na.rm = TRUE)
y_range_growth <- range(run_data$mat_pc_cagr, na.rm = TRUE)

# Approximate label positions (fractions of the growth-rate ranges above) --
# "Absolute decoupling" fills the y < 0 band; "Relative"/"No decoupling" split
# the y >= 0 band across the gap = 0 diagonal (see iso_gap below); "Peak
# decoupling" is interspersed with "Relative decoupling" in y >= 0 (same
# region, distinguished by trajectory shape, not position) so its label sits
# near that subset's approximate centroid rather than a delineated zone.
decoupling_label_pos <- tibble::tribble(
  ~decoupling_class     , ~frac_x , ~frac_y , ~short_label                     , ~angle_label ,
  "Absolute decoupling" , 0.5     , 0.2     , "Absolute\ndecoupling"           ,            0 ,
  "Peak decoupling"     , 0.2     , 0.42    , "Peak\ndecoupling"               ,            0 ,
  "Relative decoupling" , 0.45    , 0.62    , "Relative\ndecoupling"           ,           25 ,
  "No decoupling"       , 0.12    , 0.93    , "No decoupling"                  ,            0 ,
  "Top 10%"             , 0.85    , 0.15    , "Top 10% High GDP\nLow Material" ,            0
) |>
  mutate(
    x = x_range_growth[1] + frac_x * diff(x_range_growth),
    y = y_range_growth[1] + frac_y * diff(y_range_growth)
  )

# Isolines of constant growth GAP (GDP/cap CAGR - material/cap CAGR): diagonal
# y = x - gap lines. gap = 0 is the exact Relative/No-decoupling boundary;
# "pretty" gap levels auto-bracket the observed data range.
gap_range <- range(run_data$gdp_pc_cagr - run_data$mat_pc_cagr, na.rm = TRUE)
gap_levels <- pretty(gap_range, n = 5)
iso_gap <- expand.grid(gap = gap_levels, x = seq(x_range_growth[1], x_range_growth[2], length.out = 200)) |>
  mutate(y = x - gap, gap_label = paste0(sprintf("%+.1f", gap * 100), "%/yr")) |>
  dplyr::filter(y >= y_range_growth[1], y <= y_range_growth[2])

# Reference numbers near the x-axis: 2050 GDP/capita level ('000 USD) that
# each x-position (GDP/cap CAGR) corresponds to. Read directly off the run
# cloud (linear interpolation of GDPcap_2050 vs. gdp_pc_cagr, both already
# per-run) instead of re-deriving from a separate base-year anchor, so the
# numbers always match what's actually plotted. Drawn in a small padded strip
# below the data's true y-range so they never overlap a real point.
gdp_order <- order(run_data$GDPcap_2050)
level_marks <- tibble::tibble(level_kusd = c(16, 18, 20, 22)) |>
  mutate(x = approx(run_data$GDPcap_2050[gdp_order], run_data$gdp_pc_cagr[gdp_order], xout = level_kusd)$y) |>
  dplyr::filter(!is.na(x), x >= x_range_growth[1], x <= x_range_growth[2])

y_pad <- 0.14 * diff(y_range_growth)
y_lim_panel <- c(y_range_growth[1] - y_pad, y_range_growth[2])
y_tick_top <- y_range_growth[1] - 0.02 * diff(y_range_growth)
y_tick_bot <- y_range_growth[1] - 0.06 * diff(y_range_growth)
y_text <- y_range_growth[1] - 0.08 * diff(y_range_growth)
y_caption <- y_range_growth[1] - 0.13 * diff(y_range_growth)

p_right_flipped <- ggplot(run_data, aes(x = gdp_pc_cagr, y = mat_pc_cagr)) +
  geom_textline(
    data = iso_gap,
    aes(x = x, y = y, group = gap_label, label = gap_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = pb_annot_size("largeFont", 7),
    hjust = 0.85,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
  geom_density_2d(colour = GDPCAP_COLOR, linewidth = 0.25, alpha = 0.35) +
  geom_point(aes(colour = decoupling_class), size = 0.4, alpha = 0.7) +
  geom_point(
    data = run_data |> filter(selected),
    aes(fill = decoupling_class), shape = 21, colour = "black", stroke = 0.35, size = 1.0
  ) +
  geom_text(
    data = decoupling_label_pos,
    aes(x = x, y = y, label = short_label, colour = decoupling_class, angle = angle_label),
    fontface = "bold", size = pb_annot_size("largeFont", 9), inherit.aes = FALSE
  ) +
  # Per-capita GDP level reference marks (2050, '000 USD) in the padded strip
  # below the data -- replaces the old continuous sec_axis().
  geom_segment(
    data = level_marks,
    aes(x = x, xend = x, y = y_tick_top, yend = y_tick_bot),
    colour = "grey40", linewidth = 0.3, inherit.aes = FALSE
  ) +
  geom_text(
    data = level_marks,
    aes(x = x, y = y_text, label = level_kusd),
    colour = "grey40", size = pb_annot_size("largeFont", 7), vjust = 1, inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = x_range_growth[1], y = y_caption,
    label = "GDP/capita 2050 ('000 USD)",
    colour = "grey40", size = pb_annot_size("largeFont", 7), hjust = 0, vjust = 1
  ) +
  coord_cartesian(xlim = x_range_growth, ylim = y_lim_panel, expand = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = DECOUPLE_PLOT_COLORS, name = NULL) +
  scale_fill_manual(values = DECOUPLE_PLOT_COLORS, guide = "none") +
  labs(
    title = "Material decoupling",
    x = "GDP per capita annual growth 2025-2050",
    y = "Material consumption per capita annual growth 2025-2050"
  ) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 8),
    plot.margin = margin(t = 4, r = 6, b = 4, l = 2, unit = "pt") # tightened left margin: closer to panel a
  )


# STEP 4: Bottom panel - decoupling-subset parameter bars ------------------------

cat("STEP 4: Decoupling-subset parameter bar panel\n")

family_pal <- c(
  "Driver SSP" = "#1f78b4",
  "Regional divergence" = "#6a3d9a",
  "Intensity" = "#e31a1c",
  "Material recovery" = "#33a02c",
  "Mining" = "#8c510a",
  "Lifetime" = "#ff7f00",
  "Other" = "#999999"
)

sel_stats <- sel_stats |> mutate(label = factor(label, levels = unique(label[order(row_y)])))

family_headers <- sel_stats |> group_by(family) |> filter(row_y == max(row_y)) |> ungroup() |> distinct(family, row_y)

ABS_DECOUPLE_COLOR <- PALETTE_DECOUPLING[["Absolute decoupling"]]

p_bars <- ggplot(sel_stats) +
  geom_tile( # majority segment: solid, alpha by significance
    aes(x = majority_center, y = label, width = majority_pct, height = 0.8, fill = family, alpha = significant),
    colour = "black", linewidth = 0.15
  ) +
  geom_tile( # minority (leftover) segment: always faded
    aes(x = minority_center, y = label, width = minority_pct, height = 0.8, fill = family),
    colour = "black", linewidth = 0.15, alpha = 0.22
  ) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "grey20", linewidth = 0.8) +
  geom_text(data=filter(sel_stats,signed_pct>0),
    aes(x = signed_pct, y = label, label = value_label),
    hjust = -0.2, size = pb_annot_size("largeFont", 10), colour = "black", fontface = "bold"
  ) +
  geom_text(data=filter(sel_stats,signed_pct<0),
    aes(x = signed_pct, y = label, label = value_label),
    hjust = 1.2, size = pb_annot_size("largeFont", 10), colour = "black", fontface = "bold"
  ) +
  geom_text(
    aes(x = param_x, y = label, label = label_expr, hjust = param_hjust),
    size = pb_annot_size("largeFont", 8), colour = "black", parse = TRUE
  ) +
  geom_text(
    data = family_headers, aes(x = -165, y = row_y, label = family, colour = family),
    hjust = 0, vjust = 0, fontface = "bold", size = pb_annot_size("largeFont", 9), inherit.aes = FALSE
  ) +
  # Absolute-decoupling-only mean draw per parameter (different, rigorously
  # classified subset from the percentile-rank bars above) -- point only, no
  # label, same green as the "Abs" text in panel b.
  geom_point(aes(x = abs_point_x, y = label), colour = ABS_DECOUPLE_COLOR, size = 1.6,alpha=0.7) +
  # Skew-direction arrows replace the numeric x-axis (removed below): the bars
  # are mirrored around 0 = uniform draw, so left/right simply means the
  # selected subset's draws skew below/above that uniform expectation.
  annotate(
    "segment",
    x = -15,
    xend = -95,
    y = -0.1,
    yend = -0.1,
    arrow = grid::arrow(length = grid::unit(0.25, "cm"), type = "closed"),
    colour = "grey30",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = -55,
    y = -0.6,
    label = "Samples skewed to lower values",
    size = pb_annot_size("largeFont", 8),
    fontface = "bold",
    colour = "grey30"
  ) +
  annotate(
    "segment",
    x = 15,
    xend = 95,
    y = -0.1,
    yend = -0.1,
    arrow = grid::arrow(length = grid::unit(0.25, "cm"), type = "closed"),
    colour = "grey30",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 55,
    y = -0.6,
    label = "Samples skewed to higher values",
    size = pb_annot_size("largeFont", 8),
    fontface = "bold",
    colour = "grey30"
  ) +
  scale_x_continuous(
    limits = c(-170, 170), # wide margin beyond +-100 so parameter-name text isn't clipped
    expand = c(0.01, 0)
  ) +
  scale_y_discrete(expand = expansion(add = c(2, 0.6))) + # extra room at the bottom for the skew arrows
  scale_fill_manual(values = family_pal, guide = "none") +
  scale_colour_manual(values = family_pal, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 0.88, `FALSE` = 0.32), guide = "none") +
  labs(title = "Parameter conditions for Top 10% High GDP Low Material", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# STEP 5: Combine panels & save ---------------------------------------------------

cat("STEP 5: Combine & save\n")

fig <- (p_left_flipped | p_right_flipped) /
  p_bars +
  patchwork::plot_layout(heights = c(1, 3)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag.position = c(0.95, 0.95))

ggsave("Figures/Fig_VariableImportance_v2.png", fig, units = "cm", dpi = 600, width = 18, height = 8.7 * 3)
ggsave("Figures/SVG/Fig_VariableImportance_v2.svg", fig, units = "cm", width = 18, height = 8.7 * 3)
clean_svg("Figures/SVG/Fig_VariableImportance_v2.svg")
cat("  Saved: Figures/Fig_VariableImportance_v2.png\n\n")

cat("=== Done ===\n")

# EoF
