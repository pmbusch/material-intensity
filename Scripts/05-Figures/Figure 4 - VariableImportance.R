## =============================================================================
## Figure 4 - VariableImportance.R
## Top: flipped parameter-importance panel from 04-VariableImportance.R (SHAP
##   on DMC 2050, parameter contribution % vs consumption) next to a GDP/capita
##   vs consumption scatter, coloured by the 3-class CAGR-based decoupling
##   classification from 19-Decoupling.R (absolute / relative / no decoupling,
##   Total material group, 2040-2060, actual population weights).
## Bottom: 14-LowHighConsumptionParameters.R's mirrored parameter-conditions
##   bar chart for that same subset, re-keyed on the average value of the
##   selected subset's draws (instead of the share of draws above 0.5).
##
## No modelling here -- all inputs are cached by "Figure 4 - PrepareData.R"
## in Parameters/Intermediate/, so this script only loads CSVs and plots.
##
## Output: Figures/Fig4.png / Figures/SVG/Fig4.svg
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

cat("=== Figure 4 - Variable Importance & Decoupling ===\n\n")

GDPCAP_COLOR <- "#117A65"
vline_x <- 106.3 # 2024 actual primary consumption (Gt), see 05-Exploratory/04-VariableImportance.R
DECOUPLE_PLOT_COLORS <- c(PALETTE_DECOUPLING, "Not classified" = "grey70")

FONT_BUMP <- theme(
  plot.title = element_text(size = 13),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 9),
  legend.text = element_text(size = 9),
  legend.title = element_text(size = 9.5)
)


# STEP 1: Load prepared data -----------------------------------------------------

cat("STEP 1: Load prepared data\n")

run_data <- read_csv("Parameters/Intermediate/Figure4_RunScatter.csv", show_col_types = FALSE) |>
  mutate(
    decoupling_class = factor(
      dplyr::coalesce(decoupling_class, "Not classified"),
      levels = c(names(PALETTE_DECOUPLING), "Not classified")
    )
  )
plot_df <- read_csv("Parameters/Intermediate/Figure4_ParamImportance.csv", show_col_types = FALSE)
label_df <- read_csv("Parameters/Intermediate/Figure4_ParamLabels.csv", show_col_types = FALSE)
sel_stats <- read_csv("Parameters/Intermediate/Figure4_ParamBars.csv", show_col_types = FALSE)

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
    x = vline_x,
    y = 15,
    label = "2024 Consumption",
    angle = 0,
    color = "grey70",
    size = 7 * 5 / 14 * 0.8,
    hjust = 0,
    vjust = -0.3
  ) +
  geom_text(
    data = label_df |> filter(show_label),
    aes(x = mid_x_gt, y = mid_y, label = as.character(display_label), color = text_col),
    size = 1.8, hjust = 0.5, angle = 90, inherit.aes = FALSE
  ) +
  coord_flip(xlim = x_range_dmc, expand = FALSE) +
  scale_x_continuous(breaks = x_breaks_dmc, labels = x_breaks_dmc, position = "top") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
  scale_fill_manual(values = fill_vals, name = NULL, guide = "none") +
  scale_color_identity() +
  labs(title = "Parameter importance", x = "", y = "Relative contribution (%)") +
  theme_pb_large() +
  theme(axis.title.y = element_text(angle = 270)) # right-side title: undo ggplot's stuck-at-90 default


# STEP 3: Top-right panel - GDP/capita vs consumption, coloured by decoupling class ---
# Class comes from 19-Decoupling.R's rigorous CAGR classification (Total material
# group, main variant: window 2040-2060, actual population weights), not the
# percentile-rank `selected` subset (still in the data, just no longer the colour here).

cat("STEP 3: GDP/capita vs consumption scatter, coloured by decoupling class\n")

p_right_flipped <- ggplot(run_data, aes(x = GDPcap_2050, y = DMC_2050_Mt / 1e3)) +
  geom_density_2d(colour = "grey75", linewidth = 0.25, alpha = 0.7) +
  geom_point(aes(colour = decoupling_class), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = vline_x, linetype = "dashed", color = "grey70", linewidth = 0.3) +
  coord_cartesian(ylim = x_range_dmc, expand = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(breaks = x_breaks_dmc, labels = x_breaks_dmc) +
  scale_colour_manual(values = DECOUPLE_PLOT_COLORS, name = NULL) +
  labs(
    x = "GDP per capita 2050 ('000 USD)",
    y = "Material consumption 2050 (Gt/year)",
    caption = "Colour: 2040-2060 decoupling class (enabling condition under model assumptions, not a probability)"
  ) +
  theme_pb_large() +
  theme(
    legend.position = c(0.78, 0.88),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA),
    plot.caption = element_text(size = 6)
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
  geom_text(
    aes(x = signed_pct / 2, y = label, label = sprintf("%.0f%%", majority_pct)),
    hjust = 0.5, size = 3.6, colour = "black", fontface = "bold"
  ) +
  geom_text(
    aes(x = param_x, y = label, label = label_expr, hjust = param_hjust),
    size = 3, colour = "black", parse = TRUE
  ) +
  geom_text(
    data = family_headers, aes(x = -165, y = row_y, label = family, colour = family),
    hjust = 0, vjust = 0, fontface = "bold", size = 3.4, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    limits = c(-170, 170), # wide margin beyond +-100 so parameter-name text isn't clipped
    breaks = c(-100, -50, 0, 50, 100),
    labels = c("100%", "50%", "0%", "50%", "100%"),
    expand = c(0.01, 0)
  ) +
  scale_fill_manual(values = family_pal, guide = "none") +
  scale_colour_manual(values = family_pal, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 0.88, `FALSE` = 0.32), guide = "none") +
  labs(
    title = "Parameter conditions for the decoupling subset",
    x = "Average draw value of selected subset (% of parameter range; centred on 50% = uniform mean)",
    y = NULL
  ) +
  theme_pb_large() +
  FONT_BUMP +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9)
  )


# STEP 5: Combine panels & save ---------------------------------------------------

cat("STEP 5: Combine & save\n")

fig <- (p_left_flipped | p_right_flipped) / p_bars + patchwork::plot_layout(heights = c(1, 3))

ggsave("Figures/Fig4.png", fig, units = "cm", dpi = 600, width = 18, height = 8.7 * 3)
ggsave("Figures/SVG/Fig4.svg", fig, units = "cm", width = 18, height = 8.7 * 3)
clean_svg("Figures/SVG/Fig4.svg")
cat("  Saved: Figures/Fig4.png\n\n")

cat("=== Done ===\n")

# EoF
