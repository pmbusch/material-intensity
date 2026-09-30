## =============================================================================
## Figure 5 - Sensitivity.R  -> Figures/Fig5 - Sensitivity.png
## 4-panel figure (target 18x18 cm), all data cached by
## "Figure 5 - Sensitivity - PrepareData.R" -- this script only loads CSVs and plots.
## Growth outcome throughout: TOTAL material/GDP consumption CAGR,
## 2025-FORECAST_END (2060). "Absolute decoupling" = mat_cagr < 0. "Selected"
## = top 10% by composite percentile rank of high GDP growth + low material
## growth (see PrepareData STEP 2). Every panel uses coord_*(clip = "off") so
## labels placed just outside a panel's data area (legends, group headers)
## are never silently cut off.
##
## a) Parameter importance (SHAP, LightGBM surrogate of growth rate) stacked
##    by growth-rate bin (<0%, 0-1%, 1-2%, >2%) -- horizontal stacked bars,
##    direct-labelled via a colour-matched text legend in white space above
##    the bars (exactly one bar's height), each label's X position averaging
##    that parameter's own stacked-segment centre across the 4 bins, two
##    alternating label heights so neighbouring labels don't collide.
## b) GDP vs. material growth scatter with its lm regression line (no SE
##    band), absolute-decoupling points highlighted green, "selected" runs
##    (below the line by > se_mult x residual SE) black-outlined at the SAME
##    point size as the rest (only the outline marks them).
##    Marginal density panels (<=~17% of the main box on their short axis):
##    X margin marks the 2% GDP growth reference line; Y margin is a SINGLE
##    density curve split by colour at the abs-decoupling threshold (0%),
##    labelled with the shares of simulations on each side.
## c) Lever effects on growth rate: regression coefficient x a fixed,
##    physically meaningful delta per lever (e.g. "+1pp population growth",
##    "+20yr lifetime") = pp effect on growth, shown as a point + 95% CI.
##    Lever names sit inline near x = 0; lever groups (M/G, S/G, lifetime,
##    ...) get extra row spacing plus one header each (group name + delta,
##    when uniform across the group), placed along the panel's right edge.
## d) One row per material category: densities of 2060 world primary
##    consumption per capita for a Low / High group of that material's 2060
##    metric (biomass t/cap, fossil MJ/$, metal and mineral stock kg/$), with
##    0% and +2.5%/yr growth-from-2025 reference lines.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

pb_set_geom_defaults("largeFont")

cat("=== Figure 4 v2 - Four Panel ===\n\n")

ABS_GREEN <- PALETTE_DECOUPLING[["Absolute decoupling"]]
GREY_PT <- "grey55" # darkened from grey75 for visibility
JITTER_SEED <- 20260305L # local plotting-only seed (panel d jitter) -- unrelated to the MC's own GLOBAL_SEED
GROUP_GAP_EXTRA <- 0.6 # panels c/d: extra vertical spacing inserted between lever groups

family_pal <- c(
  "Driver SSP" = "#1f78b4",
  "Regional divergence" = "#6a3d9a",
  "Intensity" = "#e31a1c",
  "Material recovery" = "#33a02c",
  "Mining" = "#8c510a",
  "Lifetime" = "#ff7f00",
  "Other" = "#999999"
)

# Panel c/d colour by lever GROUP (not family): the "Intensity" family alone
# covers 3 different lever groups here (M/G, metal stock, mineral stock), so
# colouring by family painted all of them the same red -- this ramps those
# 3 into distinct shades while every other group keeps its family colour.
group_pal <- c(
  growth = family_pal[["Driver SSP"]],
  mg = "#e31a1c",
  sg_metal = "#f4772e",
  sg_mineral = "#8b1a3a",
  recycling = family_pal[["Material recovery"]],
  ore_grade = family_pal[["Mining"]],
  lifetime = family_pal[["Lifetime"]]
)


# STEP 1: Load prepared data -------------------------------------------------------

cat("STEP 1: Load prepared data\n")

plot_df_a <- read_csv("Parameters/Intermediate/Figure5_GrowthImportance.csv", show_col_types = FALSE)
scatter_df <- read_csv("Parameters/Intermediate/Figure5_Scatter.csv", show_col_types = FALSE)
effects_df <- read_csv("Parameters/Intermediate/Figure5_LeverEffects.csv", show_col_types = FALSE)
samples_df <- read_csv("Parameters/Intermediate/Figure5_LeverSamples.csv", show_col_types = FALSE)
diamonds_df <- read_csv("Parameters/Intermediate/Figure5_LeverDiamonds.csv", show_col_types = FALSE)
density_df <- read_csv("Parameters/Intermediate/Figure5_Densities.csv", show_col_types = FALSE)
density_lines <- read_csv("Parameters/Intermediate/Figure5_DensityLines.csv", show_col_types = FALSE)

label_order_a <- plot_df_a |> distinct(display_label, stack_order) |> arrange(stack_order) |> pull(display_label)
plot_df_a <- plot_df_a |>
  mutate(
    display_label = factor(display_label, levels = label_order_a),
    growth_bin = factor(growth_bin, levels = c("<0%", "0-1%", "1-2%", ">2%"))
  )
fill_vals_a <- plot_df_a |> distinct(display_label, fill_hex) |> tibble::deframe()

# Shared row position across panels c and d, WITH extra spacing inserted
# between lever groups (not the display_label TEXT -- see PrepareData note:
# several rows share a bare name across different lever groups, e.g.
# "Buildings" in both the metal-stock and mineral-stock groups). Built
# bottom-up (largest `order` = bottom row = pos 1) so a group change adds
# GROUP_GAP_EXTRA on top of the normal 1-unit row step.
effects_df <- effects_df |> arrange(order)

pos_lkp <- effects_df |> arrange(dplyr::desc(order))
grp_id <- match(pos_lkp$group_key, unique(pos_lkp$group_key))
grp_change <- c(FALSE, diff(grp_id) != 0)
pos_lkp$pos <- cumsum(ifelse(grp_change, 1 + GROUP_GAP_EXTRA, 1))
pos_lkp <- pos_lkp |> dplyr::select(order, pos)
pos_range <- range(pos_lkp$pos)

effects_df <- effects_df |> dplyr::left_join(pos_lkp, by = "order")
samples_df <- samples_df |> dplyr::left_join(pos_lkp |> dplyr::rename(y_num = pos), by = "order")
diamonds_df <- diamonds_df |> dplyr::left_join(pos_lkp |> dplyr::rename(y_num = pos), by = "order")


# STEP 2: Panel a -- growth-rate SHAP importance, stacked bars by growth bin -------
# Legend label X = that parameter's own stacked-segment centre, averaged
# across the 4 growth bins (a parameter's segment width varies bin to bin,
# so this is only an approximate "where this colour tends to sit," not any
# one bar's exact geometry) -- two alternating label heights (not a grid)
# keep neighbouring labels from overlapping, in white space reserved above
# the bars equal to exactly one bar's height.

cat("STEP 2: Panel a\n")

# Stack render order: position_stack()'s DEFAULT places the fill factor's
# FIRST level farthest from the origin (x = 0) and its LAST level closest to
# it. display_label_factor's levels are rev(all_labels_ordered) -- level 1 =
# "Other", level last = most-important param -- so "Other" renders farthest
# from x = 0 and the most-important param renders CLOSEST to it. Matching
# that here means accumulating from the HIGHEST stack_order down (descending),
# not ascending -- ascending was the bug that put every label on the wrong
# side of its actual segment.
stack_pos_a <- plot_df_a |>
  dplyr::arrange(growth_bin, dplyr::desc(stack_order)) |>
  dplyr::group_by(growth_bin) |>
  dplyr::mutate(
    cum_right = cumsum(pct),
    cum_left = dplyr::lag(cum_right, default = 0),
    mid_x = (cum_right + cum_left) / 2
  ) |>
  dplyr::ungroup()

seg_pos_a <- stack_pos_a |>
  dplyr::group_by(display_label, fill_hex, stack_order) |>
  dplyr::summarise(x = mean(mid_x), .groups = "drop") |>
  dplyr::arrange(x)

# Legend Y sits ABOVE the tallest bar's rendered top edge (bar width 0.85 ->
# half-width 0.425, so the ">2%" bar's own top edge is at 4.425) -- both
# alternating rows must clear that, or a label lands ON the bar instead of
# in the reserved white space above it.
legend_df_a <- seg_pos_a |>
  dplyr::mutate(idx = dplyr::row_number(), row = ((idx - 1) %% 2) + 1, y = ifelse(row == 1, 4 + 0.55, 4 + 0.85))

# In-bar value labels: every segment >= 3% of ITS OWN bin's total (each of
# the 4 bars judged independently, unlike the legend's cross-bin average
# position) gets its literal share printed at its own segment's centre,
# black/white chosen per-segment by WCAG relative luminance for contrast
# against that segment's own fill colour.
inbar_df_a <- stack_pos_a |> dplyr::filter(pct >= 3) |> dplyr::mutate(value_label = paste0(round(pct), "%"))
r <- strtoi(substr(inbar_df_a$fill_hex, 2, 3), 16L) / 255
g <- strtoi(substr(inbar_df_a$fill_hex, 4, 5), 16L) / 255
b <- strtoi(substr(inbar_df_a$fill_hex, 6, 7), 16L) / 255
lin_r <- ifelse(r <= 0.04045, r / 12.92, ((r + 0.055) / 1.055)^2.4)
lin_g <- ifelse(g <= 0.04045, g / 12.92, ((g + 0.055) / 1.055)^2.4)
lin_b <- ifelse(b <= 0.04045, b / 12.92, ((b + 0.055) / 1.055)^2.4)
lum <- 0.2126 * lin_r + 0.7152 * lin_g + 0.0722 * lin_b
inbar_df_a$text_col <- ifelse(lum < 0.25, "white", "black")

p_a <- ggplot(plot_df_a, aes(x = pct, y = growth_bin, fill = display_label)) +
  geom_col(position = "stack", colour = "black", linewidth = 0.15, width = 0.85) +
  geom_text(
    data = inbar_df_a, aes(x = mid_x, y = growth_bin, label = value_label, colour = text_col),
    inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("largeFont", 6)
  ) +
  geom_text(
    data = legend_df_a, aes(x = x, y = y, label = display_label, colour = fill_hex),
    inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("largeFont", 6.5)
  ) +
  scale_fill_manual(values = fill_vals_a, name = NULL, guide = "none") +
  scale_colour_identity() +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.4, 1.0))) + # top pad = exactly one bar's height
  coord_cartesian(clip = "off") +
  labs(
    title = "Variable importance",
    x = "Relative contribution (%)",
    y = "Material consumption growth\n(2025-2060, annual avg.)"
  ) +
  theme_pb_large() +
  theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt"), plot.tag.location = "panel")


# STEP 3: Panel b -- decoupling scatter with marginal densities -------------------
# Right-margin density is a SINGLE curve (one density() call on all runs),
# split into two colours at the mat_cagr = 0 threshold -- not two separately
# fitted densities -- by cutting its own (x, y) grid at x = 0 and filling
# each half as its own geom_area, sharing the exact boundary point so the
# polygons close without a gap. Marginal panels are capped at ~1/6 (<20%) of
# the main box on their short axis.

cat("STEP 3: Panel b\n")

x_range_b <- range(scatter_df$gdp_cagr, na.rm = TRUE)
y_range_b <- range(scatter_df$mat_cagr, na.rm = TRUE)
pct_abs_decouple <- mean(scatter_df$abs_decouple) * 100

# "Below trend" label: at the 80th GDP-growth percentile, 2.5 residual SEs under the regression line
fit_b <- lm(mat_cagr ~ gdp_cagr, data = scatter_df)
# Slope label "a:1" = pp of material growth per 1 pp of GDP growth, 2 significant digits,
# placed along the line at 80% of the x range and rotated to its on-screen angle
# (data slope x x-range/y-range x main-box height/width, PANEL_B_ASPECT)
PANEL_B_ASPECT <- 1.08 # rendered height / width of panel b's main box (18 x 18 cm figure)
slope_lab_b <- paste0(signif(coef(fit_b)[[2]], 2), ":1")
x_slope_b <- x_range_b[1] + 0.8 * diff(x_range_b)
y_slope_b <- coef(fit_b)[[1]] + coef(fit_b)[[2]] * x_slope_b
angle_slope_b <- atan(coef(fit_b)[[2]] * diff(x_range_b) / diff(y_range_b) * PANEL_B_ASPECT) * 180 / pi
x_lab_sel <- quantile(scatter_df$gdp_cagr, 0.8, names = FALSE)
y_lab_sel <- max(
  y_range_b[1] + 0.12 * diff(y_range_b),
  coef(fit_b)[[1]] + coef(fit_b)[[2]] * x_lab_sel - 2.5 * scatter_df$resid_sigma[1]
)

p_b_main <- ggplot(scatter_df |> arrange(abs_decouple), aes(x = gdp_cagr, y = mat_cagr)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(colour = abs_decouple), size = 0.5, alpha = 0.65) +
  # "Selected" overlay -- SAME point size as the base cloud (size = 0.5); the
  # black outline alone marks the subset, it doesn't also enlarge the point.
  geom_point(
    data = scatter_df |> dplyr::filter(selected),
    aes(fill = abs_decouple), shape = 21, colour = "black", stroke = 0.3, size = 0.5
  ) +
  # Material-vs-GDP growth regression line; "selected" = runs below it by > se_mult x residual SE
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.5) +
  annotate(
    "text",
    x = x_slope_b, y = y_slope_b, label = slope_lab_b, angle = angle_slope_b,
    hjust = 0.5, vjust = -0.5, fontface = "italic", colour = "black", size = pb_annot_size("largeFont", 6.5)
  ) +
  scale_colour_manual(values = c(`TRUE` = ABS_GREEN, `FALSE` = GREY_PT), guide = "none") +
  scale_fill_manual(values = c(`TRUE` = ABS_GREEN, `FALSE` = GREY_PT), guide = "none") +
  coord_cartesian(xlim = x_range_b, ylim = y_range_b, expand = FALSE, clip = "off") +
  scale_x_continuous(
    breaks = function(lims) {
      step <- 0.005
      idx_lo <- ceiling(lims[1] / step - 1e-9)
      idx_hi <- floor(lims[2] / step + 1e-9)
      (idx_lo:idx_hi) * step
    },
    # Whole-percent labels only (e.g. "2%"); half-percent breaks (e.g. 2.5%)
    # keep their tick mark but get no text.
    labels = function(x) {
      pct <- round(x * 100, 6)
      ifelse(abs(pct - round(pct)) < 1e-6, paste0(round(pct), "%"), "")
    }
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "GDP annual growth 2025-2060", y = "Material consumption\nannual growth 2025-2060") +
  # Manual "b" tag, INSIDE this panel's own top-left corner -- the outer
  # auto-tag sequence skips this slot (see STEP 6) because tagging the whole
  # wrap_elements() composite put the letter near the density strip, not
  # "inside the box plot" the way the other 3 panels' auto tags land.
  annotate(
    "text",
    x = x_range_b[1],
    y = y_range_b[2],
    label = "b",
    fontface = "bold",
    colour = "black",
    hjust = -0.3,
    vjust = 1.3,
    size = pb_annot_size("largeFont", 10)
  ) +
  # "Absolute decoupling" callout, anchored at X = 2% (GDP growth).
  annotate(
    "text",
    x = 0.02,
    y = y_range_b[1] + 0.08 * diff(y_range_b),
    label = "Absolute\ndecoupling",
    colour = ABS_GREEN,
    fontface = "bold",
    hjust = 1,
    vjust = 0,
    lineheight = 0.85,
    size = pb_annot_size("largeFont", 7)
  ) +
  # "Selected" callout -- placed below the regression line, under the flagged runs
  annotate(
    "text",
    x = x_lab_sel,
    y = y_lab_sel,
    label = paste0("Below trend\n(> ", scatter_df$se_mult[1], " SE)"),
    colour = "black",
    fontface = "bold",
    hjust = 0.5,
    vjust = 1,
    lineheight = 0.85,
    size = pb_annot_size("largeFont", 6)
  ) +
  theme_pb_large() +
  theme(plot.margin = margin(t = -2, r = -2, b = 4, l = 4, unit = "pt"))

p_b_top <- ggplot(scatter_df, aes(x = gdp_cagr)) +
  geom_density(colour = "grey40", fill = "grey80", alpha = 0.5, linewidth = 0.3) +
  geom_vline(xintercept = 0.02, linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  annotate(
    "text",
    x = 0.02,
    y = Inf,
    label = "2%",
    angle = 90,
    hjust = -0.1,
    vjust = -0.4,
    size = pb_annot_size("largeFont", 6.5),
    colour = "grey20"
  ) +
  coord_cartesian(xlim = x_range_b, expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 2, r = -2, b = -2, l = 4, unit = "pt"))

dens_y <- density(scatter_df$mat_cagr, n = 512, from = y_range_b[1], to = y_range_b[2])
dens_df <- tibble::tibble(x = dens_y$x, y = dens_y$y)
y_at_0 <- approx(dens_y$x, dens_y$y, xout = 0)$y
dens_below <- dens_df |>
  dplyr::filter(x <= 0) |>
  dplyr::bind_rows(tibble::tibble(x = 0, y = y_at_0)) |>
  dplyr::arrange(x)
dens_above <- dens_df |>
  dplyr::filter(x >= 0) |>
  dplyr::bind_rows(tibble::tibble(x = 0, y = y_at_0)) |>
  dplyr::arrange(x)

# Label inside the green region -- share of simulations with abs. decoupling
# -- positioned at the MEDIAN mat_cagr among those runs (guaranteed inside
# the x <= 0 filled area) and half its local density height (inset from the
# curve's edge rather than sitting on it).
x_lab_green <- median(scatter_df$mat_cagr[scatter_df$abs_decouple])
y_lab_green <- approx(dens_below$x, dens_below$y, xout = x_lab_green)$y * 0.5

p_b_right <- ggplot() +
  geom_area(data = dens_below, aes(x = x, y = y), fill = ABS_GREEN, colour = NA, alpha = 0.6) +
  geom_area(data = dens_above, aes(x = x, y = y), fill = GREY_PT, colour = NA, alpha = 0.6) +
  geom_line(data = dens_df, aes(x = x, y = y), colour = "grey30", linewidth = 0.35) +
  annotate(
    "text",
    x = x_lab_green,
    y = y_lab_green,
    label = paste0(round(pct_abs_decouple), "%"),
    colour = "white",
    fontface = "bold",
    size = pb_annot_size("largeFont", 6.5)
  ) +
  coord_flip(xlim = y_range_b, expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = -2, r = 2, b = 4, l = -2, unit = "pt"))

# Built via wrap_plots(design = ...) instead of nested (A + B) / (C + D) +
# plot_layout(widths =, heights =) -- the nested "/"+"+" form was NOT
# respecting the requested widths/heights (rendered far larger than
# specified across two separate attempts, relative AND absolute units alike)
# -- wrap_plots() with an explicit design string is patchwork's own
# documented pattern for asymmetric grids and reliably respects the ratio.
p_b_grid <- patchwork::wrap_plots(
  p_b_top,
  patchwork::plot_spacer(),
  p_b_main,
  p_b_right,
  design = "AB\nCD",
  widths = c(1, 0.12), # marginal strip = 12% of the main box (well under the requested <20%)
  heights = c(0.12, 1)
)

# wrap_elements() turns the whole composite into ONE opaque tagged unit --
# without it, patchwork's tag_levels recurses into p_b_top/right and steals
# letters "b"/"d" for the marginal density panels instead of leaving them
# for the outer a/b/c/d sequence.
p_b <- patchwork::wrap_elements(full = p_b_grid) +
  patchwork::plot_annotation(
    title = "Material & GDP coupling",
    theme = theme(plot.title = element_text(size = pb_annot_size("largeFont", 10), face = "bold"))
  )


# STEP 4: Panel c -- lever effects on growth rate (point + 95% CI) ----------------
# Row labels sit inline at x = 0 (left of the row when its effect is
# positive, right when negative); group headers (name + delta, when uniform
# across the group) sit along the panel's RIGHT edge, one per lever group,
# in the extra vertical space GROUP_GAP_EXTRA inserted above that group's
# top row (STEP 1).

cat("STEP 4: Panel c\n")

# Row label side is decided per GROUP (not per row): a group's rows should
# all read from the same side, even if one row's own effect happens to flip
# sign relative to its group-mates (e.g. "Short-lived" within Lifetime).
effects_df <- effects_df |>
  dplyr::group_by(group_key) |>
  dplyr::mutate(group_sign = sign(mean(effect_pp)), hjust_lab = if_else(group_sign >= 0, 1.15, -0.15)) |>
  dplyr::ungroup() |>
  # Central-estimate label printed just outside the panel's right edge (e.g. "+0.6%")
  dplyr::mutate(effect_lab = if_else(abs(effect_pp) < 0.05, sprintf("%+.2f%%", effect_pp), sprintf("%+.1f%%", effect_pp)))

group_top <- effects_df |>
  dplyr::group_by(group_key) |>
  dplyr::summarise(
    pos_top = max(pos),
    group_label = dplyr::first(group_label),
    group_uniform = dplyr::first(group_uniform),
    delta_label = dplyr::first(delta_label),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    header_text = if_else(group_uniform, paste0(group_label, ": ", delta_label), group_label),
    header_y = pos_top + 0.35
  ) |>
  dplyr::arrange(dplyr::desc(pos_top))

boundary_y <- group_top$pos_top[-1] + (1 + GROUP_GAP_EXTRA) / 2 # midway through each group's gap, all except the topmost

# y = `pos` (numeric row position, not the display_label TEXT): several rows
# share a bare name across different lever groups (see STEP 1 note), so the
# axis is plotted numerically and the row's own name is drawn as a separate
# text layer rather than doubling as the plotting key.
p_c <- ggplot(effects_df, aes(y = pos, x = effect_pp)) +
  geom_hline(
    data = tibble::tibble(y = boundary_y),
    aes(yintercept = y),
    colour = "grey70",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "grey20") +
  geom_segment(
    aes(x = ci_lo_pp, xend = ci_hi_pp, yend = pos, colour = group_key, alpha = significant),
    linewidth = 0.8
  ) +
  geom_point(aes(colour = group_key, alpha = significant), size = 1.5) +
  geom_text(
    aes(x = 0, label = display_label, hjust = hjust_lab),
    size = pb_annot_size("largeFont", 6.5), colour = "grey15", fontface = "bold"
  ) +
  geom_text(
    data = group_top, aes(x = Inf, y = header_y, label = header_text, colour = group_key),
    inherit.aes = FALSE, hjust = 1.02, size = pb_annot_size("largeFont", 6.5), fontface = "italic"
  ) +
  geom_text(
    aes(x = Inf, label = effect_lab, colour = group_key),
    hjust = -0.15, size = pb_annot_size("largeFont", 6.5), fontface = "plain"
  ) +
  scale_colour_manual(values = group_pal, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 0.95, `FALSE` = 0.4), guide = "none") +
  scale_x_continuous(
    breaks = function(lims) {
      step <- 0.5
      idx_lo <- ceiling(lims[1] / step - 1e-9)
      idx_hi <- floor(lims[2] / step + 1e-9)
      (idx_lo:idx_hi) * step
    },
    # Whole-percent breaks get no decimal ("+1%"); half-point breaks keep one ("+0.5%").
    labels = function(x) ifelse(abs(x - round(x)) < 1e-6, sprintf("%+.0f%%", x), sprintf("%+.1f%%", x)),
    expand = expansion(mult = 0.18)
  ) +
  scale_y_continuous(limits = c(pos_range[1] - 0.6, pos_range[2] + 0.6), expand = c(0, 0), breaks = NULL) +
  coord_cartesian(clip = "off") +
  labs(title = "Lever effects on growth rate (95% CI)", x = "Effect on material growth rate", y = NULL) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(t = 4, r = 40, b = 4, l = 16, unit = "pt"), # room for the outside effect labels
    plot.tag.location = "panel",
    plot.tag.position = "topright"
  )


# STEP 5: Panel d -- 2060 per-capita consumption densities, one row per material ----
# Each row: count-scaled densities of FORECAST_END world primary consumption
# per capita for the Low / High group of that material's 2060 grouping metric
# (PrepareData DENSITY_GROUPS), in a light tint / dark shade of the material's
# PALETTE_MATERIAL_GROUPS colour (same recipe as "Figure 4 - VariableImportance.R").
# Dashed lines: 2025 per-capita level held flat (0%/yr) or grown +2.5%/yr to 2060.

cat("STEP 5: Panel d\n")

MAT_LEVELS <- c("All materials", "Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")

# "All materials" row: one group per dominant SSP, project SSP colours
dens_pal <- c()
for (s in names(SSP_COLORS)) {
  dens_pal[paste("All materials", s)] <- SSP_COLORS[[s]]
}
# Material rows: Low = +60% toward white, High = x0.55 darker, per material
for (m in MAT_LEVELS[-1]) {
  base_rgb <- grDevices::col2rgb(PALETTE_MATERIAL_GROUPS[[m]]) / 255
  light_rgb <- pmin(1, base_rgb + (1 - base_rgb) * 0.6)
  dens_pal[paste(m, "Low")] <- grDevices::rgb(light_rgb[1], light_rgb[2], light_rgb[3])
  dens_pal[paste(m, "High")] <- grDevices::rgb(base_rgb[1] * 0.55, base_rgb[2] * 0.55, base_rgb[3] * 0.55)
}

dens_grp_df <- density_df |>
  dplyr::filter(!is.na(grp)) |>
  dplyr::group_by(material, grp) |>
  dplyr::filter(dplyr::n() >= 5) |> # density needs a handful of runs
  dplyr::ungroup() |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS), grp_key = paste(material, grp))

# Direct group labels at each group's median, Low on the first line and High on the second
dens_labels <- dens_grp_df |>
  dplyr::group_by(material, grp, grp_key) |>
  dplyr::summarise(x = median(percap_t), n = dplyr::n(), .groups = "drop") |>
  dplyr::left_join(density_lines |> dplyr::select(material, unit, lo, hi, rate_label, low_rate_min, high_rate_max), by = "material") |>
  dplyr::group_by(material) |>
  dplyr::mutate(
    label = dplyr::case_when(
      material == "All materials" ~ grp, # SSP name
      grp == "Low" & !is.na(rate_label) ~ paste0("<", lo, " ", unit, ", >", round(100 * low_rate_min), "% ", rate_label),
      grp == "High" & !is.na(rate_label) ~ paste0(">", hi, " ", unit, ", <", round(100 * high_rate_max), "% ", rate_label),
      grp == "Low" ~ paste0("<", lo, " ", unit),
      TRUE ~ paste0(">", hi, " ", unit)
    ),
    # Alternate two label lines (Low/High; SSPs ordered by their median) so neighbours don't collide
    vjust = if_else(dplyr::row_number(x) %% 2 == 1, 1.3, 2.6)
  ) |>
  dplyr::ungroup()

dens_vlines <- density_lines |>
  dplyr::select(material, v0, v25) |>
  tidyr::pivot_longer(c(v0, v25), names_to = "line", values_to = "x") |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS), label = if_else(line == "v0", "0%/yr", "+2.5%/yr"))

p_d <- ggplot(dens_grp_df, aes(x = percap_t)) +
  geom_density(aes(y = after_stat(count), fill = grp_key, colour = grp_key, group = grp_key), alpha = 0.35, linewidth = 0.45) +
  geom_vline(data = dens_vlines, aes(xintercept = x), linetype = "dashed", colour = "grey40", linewidth = 0.35) +
  # Growth-line labels on the top row only
  geom_text(
    data = dens_vlines |> dplyr::filter(material == MAT_LEVELS[1]),
    aes(x = x, y = 0, label = label), angle = 90, hjust = -0.1, vjust = -0.4,
    colour = "grey40", size = pb_annot_size("largeFont", 6)
  ) +
  geom_text(
    data = dens_labels, aes(x = x, y = Inf, label = label, colour = grp_key, vjust = vjust),
    fontface = "bold", size = pb_annot_size("largeFont", 6)
  ) +
  scale_fill_manual(values = dens_pal, guide = "none") +
  scale_colour_manual(values = dens_pal, guide = "none") +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, drop0trailing = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) + # headroom for the two label lines
  facet_wrap(~material, ncol = 1, scales = "free") +
  coord_cartesian(clip = "off") +
  labs(title = "Material consumption per capita, 2060", x = "Primary material consumption (t/person)", y = NULL) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 2, unit = "pt")
  )


# STEP 6: Combine panels & save -----------------------------------------------------

cat("STEP 6: Combine & save\n")

# tag_levels is an explicit c("a", "", "c", "d") vector, not "a": panel b's
# own "b" is placed manually (STEP 3, inside p_b_main -- the auto-tag would
# have landed on the wrap_elements() composite's outer corner, near the
# density strip, not "inside the box plot"), so its slot in the sequence is
# blank to keep c/d correctly lettered. plot.tag.location is NOT set here
# (each panel opts into "panel" placement individually) so panel d keeps
# ggplot2's own default ("plot" -- tag just outside the panel).
fig <- (p_a | p_b) /
  (p_c | p_d) +
  patchwork::plot_layout(heights = c(1, 1.3)) +
  # list() = custom tag sequence; a bare character vector is read as nested
  # tag LEVEL types (only "a" used), which auto-tagged panel b a second time
  patchwork::plot_annotation(tag_levels = list(c("a", "", "c", "d"))) &
  theme(plot.tag = element_text(face = "bold"))

ggsave("Figures/Fig5 - Sensitivity.png", fig, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity.svg", fig, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity.svg") # cleans text-length attrs + groups into Grid/Data/Labels Inkscape layers

cat("  Saved: Figures/Fig5 - Sensitivity.png, Figures/SVG/Fig5 - Sensitivity.svg\n\n")
cat("=== Figure 4 v2 done ===\n")

# EoF
