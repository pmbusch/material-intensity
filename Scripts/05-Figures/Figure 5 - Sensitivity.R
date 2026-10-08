## =============================================================================
## Figure 5 - Sensitivity.R  -> Figures/Fig5 - Sensitivity_alt1.png (alternative;
##                              main Fig5 = "Figure 5 - Alt1.R")
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
##    band), points coloured by dominant SSP (direct labels).
##    Marginal density panels (<=~17% of the main box on their short axis):
##    X margin marks the 2% GDP growth reference line; Y margin is a single
##    grey density curve.
## c) Lever effects on growth rate: regression coefficient x a fixed,
##    physically meaningful delta per lever (e.g. "+1pp population growth",
##    "+20yr lifetime") = pp effect on growth, shown as a point + 95% CI.
##    Lever names sit inline near x = 0; lever groups (M/G, S/G, lifetime,
##    ...) get extra row spacing plus one header each (group name + delta,
##    when uniform across the group), placed along the panel's right edge.
## d) One row per material category: densities of 2060 world primary
##    consumption per capita for a Low / High group of that material's 2060
##    metric (biomass t/cap, fossil MJ/$, metal and mineral stock kg/$), with
##    0% and +2%/yr growth-from-2025 reference lines.
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
BAR_WIDTH_A <- 0.7 # bar thickness; the white gaps between bars also hold legend labels

# Label slots: two rows above the top bar, then the white gaps between bars.
# Each slot is paired with an adjacent bar; a label placed there is centred on
# its OWN segment in that bar (not its cross-bin average), so it sits directly
# over/under the segment it names. Greedy placement, most important label
# first: each takes the first (slot, bar) candidate where its estimated text
# extent (pct units, ~2% per character at 6.5 pt bold) does not overlap a label
# already placed in that slot; if none is free, the candidate with the least overlap.
TOP_Y_A <- c(4 + BAR_WIDTH_A / 2 + 0.2, 4 + BAR_WIDTH_A / 2 + 0.47)
SLOT_CAND_A <- tibble::tibble(
  y = c(TOP_Y_A[1], TOP_Y_A[2], 3.5, 3.5, 2.5, 2.5, 1.5, 1.5),
  bin = c(">2%", ">2%", ">2%", "1-2%", "1-2%", "0-1%", "0-1%", "<0%")
)
CHAR_PCT_A <- 2.0
GAP_PCT_A <- 1 # minimum gap between neighbouring labels in a slot
legend_df_a <- seg_pos_a |>
  dplyr::left_join(plot_df_a |> dplyr::group_by(display_label) |> dplyr::summarise(imp = sum(pct), .groups = "drop"), by = "display_label") |>
  dplyr::mutate(half_w = nchar(as.character(display_label)) * CHAR_PCT_A / 2, y = NA_real_) |>
  dplyr::arrange(dplyr::desc(imp))
placed_a <- tibble::tibble(y = TOP_Y_A[2], lo = -Inf, hi = 4) # top row starts after the "a" tag
for (i in seq_len(nrow(legend_df_a))) {
  lbl <- legend_df_a$display_label[i]
  hw <- legend_df_a$half_w[i]
  best_k <- NA_integer_
  best_ov <- Inf
  best_x <- NA_real_
  for (k in seq_len(nrow(SLOT_CAND_A))) {
    x_k <- stack_pos_a$mid_x[stack_pos_a$display_label == lbl & stack_pos_a$growth_bin == SLOT_CAND_A$bin[k]]
    x_k <- min(max(x_k, hw), 100 - hw) # keep within 0-100%
    in_slot <- placed_a[placed_a$y == SLOT_CAND_A$y[k], ]
    ov <- sum(pmax(0, pmin(in_slot$hi, x_k + hw) - pmax(in_slot$lo, x_k - hw) + GAP_PCT_A))
    if (ov < best_ov) {
      best_ov <- ov
      best_k <- k
      best_x <- x_k
    }
    if (ov == 0) break
  }
  legend_df_a$x[i] <- best_x
  legend_df_a$y[i] <- SLOT_CAND_A$y[best_k]
  placed_a <- dplyr::bind_rows(placed_a, tibble::tibble(y = SLOT_CAND_A$y[best_k], lo = best_x - hw, hi = best_x + hw))
}

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
inbar_df_a$angle <- ifelse(inbar_df_a$pct < 5, 90, 0) # narrow segments: vertical label

p_a <- ggplot(plot_df_a, aes(x = pct, y = growth_bin, fill = display_label)) +
  geom_col(position = "stack", colour = "black", linewidth = 0.15, width = BAR_WIDTH_A) +
  geom_text(
    data = inbar_df_a, aes(x = mid_x, y = growth_bin, label = value_label, colour = text_col, angle = angle),
    inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("largeFont", 6)
  ) +
  geom_text(
    data = legend_df_a, aes(x = x, y = y, label = display_label, colour = fill_hex),
    inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("largeFont", 6.5)
  ) +
  scale_fill_manual(values = fill_vals_a, name = NULL, guide = "none") +
  scale_colour_identity() +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  # no expansion: x = 0-100%, y from the bottom bar's edge to just above the two label rows
  coord_cartesian(xlim = c(0, 100), ylim = c(1 - BAR_WIDTH_A / 2, TOP_Y_A[2] + 0.2), expand = FALSE, clip = "off") +
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

# Points coloured by the run's dominant SSP, drawn in shuffled order so no SSP
# systematically overplots another; direct SSP labels at each SSP's median point
set.seed(JITTER_SEED)
scatter_df <- scatter_df[sample(nrow(scatter_df)), ]
# Label above each SSP's cloud: x = median GDP growth, y = 95th pct of material growth
ssp_lab_b <- scatter_df |>
  dplyr::group_by(ssp) |>
  dplyr::summarise(x = median(gdp_cagr), y = quantile(mat_cagr, 0.95), .groups = "drop") |>
  dplyr::mutate(x = pmin(pmax(x, x_range_b[1] + 0.06 * diff(x_range_b)), x_range_b[2] - 0.06 * diff(x_range_b))) |> # keep inside the box
  # Manual nudges: SSP1-3 labels up, SSP5 label left
  dplyr::mutate(
    y = if_else(ssp %in% c("SSP1", "SSP2", "SSP3"), y + 0.05 * diff(y_range_b), y),
    x = if_else(ssp == "SSP5", x - 0.04 * diff(x_range_b), x)
  )

p_b_main <- ggplot(scatter_df, aes(x = gdp_cagr, y = mat_cagr)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(colour = ssp), size = 0.5, alpha = 0.65) +
  geom_text(
    data = ssp_lab_b, aes(x = x, y = y, label = ssp, colour = ssp),
    vjust = -0.3, fontface = "bold", size = pb_annot_size("largeFont", 7)
  ) +
  # Material-vs-GDP growth regression line; "selected" = runs below it by > se_mult x residual SE
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.5) +
  annotate(
    "text",
    x = x_slope_b, y = y_slope_b, label = slope_lab_b, angle = angle_slope_b,
    hjust = 0.5, vjust = -0.5, fontface = "italic", colour = "black", size = pb_annot_size("largeFont", 6.5)
  ) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_fill_manual(values = SSP_COLORS, guide = "none") +
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
  theme_pb_large() +
  theme(plot.margin = margin(t = -2, r = -2, b = 4, l = 4, unit = "pt"))

# Marginal densities: one count-scaled curve per SSP (line only, no fill)
p_b_top <- ggplot(scatter_df, aes(x = gdp_cagr)) +
  geom_density(aes(y = after_stat(count), colour = ssp), fill = NA, linewidth = 0.4, trim = TRUE) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
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
  theme(plot.margin = margin(t = 2, r = -2, b = -5.5, l = 4, unit = "pt"))

# Material-growth margin: 0% and 2%/yr reference lines
p_b_right <- ggplot(scatter_df, aes(x = mat_cagr)) +
  geom_density(aes(y = after_stat(count), colour = ssp), fill = NA, linewidth = 0.4, trim = TRUE) +
  geom_vline(xintercept = c(0, 0.02), linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  annotate(
    "text",
    x = c(0, 0.02), y = Inf, label = c("0%", "2%"),
    hjust = -0.1, vjust = -0.3, size = pb_annot_size("largeFont", 6.5), colour = "grey20"
  ) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  coord_flip(xlim = y_range_b, expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = -2, r = 2, b = 4, l = -5.5, unit = "pt"))

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
# Title set on the wrapped element itself (a nested plot_annotation() title is dropped)
p_b <- patchwork::wrap_elements(panel = p_b_grid) + # panel = leaves the title row free
  labs(title = "Material & GDP coupling") +
  theme(plot.title = theme_pb_large()$plot.title)


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
  # Manual nudges: long left-side names shifted right (clear the panel edge);
  # Wood placed right of its (wide) CI
  dplyr::mutate(
    lab_x = if_else(row_label == "Wood", ci_hi_pp, 0),
    hjust_lab = dplyr::case_when(
      row_label == "Wood" ~ -0.1,
      row_label %in% c("Grazed biomass", "Infrastructure") & hjust_lab > 1 ~ 1.02,
      TRUE ~ hjust_lab
    )
  ) |>
  # Central-estimate label printed just outside the panel's right edge (e.g. "+0.6%")
  dplyr::mutate(effect_lab = if_else(abs(effect_pp) < 0.05, sprintf("%+.2f%%", effect_pp), sprintf("%+.1f%%", effect_pp)))

group_top <- effects_df |>
  dplyr::group_by(group_key) |>
  dplyr::summarise(
    pos_top = max(pos),
    pos_bottom = min(pos),
    ci_lo_top = ci_lo_pp[which.max(pos)],
    group_label = dplyr::first(group_label),
    group_uniform = dplyr::first(group_uniform),
    delta_label = dplyr::first(delta_label),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    header_text = if_else(group_uniform, paste0(group_label, ": ", delta_label), group_label),
    # Default: right edge, just above the group's top row. Growth (single row):
    # on its row, left of the point; M/G: below the group's bottom row
    header_y = dplyr::case_when(
      group_key == "growth" ~ pos_top,
      group_key == "mg" ~ pos_bottom,
      TRUE ~ pos_top + 0.35
    ),
    header_x = if_else(group_key == "growth", ci_lo_top - 0.03, Inf),
    header_hjust = if_else(group_key == "growth", 1, 1.02)
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
    aes(x = lab_x, label = display_label, hjust = hjust_lab),
    size = pb_annot_size("largeFont", 6.5), colour = "grey15", fontface = "bold"
  ) +
  geom_text(
    data = group_top, aes(x = header_x, y = header_y, label = header_text, colour = group_key, hjust = header_hjust),
    inherit.aes = FALSE, size = pb_annot_size("largeFont", 6.5), fontface = "italic"
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
    expand = expansion(mult = c(0.25, 0.18)) # extra left room for long row names
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
# Dashed lines: 2025 per-capita level held flat (0%/yr) or grown +2%/yr to 2060.

cat("STEP 5: Panel d\n")

MAT_LEVELS <- c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
GROWTH_REF <- 0.02 # upper reference line: 2025 level grown at 2%/yr to FORECAST_END

# Material rows: Low = +60% toward white, High = x0.55 darker, per material
dens_pal <- c()
for (m in MAT_LEVELS) {
  base_rgb <- grDevices::col2rgb(PALETTE_MATERIAL_GROUPS[[m]]) / 255
  light_rgb <- pmin(1, base_rgb + (1 - base_rgb) * 0.6)
  dens_pal[paste(m, "Low")] <- grDevices::rgb(light_rgb[1], light_rgb[2], light_rgb[3])
  dens_pal[paste(m, "High")] <- grDevices::rgb(base_rgb[1] * 0.55, base_rgb[2] * 0.55, base_rgb[3] * 0.55)
}

# Runs per Low / High / unclassified group, per material (of all MC runs)
dens_counts <- density_df |>
  dplyr::filter(material %in% MAT_LEVELS) |>
  dplyr::group_by(material) |>
  dplyr::summarise(n_low = sum(grp == "Low", na.rm = TRUE), n_high = sum(grp == "High", na.rm = TRUE), n_neither = sum(is.na(grp)), n_total = dplyr::n(), .groups = "drop")
print(dens_counts)

dens_grp_df <- density_df |>
  dplyr::filter(!is.na(grp), material %in% MAT_LEVELS) |>
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
  dplyr::filter(material %in% MAT_LEVELS) |>
  dplyr::mutate(v2 = v0 * (1 + GROWTH_REF)^(FORECAST_END - 2025L)) |>
  dplyr::select(material, v0, v2) |>
  tidyr::pivot_longer(c(v0, v2), names_to = "line", values_to = "x") |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS), label = if_else(line == "v0", "0%/yr", paste0("+", GROWTH_REF * 100, "%/yr")))

p_d <- ggplot(dens_grp_df, aes(x = percap_t)) +
  geom_density(aes(y = after_stat(count), colour = grp_key, group = grp_key), fill = NA, linewidth = 0.45) +
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
    strip.text = element_text(face = "bold", hjust = 0, margin = margin(b = 2, l = 12, unit = "pt")), # l: room for the "d" tag
    plot.margin = margin(t = 4, r = 4, b = 4, l = 2, unit = "pt"),
    plot.tag.location = "panel",
    plot.tag.position = c(0.012, 1.028) # level with the first strip, just above the first box
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

# Alternative version (main Fig5 = "Figure 5 - Alt1.R", panel c = drivers of growth)
ggsave("Figures/Fig5 - Sensitivity_alt1.png", fig, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity_alt1.svg", fig, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity_alt1.svg") # cleans text-length attrs + groups into Grid/Data/Labels Inkscape layers

cat("  Saved: Figures/Fig5 - Sensitivity_alt1.png, Figures/SVG/Fig5 - Sensitivity_alt1.svg\n\n")
cat("=== Figure 4 v2 done ===\n")

# EoF
