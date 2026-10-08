## Figure 4 - Climate.R  -> Figures/Fig4 - Climate.png (MAIN Figure 4)
##                         + Figures/Supporting-Figures/S23_Climate_Percentiles.png
## Runs assigned to their SSP label (closest world GDP/cap growth); SSP4 is not sampled by the MC (SSP_SAMPLED).
##   Fig4 - Climate, one column, three panels:
##     a) material vs. GDP growth (Figure5_Scatter.csv), margins: material density
##        (top), GDP histogram (right)
##     b) cumulative material use vs. 2060 warming: scatter + 33%/66% HDR contours,
##        each run at the IDW temperature of its nearest ScenarioMIP scenarios
##        (Figure 4 - Climate - PrepareData.R); temperature margin (right)
##     c) cumulative material composition per SSP
##   S23_Climate_Percentiles -- panel b's scatter at the 33rd / 67th percentile warming
## Dashed lines: 1.5 °C / 2 °C (Paris), and FIG_START annual use held constant.
## Deprecated Ridge / Histogram / Even versions: Scripts/05-Figures/Deprecated/.

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

pb_set_geom_defaults("small")

cat("=== Figure 6 - Climate ===\n\n")

# Parameters ------------------------------------------------------------

FIG_START <- 2025L
FIG_END <- FORECAST_END # 2060 (Scripts/00-CommonParameters.R)
N_YEARS <- FIG_END - FIG_START + 1L # 36 yr, both ends inclusive
PARIS_LEVELS <- c(1.5, 2.0) # °C above 1850-1900
RIDGE_HEIGHT <- 0.12 # °C, peak height of the tallest ridge/bar (low, so close SSPs stay readable)
RIDGE_ALPHA <- 0.6
N_BINS <- 50 # histogram version, approximate (pretty breaks)
GROWTH_RATE <- 0.02 # reference line: FIG_START annual use growing 2%/yr

CLIMATE_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-0e7dfab0-46ee-486b-b50d-4faf3de27da4.csv"
TEMP_VARS <- c(
  "Climate Assessment|Surface Temperature (GSAT)|33rd Percentile [MAGICC v7.6.0a3]" = "t_lo",
  "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICC v7.6.0a3]" = "t_med",
  "Climate Assessment|Surface Temperature (GSAT)|67th Percentile [MAGICC v7.6.0a3]" = "t_hi"
)
TEMP_MEDIAN_VAR <- "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICC v7.6.0a3]"
TEMP_TITLE <- paste0("Temperature change ", FIG_END, " (vs. 1850–1900)")
LABEL_RIGHT <- "SSP5" # SSP label placed right of its ridge (avoids overlap with SSP3)

# Right-margin bars: full 2060 median-temperature range across each SSP's scenarios
RANGE_STEP <- 0.025 # spacing between bars, share of the x range
RANGE_MARGIN <- 26 # pt, right plot margin that holds the bars

# Mapped version
MAP_ALPHA <- 0.12 # point transparency
HDR_PROBS <- c(0.66, 0.33) # contours enclosing 66% / 33% of the density
MAP_BW <- c(1500, 0.25) # 2D-density bandwidths (Gt, °C), MASS::kde2d convention
MAT_SHORT <- c("Non-metallic minerals" = "Minerals", "Metal ores" = "Metals", "Fossil fuels" = "Fossil", "Biomass" = "Biomass")

# IIASA marker run per SSP; SSP2 has four markers -> "Medium" (middle-of-the-road forcing)
SSP_MARKERS <- c(
  "SSP1" = "Very Low - SSP1 (Marker)",
  "SSP2" = "Medium - SSP2 (Marker)",
  "SSP3" = "High - SSP3 (Marker)",
  "SSP5" = "High-to-Low - SSP5 (Marker)"
)

MATERIAL_MAP <- c(
  "biomass" = "Biomass", "fossil_fuels" = "Fossil fuels", "metal_fe" = "Metal ores",
  "metal_nonfe" = "Metal ores", "nonmetallic_minerals" = "Non-metallic minerals"
)
# Stacking order, bottom -> top
MAT_LEVELS <- c("Non-metallic minerals", "Metal ores", "Fossil fuels", "Biomass")

# Load data ---------------------------------------------------------------

# Annual world primary consumption per run, year and material group (summed over regions)
mc_annual <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year >= FIG_START, year <= FIG_END) |>
  dplyr::group_by(run_id, year, material_group, ssp = ssp_label) |> # SSP label: closest world GDP/cap growth
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::collect() |>
  dplyr::mutate(material = unname(MATERIAL_MAP[material_group]))
stopifnot(!anyNA(mc_annual$material))

clim_raw <- readr::read_csv(CLIMATE_FILE, col_types = readr::cols(.default = "c")) |>
  dplyr::distinct()

# 2060 temperature percentiles of each SSP's marker run
temp_ssp <- clim_raw |>
  dplyr::filter(scenario %in% SSP_MARKERS, variable %in% names(TEMP_VARS)) |>
  dplyr::transmute(
    ssp = names(SSP_MARKERS)[match(scenario, SSP_MARKERS)],
    stat = unname(TEMP_VARS[variable]),
    value = as.numeric(.data[[as.character(FIG_END)]])
  ) |>
  tidyr::pivot_wider(names_from = stat, values_from = value)
stopifnot(nrow(temp_ssp) == length(SSP_MARKERS))

# Full range of the 2060 median GSAT across all of each SSP's emissions scenarios
temp_range <- clim_raw |>
  dplyr::filter(variable == TEMP_MEDIAN_VAR) |>
  dplyr::transmute(ssp = stringr::str_extract(scenario, "SSP[1-5]"), t = as.numeric(.data[[as.character(FIG_END)]])) |>
  dplyr::filter(ssp %in% SSP_SAMPLED) |>
  dplyr::group_by(ssp) |>
  dplyr::summarise(t_min = min(t), t_max = max(t), .groups = "drop") |>
  dplyr::mutate(x_idx = match(ssp, SSP_SAMPLED))

# Each run's 2060 median GSAT, IDW of its nearest ScenarioMIP scenarios (Figure 4 - Climate - PrepareData.R)
run_map <- readr::read_csv("Parameters/Intermediate/Figure4_RunScenarioMap.csv", show_col_types = FALSE)

# Cumulative use per run --------------------------------------------------

# Cumulative FIG_START-FIG_END use per run and material group (Gt), and each
# group's share of the run total
cum_mat <- mc_annual |>
  dplyr::group_by(run_id, ssp, material) |>
  dplyr::summarise(cum_Gt = sum(mass_Mt) / 1e3, .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(total_Gt = sum(cum_Gt), share = cum_Gt / total_Gt) |>
  dplyr::ungroup()

# Today's use held constant: median FIG_START annual total across runs x N_YEARS
today_annual <- mc_annual |>
  dplyr::filter(year == FIG_START) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(total_Mt = sum(mass_Mt), .groups = "drop")
today_const_Gt <- median(today_annual$total_Mt) * N_YEARS / 1e3

# Same FIG_START level growing GROWTH_RATE per year: geometric sum over N_YEARS
growth_Gt <- median(today_annual$total_Mt) * ((1 + GROWTH_RATE)^N_YEARS - 1) / GROWTH_RATE / 1e3

VLINES <- tibble::tibble(
  x = c(today_const_Gt, growth_Gt),
  label = c("0%/yr growth", paste0("+", GROWTH_RATE * 100, "%/yr growth"))
)

cat(
  "  Runs:", dplyr::n_distinct(cum_mat$run_id), "| today's use held constant:", round(today_const_Gt),
  "Gt | +2%/yr:", round(growth_Gt), "Gt\n"
)
print(temp_ssp)

# Per-run cumulative total (mapped version below)
totals_df <- cum_mat |>
  dplyr::distinct(run_id, ssp, total_Gt) |>
  dplyr::mutate(ssp = factor(ssp, levels = SSP_SAMPLED))

# Mapped version: scatter + 2D density ---------------------------------------

mapped_df <- totals_df |>
  dplyr::mutate(ssp = as.character(ssp)) |>
  dplyr::inner_join(run_map |> dplyr::select(run_id, t_med, t_p33, t_p67), by = "run_id")
stopifnot(nrow(mapped_df) == nrow(totals_df))

X_LIM_MAP <- c(floor(min(mapped_df$total_Gt) / 500) * 500, ceiling((max(mapped_df$total_Gt) + 1300) / 500) * 500) # right room for the inset

# Single reference line: FIG_START use held constant
VLINES_MAP <- VLINES[1, ] |> dplyr::mutate(label = paste0("Fixed ", FIG_START, " consumption"))

# One scatter per run temperature statistic: median (main Fig 4) and the 33rd /
# 67th percentile (SI); the two SI panels share one y range so they compare directly
map_panels <- list()
map_ylim <- list()
for (yv in c("t_med", "t_p33", "t_p67")) {
  map_df_y <- mapped_df |> dplyr::mutate(t_y = .data[[yv]])
  y_pool <- if (yv == "t_med") mapped_df$t_med else c(mapped_df$t_p33, mapped_df$t_p67)

  # Y range: data + range bars, with a small pad (not rounded to 0.25, so the
  # axis ends just above the highest value)
  # (runs only: the scenario range bars are not drawn in this version)
  Y_LIM_MAP <- c(
    floor((min(y_pool) - 0.02) * 20) / 20,
    ceiling((max(y_pool) + 0.02) * 20) / 20
  )
  # ticks and labels every 0.2 °C (one decimal)
  Y_BREAKS_MAP <- seq(ceiling(Y_LIM_MAP[1] * 5) / 5, floor(Y_LIM_MAP[2] * 5) / 5, by = 0.2)
  Y_LABELS_MAP <- paste0("+", sprintf("%.1f", Y_BREAKS_MAP), "°C")
  map_ylim[[yv]] <- Y_LIM_MAP

  # Per-SSP median run position (big mark + label)
  map_median <- map_df_y |>
    dplyr::group_by(ssp) |>
    dplyr::summarise(x = median(total_Gt), y = median(t_y), .groups = "drop")

  # Contour key (empty upper-right area), drawn as one "fried egg" like the data:
  # dot = median run, inner/outer circle = 33%/66% highest-density contour,
  # each with a thin leader line to its label. Sizes in mm, converted to data
  # units with the approximate rendered panel size (67 x 70 mm)
  # (main Fig 4, t_med: ~55 x 60 mm panel, key in the empty lower-right corner)
  MM_X <- diff(X_LIM_MAP) / dplyr::if_else(yv == "t_med", 54, 67)
  MM_Y <- diff(Y_LIM_MAP) / dplyr::if_else(yv == "t_med", 54, 70)
  KEY_X0 <- X_LIM_MAP[1] + dplyr::if_else(yv == "t_med", 0.57, 0.68) * diff(X_LIM_MAP)
  KEY_Y0 <- Y_LIM_MAP[1] + dplyr::if_else(yv == "t_med", 0.2, 0.6) * diff(Y_LIM_MAP)
  KEY_R <- c(inner = 1.8, outer = 3.2) # circle radii, mm
  KEY_ANG <- seq(0, 2 * pi, length.out = 100)
  key_circles <- tibble::tibble(
    ring = rep(names(KEY_R), each = length(KEY_ANG)),
    x = KEY_X0 + rep(KEY_R, each = length(KEY_ANG)) * MM_X * cos(KEY_ANG),
    y = KEY_Y0 + rep(KEY_R, each = length(KEY_ANG)) * MM_Y * sin(KEY_ANG)
  )
  # Leader start (on the egg) and label position, both in mm from the centre
  key_labels <- tibble::tibble(
    label = c("66% of runs", "33% of runs", "Median run"),
    x_from = c(KEY_R[["outer"]] * cos(pi / 4), KEY_R[["inner"]], 0.5),
    y_from = c(KEY_R[["outer"]] * sin(pi / 4), 0, -0.3),
    y_to = c(3, 0, -3)
  ) |>
    dplyr::mutate(
      x = KEY_X0 + x_from * MM_X, y = KEY_Y0 + y_from * MM_Y,
      xend = KEY_X0 + 5 * MM_X, yend = KEY_Y0 + y_to * MM_Y
    )
  # SI panel a (33rd percentile): no contour key, shown once in panel b
  key_alpha <- 1
  if (yv == "t_p33") {
    key_circles <- key_circles[0, ]
    key_labels <- key_labels[0, ]
    key_alpha <- 0
  }

  map_panels[[yv]] <- ggplot(map_df_y, aes(x = total_Gt, y = t_y, colour = ssp)) +
  geom_hline(yintercept = PARIS_LEVELS[PARIS_LEVELS > Y_LIM_MAP[1] & PARIS_LEVELS < Y_LIM_MAP[2]], colour = "grey80", linewidth = 0.3) +
  geomtextpath::geom_textvline(
    data = VLINES_MAP, aes(xintercept = x, label = label), inherit.aes = FALSE,
    linetype = "dashed", colour = "grey55", linewidth = 0.3, hjust = 0.03, vjust = -0.3,
    size = pb_annot_size("small", 7)
  ) +
  geom_point(size = 0.4, alpha = MAP_ALPHA) +
  # Highest-density regions enclosing HDR_PROBS of each SSP's runs; fixed
  # bandwidths, grid spans the whole panel so contours are not cut at the data range
  ggdensity::geom_hdr_lines(
    probs = HDR_PROBS, method = ggdensity::method_kde(h = MAP_BW),
    xlim = X_LIM_MAP, ylim = Y_LIM_MAP, alpha = 1, linewidth = 0.4
  ) +
  geom_point(data = map_median, aes(x = x, y = y), shape = 16, size = 1.8) +
  geom_text(
    data = map_median, aes(x = x, y = y, label = ssp),
    hjust = -0.3, vjust = -0.5, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  geom_path(data = key_circles, aes(x = x, y = y, group = ring), inherit.aes = FALSE, colour = "grey30", linewidth = 0.4) +
  annotate("point", x = KEY_X0, y = KEY_Y0, shape = 16, size = 1.8, colour = "grey30", alpha = key_alpha) +
  geom_segment(
    data = key_labels, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE,
    colour = "grey50", linewidth = 0.2
  ) +
  geom_text(
    data = key_labels, aes(x = xend, y = yend, label = label), inherit.aes = FALSE,
    hjust = -0.08, colour = "grey20", size = pb_annot_size("small", 7)
  ) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_fill_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(breaks = Y_BREAKS_MAP, labels = Y_LABELS_MAP) +
  coord_cartesian(xlim = X_LIM_MAP, ylim = Y_LIM_MAP, expand = FALSE, clip = "off") +
  labs(x = paste0("Cumulative material use ", FIG_START, "–", FIG_END, " (Gt)"), y = TEMP_TITLE) +
  theme_pb_small() +
  theme(plot.margin = margin(5.5, 8, 5.5, 5.5))
}

# Inset: pooled cumulative material composition per SSP (sums to 100%),
# stacked left -> right as Biomass, Fossil, Metals, Minerals
MAT_ORDER_INSET <- rev(MAT_LEVELS)
SSP_ORDER_INSET <- c("SSP5", "SSP3", "SSP2", "SSP1") # top -> bottom

comp_df <- cum_mat |>
  dplyr::group_by(ssp, material) |>
  dplyr::summarise(cum_Gt = sum(cum_Gt), .groups = "drop") |>
  dplyr::group_by(ssp) |>
  dplyr::mutate(share = cum_Gt / sum(cum_Gt)) |>
  dplyr::ungroup() |>
  dplyr::mutate(ssp = factor(ssp, levels = rev(SSP_ORDER_INSET)), material = factor(material, levels = MAT_LEVELS)) |>
  dplyr::arrange(ssp, match(material, MAT_ORDER_INSET)) |>
  dplyr::group_by(ssp) |>
  dplyr::mutate(x_mid = cumsum(share) - share / 2) |>
  dplyr::ungroup()

# In-bar % label colour: black/white by WCAG relative luminance of the fill
fill_hex_comp <- PALETTE_MATERIAL_GROUPS[as.character(comp_df$material)]
rgb_comp <- grDevices::col2rgb(fill_hex_comp) / 255
lin_comp <- ifelse(rgb_comp <= 0.04045, rgb_comp / 12.92, ((rgb_comp + 0.055) / 1.055)^2.4)
lum_comp <- 0.2126 * lin_comp[1, ] + 0.7152 * lin_comp[2, ] + 0.0722 * lin_comp[3, ]
comp_df$text_col <- ifelse(lum_comp < 0.25, "white", "black")

# Material names below the bottom bar in two rows: Biomass/Metals, then Fossil/Minerals
n_ssp_comp <- length(SSP_SAMPLED)
INSET_BAR_W <- 0.85 # bar thickness; box edges sit flush on the outer bars
INSET_ROW_GAP <- 0.6 # vertical step between the two label rows (bar units)
mat_lab_comp <- dplyr::bind_rows(
  comp_df |> dplyr::filter(ssp == SSP_ORDER_INSET[n_ssp_comp], material %in% c("Biomass", "Metal ores")) |> dplyr::mutate(y = 1 - INSET_BAR_W / 2 - 0.2, vj = 1),
  comp_df |> dplyr::filter(ssp == SSP_ORDER_INSET[n_ssp_comp], material %in% c("Fossil fuels", "Non-metallic minerals")) |> dplyr::mutate(y = 1 - INSET_BAR_W / 2 - 0.2 - INSET_ROW_GAP, vj = 1)
) |>
  dplyr::mutate(
    label = MAT_SHORT[as.character(material)],
    # Biomass right-aligned to its segment's end so it clears "Metals"
    hj = dplyr::if_else(material == "Biomass", 1, 0.5),
    x_lab = dplyr::if_else(material == "Biomass", x_mid + share / 2, x_mid)
  )

p_comp <- ggplot(comp_df, aes(x = share, y = ssp, fill = material)) +
  geom_col(width = INSET_BAR_W, colour = "black", linewidth = 0.1) +
  geom_text(
    aes(x = x_mid, label = paste0(round(share * 100), "%"), colour = text_col),
    size = pb_annot_size("small", 7)
  ) +
  geom_text(
    data = mat_lab_comp, aes(x = x_lab, y = y, label = label, colour = PALETTE_MATERIAL_GROUPS[as.character(material)], vjust = vj, hjust = hj),
    inherit.aes = FALSE, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  geom_text(
    data = tibble::tibble(ssp = factor(SSP_ORDER_INSET, levels = rev(SSP_ORDER_INSET))),
    aes(x = 0, y = ssp, label = ssp, colour = SSP_COLORS[as.character(ssp)]),
    inherit.aes = FALSE, hjust = 1.1, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  annotate(
    "text", x = 0, y = n_ssp_comp + INSET_BAR_W / 2 + 0.2, label = "Material use share (%)",
    hjust = 0, vjust = 0, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_colour_identity() +
  coord_cartesian(ylim = c(1 - INSET_BAR_W / 2, n_ssp_comp + INSET_BAR_W / 2), expand = FALSE, clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_pb_small() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
    plot.background = element_rect(fill = NA, colour = NA), plot.margin = margin(9, 3, 18, 24)
  )

# Middle panel (b): median-warming scatter + temperature margin by SSP ----------
# (material composition, the former inset, is panel c below)

p4a_main <- map_panels$t_med +
  annotate("text", x = X_LIM_MAP[1], y = map_ylim$t_med[2], label = "b", fontface = "bold", hjust = -0.4, vjust = 1.3, size = pb_annot_size("small", 10)) +
  labs(title = "Material use & Climate change") +
  theme(plot.margin = margin(t = 10, r = 0, b = 4, l = 4, unit = "pt")) # r = 0: margin flush on the box

# Temperature margin: Paris levels as dashed reference lines. Each SSP's curve
# scaled to its own peak (SSP5's narrow spread would otherwise flatten the
# rest), with a wider bandwidth to smooth the scenario-level spikes
# (only the Paris levels inside the panel's temperature range)
paris_in <- PARIS_LEVELS[PARIS_LEVELS > map_ylim$t_med[1] & PARIS_LEVELS < map_ylim$t_med[2]]
p4a_right <- ggplot(mapped_df, aes(x = t_med)) +
  geom_density(aes(y = after_stat(scaled), colour = ssp), fill = NA, linewidth = 0.4, adjust = 1.8, trim = TRUE) +
  geom_vline(xintercept = paris_in, linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  annotate("text", x = paris_in, y = Inf, label = paste0(sprintf("%.1f", paris_in), "°C"), hjust = -0.1, vjust = -0.3, size = pb_annot_size("small", 7), colour = "grey20") +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  coord_flip(xlim = map_ylim$t_med, ylim = c(0, NA), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 22, b = 4, l = 0, unit = "pt")) # r: room for the °C labels; l = 0: flush on the box


# Top panel (a): material vs. GDP growth (moved from Figure 5) ------------------------
# Same data as "Figure 5 - Sensitivity.R" STEP 3 ("Figure 5 - Sensitivity -
# PrepareData.R"), axes swapped so material is on X as in panel b; top margin only.

scatter_df <- read_csv("Parameters/Intermediate/Figure5_Scatter.csv", show_col_types = FALSE)
JITTER_SEED <- 20260305L # local plotting-only seed (point draw order)
PANEL_B_ASPECT <- 1.0 # rendered height / width of the main box

x_range_b <- c(-0.005, 0.035) # X: material growth, fixed -0.5% to 3.5%
y_range_b <- range(scatter_df$gdp_cagr, na.rm = TRUE) # Y: GDP growth

# Regression of material on GDP growth (slope "a:1" = pp of material growth per
# 1 pp of GDP growth), drawn in the swapped axes; label rotated to its on-screen angle
fit_b <- lm(mat_cagr ~ gdp_cagr, data = scatter_df)
fit_line_b <- tibble::tibble(gdp_cagr = y_range_b) |> dplyr::mutate(mat_cagr = coef(fit_b)[[1]] + coef(fit_b)[[2]] * gdp_cagr)
slope_lab_b <- paste0(signif(coef(fit_b)[[2]], 2), ":1")
y_slope_b <- y_range_b[1] + 0.8 * diff(y_range_b)
x_slope_b <- coef(fit_b)[[1]] + coef(fit_b)[[2]] * y_slope_b
angle_slope_b <- atan(1 / coef(fit_b)[[2]] * diff(x_range_b) / diff(y_range_b) * PANEL_B_ASPECT) * 180 / pi

# Points drawn in shuffled order so no SSP systematically overplots another
set.seed(JITTER_SEED)
scatter_df <- scatter_df[sample(nrow(scatter_df)), ]
# Label left of each SSP's cloud: x = 5th pct of material growth, y = median GDP growth
ssp_lab_b <- scatter_df |>
  dplyr::group_by(ssp) |>
  dplyr::summarise(x = quantile(mat_cagr, 0.05), y = median(gdp_cagr), .groups = "drop") |>
  dplyr::mutate(
    # keep inside the box (labels are right-aligned at x, so x leaves room for the text)
    y = pmin(pmax(y, y_range_b[1] + 0.08 * diff(y_range_b)), y_range_b[2] - 0.04 * diff(y_range_b)),
    x = pmax(x, x_range_b[1] + 0.14 * diff(x_range_b))
  )

p4b_main <- ggplot(scatter_df, aes(x = mat_cagr, y = gdp_cagr)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(colour = ssp), size = 0.4, alpha = 0.65) +
  geom_text(data = ssp_lab_b, aes(x = x, y = y, label = ssp, colour = ssp), hjust = 1.1, fontface = "bold", size = pb_annot_size("small", 7)) +
  geom_line(data = fit_line_b, colour = "black", linewidth = 0.5) +
  annotate(
    "text", x = x_slope_b, y = y_slope_b, label = slope_lab_b, angle = angle_slope_b,
    hjust = 0.5, vjust = -0.5, fontface = "italic", colour = "black", size = pb_annot_size("small", 7)
  ) +
  annotate("text", x = x_range_b[1], y = y_range_b[2], label = "a", fontface = "bold", hjust = -0.4, vjust = 1.3, size = pb_annot_size("small", 10)) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  coord_cartesian(xlim = x_range_b, ylim = y_range_b, expand = FALSE, clip = "off") +
  # Both axes: ticks every 0.5%, labels on whole percents only
  scale_x_continuous(
    breaks = seq(-0.005, 0.035, by = 0.005),
    labels = function(x) {
      pct <- round(x * 100, 6)
      ifelse(abs(pct - round(pct)) < 1e-6, paste0(round(pct), "%"), "")
    }
  ) +
  scale_y_continuous(
    breaks = function(lims) (ceiling(lims[1] / 0.005 - 1e-9):floor(lims[2] / 0.005 + 1e-9)) * 0.005,
    labels = function(x) {
      pct <- round(x * 100, 6)
      ifelse(abs(pct - round(pct)) < 1e-6, paste0(round(pct), "%"), "")
    }
  ) +
  labs(x = paste0("Material consumption annual growth ", FIG_START, "–", FIG_END), y = paste0("GDP annual growth ", FIG_START, "–", FIG_END)) +
  theme_pb_small() +
  theme(plot.margin = margin(t = 0, r = 0, b = 4, l = 4, unit = "pt")) # t, r = 0: margins flush on the box

# Panel a margins: densities per SSP over each SSP's own data range (truncated),
# closed by a vertical drop line at both ends so the cut reads as a bound of the
# runs, not a curve fading out. GDP growth is bounded by construction: each
# run's world GDP/cap growth lies between the lowest- and highest-growth SSP
# (02-RunSimulations.R STEP 4).
marg_a_rows <- list()
for (s in unique(scatter_df$ssp)) {
  for (v in c("mat_cagr", "gdp_cagr")) {
    vals <- scatter_df[[v]][scatter_df$ssp == s]
    d <- stats::density(vals, from = min(vals), to = max(vals), n = 256)
    # material: count-scaled (as before); GDP: each SSP's curve shifted so its
    # lowest point sits on the box edge (truncated, fairly flat densities would
    # otherwise float off the box), then scaled to its own peak
    dens_y <- if (v == "mat_cagr") d$y * length(vals) else (d$y - min(d$y)) / (max(d$y) - min(d$y))
    marg_a_rows[[paste(s, v)]] <- tibble::tibble(ssp = s, var = v, x = d$x, y = dens_y)
  }
}
marg_a <- dplyr::bind_rows(marg_a_rows)
marg_a_ends <- marg_a |>
  dplyr::group_by(ssp, var) |>
  dplyr::filter(x == min(x) | x == max(x)) |>
  dplyr::ungroup()

# Material-growth margin (top): 0% and 2%/yr reference lines
p4b_top <- ggplot(marg_a |> dplyr::filter(var == "mat_cagr"), aes(x = x, y = y, colour = ssp)) +
  geom_line(linewidth = 0.4) +
  geom_segment(data = marg_a_ends |> dplyr::filter(var == "mat_cagr"), aes(xend = x, yend = 0), linewidth = 0.4) +
  geom_vline(xintercept = c(0, 0.02), linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  annotate("text", x = c(0, 0.02), y = Inf, label = c("0%", "2%"), hjust = -0.2, vjust = 1, size = pb_annot_size("small", 7), colour = "grey20") +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  coord_cartesian(xlim = x_range_b, expand = FALSE, clip = "off") +
  labs(title = "Material use & GDP") +
  theme_void() +
  theme(plot.title = theme_pb_small()$plot.title, plot.margin = margin(t = 2, r = 0, b = 0, l = 4, unit = "pt")) # b = 0: flush on the box

# GDP-growth margin (right): 2%/yr reference line; no end drop lines
p4b_right <- ggplot(marg_a |> dplyr::filter(var == "gdp_cagr"), aes(x = x, y = y, colour = ssp)) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = 0.02, linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  annotate("text", x = 0.02, y = Inf, label = "2%", hjust = -0.1, vjust = -0.3, size = pb_annot_size("small", 7), colour = "grey20") +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  coord_flip(xlim = y_range_b, ylim = c(0, NA), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 22, b = 4, l = 0, unit = "pt")) # l = 0: flush on the box


# Assemble and save Figure 4 ------------------------------------------------------

# One aligned grid: a = material use & GDP, b = material use & climate change
# (material composition per SSP is the separate SI figure S24 below).
# Absolute sizes so a and b are equal squares.
BOX_CM <- 5.4 # side of panels a and b
fig4 <- patchwork::wrap_plots(
  # spacer margin 0: its default 5.5 pt would open a gap between the boxes and their margins
  p4b_top, patchwork::plot_spacer() + theme(plot.margin = margin(0, 0, 0, 0)), p4b_main, p4b_right,
  p4a_main, p4a_right,
  design = "AB\nCD\nEF",
  widths = grid::unit(c(BOX_CM, 0.8), "cm"), heights = grid::unit(c(0.9, BOX_CM, BOX_CM), "cm")
)
ggsave("Figures/Fig4 - Climate.png", fig4, units = "cm", dpi = 600, width = 8.7, height = 16.4, bg = "white")
ggsave("Figures/SVG/Fig4 - Climate.svg", fig4, units = "cm", width = 8.7, height = 16.4, bg = "transparent")
clean_svg("Figures/SVG/Fig4 - Climate.svg")


# SI: same scatter at the 33rd / 67th percentile warming -------------------------

# Tags set per panel (labs(tag)), not auto-tagged, so the inset gets no letter;
# inset and contour key in panel b only
fig4_si <- (map_panels$t_p33 + labs(title = "33rd percentile warming", tag = "a") + theme(plot.tag = element_text(face = "bold"))) |
  (map_panels$t_p67 + labs(title = "67th percentile warming", tag = "b") + theme(plot.tag = element_text(face = "bold")) +
    patchwork::inset_element(p_comp, left = 0.42, bottom = 0.04, right = 1, top = 0.42, align_to = "panel"))
ggsave("Figures/Supporting-Figures/S23_Climate_Percentiles.png", fig4_si, units = "cm", dpi = 600, width = 17, height = 9, bg = "white")
ggsave("Figures/SVG/Supporting-Figures/S23_Climate_Percentiles.svg", fig4_si, units = "cm", width = 17, height = 9, bg = "transparent")
clean_svg("Figures/SVG/Supporting-Figures/S23_Climate_Percentiles.svg")

print(comp_df |> dplyr::select(ssp, material, share) |> tidyr::pivot_wider(names_from = material, values_from = share))


# SI S24: cumulative material composition per SSP (former Figure 4 panel c) ------
# One panel per material group (columns), one bar per SSP (rows): pooled share
# of FIG_START-FIG_END cumulative use (= former panel c); whisker below each bar =
# 95% range (2.5th-97.5th percentile) of the per-run share within that SSP.
S24_BAR <- c(lo = -0.05, hi = 0.35) # bar extent around the row position (row units)
S24_WHISK_Y <- -0.2 # whisker offset below the row position
S24_CAP <- 0.07 # whisker end-cap half height

s24_df <- cum_mat |>
  dplyr::group_by(ssp, material) |>
  dplyr::summarise(q_lo = stats::quantile(share, 0.025), q_hi = stats::quantile(share, 0.975), .groups = "drop") |>
  dplyr::inner_join(comp_df |> dplyr::transmute(ssp = as.character(ssp), material = as.character(material), share), by = c("ssp", "material")) |>
  dplyr::mutate(
    y = match(ssp, rev(SSP_ORDER_INSET)), # SSP5 on top
    material = factor(material, levels = MAT_ORDER_INSET)
  )
s24_title <- tibble::tibble(material = factor(MAT_ORDER_INSET, levels = MAT_ORDER_INSET))

ggplot(s24_df) +
  geom_rect(aes(xmin = 0, xmax = share, ymin = y + S24_BAR[["lo"]], ymax = y + S24_BAR[["hi"]], fill = material), colour = "black", linewidth = 0.15) +
  geom_segment(aes(x = q_lo, xend = q_hi, y = y + S24_WHISK_Y, yend = y + S24_WHISK_Y), linewidth = 0.4) +
  geom_segment(aes(x = q_lo, xend = q_lo, y = y + S24_WHISK_Y - S24_CAP, yend = y + S24_WHISK_Y + S24_CAP), linewidth = 0.4) +
  geom_segment(aes(x = q_hi, xend = q_hi, y = y + S24_WHISK_Y - S24_CAP, yend = y + S24_WHISK_Y + S24_CAP), linewidth = 0.4) +
  geom_text(aes(x = share, y = y + (S24_BAR[["lo"]] + S24_BAR[["hi"]]) / 2, label = paste0(round(share * 100), "%")), hjust = -0.2, size = pb_annot_size("small", 7)) +
  # material name as the panel title, in its colour
  geom_text(data = s24_title, aes(x = 0, y = n_ssp_comp + 0.8, label = material, colour = material), hjust = 0, fontface = "bold", size = pb_annot_size("small", 7)) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.3))) +
  scale_y_continuous(breaks = seq_len(n_ssp_comp), labels = rev(SSP_ORDER_INSET), limits = c(0.6, n_ssp_comp + 1), expand = c(0, 0)) +
  facet_wrap(~material, nrow = 1, scales = "free_x") +
  coord_cartesian(clip = "off") +
  labs(x = paste0("Share of cumulative material use ", FIG_START, "–", FIG_END), y = NULL) +
  theme_pb_small() +
  theme(
    panel.border = element_blank(), panel.background = element_blank(), panel.grid = element_blank(),
    strip.text = element_blank(), strip.background = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.3), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = "bold", colour = unname(SSP_COLORS[rev(SSP_ORDER_INSET)])),
    panel.spacing.x = unit(5, "pt")
  )
ggsave("Figures/Supporting-Figures/S24_MaterialShare_SSP.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 6.5, bg = "white")
ggsave("Figures/SVG/Supporting-Figures/S24_MaterialShare_SSP.svg", ggplot2::last_plot(), units = "cm", width = 17, height = 6.5, bg = "transparent")
clean_svg("Figures/SVG/Supporting-Figures/S24_MaterialShare_SSP.svg")
print(s24_df |> dplyr::select(ssp, material, share, q_lo, q_hi) |> dplyr::mutate(dplyr::across(c(share, q_lo, q_hi), \(v) round(100 * v, 1))) |> as.data.frame())

cat("  Saved: Figures/Fig4 - Climate.png, Supporting-Figures/S23_Climate_Percentiles.png, S24_MaterialShare_SSP.png (+ SVG)\n")
cat("=== Figure 6 done ===\n")

# EoF
