## Figure 4 - Climate.R  -> Figures/Fig4 - Climate.png (+ Ridge/Histogram/Even variants)
## One-panel MC figure linking cumulative material use to 2060 warming by SSP:
##   X = per-run cumulative primary consumption, FIG_START-FIG_END (Gt, all
##       material groups summed)
##   Y = 2060 GSAT (°C vs 1850-1900, the Paris Agreement baseline) of each
##       SSP's IIASA marker run: ridge baseline at the median, light band
##       behind it = 33rd-67th percentile
## Runs assigned to their dominant SSP (same rule as Figure 4 v2 -
## PrepareData.R); SSP4 is not sampled by the MC (SSP_SAMPLED).
## Versions:
##   Fig6_Climate           -- density ridges, split by conditional material-group share
##   Fig4 - Climate (Histogram) -- same layout, stacked histogram bars
##   Fig4 - Climate (Even)      -- evenly spaced ridges per SSP + marker temperature dot/whisker
##   Fig4 - Climate    -- scatter + 33%/66% HDR contours, each run at the IDW temperature
##                             of its nearest ScenarioMIP scenarios (Figure 4 - Climate - PrepareData.R);
##                             inset = cumulative material composition per SSP
## Right-margin bars (ridge, histogram, mapped): full range of the 2060 median
## temperature across all of the SSP's ScenarioMIP scenarios, dot = marker median.
## Dashed lines: 1.5 °C / 2 °C (Paris), and FIG_START annual use growing
## 0%/yr (held constant) / 2%/yr to FIG_END.

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
TEMP_TITLE <- paste0("Temperature in ", FIG_END, " (relative to 1850–1900)")
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
  dplyr::group_by(run_id, year, material_group, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::collect() |>
  dplyr::mutate(
    ssp = dplyr::if_else(ssp_share_lo >= 0.5, ssp_lo, ssp_hi), # dominant SSP of the blend
    material = unname(MATERIAL_MAP[material_group])
  )
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

# Shared axes (ridge + histogram): x leaves room for the SSP labels left of the
# ridges (and right of LABEL_RIGHT); y ticks every 0.25 °C, labelled every 0.5 °C
X_LIM <- c(floor((min(cum_mat$total_Gt) - 900) / 500) * 500, ceiling((max(cum_mat$total_Gt) + 800) / 500) * 500)
Y_LIM <- c(
  floor(min(temp_ssp$t_lo, temp_range$t_min) * 4) / 4,
  ceiling(max(temp_ssp$t_hi, temp_range$t_max, temp_ssp$t_med + RIDGE_HEIGHT) * 4) / 4
)
Y_BREAKS <- seq(Y_LIM[1], Y_LIM[2], by = 0.25)
Y_LABELS <- dplyr::if_else(Y_BREAKS %% 0.5 == 0, paste0("+", sprintf("%.1f", Y_BREAKS), "°C"), "")

# Legend in the gap between the SSP1 and SSP2 ridges (npc of the y range)
LEGEND_Y <- (mean(temp_ssp$t_med[temp_ssp$ssp %in% c("SSP1", "SSP2")]) - Y_LIM[1]) / diff(Y_LIM)

# Ridge version: conditional-density split ---------------------------------

# Each group's layer = density of run totals weighted by that group's share,
# times its mean share. With one shared bandwidth per SSP the layers sum
# exactly to the SSP's total density, and each layer's thickness at x is the
# average material mix of runs whose total use is near x.
X_GRID <- seq(0.95 * min(cum_mat$total_Gt), 1.05 * max(cum_mat$total_Gt), length.out = 512)

ridge_df <- cum_mat |>
  dplyr::group_by(ssp) |>
  dplyr::mutate(bw = stats::bw.nrd0(total_Gt[material == MAT_LEVELS[1]])) |>
  dplyr::group_by(ssp, material) |>
  dplyr::reframe(
    x = X_GRID,
    dens = stats::density(
      total_Gt,
      weights = share / sum(share), bw = bw[1], from = min(X_GRID), to = max(X_GRID), n = length(X_GRID)
    )$y * mean(share)
  ) |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS)) |>
  dplyr::arrange(ssp, x, material) |>
  dplyr::group_by(ssp, x) |>
  dplyr::mutate(top = cumsum(dens), bottom = top - dens, total = sum(dens)) |>
  dplyr::ungroup() |>
  dplyr::filter(total >= 0.005 * max(total)) |> # trim flat tails
  dplyr::left_join(temp_ssp, by = "ssp") |>
  dplyr::mutate(ymin = t_med + bottom * RIDGE_HEIGHT / max(top), ymax = t_med + top * RIDGE_HEIGHT / max(top))

# SSP bands: fill scale shared with the material groups (legend shows materials only)
ggplot() +
  geom_rect(data = temp_ssp, aes(xmin = -Inf, xmax = Inf, ymin = t_lo, ymax = t_hi, fill = ssp), alpha = 0.15) +
  geom_hline(yintercept = PARIS_LEVELS, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geomtextpath::geom_textvline(
    data = VLINES, aes(xintercept = x, label = label),
    linetype = "dashed", colour = "grey20", linewidth = 0.3, hjust = LEGEND_Y, vjust = -0.3,
    size = pb_annot_size("small", 7)
  ) +
  geom_ribbon(
    data = ridge_df, aes(x = x, ymin = ymin, ymax = ymax, fill = material, group = interaction(ssp, material)),
    alpha = RIDGE_ALPHA, colour = "black", linewidth = 0.1
  ) +
  geom_text(
    data = ridge_df |>
      dplyr::group_by(ssp, t_med) |>
      dplyr::summarise(x = if (ssp[1] %in% LABEL_RIGHT) max(x) else min(x), .groups = "drop") |>
      dplyr::mutate(hj = dplyr::if_else(ssp %in% LABEL_RIGHT, -0.1, 1.1)),
    aes(x = x, y = t_med, label = ssp, colour = ssp, hjust = hj),
    vjust = 0, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  scale_fill_manual(values = c(SSP_COLORS, PALETTE_MATERIAL_GROUPS), breaks = rev(MAT_LEVELS), name = NULL) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma()) +
  # Right margin: full scenario range per SSP (bar) with the marker median (dot)
  geom_segment(
    data = temp_range,
    aes(x = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, xend = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, y = t_min, yend = t_max, colour = ssp),
    linewidth = 0.8
  ) +
  geom_point(
    data = temp_ssp |> dplyr::mutate(x_idx = match(ssp, SSP_SAMPLED)),
    aes(x = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, y = t_med, colour = ssp), size = 1.2
  ) +
  scale_y_continuous(breaks = Y_BREAKS, labels = Y_LABELS) +
  coord_cartesian(xlim = X_LIM, ylim = Y_LIM, expand = FALSE, clip = "off") +
  labs(x = paste0("Cumulative material use (", FIG_START, "–", FIG_END, ", Gt)"), y = TEMP_TITLE) +
  theme_pb_small() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, LEGEND_Y), legend.justification = c(1, 0.5),
    plot.margin = margin(5.5, RANGE_MARGIN, 5.5, 5.5)
  )

ggsave("Figures/Fig4 - Climate (Ridge).png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7, bg = "white")
ggsave("Figures/SVG/Fig4 - Climate (Ridge).svg", ggplot2::last_plot(), units = "cm", width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig4 - Climate (Ridge).svg")

# Histogram version: stacked bars ------------------------------------------

# Bar height = share of the SSP's runs in the bin; split by summing each
# group's per-run share over the runs in the bin (same decomposition as above)
BIN_BREAKS <- pretty(range(cum_mat$total_Gt), n = N_BINS)

hist_df <- cum_mat |>
  dplyr::mutate(bin = cut(total_Gt, BIN_BREAKS, include.lowest = TRUE, labels = FALSE)) |>
  dplyr::group_by(ssp) |>
  dplyr::mutate(n_runs = dplyr::n_distinct(run_id)) |>
  dplyr::group_by(ssp, bin, material) |>
  dplyr::summarise(h = sum(share) / n_runs[1], .groups = "drop") |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS), xmin = BIN_BREAKS[bin], xmax = BIN_BREAKS[bin + 1]) |>
  dplyr::arrange(ssp, bin, material) |>
  dplyr::group_by(ssp, bin) |>
  dplyr::mutate(top = cumsum(h), bottom = top - h) |>
  dplyr::ungroup() |>
  dplyr::left_join(temp_ssp, by = "ssp") |>
  dplyr::mutate(ymin = t_med + bottom * RIDGE_HEIGHT / max(top), ymax = t_med + top * RIDGE_HEIGHT / max(top))

ggplot() +
  geom_rect(data = temp_ssp, aes(xmin = -Inf, xmax = Inf, ymin = t_lo, ymax = t_hi, fill = ssp), alpha = 0.15) +
  geom_hline(yintercept = PARIS_LEVELS, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geomtextpath::geom_textvline(
    data = VLINES, aes(xintercept = x, label = label),
    linetype = "dashed", colour = "grey20", linewidth = 0.3, hjust = LEGEND_Y, vjust = -0.3,
    size = pb_annot_size("small", 7)
  ) +
  geom_rect(
    data = hist_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = material),
    alpha = RIDGE_ALPHA, colour = "black", linewidth = 0.1
  ) +
  geom_text(
    data = hist_df |>
      dplyr::group_by(ssp, t_med) |>
      dplyr::summarise(x = if (ssp[1] %in% LABEL_RIGHT) max(xmax) else min(xmin), .groups = "drop") |>
      dplyr::mutate(hj = dplyr::if_else(ssp %in% LABEL_RIGHT, -0.1, 1.1)),
    aes(x = x, y = t_med, label = ssp, colour = ssp, hjust = hj),
    vjust = 0, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  scale_fill_manual(values = c(SSP_COLORS, PALETTE_MATERIAL_GROUPS), breaks = rev(MAT_LEVELS), name = NULL) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma()) +
  # Right margin: full scenario range per SSP (bar) with the marker median (dot)
  geom_segment(
    data = temp_range,
    aes(x = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, xend = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, y = t_min, yend = t_max, colour = ssp),
    linewidth = 0.8
  ) +
  geom_point(
    data = temp_ssp |> dplyr::mutate(x_idx = match(ssp, SSP_SAMPLED)),
    aes(x = X_LIM[2] + RANGE_STEP * diff(X_LIM) * x_idx, y = t_med, colour = ssp), size = 1.2
  ) +
  scale_y_continuous(breaks = Y_BREAKS, labels = Y_LABELS) +
  coord_cartesian(xlim = X_LIM, ylim = Y_LIM, expand = FALSE, clip = "off") +
  labs(x = paste0("Cumulative material use (", FIG_START, "–", FIG_END, ", Gt)"), y = TEMP_TITLE) +
  theme_pb_small() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, LEGEND_Y), legend.justification = c(1, 0.5),
    plot.margin = margin(5.5, RANGE_MARGIN, 5.5, 5.5)
  )

ggsave("Figures/Fig4 - Climate (Histogram).png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7, bg = "white")
ggsave("Figures/SVG/Fig4 - Climate (Histogram).svg", ggplot2::last_plot(), units = "cm", width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig4 - Climate (Histogram).svg")

# Evenly spaced version: ridges per SSP + marker temperature ----------------

totals_df <- cum_mat |>
  dplyr::distinct(run_id, ssp, total_Gt) |>
  dplyr::mutate(ssp = factor(ssp, levels = SSP_SAMPLED))

X_LIM_EVEN <- c(floor(min(totals_df$total_Gt) / 500) * 500, ceiling(max(totals_df$total_Gt) / 500) * 500)
Y_LIM_EVEN <- c(0.8, length(SSP_SAMPLED) + 1) # room above the top ridge
DOT_OFFSET <- 0.3 # temperature dot sits inside its ridge's row, above the baseline

p_even_ridge <- ggplot(totals_df, aes(x = total_Gt, y = ssp, fill = ssp)) +
  ggridges::geom_density_ridges(scale = 0.9, alpha = RIDGE_ALPHA, colour = "black", linewidth = 0.2) +
  geomtextpath::geom_textvline(
    data = VLINES, aes(xintercept = x, label = label), inherit.aes = FALSE,
    linetype = "dashed", colour = "grey20", linewidth = 0.3, hjust = 0.97, vjust = -0.3,
    size = pb_annot_size("small", 7)
  ) +
  scale_fill_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = X_LIM_EVEN, ylim = Y_LIM_EVEN, expand = FALSE) +
  labs(x = paste0("Cumulative material use (", FIG_START, "–", FIG_END, ", Gt)"), y = NULL) +
  theme_pb_small()

p_even_temp <- ggplot(temp_ssp, aes(y = match(ssp, SSP_SAMPLED) + DOT_OFFSET, colour = ssp)) +
  geom_vline(xintercept = PARIS_LEVELS, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  # thin = full range across the SSP's scenarios, thick = marker 33rd-67th percentile
  geom_linerange(data = temp_range, aes(xmin = t_min, xmax = t_max), linewidth = 0.3) +
  geom_linerange(aes(xmin = t_lo, xmax = t_hi), linewidth = 0.8) +
  geom_point(aes(x = t_med), size = 2) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(breaks = Y_BREAKS, labels = Y_LABELS) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(xlim = Y_LIM, ylim = Y_LIM_EVEN, expand = FALSE) +
  labs(x = paste0("Temperature in ", FIG_END, "\n(relative to 1850–1900)"), y = NULL) +
  theme_pb_small() +
  theme(plot.margin = margin(5.5, 12, 5.5, 5.5))

fig6_even <- p_even_ridge + p_even_temp + patchwork::plot_layout(widths = c(3, 1.2))

ggsave("Figures/Fig4 - Climate (Even).png", fig6_even, units = "cm", dpi = 600, width = 17, height = 8.7, bg = "white")
ggsave("Figures/SVG/Fig4 - Climate (Even).svg", fig6_even, units = "cm", width = 17, height = 8.7)
clean_svg("Figures/SVG/Fig4 - Climate (Even).svg")

# Mapped version: scatter + 2D density ---------------------------------------

mapped_df <- totals_df |>
  dplyr::mutate(ssp = as.character(ssp)) |>
  dplyr::inner_join(run_map |> dplyr::select(run_id, t_med), by = "run_id")
stopifnot(nrow(mapped_df) == nrow(totals_df))

# Y range: data + range bars, with a small pad (not rounded to 0.25, so the
# axis ends just above the highest value)
X_LIM_MAP <- c(floor(min(mapped_df$total_Gt) / 500) * 500, ceiling((max(mapped_df$total_Gt) + 800) / 500) * 500)
Y_LIM_MAP <- c(
  floor((min(mapped_df$t_med, temp_range$t_min) - 0.05) * 20) / 20,
  ceiling((max(mapped_df$t_med, temp_range$t_max) + 0.03) * 20) / 20
)
Y_BREAKS_MAP <- seq(ceiling(Y_LIM_MAP[1] * 4) / 4, floor(Y_LIM_MAP[2] * 4) / 4, by = 0.25)
Y_LABELS_MAP <- dplyr::if_else(Y_BREAKS_MAP %% 0.5 == 0, paste0("+", sprintf("%.1f", Y_BREAKS_MAP), "°C"), "")

# Per-SSP median run position (big mark + label)
map_median <- mapped_df |>
  dplyr::group_by(ssp) |>
  dplyr::summarise(x = median(total_Gt), y = median(t_med), .groups = "drop")

p_map <- ggplot(mapped_df, aes(x = total_Gt, y = t_med, colour = ssp)) +
  geom_hline(yintercept = PARIS_LEVELS, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geomtextpath::geom_textvline(
    data = VLINES, aes(xintercept = x, label = label), inherit.aes = FALSE,
    linetype = "dashed", colour = "grey20", linewidth = 0.3, hjust = 0.97, vjust = -0.3,
    size = pb_annot_size("small", 7)
  ) +
  geom_point(size = 0.4, alpha = MAP_ALPHA) +
  # Highest-density regions enclosing HDR_PROBS of each SSP's runs; fixed
  # bandwidths, grid spans the whole panel so contours are not cut at the data range
  ggdensity::geom_hdr_lines(
    probs = HDR_PROBS, method = ggdensity::method_kde(h = MAP_BW),
    xlim = X_LIM_MAP, ylim = Y_LIM_MAP, alpha = 1, linewidth = 0.4
  ) +
  geom_point(data = map_median, aes(x = x, y = y, fill = ssp), shape = 21, colour = "black", stroke = 0.5, size = 2.8) +
  geom_text(
    data = map_median, aes(x = x, y = y, label = ssp),
    hjust = -0.3, vjust = -0.5, fontface = "bold", size = pb_annot_size("small", 7)
  ) +
  # Right margin: full scenario range per SSP (bar) with the marker median (dot)
  geom_segment(
    data = temp_range,
    aes(x = X_LIM_MAP[2] + RANGE_STEP * diff(X_LIM_MAP) * x_idx, xend = X_LIM_MAP[2] + RANGE_STEP * diff(X_LIM_MAP) * x_idx, y = t_min, yend = t_max, colour = ssp),
    linewidth = 0.8
  ) +
  geom_point(
    data = temp_ssp |> dplyr::mutate(x_idx = match(ssp, SSP_SAMPLED)),
    aes(x = X_LIM_MAP[2] + RANGE_STEP * diff(X_LIM_MAP) * x_idx, y = t_med, colour = ssp), size = 1.2
  ) +
  scale_colour_manual(values = SSP_COLORS, guide = "none") +
  scale_fill_manual(values = SSP_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(breaks = Y_BREAKS_MAP, labels = Y_LABELS_MAP) +
  coord_cartesian(xlim = X_LIM_MAP, ylim = Y_LIM_MAP, expand = FALSE, clip = "off") +
  labs(x = paste0("Cumulative material use (", FIG_START, "–", FIG_END, ", Gt)"), y = TEMP_TITLE) +
  theme_pb_small() +
  theme(plot.margin = margin(5.5, RANGE_MARGIN, 5.5, 5.5))

# Inset: pooled cumulative material composition per SSP (sums to 100%)
comp_df <- cum_mat |>
  dplyr::group_by(ssp, material) |>
  dplyr::summarise(cum_Gt = sum(cum_Gt), .groups = "drop") |>
  dplyr::group_by(ssp) |>
  dplyr::mutate(share = cum_Gt / sum(cum_Gt)) |>
  dplyr::ungroup() |>
  dplyr::mutate(ssp = factor(ssp, levels = rev(SSP_SAMPLED)), material = factor(material, levels = rev(MAT_LEVELS)))

p_comp <- ggplot(comp_df, aes(x = share, y = ssp, fill = material)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.1) +
  scale_fill_manual(values = PALETTE_MATERIAL_GROUPS, breaks = MAT_LEVELS, labels = MAT_SHORT[MAT_LEVELS], name = NULL) +
  scale_x_continuous(labels = scales::label_percent(), breaks = c(0, 0.5, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_pb_small() +
  theme(
    legend.position = "bottom", legend.key.size = grid::unit(0.25, "cm"), legend.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(2, 8, 2, 2)
  )

fig6_map <- p_map + patchwork::inset_element(p_comp, left = 0.5, bottom = 0.07, right = 0.93, top = 0.44, align_to = "panel")

ggsave("Figures/Fig4 - Climate.png", fig6_map, units = "cm", dpi = 600, width = 8.7, height = 8.7, bg = "white")
ggsave("Figures/SVG/Fig4 - Climate.svg", fig6_map, units = "cm", width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig4 - Climate.svg")

print(comp_df |> dplyr::select(ssp, material, share) |> tidyr::pivot_wider(names_from = material, values_from = share))

cat("  Saved: Figures/Fig4 - Climate{, (Ridge), (Histogram), (Even)}.png (+ SVG)\n")
cat("=== Figure 6 done ===\n")

# EoF
