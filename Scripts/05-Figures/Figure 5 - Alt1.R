## =============================================================================
## Figure 5 - Alt1.R  -> Figures/Fig5 - Sensitivity.png (MAIN Figure 5)
##                       + Figures/Supporting-Figures/S22_GDPElasticity.png
## Main Figure 5 (v2 layout): a = variable importance (population and GDP per
## capita as drivers), b = drivers of material growth with range + IQR box,
## c = 2060 per-capita use by GDP growth (<2% / >2%) and favourable /
## unfavourable levers. v1 layout saved as Fig5 - Sensitivity_v1.png.
## Panel a comes from "Figure 5 - Sensitivity.R" (sourced first; saves the
## alternative as Fig5 - Sensitivity_alt1.png, panel c = lever effects).
##
## Panel c, one row per term, each row split in two halves:
##   top    bar = the term's median contribution, stacked as a cascade (each bar
##          starts where the previous one ends); Total row = median total growth
##   bottom the term's distribution across runs, drawn downward, shifted to the
##          bar's start so its median line falls on the bar's end
##   colour: adds to growth (median > 0) vs reduces growth (median < 0)
##   Unit: contribution to the 2025-2060 annual growth rate of total material
##   consumption, in % per year (terms add up to the total growth rate).
## S22 GDP elasticity of material growth per material group (dashed line at 1).
## =============================================================================

source("Scripts/05-Figures/Figure 5 - Sensitivity.R", encoding = "UTF-8")

cat("\n=== Figure 5 - Alt1 ===\n\n")

dec <- read_csv("Parameters/Intermediate/Figure5_Alt1_Decomposition.csv", show_col_types = FALSE)
s22 <- read_csv("Parameters/Intermediate/FigureS22_GDPElasticity.csv", show_col_types = FALSE)

TERM_ORDER <- c(
  "Population", "GDP per capita", "Biomass per GDP", "Fossil fuels per GDP",
  "Metal stock per GDP", "Metal inflow / stock", "Fe ore grade", "Non-Fe ore grade", "Metal recycling",
  "Mineral stock per GDP", "Mineral inflow / stock", "Mineral downcycling"
)
# Row labels: the material is named once in its group header (GROUP_HEADER), not on every row
TERM_LABELS <- c(
  "Population" = "Population",
  "GDP per capita" = "GDP per capita",
  "Biomass per GDP" = "Material intensity",
  "Fossil fuels per GDP" = "Material intensity",
  "Metal stock per GDP" = "Stock intensity",
  "Mineral stock per GDP" = "Stock intensity",
  "Metal inflow / stock" = "Stock inflow rate",
  "Mineral inflow / stock" = "Stock inflow rate",
  "Metal recycling" = "Recycling",
  "Mineral downcycling" = "Downcycling",
  "Fe ore grade" = "Ferrous ore grade",
  "Non-Fe ore grade" = "Non-ferrous ore grade",
  "Total" = "Total growth"
)
# Term -> group (row label colour, group header, black separator lines)
TERM_GROUP <- c(
  "Population" = "drivers", "GDP per capita" = "drivers",
  "Biomass per GDP" = "Biomass", "Fossil fuels per GDP" = "Fossil fuels",
  "Metal stock per GDP" = "Metal ores", "Metal inflow / stock" = "Metal ores", "Metal recycling" = "Metal ores",
  "Fe ore grade" = "Metal ores", "Non-Fe ore grade" = "Metal ores",
  "Mineral stock per GDP" = "Non-metallic minerals", "Mineral inflow / stock" = "Non-metallic minerals",
  "Mineral downcycling" = "Non-metallic minerals",
  "Total" = "total"
)
GROUP_HEADER <- c(
  "drivers" = "2025 → 2060", "Biomass" = "Biomass", "Fossil fuels" = "Fossil fuels",
  "Metal ores" = "Metal ores", "Non-metallic minerals" = "Non-metallic minerals"
)
GROUP_COLS <- c(
  "drivers" = "grey15", "total" = "grey15",
  PALETTE_MATERIAL_GROUPS[c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")]
)
SIGN_COLS <- c("adds" = "#B2182B", "reduces" = "#1B7837", "total" = "grey25")
DENS_ALPHA <- 0.75 # density fill opacity
HALF <- 0.45 # half-row height: bar above the row line, density below it
HEADER_GAP <- 0.8 # vertical slot above each group holding its header


# Panel c -- median cascade (top half) + inverted distributions (bottom half) ------

# Rows top -> bottom (y decreasing): each group opens with a header slot (black
# separator line on its top edge, except the first group), then one unit per
# row; Total last, after a half-row gap and a separator
cursor <- 0
row_y <- c()
header_y <- c()
sep_y <- c()
for (i in seq_along(TERM_ORDER)) {
  g <- TERM_GROUP[[TERM_ORDER[i]]]
  if (i == 1 || g != TERM_GROUP[[TERM_ORDER[i - 1]]]) {
    if (i > 1) sep_y <- c(sep_y, cursor)
    header_y[g] <- cursor - HEADER_GAP / 2
    cursor <- cursor - HEADER_GAP
  }
  row_y[TERM_ORDER[i]] <- cursor - 0.5
  cursor <- cursor - 1
}
sep_y <- c(sep_y, cursor)
row_y["Total"] <- cursor - 0.75
Y_LIM_C <- c(row_y[["Total"]] - HALF - 0.1, 0.1)

# Left-margin labels (drawn as text, so each can carry its group's colour)
lab_c <- dplyr::bind_rows(
  tibble::tibble(
    y = row_y, label = TERM_LABELS[names(row_y)], col = GROUP_COLS[TERM_GROUP[names(row_y)]],
    face = dplyr::if_else(names(row_y) == "Total", "bold", "plain")
  ),
  tibble::tibble(y = header_y, label = GROUP_HEADER[names(header_y)], col = GROUP_COLS[names(header_y)], face = "bold")
)

casc <- dec |>
  dplyr::group_by(term) |>
  dplyr::summarise(med = stats::median(pct_yr), .groups = "drop") |>
  dplyr::filter(term != "Total") |>
  dplyr::mutate(term = factor(term, levels = TERM_ORDER)) |>
  dplyr::arrange(term) |>
  dplyr::mutate(end = cumsum(med), start = dplyr::lag(end, default = 0), term = as.character(term), sign = dplyr::if_else(med >= 0, "adds", "reduces"))
casc <- dplyr::bind_rows(
  casc,
  tibble::tibble(term = "Total", med = stats::median(dec$pct_yr[dec$term == "Total"])) |> dplyr::mutate(start = 0, end = med, sign = "total")
) |>
  dplyr::mutate(y = row_y[term], next_y = dplyr::lead(y))
cat(sprintf("  Cascade of term medians ends at %+.2f %%/yr; median total growth %+.2f %%/yr\n", casc$end[casc$term == TERM_ORDER[length(TERM_ORDER)]], casc$med[casc$term == "Total"]))

# Densities across runs, shifted by the bar's start (median -> bar end), drawn
# downward. Each curve is split at zero contribution (x = bar start): runs where
# the term adds growth (right) vs reduces it (left), sharing the boundary point.
dens_rows <- list()
for (tm in casc$term) {
  v <- dec$pct_yr[dec$term == tm]
  d <- stats::density(v, n = 256)
  cr <- casc[casc$term == tm, ]
  dd <- tibble::tibble(x = cr$start + d$x, depth = d$y / max(d$y) * (HALF - 0.03))
  if (cr$start > min(dd$x) && cr$start < max(dd$x)) {
    dd <- dplyr::bind_rows(dd, tibble::tibble(x = cr$start, depth = stats::approx(dd$x, dd$depth, xout = cr$start)$y)) |> dplyr::arrange(x)
  }
  dens_rows[[tm]] <- dplyr::bind_rows(
    dd |> dplyr::filter(x <= cr$start) |> dplyr::mutate(sign = "reduces"),
    dd |> dplyr::filter(x >= cr$start) |> dplyr::mutate(sign = "adds")
  ) |>
    dplyr::mutate(term = tm, y0 = cr$y - 0.02, grp = paste(term, sign))
}
dens <- dplyr::bind_rows(dens_rows)

# v2 alternative to the densities: full range (min-max line) + interquartile box,
# shifted by the bar's start like the densities
rng <- dec |>
  dplyr::group_by(term) |>
  dplyr::summarise(mn = min(pct_yr), q25 = stats::quantile(pct_yr, 0.25), q75 = stats::quantile(pct_yr, 0.75), mx = max(pct_yr), .groups = "drop") |>
  dplyr::inner_join(casc |> dplyr::select(term, start, y), by = "term") |>
  dplyr::inner_join(casc |> dplyr::select(term, end), by = "term") |> # end = start + median
  dplyr::mutate(dplyr::across(c(mn, q25, q75, mx), \(v) start + v), y_mid = y - HALF / 2)

# Shared x range (both versions), padded right for the value labels
X_RNG_C <- range(c(0, dens$x, rng$mn, rng$mx, casc$start, casc$end))
X_LIM_C <- X_RNG_C + c(-0.03, 0.12) * diff(X_RNG_C)

# Panel c, both versions: v1 = densities (main Figure 5), v2 = range + IQR box
p_c_ver <- list()
for (ver in c("v1", "v2")) {
  p <- ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.35, colour = "grey20") +
    geom_hline(yintercept = row_y, colour = "grey85", linewidth = 0.2) +
    geom_hline(yintercept = sep_y, colour = "black", linewidth = 0.3) +
    # cascade connectors: each bar's end is the next bar's start
    geom_segment(
      data = casc |> dplyr::filter(!term %in% c(TERM_ORDER[length(TERM_ORDER)], "Total")),
      aes(x = end, xend = end, y = y, yend = next_y + HALF), colour = "grey60", linewidth = 0.25, linetype = "dashed"
    )
  if (ver == "v1") {
    # bottom half: distribution across runs (inverted)
    p <- p +
      geom_ribbon(data = dens, aes(x = x, ymin = y0 - depth, ymax = y0, fill = sign, group = grp), alpha = DENS_ALPHA, colour = NA) +
      geom_line(data = dens, aes(x = x, y = y0 - depth, colour = sign, group = grp), linewidth = 0.3)
  } else {
    # bottom half: min-max line + 25-75% box, neutral grey so the bars stay the focus
    p <- p +
      geom_segment(data = rng, aes(x = mn, xend = mx, y = y_mid, yend = y_mid), colour = "grey45", linewidth = 0.3) +
      geom_rect(data = rng, aes(xmin = q25, xmax = q75, ymin = y_mid - HALF * 0.25, ymax = y_mid + HALF * 0.25), fill = "grey75", colour = "grey35", linewidth = 0.15) +
      # median line through the box (= bar end)
      geom_segment(data = rng, aes(x = end, xend = end, y = y_mid - HALF * 0.25, yend = y_mid + HALF * 0.25), colour = "black", linewidth = 0.4)
  }
  p_c_ver[[ver]] <- p +
    # top half: median contribution bar
    geom_rect(data = casc, aes(xmin = pmin(start, end), xmax = pmax(start, end), ymin = y + 0.02, ymax = y + HALF, fill = sign), colour = "black", linewidth = 0.15) +
    # median line at the bar end (top half only; none on the distribution)
    geom_segment(data = casc, aes(x = end, xend = end, y = y + 0.02, yend = y + HALF), colour = "black", linewidth = 0.4) +
    geom_text(data = casc, aes(x = pmax(start, end), y = y + HALF / 2, label = sprintf("%+.2f%%", med), colour = sign), hjust = -0.2, size = pb_annot_size("largeFont", 6.5), fontface = "bold") +
    # row labels and group headers, left of the panel, in their group colour
    geom_text(
      data = lab_c, aes(x = X_LIM_C[1] - 0.015 * diff(X_LIM_C), y = y, label = label),
      colour = lab_c$col, fontface = lab_c$face, size = pb_annot_size("largeFont", 6.5), hjust = 1
    ) +
    scale_fill_manual(values = SIGN_COLS, guide = "none") +
    scale_colour_manual(values = SIGN_COLS, guide = "none") +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
    coord_cartesian(xlim = X_LIM_C, ylim = Y_LIM_C, expand = FALSE, clip = "off") +
    labs(title = "Drivers of material growth", x = "Contribution to growth, 2025-2060 (%/yr)", y = NULL) +
    theme_pb_large() +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 4, r = 8, b = 4, l = 82, unit = "pt"), # l: room for the row labels
      plot.tag.location = "panel",
      plot.tag.position = "topright"
    )
}


# Panel c, v2 -- low vs high GDP growth, each split into favourable / unfavourable runs
# One facet per material, two rows per facet (GDP growth >2%/yr top, <2%/yr bottom;
# total GDP annual growth 2025-2060). In each row: densities (outline only) of
# FORECAST_END world primary consumption (Gt) for that row's runs in the bottom vs
# top p of an "unfavourable" score (across ALL runs, then filtered by row; p from
# the quartile, widened until each density has >= MIN_N_DENS runs) = mean
# percentile rank of:
#   biomass, fossil fuels: M/G (consumption per GDP, kg/$)
#   metals:   S/G (stock per GDP), low Fe ore grade, low non-Fe ore grade, low recycling
#   minerals: S/G, low downcycling
# Material colour ("Low") = favourable quartile, dark shade ("High") = unfavourable.
# Each curve is labelled with its group's medians (key in the facet strip): M/G
# (biomass, fossil); S/G, Fe / non-Fe ore grade and recycling (metals); S/G and
# downcycling (minerals).

GDP_SPLIT <- 0.02 # total GDP annual growth splitting the two rows
SSP_ROWS <- c("<2%" = 0, ">2%" = 1.15) # row baselines (name kept: row key used below)
ROW_H <- 0.75 # tallest curve per facet, in row units
PANEL_C_MM <- 70 # approx. rendered panel width (label clamping)
CHAR_MM <- 1.6 # approx. width of one 8 pt bold character
STRIP_KEY <- c(
  "Biomass" = "Biomass · M/G",
  "Fossil fuels" = "Fossil fuels · M/G",
  "Metal ores" = "Metal ores · S/G · Fe/non-Fe grade · recycling",
  "Non-metallic minerals" = "Non-metallic minerals · S/G · downcycling"
)
# Favourable = material colour, unfavourable = dark shade (as dens_pal "High")
pal_v2 <- dens_pal
pal_v2[paste(MAT_LEVELS, "Low")] <- unname(PALETTE_MATERIAL_GROUPS[MAT_LEVELS])

q_df <- density_df |>
  dplyr::filter(material %in% MAT_LEVELS) |>
  dplyr::inner_join(scatter_df |> dplyr::select(run_id, gdp_cagr), by = "run_id") |>
  dplyr::mutate(
    ssp = dplyr::if_else(gdp_cagr < GDP_SPLIT, "<2%", ">2%"),
    intensity = dplyr::if_else(material %in% c("Biomass", "Fossil fuels"), mg, metric)
  ) |>
  # Score across ALL runs of the material (global), so both GDP rows share the
  # same favourable / unfavourable condition; rows only filter
  dplyr::group_by(material) |>
  dplyr::mutate(
    score = dplyr::case_when(
      material == "Metal ores" ~ (dplyr::percent_rank(intensity) + (1 - dplyr::percent_rank(grade_ore_fe)) +
        (1 - dplyr::percent_rank(grade_ore_nonfe)) + (1 - dplyr::percent_rank(rate))) / 4,
      material == "Non-metallic minerals" ~ (dplyr::percent_rank(intensity) + (1 - dplyr::percent_rank(rate))) / 2,
      TRUE ~ dplyr::percent_rank(intensity)
    )
  ) |>
  dplyr::ungroup()

# Global group bounds per material, set separately for each group:
#   Low  = score <= its p_low-quantile   (favourable: low M/G or S/G, ...)
#   High = score >= its (1 - p_high)-quantile
# Each p starts at the quartile (0.25) and widens in 0.01 steps (max 0.5) only
# until that group's density in every GDP row holds >= MIN_N_DENS runs -- so a
# group with enough runs keeps the quartile bound.
MIN_N_DENS <- 70
q_bounds <- tibble::tibble(material = MAT_LEVELS, p_low = NA_real_, p_high = NA_real_, lo = NA_real_, hi = NA_real_)
for (j in seq_along(MAT_LEVELS)) {
  sc <- q_df$score[q_df$material == MAT_LEVELS[j]]
  rw <- q_df$ssp[q_df$material == MAT_LEVELS[j]]
  for (p_high in seq(0.25, 0.5, by = 0.01)) {
    hi <- stats::quantile(sc, 1 - p_high, names = FALSE)
    n_high <- min(sapply(names(SSP_ROWS), \(r) sum(sc >= hi & rw == r)))
    if (n_high >= MIN_N_DENS) break
  }
  # Low may widen past the median, up to where the High group starts (no overlap)
  for (p_low in seq(0.25, 1 - p_high - 0.01, by = 0.01)) {
    lo <- stats::quantile(sc, p_low, names = FALSE)
    n_low <- min(sapply(names(SSP_ROWS), \(r) sum(sc <= lo & rw == r)))
    if (n_low >= MIN_N_DENS) break
  }
  if (min(n_low, n_high) < MIN_N_DENS) warning("Panel c: ", MAT_LEVELS[j], " has only ", min(n_low, n_high), " runs in its smallest density at the widest non-overlapping bounds")
  q_bounds$p_low[j] <- p_low
  q_bounds$p_high[j] <- p_high
  q_bounds$lo[j] <- lo
  q_bounds$hi[j] <- hi
}
cat("  v2 panel c global group bounds (Low = bottom p_low, High = top p_high of the score):\n")
print(as.data.frame(q_bounds |> dplyr::select(material, p_low, p_high)))

q_df <- q_df |>
  dplyr::left_join(q_bounds, by = "material") |>
  dplyr::mutate(q = dplyr::case_when(score <= lo ~ "Low", score >= hi ~ "High", TRUE ~ NA_character_)) |>
  dplyr::filter(!is.na(q))
cat("  v2 panel c runs per density (material x GDP-growth row x favourable Low / unfavourable High):\n")
print(as.data.frame(tidyr::pivot_wider(dplyr::count(q_df, material, ssp, q), names_from = q, values_from = n)))

# Densities of FORECAST_END world primary consumption (Gt) per material x row x
# group, scaled to the facet's tallest curve
dens_q_rows <- list()
for (m in MAT_LEVELS) {
  for (s in names(SSP_ROWS)) {
    for (g in c("Low", "High")) {
      d <- stats::density(q_df$total_Gt[q_df$material == m & q_df$ssp == s & q_df$q == g], n = 256)
      dens_q_rows[[paste(m, s, g)]] <- tibble::tibble(material = m, ssp = s, q = g, x = d$x, dens = d$y)
    }
  }
}
dens_q <- dplyr::bind_rows(dens_q_rows) |>
  dplyr::group_by(material) |>
  dplyr::mutate(h = dens / max(dens) * ROW_H) |>
  dplyr::ungroup() |>
  dplyr::mutate(base = unname(SSP_ROWS[ssp]), col = unname(pal_v2[paste(material, q)]), material = factor(material, levels = MAT_LEVELS))

# Group medians: intensity rounded to 1 significant digit (2 when that would make
# the Low and High labels of the same row identical); ore grades and recovery
# rates as % (2 significant digits)
# (global group medians: the condition itself, identical in both GDP rows)
q_val <- q_df |>
  dplyr::group_by(material, q) |>
  dplyr::summarise(
    v = stats::median(intensity),
    g_fe = stats::median(grade_ore_fe),
    g_nfe = stats::median(grade_ore_nonfe),
    r = stats::median(rate),
    .groups = "drop"
  ) |>
  dplyr::group_by(material) |>
  dplyr::mutate(
    v_round = if (dplyr::n_distinct(signif(v, 1)) < dplyr::n()) signif(v, 2) else signif(v, 1),
    v_txt = paste0(format(v_round, drop0trailing = TRUE, trim = TRUE, scientific = FALSE), " kg/$"),
    label = dplyr::case_when(
      material == "Metal ores" ~ paste0(v_txt, " · ", signif(100 * g_fe, 2), "/", signif(100 * g_nfe, 2), "% · ", signif(100 * r, 2), "%"),
      material == "Non-metallic minerals" ~ paste0(v_txt, " · ", signif(100 * r, 2), "%"),
      TRUE ~ v_txt
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS))

# Labels just above each curve's peak: the curve further left in its row
# right-aligned on its peak, the other left-aligned on its peak (never cross);
# shifted inward where the estimated text width would leave the panel.
# GDP-growth labels left of each row
q_lab <- dens_q |>
  dplyr::group_by(material, ssp, q, col, base) |>
  dplyr::slice_max(h, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::left_join(q_val |> dplyr::select(material, q, label), by = c("material", "q")) |>
  dplyr::left_join(
    dens_q |> dplyr::group_by(material) |> dplyr::summarise(x_min = min(x), x_max = max(x), .groups = "drop"),
    by = "material"
  ) |>
  dplyr::mutate(w = nchar(label) * CHAR_MM / PANEL_C_MM * (x_max - x_min)) |> # text width, data units
  dplyr::group_by(material, ssp) |>
  dplyr::mutate(
    left = x == min(x),
    hj = dplyr::if_else(left, 1, 0),
    x_lab = dplyr::if_else(left, pmax(x, x_min + w), pmin(x, x_max - w)),
    # right label starts after the left label's end (+ a 2-character gap)
    x_lab = dplyr::if_else(!left, pmax(x_lab, max(x_lab[left]) + 2 * CHAR_MM / PANEL_C_MM * (x_max - x_min)), x_lab),
    y_lab = base + h + 0.03
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(ssp == ">2%") # labels on the upper row only (same condition in both rows)

# Reference lines in Gt: 2025 world total (median across runs) held flat or grown +2%/yr
dens_vlines_gt <- density_lines |>
  dplyr::filter(material %in% MAT_LEVELS) |>
  dplyr::transmute(material, v0 = v0_Gt, v2 = v0_Gt * (1 + GROWTH_REF)^(FORECAST_END - 2025L)) |>
  tidyr::pivot_longer(c(v0, v2), names_to = "line", values_to = "x") |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS), label = if_else(line == "v0", "0%/yr", paste0("+", GROWTH_REF * 100, "%/yr")))
row_lab <- tidyr::expand_grid(material = factor(MAT_LEVELS, levels = MAT_LEVELS), ssp = names(SSP_ROWS)) |>
  dplyr::mutate(base = unname(SSP_ROWS[ssp]), col = "grey20", label = paste0("GDP\n", ssp))

p_d_v2 <- ggplot(dens_q, aes(x = x)) +
  geom_hline(data = row_lab, aes(yintercept = base), colour = "grey80", linewidth = 0.2) +
  geom_vline(data = dens_vlines_gt, aes(xintercept = x), linetype = "dashed", colour = "grey40", linewidth = 0.35) +
  # growth-line labels inside the top of the first facet (empty band above the
  # upper row's curves), right of each line
  geom_text(
    data = dens_vlines_gt |> dplyr::filter(material == MAT_LEVELS[1]),
    aes(x = x, y = Inf, label = label), hjust = -0.08, vjust = 1.3,
    colour = "grey40", size = pb_annot_size("largeFont", 6)
  ) +
  geom_line(aes(y = base + h, colour = col, group = interaction(ssp, q)), linewidth = 0.5) +
  geom_text(data = q_lab, aes(x = x_lab, y = y_lab, label = label, colour = col, hjust = hj), vjust = 0, fontface = "bold", size = pb_annot_size("largeFont", 6)) +
  geom_text(data = row_lab, aes(x = -Inf, y = base + 0.4, label = label, colour = col), hjust = 1.15, lineheight = 0.9, fontface = "bold", size = pb_annot_size("largeFont", 6.5)) +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, drop0trailing = TRUE)) +
  scale_y_continuous(limits = c(0, 2.4), expand = c(0, 0)) + # headroom for the group labels
  facet_wrap(~material, ncol = 1, scales = "free_x", labeller = labeller(material = STRIP_KEY)) +
  coord_cartesian(clip = "off") +
  labs(title = "Material levers vs. GDP growth, 2060", x = "Primary material consumption (Gt)", y = NULL) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, margin = margin(b = 2, l = 0, unit = "pt")),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 30, unit = "pt"), # l: room for the SSP row labels
    plot.tag.location = "panel",
    plot.tag.position = c(-0.08, 1.028)
  )


# Assemble and save: panel a full width (panel b moved to Figure 4) ------------------

# Runs per dominant SSP (of N_RUNS)
cat("  Runs per dominant SSP:\n")
print(table(scatter_df$ssp))

# wrap_elements(full =): panel a laid out on its own, not aligned to panel b's wide label margin
# v1 (alternative): panel b = densities, panel c = densities by material metric group
fig_alt1 <- patchwork::wrap_elements(full = p_a) / (p_c_ver$v1 | p_d) +
  patchwork::plot_layout(heights = c(0.75, 1.6)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("Figures/Fig5 - Sensitivity_v1.png", fig_alt1, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity_v1.svg", fig_alt1, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity_v1.svg") # cleans text-length attrs + groups into Grid/Data/Labels Inkscape layers
cat("  Saved: Figures/Fig5 - Sensitivity_v1.png, Figures/SVG/Fig5 - Sensitivity_v1.svg\n")

# Main Figure 5 (v2): panel b = range + IQR box, panel c = densities by GDP growth
fig5_v2 <- patchwork::wrap_elements(full = p_a) / (p_c_ver$v2 + p_d_v2 + patchwork::plot_layout(widths = c(0.9, 1.1))) +
  patchwork::plot_layout(heights = c(0.75, 1.6)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("Figures/Fig5 - Sensitivity.png", fig5_v2, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity.svg", fig5_v2, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity.svg")
cat("  Saved: Figures/Fig5 - Sensitivity.png, Figures/SVG/Fig5 - Sensitivity.svg\n")


# SI S22 -- GDP elasticity per material group ---------------------------------------

S22_ORDER <- c("Biomass", "Fossil fuels", "Metal ores Fe", "Metal ores NonFe", "Non-metallic minerals", "Total")
S22_COLS <- c(
  "Biomass" = PALETTE_MATERIAL_GROUPS[["Biomass"]], "Fossil fuels" = PALETTE_MATERIAL_GROUPS[["Fossil fuels"]],
  "Metal ores Fe" = PALETTE_MATERIALS[["Ferrous ores"]], "Metal ores NonFe" = PALETTE_MATERIALS[["Non-ferrous ores"]],
  "Non-metallic minerals" = PALETTE_MATERIAL_GROUPS[["Non-metallic minerals"]], "Total" = "black"
)
pb_set_geom_defaults("small")
ggplot(s22 |> dplyr::mutate(group = factor(group, levels = rev(S22_ORDER))), aes(x = elasticity, y = group, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.35) +
  geom_point(size = 2) +
  geom_text(aes(label = sprintf("%.2f", elasticity)), vjust = -0.9, size = pb_annot_size("small", 7), show.legend = FALSE) +
  scale_colour_manual(values = S22_COLS, guide = "none") +
  labs(title = "GDP elasticity of material growth, 2025-2060", x = "% material growth per 1% GDP growth", y = NULL) +
  theme_pb_small()
ggsave("Figures/Supporting-Figures/S22_GDPElasticity.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)
ggsave("Figures/SVG/Supporting-Figures/S22_GDPElasticity.svg", ggplot2::last_plot(), units = "cm", width = 8.7, height = 8.7)
cat("  Saved: Figures/Supporting-Figures/S22_GDPElasticity.png\n")

cat("=== Figure 5 - Alt1 done ===\n")

# EoF
