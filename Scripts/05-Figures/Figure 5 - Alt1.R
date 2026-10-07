## =============================================================================
## Figure 5 - Alt1.R  -> Figures/Fig5 - Sensitivity.png (MAIN Figure 5)
##                       + Figures/Supporting-Figures/S22_GDPElasticity.png
## Main Figure 5: panel c = drivers of material growth (data from
## "Figure 5 - Alt1 - PrepareData.R"); panels a, b, d come unchanged from
## "Figure 5 - Sensitivity.R" (sourced first; saves the alternative as
## Fig5 - Sensitivity_alt1.png, panel c = lever effects).
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
  tibble::tibble(y = row_y, label = TERM_LABELS[names(row_y)], col = GROUP_COLS[TERM_GROUP[names(row_y)]], face = "bold"),
  tibble::tibble(y = header_y, label = GROUP_HEADER[names(header_y)], col = GROUP_COLS[names(header_y)], face = "bold.italic")
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


# Panel c, v2 -- SSP1 vs SSP3, each split into favourable / unfavourable runs ------
# One facet per material, two rows per facet (SSP3 top, SSP1 bottom). In each row:
# densities (outline only) of FORECAST_END per-capita consumption for that SSP's
# runs in the lowest vs highest quartile (within the SSP) of an "unfavourable"
# score = mean percentile rank of:
#   biomass, fossil fuels: M/G (consumption per GDP, kg/$)
#   metals:   S/G (stock per GDP), low Fe ore grade, low non-Fe ore grade, low recycling
#   minerals: S/G, low downcycling
# Light ("Low") = favourable quartile, dark ("High") = unfavourable, as panel d (dens_pal).
# Each curve is labelled with its group's median intensity (M/G or S/G, kg/$).

SSP_ROWS <- c("SSP1" = 0, "SSP3" = 1) # row baselines
ROW_H <- 0.85 # tallest curve per facet, in row units

q_df <- density_df |>
  dplyr::filter(material %in% MAT_LEVELS) |>
  dplyr::inner_join(scatter_df |> dplyr::select(run_id, ssp), by = "run_id") |>
  dplyr::filter(ssp %in% names(SSP_ROWS)) |>
  dplyr::mutate(intensity = dplyr::if_else(material %in% c("Biomass", "Fossil fuels"), mg, metric)) |>
  dplyr::group_by(material, ssp) |>
  dplyr::mutate(
    score = dplyr::case_when(
      material == "Metal ores" ~ (dplyr::percent_rank(intensity) + (1 - dplyr::percent_rank(grade_ore_fe)) +
        (1 - dplyr::percent_rank(grade_ore_nonfe)) + (1 - dplyr::percent_rank(rate))) / 4,
      material == "Non-metallic minerals" ~ (dplyr::percent_rank(intensity) + (1 - dplyr::percent_rank(rate))) / 2,
      TRUE ~ dplyr::percent_rank(intensity)
    ),
    q = dplyr::case_when(
      score <= stats::quantile(score, 0.25) ~ "Low",
      score >= stats::quantile(score, 0.75) ~ "High",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(q))
cat("  v2 panel c runs per material x SSP x quartile group:\n")
print(dplyr::count(q_df, material, ssp, q))

# Densities per material x SSP x group, scaled to the facet's tallest curve
dens_q_rows <- list()
for (m in MAT_LEVELS) {
  for (s in names(SSP_ROWS)) {
    for (g in c("Low", "High")) {
      d <- stats::density(q_df$percap_t[q_df$material == m & q_df$ssp == s & q_df$q == g], n = 256)
      dens_q_rows[[paste(m, s, g)]] <- tibble::tibble(material = m, ssp = s, q = g, x = d$x, dens = d$y)
    }
  }
}
dens_q <- dplyr::bind_rows(dens_q_rows) |>
  dplyr::group_by(material) |>
  dplyr::mutate(h = dens / max(dens) * ROW_H) |>
  dplyr::ungroup() |>
  dplyr::mutate(base = unname(SSP_ROWS[ssp]), col = unname(dens_pal[paste(material, q)]), material = factor(material, levels = MAT_LEVELS))

# Group median intensity, rounded to 1 significant digit (2 when that would make
# the Low and High labels of the same row identical)
q_val <- q_df |>
  dplyr::group_by(material, ssp, q) |>
  dplyr::summarise(v = stats::median(intensity), .groups = "drop") |>
  dplyr::group_by(material, ssp) |>
  dplyr::mutate(
    v_round = if (dplyr::n_distinct(signif(v, 1)) < dplyr::n()) signif(v, 2) else signif(v, 1),
    label = paste0(format(v_round, drop0trailing = TRUE, trim = TRUE, scientific = FALSE), " kg/$")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(material = factor(material, levels = MAT_LEVELS))

# Labels at each curve's peak; SSP labels left of each row
q_lab <- dens_q |>
  dplyr::group_by(material, ssp, q, col, base) |>
  dplyr::slice_max(h, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::left_join(q_val |> dplyr::select(material, ssp, q, label), by = c("material", "ssp", "q")) |>
  # labels that would overlap (peaks < 20% of the facet's x range apart and similar
  # height): High label stacked one line above the higher of the two
  dplyr::left_join(dens_q |> dplyr::group_by(material) |> dplyr::summarise(x_span = diff(range(x)), .groups = "drop"), by = "material") |>
  dplyr::group_by(material, ssp) |>
  dplyr::mutate(
    y_lab = base + h,
    y_lab = dplyr::if_else(q == "High" & diff(range(x)) < 0.2 * x_span & diff(range(y_lab)) < 0.25, max(y_lab) + 0.28, y_lab)
  ) |>
  dplyr::ungroup()
row_lab <- tidyr::expand_grid(material = factor(MAT_LEVELS, levels = MAT_LEVELS), ssp = names(SSP_ROWS)) |>
  dplyr::mutate(base = unname(SSP_ROWS[ssp]), col = unname(SSP_COLORS[ssp]))

p_d_v2 <- ggplot(dens_q, aes(x = x)) +
  geom_hline(data = row_lab, aes(yintercept = base), colour = "grey80", linewidth = 0.2) +
  geom_vline(data = dens_vlines, aes(xintercept = x), linetype = "dashed", colour = "grey40", linewidth = 0.35) +
  geom_text(
    data = dens_vlines |> dplyr::filter(material == MAT_LEVELS[1]),
    aes(x = x, y = 0, label = label), angle = 90, hjust = -0.1, vjust = -0.4,
    colour = "grey40", size = pb_annot_size("largeFont", 6)
  ) +
  geom_line(aes(y = base + h, colour = col, group = interaction(ssp, q)), linewidth = 0.5) +
  geom_text(data = q_lab, aes(x = x, y = y_lab, label = label, colour = col), vjust = -0.3, fontface = "bold", size = pb_annot_size("largeFont", 6)) +
  geom_text(data = row_lab, aes(x = -Inf, y = base + 0.4, label = ssp, colour = col), hjust = 1.15, fontface = "bold", size = pb_annot_size("largeFont", 6.5)) +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, drop0trailing = TRUE)) +
  scale_y_continuous(limits = c(0, 2.45), expand = c(0, 0)) + # headroom for the group labels
  facet_wrap(~material, ncol = 1, scales = "free_x") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Material consumption per capita, 2060",
    subtitle = "Light: low M/G or S/G; metals + high ore grade\n& recycling; minerals + high downcycling.\nDark: opposite. Labels: median M/G or S/G.",
    x = "Primary material consumption (t/person)", y = NULL
  ) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.subtitle = element_text(size = 8, colour = "grey30", hjust = 0),
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
fig_alt1 <- patchwork::wrap_elements(full = p_a) / (p_c_ver$v1 | p_d) +
  patchwork::plot_layout(heights = c(0.75, 1.6)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("Figures/Fig5 - Sensitivity.png", fig_alt1, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity.svg", fig_alt1, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity.svg") # cleans text-length attrs + groups into Grid/Data/Labels Inkscape layers
cat("  Saved: Figures/Fig5 - Sensitivity.png, Figures/SVG/Fig5 - Sensitivity.svg\n")

# v2: panel b = range + IQR box, panel c = densities by SSP
fig5_v2 <- patchwork::wrap_elements(full = p_a) / (p_c_ver$v2 | p_d_v2) +
  patchwork::plot_layout(heights = c(0.75, 1.6)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("Figures/Fig5 - Sensitivity_v2.png", fig5_v2, units = "cm", dpi = 600, width = 18, height = 18)
ggsave("Figures/SVG/Fig5 - Sensitivity_v2.svg", fig5_v2, units = "cm", width = 18, height = 18)
group_svg_layers("Figures/SVG/Fig5 - Sensitivity_v2.svg")
cat("  Saved: Figures/Fig5 - Sensitivity_v2.png, Figures/SVG/Fig5 - Sensitivity_v2.svg\n")


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
