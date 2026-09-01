## =============================================================================
## Cascade.R
## Twelve-panel Kaya cascade-waterfall: 4 big material categories (columns) x
## 3 scenario selections (rows: median of all MC runs, top 10% decoupling
## subset, absolute-decoupling subset), each panel a waterfall chart
## decomposing the 2024->2050 change in primary material consumption into
## Population, GDP/P, intensity (M/GDP or S/GDP) and -- for stocks -- Secondary
## material effects.
##
## Input: Parameters/Intermediate/Cascade.csv (from Cascade_PrepareData.R)
## Output: Figures/Other/Fig_Cascade.png / Figures/Other/SVG/Fig_Cascade.svg
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

pb_set_geom_defaults("largeFont")

cat("=== Cascade ===\n\n")


# STEP 1: Load prepared data & convert to Gt -----------------------------------

cat("STEP 1: Load prepared data\n")

bars <- read_csv("Parameters/Intermediate/Cascade.csv", show_col_types = FALSE)

bars <- bars |>
  mutate(
    material_group = factor(
      material_group,
      levels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    ),
    selection = factor(selection, levels = c("median", "top10", "absdecoupling")),
    # Mt -> Gt for display only; the decomposition itself (Cascade_PrepareData.R)
    # stays in Mt, this just rescales the plotted y-positions.
    across(c(ymin, ymax, col_y_min, col_y_max, label_y_top), ~ . / 1000)
  )

SELECTION_TITLE <- c("median" = "Median", "top10" = "Top 10% decoupling", "absdecoupling" = "Absolute decoupling")


# STEP 2: Colour system --------------------------------------------------------
# Each effect driver gets ONE fixed colour reused across all 12 panels; the
# BASE_YEAR total bar uses the column's own material colour, TARGET_YEAR uses
# a LIGHTER TINT of that same material colour (blend-toward-white, factor 0.6
# -- the same technique Figure 4 - PrepareData.R uses for its family_pal
# shades) so the outcome bar still reads as belonging to its column while
# staying visually distinct from the BASE_YEAR bar. Scenario-row labels reuse
# this project's PALETTE_DECOUPLING colours (00-CommonParameters.R) where they
# map directly (Absolute decoupling); the Top-10% row (a different,
# percentile-rank selection, not the CAGR-based decoupling_class) borrows the
# same family's "Relative decoupling" gold as the closest existing semantic
# match.

cat("STEP 2: Colour system\n")

POP_COLOR <- "#1F618D" # Figure 2 - ProjectionMethod.R:44
GDPCAP_COLOR <- "#117A65" # Figure 2 - ProjectionMethod.R:45
INTENSITY_COLOR <- "#e31a1c" # "Intensity" family, Figure 4 - PrepareData.R family_pal
SECONDARY_COLOR <- "#33a02c" # "Material recovery" family, Figure 4 - PrepareData.R family_pal

# Lighter tint of each material's own PALETTE_MATERIAL_GROUPS colour,
# hardcoded directly as literal hex (blend-toward-white 0.6, computed by hand,
# not at runtime): Biomass #558B2F -> #BBD1AC | Fossil fuels #5D4037 ->
# #BEB3AF | Metal ores #B71C1C -> #E2A4A4 | Non-metallic minerals #78909C ->
# #C9D3D7.
MATERIAL_2050_COLORS <- c(
  "Biomass 2050" = "#BBD1AC",
  "Fossil fuels 2050" = "#BEB3AF",
  "Metal ores 2050" = "#E2A4A4",
  "Non-metallic minerals 2050" = "#C9D3D7"
)

DRIVER_COLORS <- c(
  "Population" = POP_COLOR,
  "GDP/P" = GDPCAP_COLOR,
  "Intensity" = INTENSITY_COLOR,
  "Secondary material" = SECONDARY_COLOR
)
CASCADE_PAL <- c(PALETTE_MATERIAL_GROUPS, DRIVER_COLORS, MATERIAL_2050_COLORS) # PALETTE_MATERIAL_GROUPS: 00-CommonParameters.R

SELECTION_COLORS <- c(
  "median" = "#424242",
  "top10" = unname(PALETTE_DECOUPLING["Relative decoupling"]),
  "absdecoupling" = unname(PALETTE_DECOUPLING["Absolute decoupling"])
)

# Every text/fill colour used below is resolved to a literal hex (or "black"/
# "white") column ahead of time and drawn via scale_..._identity() -- avoids
# needing two different manual colour scales (fill_key's palette vs. a plain
# black/white contrast colour) on the same `colour` aesthetic, which ggplot2
# can't do in one plot.
bars <- bars |>
  mutate(
    bar_colour_hex = unname(CASCADE_PAL[fill_key]),
    r = strtoi(substr(bar_colour_hex, 2, 3), 16L) / 255,
    g = strtoi(substr(bar_colour_hex, 4, 5), 16L) / 255,
    b = strtoi(substr(bar_colour_hex, 6, 7), 16L) / 255,
    lin_r = ifelse(r <= 0.04045, r / 12.92, ((r + 0.055) / 1.055)^2.4),
    lin_g = ifelse(g <= 0.04045, g / 12.92, ((g + 0.055) / 1.055)^2.4),
    lin_b = ifelse(b <= 0.04045, b / 12.92, ((b + 0.055) / 1.055)^2.4),
    lum = 0.2126 * lin_r + 0.7152 * lin_g + 0.0722 * lin_b,
    value_text_col = ifelse(lum < 0.25, "white", "black"),
    value_label = if_else(bar_type == "total", sprintf("%.1f", ymax), NA_character_)
  ) |>
  dplyr::select(-r, -g, -b, -lin_r, -lin_g, -lin_b, -lum)


# STEP 3: Panel-builder ---------------------------------------------------------
# Bottom-of-bar driver names (Population, GDP/P, ...) only appear once each --
# panel "a" (Biomass/median, first flow panel) for the 3 flow-shared drivers,
# panel "c" (Metal ores/median, first stock panel) for S/GDP and Secondary
# material -- since colour already ties every bar to its driver across all 12
# panels. Letter tag stays an in-panel annotate() (top-left corner, inside the
# plot area). Material title (row 1) and scenario label (column 1) use
# ggplot's OWN plot.title / axis.title.y slots instead of annotate() -- those
# reserve dedicated margin space outside the panel's data area, so they can't
# overlap the bars or get clipped by patchwork's tight grid the way a
# manually positioned Inf-anchored annotation can.

cat("STEP 3: Build panels\n")

build_cascade_panel <- function(df_panel, tag_letter, label_bar_keys, show_material_title, show_selection_label) {
  y_lims <- c(df_panel$col_y_min[1], df_panel$col_y_max[1])
  n_bars <- max(df_panel$bar_order)
  mat_colour <- unname(PALETTE_MATERIAL_GROUPS[as.character(df_panel$material_group[1])])
  sel_key <- as.character(df_panel$selection[1])

  p <- ggplot(df_panel) +
    geom_rect(
      aes(xmin = bar_order - 0.35, xmax = bar_order + 0.35, ymin = ymin, ymax = ymax, fill = fill_key),
      colour = "black",
      linewidth = 0.15
    ) +
    geom_text(
      aes(x = bar_order, y = label_y_top, label = pct_label),
      size = pb_annot_size("largeFont", 8), fontface = "italic", angle = 90, hjust = 0, vjust = 0.5
    ) +
    geom_text(
      data = df_panel |> filter(bar_type == "total"),
      aes(x = bar_order, y = (ymin + ymax) / 2, label = value_label, colour = value_text_col),
      size = pb_annot_size("largeFont", 7), angle = 90, fontface = "bold"
    ) +
    scale_fill_manual(values = CASCADE_PAL, guide = "none") +
    scale_colour_identity() +
    scale_x_continuous(
      breaks = c(1, n_bars),
      labels = c("2024", "2050"),
      limits = c(0.5, n_bars + 0.5),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = y_lims, clip = "off") +
    labs(
      x = NULL,
      y = if (show_selection_label) SELECTION_TITLE[[sel_key]] else NULL,
      title = if (show_material_title) as.character(df_panel$material_group[1]) else NULL
    ) +
    theme_pb_large() +
    theme(
      axis.text.x = element_text(colour = "black", angle = 0, size = 7),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = if (show_selection_label) {
        element_text(colour = SELECTION_COLORS[[sel_key]], face = "bold", size = 7)
      } else {
        element_blank()
      },
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.title = if (show_material_title) {
        element_text(colour = mat_colour, face = "bold", size = 8, hjust = 0.5, margin = margin(b = 4))
      } else {
        element_blank()
      },
      plot.margin = margin(t = 4, r = 3, b = 2, l = 3)
    ) +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = tag_letter,
      hjust = -0.3,
      vjust = 1.1,
      fontface = "bold",
      size = pb_annot_size("largeFont", 9)
    )

  if (length(label_bar_keys) > 0) {
    p <- p +
      geom_text(
      data = df_panel |> filter(bar_key %in% label_bar_keys),
      aes(x = bar_order, y = ymin, label = bar_label, colour = bar_colour_hex),
      size = pb_annot_size("largeFont", 7), angle = 90, hjust = 1, vjust = 0.5
    )
  }

  p
}

# Panel order (row-major, matches the ncol=4 wrap_plots grid below): row 1 =
# median (a-d), row 2 = top10 (e-h), row 3 = absdecoupling (i-l); within each
# row, columns are Biomass, Fossil fuels, Metal ores, Non-metallic minerals.
# Panel "a" (Biomass/median, first flow panel) carries ONLY the Population/
# GDP-P/M-GDP driver-name labels; panel "c" (Metal ores/median, first stock
# panel) carries ONLY S/GDP and Secondary material (not Population/GDP-P
# again -- those were already shown on panel a). Column 1 (Biomass) carries
# the rotated scenario-row label.
LABELS_PANEL_A <- c("population", "gdp_cap", "intensity")
LABELS_PANEL_C <- c("intensity", "secondary")
NO_LABELS <- character(0)

p_a <- build_cascade_panel(
  bars |> filter(material_group == "Biomass", selection == "median"),
  "a",
  LABELS_PANEL_A,
  TRUE,
  TRUE
)
p_b <- build_cascade_panel(
  bars |> filter(material_group == "Fossil fuels", selection == "median"),
  "b",
  NO_LABELS,
  TRUE,
  FALSE
)
p_c <- build_cascade_panel(
  bars |> filter(material_group == "Metal ores", selection == "median"),
  "c",
  LABELS_PANEL_C,
  TRUE,
  FALSE
)
p_d <- build_cascade_panel(
  bars |> filter(material_group == "Non-metallic minerals", selection == "median"),
  "d",
  NO_LABELS,
  TRUE,
  FALSE
)

p_e <- build_cascade_panel(
  bars |> filter(material_group == "Biomass", selection == "top10"),
  "e",
  NO_LABELS,
  FALSE,
  TRUE
)
p_f <- build_cascade_panel(
  bars |> filter(material_group == "Fossil fuels", selection == "top10"),
  "f",
  NO_LABELS,
  FALSE,
  FALSE
)
p_g <- build_cascade_panel(
  bars |> filter(material_group == "Metal ores", selection == "top10"),
  "g",
  NO_LABELS,
  FALSE,
  FALSE
)
p_h <- build_cascade_panel(
  bars |> filter(material_group == "Non-metallic minerals", selection == "top10"),
  "h",
  NO_LABELS,
  FALSE,
  FALSE
)

p_i <- build_cascade_panel(
  bars |> filter(material_group == "Biomass", selection == "absdecoupling"),
  "i",
  NO_LABELS,
  FALSE,
  TRUE
)
p_j <- build_cascade_panel(
  bars |> filter(material_group == "Fossil fuels", selection == "absdecoupling"),
  "j",
  NO_LABELS,
  FALSE,
  FALSE
)
p_k <- build_cascade_panel(
  bars |> filter(material_group == "Metal ores", selection == "absdecoupling"),
  "k",
  NO_LABELS,
  FALSE,
  FALSE
)
p_l <- build_cascade_panel(
  bars |> filter(material_group == "Non-metallic minerals", selection == "absdecoupling"),
  "l",
  NO_LABELS,
  FALSE,
  FALSE
)


# STEP 4: Assemble & save ---------------------------------------------------------

cat("STEP 4: Assemble & save\n")

fig <- patchwork::wrap_plots(p_a, p_b, p_c, p_d, p_e, p_f, p_g, p_h, p_i, p_j, p_k, p_l, ncol = 4) &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave("Figures/Other/Fig_Cascade.png", fig, units = "cm", dpi = 600, width = 18, height = 12)
ggsave("Figures/Other/SVG/Fig_Cascade.svg", fig, units = "cm", width = 18, height = 12)
clean_svg("Figures/Other/SVG/Fig_Cascade.svg")

cat("  Saved: Figures/Other/Fig_Cascade.png\n\n")
cat("=== Cascade done ===\n")

# EoF
