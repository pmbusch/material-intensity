# A compact custom ggplot2 theme based on common settings
# Usage:
#   library(ggplot2)
#   source("R/theme_pmb.R")
#   ggplot(...) + ... + theme_pmb("small")   # for 8 x 8 cm figures
#   ggplot(...) + ... + theme_pmb("wide")    # for 18 x 8 cm figures
#
# Also see recommended geom and ggsave settings below.
theme_pb <- function(
  preset = c("small", "wide", "largeFont"),
  base_family = "",
  text_col = "#222222",
  panel_border_col = "black",
  panel_background = "transparent"
) {
  preset <- match.arg(preset)

  # Font floor: NOTHING renders below this (incl. annotations).
  # Max/min ratio kept < 1.5 by construction.
  sz_min <- switch(preset, small = 7, wide = 7, largeFont = 8)

  sz_axis_text <- sz_min # tick labels
  sz_legend_text <- sz_min
  sz_caption <- sz_min # was base_size - 2: too small, removed
  sz_axis_title <- sz_min + 1
  sz_strip_text <- sz_min + 1
  sz_subtitle <- sz_min + 1
  sz_plot_tag <- round(sz_min * 1.3) # bold
  sz_plot_title <- round(sz_min * 1.4) # bold; ratio 1.4 < 1.5

  ggplot2::theme_bw(base_size = sz_axis_title, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = text_col, family = base_family),

      plot.title = ggplot2::element_text(size = sz_plot_title, face = "bold", colour = text_col, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = sz_subtitle, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = sz_caption, hjust = 1, colour = text_col, lineheight = 0.9),
      plot.tag = ggplot2::element_text(size = sz_plot_tag, face = "bold"),
      plot.background = ggplot2::element_rect(fill = panel_background, color = NA),

      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = panel_background, colour = panel_border_col),
      panel.border = ggplot2::element_rect(colour = panel_border_col, fill = NA),

      axis.title = ggplot2::element_text(size = sz_axis_title, colour = text_col),
      axis.text = ggplot2::element_text(size = sz_axis_text, colour = text_col),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),

      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text = ggplot2::element_text(
        size = sz_strip_text,
        colour = text_col,
        margin = ggplot2::margin(t = 2, b = 2),
        face = "bold"
      ),

      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.text = ggplot2::element_text(size = sz_legend_text, colour = text_col),
      legend.title = ggplot2::element_text(size = sz_legend_text),
      legend.box.spacing = ggplot2::unit(0.1, "cm"),
      legend.key.height = ggplot2::unit(0.25, "cm"),
      legend.key.width = ggplot2::unit(0.25, "cm"),

      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4, unit = "pt")
    )
}

# ---- Annotation sizes (geom_text / geom_label / ggrepel) ----
# ggplot geoms use mm, themes use pt. Convert with .pt (≈ 2.845).
# CRITERION: annotation size >= font floor of the preset, <= axis title size.
pb_annot_size <- function(preset = c("small", "wide", "largeFont"), pt = NULL) {
  preset <- match.arg(preset)
  floor_pt <- switch(preset, small = 7, wide = 7, largeFont = 8)
  pt <- if (is.null(pt)) floor_pt else max(pt, floor_pt) # never below floor
  pt / ggplot2::.pt
}

# Set once per script so all geom_text/geom_label default to the floor:
pb_set_geom_defaults <- function(preset = c("small", "wide", "largeFont")) {
  s <- pb_annot_size(preset)
  ggplot2::update_geom_defaults("text", list(size = s))
  ggplot2::update_geom_defaults("label", list(size = s))
  invisible(s)
}

theme_pb_small <- function(...) theme_pb(preset = "small", ...)
theme_pb_wide <- function(...) theme_pb(preset = "wide", ...)
theme_pb_large <- function(...) theme_pb(preset = "largeFont", ...)
pb_set_geom_defaults("largeFont")
