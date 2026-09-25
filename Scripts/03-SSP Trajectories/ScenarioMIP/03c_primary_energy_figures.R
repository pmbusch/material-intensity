## =============================================================================
## 03c_primary_energy_figures.R
## Primary energy intensity (/GDP, MJ/USD) trajectory figures, one per
## primary energy source, using the same panel design as
## 03_trajectory_figures.R (runs coloured by SSP, marker emphasis, dashed
## RATIO_BASE_YEAR reference + index secondary axis, end-of-horizon min-max
## bar per SSP, two ratio tables below) and 03b_trajectory_by_ssp_region.R
## (panels = SSP, colour = region). GDP-intensity only.
##
## Per source:
##   World figure                 -> PrimaryEnergy/{NN}_{var}.png
##   By-region grid (R10 + World) -> PrimaryEnergy/ByRegion/{NN}_{var}.png
##   By-SSP grid (colour=region)  -> PrimaryEnergy/BySSP/{NN}_{var}.png
## Plus one large figure with every source's World panel:
##                                -> PrimaryEnergy/00_all_sources.png
## Aggregates (Fossil, Non-Biomass Renewables) are placed last; Ocean is
## reported by a single model.
##
## Input:  Parameters/SSP_ScenarioMIP/metrics_levels_raw.csv (from 02_compute_metrics.R)
## Outputs:
##   Figures/IIASA/Trajectories/GDP/PrimaryEnergy/... (+ Figures/SVG mirror)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

BASE_YEAR <- 2020L # matches 01_load_and_check.R / 02_compute_metrics.R -- data/plot x-axis start
RATIO_BASE_YEAR <- 2025L # reference year for all ratio/index calcs (2020 excluded: an anomalous year)
OUT_DIR <- "Parameters/SSP_ScenarioMIP"
FIG_DIR <- "Figures/IIASA/Trajectories/GDP/PrimaryEnergy"
FIG_DIR_SVG <- "Figures/SVG/IIASA/Trajectories/GDP/PrimaryEnergy"

for (dd in c(FIG_DIR, FIG_DIR_SVG)) {
  dir.create(file.path(dd, "ByRegion"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(dd, "BySSP"), showWarnings = FALSE, recursive = TRUE)
}


# Step 1: Load metric levels + units --------------------------------------------

cat("STEP 1: Load metric levels\n")

metrics_levels_raw <- read_csv(file.path(OUT_DIR, "metrics_levels_raw.csv"), show_col_types = FALSE)
metric_units <- read_csv(file.path(OUT_DIR, "metric_units.csv"), show_col_types = FALSE)


# Step 2: Primary energy sources (semantic order: total, fossil, nuclear, ----
# renewables, other, then aggregates) ---------------------------------------------

SOURCE_VARS <- tibble::tribble(
  ~idx, ~var, ~var_label,
  1L, "pe", "Total Primary Energy",
  2L, "pe_coal", "Coal",
  3L, "pe_oil", "Oil",
  4L, "pe_gas", "Gas",
  5L, "pe_nuclear", "Nuclear",
  6L, "pe_biomass", "Biomass",
  7L, "pe_hydro", "Hydro",
  8L, "pe_geothermal", "Geothermal",
  9L, "pe_ocean", "Ocean (1 model)",
  10L, "pe_other", "Other",
  11L, "pe_fossil", "Fossil (aggregate)",
  12L, "pe_nonbio_renew", "Non-Biomass Renewables (aggregate)"
)

metric_pages <- SOURCE_VARS %>%
  mutate(
    metric = paste0(var, "_gdp"),
    metric_label = paste0("Primary Energy: ", var_label, " (/GDP)"),
    filename = sprintf("%02d_%s", idx, var)
  ) %>%
  left_join(metric_units, by = "metric") %>%
  arrange(idx)

REGION_ORDER <- c(
  "World", "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Pacific OECD (R10)", "Reforming Economies (R10)", "Rest of Asia (R10)"
)

# fmt: skip
PALETTE_R10 <- c(
  "World"                      = "#000000",
  "Africa (R10)"                = "#EE7733",
  "China+ (R10)"                = "#CC3311",
  "Europe (R10)"                = "#0077BB",
  "India+ (R10)"                = "#AA3377",
  "Latin America (R10)"         = "#228B22",
  "Middle East (R10)"           = "#CCBB44",
  "North America (R10)"         = "#004488",
  "Pacific OECD (R10)"          = "#33BBEE",
  "Reforming Economies (R10)"   = "#882255",
  "Rest of Asia (R10)"          = "#44AA99"
)

REGION_SHORT <- c(
  "World" = "Wld", "Africa (R10)" = "Afr", "China+ (R10)" = "Chn+", "Europe (R10)" = "Eur",
  "India+ (R10)" = "Ind+", "Latin America (R10)" = "LAm", "Middle East (R10)" = "MEast",
  "North America (R10)" = "NAm", "Pacific OECD (R10)" = "Pac", "Reforming Economies (R10)" = "RefEc",
  "Rest of Asia (R10)" = "RoAsia"
)

SSP_LEVELS <- names(SSP_COLORS)
END_YEAR <- max(metrics_levels_raw$year) # FORECAST_END, read off the data itself

pb_set_geom_defaults("wide")

TBL_THEME <- gridExtra::ttheme_minimal(
  base_size = 7,
  padding = grid::unit(c(2, 1.5), "mm"),
  core = list(fg_params = list(fontsize = 7, col = "#222222")),
  colhead = list(fg_params = list(fontsize = 7, fontface = "bold", col = "#222222"))
)

TABLE_CAPTION <- paste0(
  "Min/Max/CV%: ", RATIO_BASE_YEAR, "->", END_YEAR, " ratio (value", END_YEAR, "/value", RATIO_BASE_YEAR,
  "). Left table: 'All Runs' = literal extremes across every run; SSP/Model/Scenario = one-way marginal ",
  "spread (each holding the other two fixed by averaging first), so its min/max need not match any single ",
  "run. Right table: the 'SSP' row broken back out to one row per SSP, using its individual (unaveraged) runs."
)


# Step 3: World + by-region panels per source (03_trajectory_figures.R design) --

cat("\nSTEP 3: World + by-region panels\n")

world_panels <- list()

for (i in seq_len(nrow(metric_pages))) {
  pg <- metric_pages[i, ]
  met <- pg$metric
  panel_list <- list()

  for (reg in REGION_ORDER) {
    d <- metrics_levels_raw %>% filter(metric == met, region == reg)
    if (nrow(d) == 0) {
      cat("  [SKIP]", met, "|", reg, "-- no data\n")
      next
    }
    d <- d %>% mutate(ssp = factor(ssp, levels = SSP_LEVELS))

    # Emphasis: marker run, OR the only run reporting this SSP on this page
    n_runs_ssp <- d %>% distinct(model, family, ssp) %>% count(ssp, name = "n_runs")
    d <- d %>%
      left_join(n_runs_ssp, by = "ssp") %>%
      mutate(emphasize = marker | n_runs == 1)

    # Runs with >1 year get a line; a single-year run gets a point instead
    n_years_run <- d %>% group_by(model, family, ssp) %>% mutate(n_years = n_distinct(year)) %>% ungroup()
    d_line <- n_years_run %>% filter(n_years > 1)
    d_point <- n_years_run %>% filter(n_years == 1)

    # Cross-run median RATIO_BASE_YEAR value: dashed reference + secondary
    # axis constant. A source absent in the base year (median 0) gets no
    # secondary axis (a /0 transform is undefined).
    ref_base <- median(d$value[d$year == RATIO_BASE_YEAR], na.rm = TRUE)
    has_ref <- is.finite(ref_base) && ref_base > 0

    # End-of-horizon min-max range per SSP; single-run SSPs drawn as a point
    end_by_ssp <- d %>%
      filter(year == END_YEAR) %>%
      group_by(ssp) %>%
      summarise(ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(x = END_YEAR + 2 + (as.integer(ssp) - 1) * 2.2)
    end_by_ssp_range <- end_by_ssp %>% filter(ymax > ymin)
    end_by_ssp_single <- end_by_ssp %>% filter(ymax == ymin)

    p_line <- ggplot(d, aes(year, value, colour = ssp, group = interaction(model, family, ssp))) +
      geom_hline(yintercept = if (has_ref) ref_base else NA_real_, linetype = "dashed", colour = "grey50", linewidth = 0.3, na.rm = TRUE) +
      geom_line(data = d_line, aes(linewidth = emphasize, alpha = emphasize)) +
      geom_point(data = d_point, aes(alpha = emphasize), size = 1.3) +
      geom_segment(
        data = end_by_ssp_range, aes(x = x, xend = x, y = ymin, yend = ymax, colour = ssp),
        inherit.aes = FALSE, linewidth = 1.1, lineend = "round"
      ) +
      geom_point(
        data = end_by_ssp_single, aes(x = x, y = ymax, colour = ssp),
        inherit.aes = FALSE, size = 2.2
      ) +
      geom_text(
        data = end_by_ssp, aes(x = x, y = ymax, label = ssp, colour = ssp),
        inherit.aes = FALSE, angle = 90, hjust = 0, vjust = 0.5, fontface = "bold",
        size = pb_annot_size("wide", 7)
      ) +
      scale_colour_manual(values = SSP_COLORS, guide = "none") +
      scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.4), guide = "none") +
      scale_alpha_manual(values = c(`TRUE` = 0.95, `FALSE` = 0.4), guide = "none") +
      scale_x_continuous(
        breaks = seq(BASE_YEAR, END_YEAR, by = 10),
        limits = c(BASE_YEAR, max(end_by_ssp$x) + 2), expand = expansion(mult = c(0.02, 0.01))
      ) +
      scale_y_continuous(
        labels = scales::label_comma(),
        limits = c(0, NA), expand = expansion(mult = c(0, 0)),
        sec.axis = if (has_ref) {
          sec_axis(as.formula(paste0("~ . / ", format(ref_base, digits = 17))), name = paste0("Index (", RATIO_BASE_YEAR, " = 1)"), labels = scales::label_number(accuracy = 0.1))
        } else {
          waiver()
        }
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = pg$unit, title = if (reg == "World") pg$metric_label else paste0(pg$metric_label, " — ", reg)) +
      theme_pb_wide() +
      theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4))

    # RATIO_BASE_YEAR->END_YEAR ratio per run: "All Runs" literal extremes,
    # SSP/Model/Scenario one-way marginal spread (see 03_trajectory_figures.R)
    base_runs <- d %>% filter(year == RATIO_BASE_YEAR) %>% dplyr::select(model, family, ssp, value_base = value)
    rel_runs <- d %>%
      filter(year == END_YEAR) %>%
      dplyr::select(model, family, ssp, value_end = value) %>%
      inner_join(base_runs, by = c("model", "family", "ssp")) %>%
      filter(value_base > 0) %>%
      mutate(rel = value_end / value_base)

    by_ssp <- rel_runs %>% group_by(ssp) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")
    by_model <- rel_runs %>% group_by(model) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")
    by_family <- rel_runs %>% group_by(family) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")

    # suppressWarnings: min/max of an empty set (no run with a positive base) -> "-"
    table_data <- suppressWarnings(tibble::tibble(
      Dimension = c("All Runs", "SSP", "Model", "Scenario"),
      N = c(nrow(rel_runs), nrow(by_ssp), nrow(by_model), nrow(by_family)),
      Min = c(min(rel_runs$rel), min(by_ssp$v), min(by_model$v), min(by_family$v)),
      Max = c(max(rel_runs$rel), max(by_ssp$v), max(by_model$v), max(by_family$v)),
      CV_pct = c(
        sd(rel_runs$rel) / mean(rel_runs$rel) * 100,
        sd(by_ssp$v) / mean(by_ssp$v) * 100,
        sd(by_model$v) / mean(by_model$v) * 100,
        sd(by_family$v) / mean(by_family$v) * 100
      )
    )) %>%
      mutate(
        Min = ifelse(is.finite(Min), sprintf("%.2f", Min), "-"),
        Max = ifelse(is.finite(Max), sprintf("%.2f", Max), "-"),
        `CV%` = ifelse(is.finite(CV_pct), paste0(round(CV_pct, 1), "%"), "-")
      ) %>%
      dplyr::select(Dimension, N, Min, Max, `CV%`)

    table_data2 <- rel_runs %>%
      group_by(ssp) %>%
      summarise(N = n(), Min = min(rel), Max = max(rel), CV_pct = sd(rel) / mean(rel) * 100, .groups = "drop") %>%
      mutate(
        Min = sprintf("%.2f", Min),
        Max = sprintf("%.2f", Max),
        `CV%` = ifelse(is.finite(CV_pct), paste0(round(CV_pct, 1), "%"), "-")
      ) %>%
      transmute(SSP = as.character(ssp), N, Min, Max, `CV%`)
    if (nrow(table_data2) == 0) table_data2 <- tibble::tibble(SSP = "-", N = 0L, Min = "-", Max = "-", `CV%` = "-")

    tbl_grob <- gridExtra::tableGrob(table_data, rows = NULL, theme = TBL_THEME)
    tbl_grob2 <- gridExtra::tableGrob(table_data2, rows = NULL, theme = TBL_THEME)

    p_panel <- (p_line / (patchwork::wrap_elements(full = tbl_grob) | patchwork::wrap_elements(full = tbl_grob2))) +
      patchwork::plot_layout(heights = c(3.3, 1.2))
    panel_list[[reg]] <- p_panel

    if (reg == "World") {
      world_panels[[met]] <- p_panel
      p_world <- p_panel +
        patchwork::plot_annotation(
          caption = TABLE_CAPTION,
          theme = theme(plot.caption = element_text(size = 7, hjust = 0, colour = "#666666"))
        ) &
        theme(plot.background = element_rect(fill = "transparent", color = NA))

      ggsave(file.path(FIG_DIR, paste0(pg$filename, ".png")), p_world, units = "cm", dpi = 600, width = 17, height = 12)
      ggsave(file.path(FIG_DIR_SVG, paste0(pg$filename, ".svg")), p_world, units = "cm", width = 17, height = 12)
      group_svg_layers(file.path(FIG_DIR_SVG, paste0(pg$filename, ".svg")))
    }

    cat(
      "  [OK]", met, "|", reg, "| runs:", n_distinct(paste(d$model, d$family, d$ssp)),
      "| models:", n_distinct(d$model), "| families:", n_distinct(d$family), "\n"
    )
  }

  # By-region grid (only when more than the World panel was built)
  if (length(panel_list) > 1) {
    ordered_regions <- REGION_ORDER[REGION_ORDER %in% names(panel_list)]
    GRID_NCOL <- 4L
    n_rows <- ceiling(length(ordered_regions) / GRID_NCOL)
    p_grid <- patchwork::wrap_plots(panel_list[ordered_regions], ncol = GRID_NCOL) +
      patchwork::plot_annotation(
        title = paste0(pg$metric_label, " by Region"),
        caption = TABLE_CAPTION,
        theme = theme(
          plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 7, hjust = 0, colour = "#666666")
        )
      )

    ggsave(
      file.path(FIG_DIR, "ByRegion", paste0(pg$filename, ".png")), p_grid,
      units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 9.5 * n_rows
    )
    ggsave(
      file.path(FIG_DIR_SVG, "ByRegion", paste0(pg$filename, ".svg")), p_grid,
      units = "cm", width = 11 * GRID_NCOL, height = 9.5 * n_rows
    )
    group_svg_layers(file.path(FIG_DIR_SVG, "ByRegion", paste0(pg$filename, ".svg")))

    cat("  [OK] by-region grid:", met, "|", length(ordered_regions), "regions\n")
  }
}


# Step 4: Large figure -- every source's World panel ------------------------------

cat("\nSTEP 4: All-sources World figure\n")

ordered_mets <- metric_pages$metric[metric_pages$metric %in% names(world_panels)]
GRID_NCOL <- 4L
n_rows <- ceiling(length(ordered_mets) / GRID_NCOL)
p_all <- patchwork::wrap_plots(world_panels[ordered_mets], ncol = GRID_NCOL) +
  patchwork::plot_annotation(
    title = "Primary Energy Intensity (/GDP) by Source — World",
    caption = TABLE_CAPTION,
    theme = theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 7, hjust = 0, colour = "#666666")
    )
  )

ggsave(file.path(FIG_DIR, "00_all_sources.png"), p_all, units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 9.5 * n_rows)
ggsave(file.path(FIG_DIR_SVG, "00_all_sources.svg"), p_all, units = "cm", width = 11 * GRID_NCOL, height = 9.5 * n_rows)
group_svg_layers(file.path(FIG_DIR_SVG, "00_all_sources.svg"))
cat("  [OK] all-sources figure:", length(ordered_mets), "panels\n")


# Step 5: By-SSP grid per source (03b_trajectory_by_ssp_region.R design) ------

cat("\nSTEP 5: SSP-panelled, region-coloured figures\n")

for (i in seq_len(nrow(metric_pages))) {
  pg <- metric_pages[i, ]
  met <- pg$metric
  d_all <- metrics_levels_raw %>%
    filter(metric == met, region %in% REGION_ORDER) %>%
    mutate(region = factor(region, levels = REGION_ORDER), ssp = factor(ssp, levels = SSP_LEVELS))

  if (nrow(d_all) == 0) {
    cat("  [SKIP]", met, "-- no data\n")
    next
  }

  panel_list <- list()

  for (this_ssp in SSP_LEVELS) {
    d <- d_all %>% filter(ssp == this_ssp) %>% droplevels()
    if (nrow(d) == 0) {
      cat("  [SKIP]", met, "|", this_ssp, "-- no data\n")
      next
    }

    # Emphasis / point fallback keyed on REGION (see 03b)
    n_runs_region <- d %>% distinct(model, family, region) %>% count(region, name = "n_runs")
    d <- d %>%
      left_join(n_runs_region, by = "region") %>%
      mutate(emphasize = marker | n_runs == 1)

    n_years_run <- d %>% group_by(model, family, region) %>% mutate(n_years = n_distinct(year)) %>% ungroup()
    d_line <- n_years_run %>% filter(n_years > 1)
    d_point <- n_years_run %>% filter(n_years == 1)

    ref_base <- median(d$value[d$year == RATIO_BASE_YEAR], na.rm = TRUE)
    has_ref <- is.finite(ref_base) && ref_base > 0

    end_by_region <- d %>%
      filter(year == END_YEAR) %>%
      group_by(region) %>%
      summarise(ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(x = END_YEAR + 2 + (as.integer(region) - 1) * 1.3, label = REGION_SHORT[as.character(region)])
    end_by_region_range <- end_by_region %>% filter(ymax > ymin)
    end_by_region_single <- end_by_region %>% filter(ymax == ymin)

    p_line <- ggplot(d, aes(year, value, colour = region, group = interaction(model, family, region))) +
      geom_hline(yintercept = if (has_ref) ref_base else NA_real_, linetype = "dashed", colour = "grey50", linewidth = 0.3, na.rm = TRUE) +
      geom_line(data = d_line, aes(linewidth = emphasize, alpha = emphasize)) +
      geom_point(data = d_point, aes(alpha = emphasize), size = 1.1) +
      geom_segment(
        data = end_by_region_range, aes(x = x, xend = x, y = ymin, yend = ymax, colour = region),
        inherit.aes = FALSE, linewidth = 0.9, lineend = "round"
      ) +
      geom_point(
        data = end_by_region_single, aes(x = x, y = ymax, colour = region),
        inherit.aes = FALSE, size = 2.0
      ) +
      geom_text(
        data = end_by_region, aes(x = x, y = ymax, label = label, colour = region),
        inherit.aes = FALSE, angle = 90, hjust = 0, vjust = 0.5, fontface = "bold",
        size = pb_annot_size("wide", 7)
      ) +
      scale_colour_manual(values = PALETTE_R10, guide = "none") +
      scale_linewidth_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.35), guide = "none") +
      scale_alpha_manual(values = c(`TRUE` = 0.95, `FALSE` = 0.4), guide = "none") +
      scale_x_continuous(
        breaks = seq(BASE_YEAR, END_YEAR, by = 10),
        limits = c(BASE_YEAR, max(end_by_region$x) + 1.5), expand = expansion(mult = c(0.02, 0.01))
      ) +
      scale_y_continuous(
        labels = scales::label_comma(),
        limits = c(0, NA), expand = expansion(mult = c(0, 0)),
        sec.axis = if (has_ref) {
          sec_axis(as.formula(paste0("~ . / ", format(ref_base, digits = 17))), name = paste0("Index (", RATIO_BASE_YEAR, " = 1)"), labels = scales::label_number(accuracy = 0.1))
        } else {
          waiver()
        }
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = pg$unit, title = this_ssp) +
      theme_pb_wide() +
      theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4))

    base_runs <- d %>% filter(year == RATIO_BASE_YEAR) %>% dplyr::select(model, family, region, value_base = value)
    rel_runs <- d %>%
      filter(year == END_YEAR) %>%
      dplyr::select(model, family, region, value_end = value) %>%
      inner_join(base_runs, by = c("model", "family", "region")) %>%
      filter(value_base > 0) %>%
      mutate(rel = value_end / value_base)

    by_model <- rel_runs %>% group_by(model) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")
    by_family <- rel_runs %>% group_by(family) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")
    by_region <- rel_runs %>% group_by(region) %>% summarise(v = mean(rel, na.rm = TRUE), .groups = "drop")

    table_data <- suppressWarnings(tibble::tibble(
      Dimension = c("All Runs", "Model", "Scenario", "Region"),
      N = c(nrow(rel_runs), nrow(by_model), nrow(by_family), nrow(by_region)),
      Min = c(min(rel_runs$rel), min(by_model$v), min(by_family$v), min(by_region$v)),
      Max = c(max(rel_runs$rel), max(by_model$v), max(by_family$v), max(by_region$v)),
      CV_pct = c(
        sd(rel_runs$rel) / mean(rel_runs$rel) * 100,
        sd(by_model$v) / mean(by_model$v) * 100,
        sd(by_family$v) / mean(by_family$v) * 100,
        sd(by_region$v) / mean(by_region$v) * 100
      )
    )) %>%
      mutate(
        Min = ifelse(is.finite(Min), sprintf("%.2f", Min), "-"),
        Max = ifelse(is.finite(Max), sprintf("%.2f", Max), "-"),
        `CV%` = ifelse(is.finite(CV_pct), paste0(round(CV_pct, 1), "%"), "-")
      ) %>%
      dplyr::select(Dimension, N, Min, Max, `CV%`)

    tbl_grob <- gridExtra::tableGrob(table_data, rows = NULL, theme = TBL_THEME)

    panel_list[[this_ssp]] <- (p_line / patchwork::wrap_elements(full = tbl_grob)) +
      patchwork::plot_layout(heights = c(3.3, 1.2))

    cat(
      "  [OK]", met, "|", this_ssp, "| runs:", n_distinct(paste(d$model, d$family, d$region)),
      "| models:", n_distinct(d$model), "| regions:", n_distinct(d$region), "\n"
    )
  }

  if (length(panel_list) == 0) next

  ordered_ssps <- SSP_LEVELS[SSP_LEVELS %in% names(panel_list)]
  GRID_NCOL <- 3L
  n_rows <- ceiling(length(ordered_ssps) / GRID_NCOL)
  p_grid <- patchwork::wrap_plots(panel_list[ordered_ssps], ncol = GRID_NCOL) +
    patchwork::plot_annotation(
      title = paste0(pg$metric_label, " by SSP (colour = Region)"),
      caption = paste0(
        "Min/Max/CV%: ", RATIO_BASE_YEAR, "->", END_YEAR, " ratio (value", END_YEAR, "/value", RATIO_BASE_YEAR,
        "). 'All Runs' = literal extremes across every run; Model/Scenario/Region = one-way marginal spread ",
        "(each holding the other two fixed by averaging first), so its min/max need not match any single run."
      ),
      theme = theme(
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 7, hjust = 0, colour = "#666666")
      )
    )

  ggsave(
    file.path(FIG_DIR, "BySSP", paste0(pg$filename, ".png")), p_grid,
    units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 9.5 * n_rows
  )
  ggsave(
    file.path(FIG_DIR_SVG, "BySSP", paste0(pg$filename, ".svg")), p_grid,
    units = "cm", width = 11 * GRID_NCOL, height = 9.5 * n_rows
  )
  group_svg_layers(file.path(FIG_DIR_SVG, "BySSP", paste0(pg$filename, ".svg")))

  cat("  [OK] by-SSP figure:", met, "|", length(panel_list), "SSP panels\n")
}

cat("\nDone ->", FIG_DIR, "\n")

# EoF
