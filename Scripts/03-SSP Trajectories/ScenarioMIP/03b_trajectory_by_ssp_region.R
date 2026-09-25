## =============================================================================
## 03b_trajectory_by_ssp_region.R
## v2 companion to 03_trajectory_figures.R's by-region grid: same intensity
## (/GDP) figure, but with the two roles swapped -- panels = SSP (5, arranged
## as a 3x2 grid), and within each panel every (model, family, region) run is
## plotted as its own line coloured by REGION (World + IIASA R10), with
## end-of-horizon range bars/direct labels also coloured by region. One
## figure per focus variable (10 /GDP + 3 food per-capita = 13 total).
##
## Design is otherwise identical to 03_trajectory_figures.R: dashed
## RATIO_BASE_YEAR (2025, not 2020 -- see BASE_YEAR/RATIO_BASE_YEAR note
## there) line = secondary-axis reference; y-axis starts at 0 with zero
## expansion; table stacked below the plot at the same width; a region's
## end-of-horizon range with only 1 run (ymin==ymax, a zero-height segment)
## is drawn as a plain point instead. The table's rows change to match what
## actually varies WITHIN an SSP panel: "All Runs" (literal min/max across
## every run) / Model / Scenario family / Region (there is no SSP row here
## -- SSP is the facet, fixed within a panel).
##
## Input:  Parameters/SSP_ScenarioMIP/metrics_levels_raw.csv (from 02_compute_metrics.R)
## Outputs:
##   Figures/IIASA/Trajectories/GDP/BySSP/{NN}_{var}.png (+ SVG mirror)
##   Figures/IIASA/Trajectories/PerCapita/BySSP/{NN}_food_*.png (+ SVG mirror)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

BASE_YEAR <- 2020L # matches 01_load_and_check.R / 02_compute_metrics.R -- data/plot x-axis start
RATIO_BASE_YEAR <- 2025L # reference year for all ratio/index calcs (2020 excluded: an anomalous year)
OUT_DIR <- "Parameters/SSP_ScenarioMIP"


# Step 1: Load metric levels + units, focus variables (GDP-intensity only) ----

cat("STEP 1: Load metric levels\n")

metrics_levels_raw <- read_csv(file.path(OUT_DIR, "metrics_levels_raw.csv"), show_col_types = FALSE)
metric_units <- read_csv(file.path(OUT_DIR, "metric_units.csv"), show_col_types = FALSE)

FOCUS_VARS <- tibble::tribble(
  ~idx, ~var, ~var_label,
  1L, "coal_extraction", "Coal Extraction",
  2L, "gas_extraction", "Gas Extraction",
  3L, "oil_extraction", "Oil Extraction",
  4L, "steel", "Steel Production",
  5L, "aluminum", "Aluminum Production",
  6L, "cement", "Cement Production",
  7L, "floor_res", "Residential Floor Space",
  8L, "floor_com", "Commercial Floor Space",
  9L, "agri_crops", "Crop Production",
  10L, "forestry_production", "Roundwood Production",
  11L, "food_crops", "Food Intake (Crops)",
  12L, "food_livestock", "Food Intake (Livestock)",
  13L, "food_waste", "Food Waste",
  14L, "agri_livestock", "Livestock Production"
)

# food_* exist per capita only -> "_pc" metric, saved under PerCapita/BySSP
metric_pages <- FOCUS_VARS %>%
  mutate(
    is_food = str_starts(var, "food_"),
    metric = paste0(var, if_else(is_food, "_pc", "_gdp")),
    metric_label = paste0(var_label, if_else(is_food, " per Capita", " Intensity (/GDP)")),
    fig_dir = file.path("Figures/IIASA/Trajectories", if_else(is_food, "PerCapita", "GDP"), "BySSP"),
    fig_dir_svg = file.path("Figures/SVG/IIASA/Trajectories", if_else(is_food, "PerCapita", "GDP"), "BySSP"),
    filename = sprintf("%02d_%s", idx, var)
  ) %>%
  left_join(metric_units, by = "metric") %>%
  arrange(idx)

cat("  Pages:", nrow(metric_pages), "\n")


# Step 2: Region palette + order (World + IIASA R10), short end-bar labels ---

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


# Step 3: One 5-panel (SSP1..SSP5) figure per focus variable ------------------

cat("\nSTEP 3: Build SSP-panelled, region-coloured figures\n")

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

    # Emphasis / point-fallback logic mirrors 03_trajectory_figures.R, keyed
    # on REGION instead of SSP: the only run reporting a given region on this
    # panel is drawn at marker weight so it isn't lost.
    n_runs_region <- d %>% distinct(model, family, region) %>% count(region, name = "n_runs")
    d <- d %>%
      left_join(n_runs_region, by = "region") %>%
      mutate(emphasize = marker | n_runs == 1)

    n_years_run <- d %>% group_by(model, family, region) %>% mutate(n_years = n_distinct(year)) %>% ungroup()
    d_line <- n_years_run %>% filter(n_years > 1)
    d_point <- n_years_run %>% filter(n_years == 1)

    ref_base <- median(d$value[d$year == RATIO_BASE_YEAR], na.rm = TRUE)

    # A range with only 1 run has ymin==ymax -- a zero-height geom_segment
    # draws nothing -- so it is split off and drawn as a point instead.
    end_by_region <- d %>%
      filter(year == END_YEAR) %>%
      group_by(region) %>%
      summarise(ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(x = END_YEAR + 2 + (as.integer(region) - 1) * 1.3, label = REGION_SHORT[as.character(region)])
    end_by_region_range <- end_by_region %>% filter(ymax > ymin)
    end_by_region_single <- end_by_region %>% filter(ymax == ymin)

    p_line <- ggplot(d, aes(year, value, colour = region, group = interaction(model, family, region))) +
      geom_hline(yintercept = ref_base, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
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
        sec.axis = sec_axis(
          as.formula(paste0("~ . / ", format(ref_base, digits = 17))), # value baked in: grids are built after the loop, when ref_base has moved on
          name = paste0("Index (", RATIO_BASE_YEAR, " = 1)"),
          labels = scales::label_number(accuracy = 0.1)
        )
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = pg$unit, title = this_ssp) +
      theme_pb_wide() +
      theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4))

    # RATIO_BASE_YEAR->FORECAST_END ratio per run: "All Runs" = literal
    # min/max across every run (matches the visible line extremes); Model /
    # Scenario family / Region = one-way MARGINAL spread (average out the
    # other two dimensions first). SSP is fixed within this panel, so unlike
    # 03_trajectory_figures.R's table it is not one of the rows here.
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

    table_data <- tibble::tibble(
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
    ) %>%
      mutate(
        Min = sprintf("%.2f", Min),
        Max = sprintf("%.2f", Max),
        `CV%` = ifelse(is.na(CV_pct), "-", paste0(round(CV_pct, 1), "%"))
      ) %>%
      dplyr::select(Dimension, N, Min, Max, `CV%`)

    tbl_grob <- gridExtra::tableGrob(
      table_data, rows = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = 7,
        padding = grid::unit(c(2, 1.5), "mm"),
        core = list(fg_params = list(fontsize = 7, col = "#222222")),
        colhead = list(fg_params = list(fontsize = 7, fontface = "bold", col = "#222222"))
      )
    )

    panel_list[[this_ssp]] <- (p_line / patchwork::wrap_elements(full = tbl_grob)) +
      patchwork::plot_layout(heights = c(3.3, 1.2))

    cat(
      "  [OK]", met, "|", this_ssp, "| runs:", n_distinct(paste(d$model, d$family, d$region)),
      "| models:", n_distinct(d$model), "| families:", n_distinct(d$family), "| regions:", n_distinct(d$region), "\n"
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

  dir.create(pg$fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(pg$fig_dir_svg, showWarnings = FALSE, recursive = TRUE)
  ggsave(
    file.path(pg$fig_dir, paste0(pg$filename, ".png")), p_grid,
    units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 9.5 * n_rows
  )
  ggsave(
    file.path(pg$fig_dir_svg, paste0(pg$filename, ".svg")), p_grid,
    units = "cm", width = 11 * GRID_NCOL, height = 9.5 * n_rows
  )
  group_svg_layers(file.path(pg$fig_dir_svg, paste0(pg$filename, ".svg")))

  cat("  [OK] figure:", met, "|", length(panel_list), "SSP panels\n")
}

cat("\nDone -> Figures/IIASA/Trajectories/{GDP,PerCapita}/BySSP\n")

# EoF
