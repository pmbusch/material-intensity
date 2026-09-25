## =============================================================================
## 03_trajectory_figures.R
## Time series trajectory figures for the 10 focus ScenarioMIP variables
## (Coal/Gas/Oil extraction, Steel, Aluminum, Cement, Residential/Commercial
## floor space, Crop production, Roundwood), World only, x 3 versions each
## (raw total, intensity /GDP, per capita) = 30 figures, plus 3 food variables
## (Food Intake Crops/Livestock, Food Waste) per capita only. Plus a by-region
## variant of the intensity (/GDP) figure (and of the food per-capita figure):
## same design, one panel per IIASA R10 region + World, arranged in a grid.
##
## Each page: every (model, family, SSP) run plotted as its own line,
## coloured by SSP. Line width/alpha = "marker" (the IIASA-designated
## reference run for that family x SSP) EXCEPT when an SSP has only one run
## in the whole page -- that lone run is drawn at marker weight too, since a
## single thin/translucent line is otherwise nearly invisible next to SSPs
## with many overlapping runs (this was happening to SSP4: it had a real,
## complete, non-marker series, just only one of them). A run with only one
## year of data (not currently the case for World, may occur for some region
## x model x metric cells) is drawn as a point, since geom_line silently
## drops single-point groups.
## No legend -- SSP is direct-labelled next to the end-of-horizon range bars.
## Y-axis starts at 0 with zero expansion (scale_y_continuous(limits=c(0,NA),
## expand=c(0,0))) so the dashed RATIO_BASE_YEAR reference line and the
## secondary axis's own zero align exactly at the panel's bottom edge.
## A dashed grey horizontal line marks RATIO_BASE_YEAR (2025 -- NOT the plot's
## first year, BASE_YEAR=2020, which is excluded from all ratio/index
## calculations as an anomalous year), used by the secondary axis (value
## relative to RATIO_BASE_YEAR, cross-run median).
## At FORECAST_END, one vertical min-max range bar per SSP, past the last
## data year -- an SSP with only 1 run has ymin==ymax (a zero-height segment
## draws nothing), so that case is drawn as a plain point instead (this was
## SSP4's missing end-bar).
## Below the plot (side by side, together spanning the same width, compact
## rows), TWO tables:
##   Left (columns: N / Min / Max / CV%, 4 rows):
##     "All Runs" -- literal min/max of the ratio value_FORECAST_END /
##       value_RATIO_BASE_YEAR (1.10 = +10%) across EVERY individual run --
##       matches whatever the highest/lowest visible line actually does.
##     "SSP" / "Model" / "Scenario" -- the one-way MARGINAL spread of that
##       same ratio: average out the other two dimensions first, then
##       min/max/CV across the levels of the one dimension named in the row.
##       This can legitimately disagree with "All Runs" (it is an average of
##       averages, not any single run), which is why both are shown.
##   Right (columns: N / Min / Max / CV%, one row per SSP): the left table's
##     "SSP" row broken back out -- for each SSP, N/min/max/CV of its own
##     individual (unaveraged) runs, showing which SSP drives that row's spread.
## "Scenario" = the ScenarioMIP forcing family (policy narrative), distinct
## from the SSP socioeconomic narrative.
## GDP-intensity units: MJ/USD (fossil extraction), kg/USD (steel, aluminum,
## cement, crops), m2/USD (floor space) -- see 02_compute_metrics.R.
## See also 03b_trajectory_by_ssp_region.R: same intensity figure with the
## panel/colour roles swapped (panels = SSP, colour = region).
##
## Input:  Parameters/SSP_ScenarioMIP/metrics_levels_raw.csv (from 02_compute_metrics.R)
## Outputs:
##   Parameters/SSP_ScenarioMIP/trajectories/{metric}_{region}.csv -- tidy data behind each figure
##   Figures/IIASA/Trajectories/{Total,GDP,PerCapita}/{NN}_{var}.png       -- World-only
##   Figures/IIASA/Trajectories/{GDP,PerCapita}/ByRegion/{NN}_{var}.png    -- by-region grid
##   (summary_ratio_*.csv is now produced by 04_summary_ratio.R)
##   Figures/SVG/IIASA/Trajectories/... (same tree, SVG)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

BASE_YEAR <- 2020L # matches 01_load_and_check.R / 02_compute_metrics.R -- data/plot x-axis start
RATIO_BASE_YEAR <- 2025L # reference year for all ratio/index calcs (2020 excluded: an anomalous year)
OUT_DIR <- "Parameters/SSP_ScenarioMIP"
TRAJ_PARAM_DIR <- file.path(OUT_DIR, "trajectories")
FIG_DIR <- "Figures/IIASA/Trajectories"
FIG_DIR_SVG <- "Figures/SVG/IIASA/Trajectories"

dir.create(TRAJ_PARAM_DIR, showWarnings = FALSE, recursive = TRUE)


# Step 1: Load metric levels + units --------------------------------------------

cat("STEP 1: Load metric levels\n")

metrics_levels_raw <- read_csv(file.path(OUT_DIR, "metrics_levels_raw.csv"), show_col_types = FALSE)
metric_units <- read_csv(file.path(OUT_DIR, "metric_units.csv"), show_col_types = FALSE)

cat("  Rows:", nrow(metrics_levels_raw), "\n")


# Step 2: 9 focus variables (numbered so related ones sit together in a file ─
# listing: fossil extraction 1-3, production 4-6, floor space 7-8, agri 9) x
# 3 versions (total / gdp / pc) -> 27 World pages, + 9 by-region gdp pages ────

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

VERSIONS <- tibble::tribble(
  ~version, ~version_suffix, ~title_suffix, ~folder,
  "total", "_total", "", "Total",
  "gdp", "_gdp", " Intensity (/GDP)", "GDP",
  "pc", "_pc", " per Capita", "PerCapita"
)

metric_pages <- FOCUS_VARS %>%
  tidyr::crossing(VERSIONS) %>%
  mutate(
    metric = paste0(var, version_suffix),
    metric_label = paste0(var_label, title_suffix),
    filename = sprintf("%02d_%s", idx, var)
  ) %>%
  filter(metric %in% metric_units$metric) %>% # food_* exist per capita only
  left_join(metric_units, by = "metric") %>%
  arrange(idx, version)

cat("  World pages:", nrow(metric_pages), "\n")

REGION_ORDER <- c(
  "World", "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Pacific OECD (R10)", "Reforming Economies (R10)", "Rest of Asia (R10)"
)

# Manifest of every (metric, region) page to build: all metrics at World,
# plus the "_gdp" metrics (and the per-capita-only food_* metrics) repeated
# across every R10 region (World already covered by the first block -- it is
# reused as one of the by-region panels).
metric_pages <- metric_pages %>% mutate(by_region = version == "gdp" | str_starts(var, "food_"))

page_manifest <- bind_rows(
  metric_pages %>% mutate(region = "World"),
  metric_pages %>% filter(by_region) %>% tidyr::crossing(region = setdiff(REGION_ORDER, "World"))
)

cat("  Total (metric, region) pages to build:", nrow(page_manifest), "\n")


# Step 3: Shared plot constants ---------------------------------------------------

SSP_LEVELS <- names(SSP_COLORS)
END_YEAR <- max(metrics_levels_raw$year) # FORECAST_END, read off the data itself

pb_set_geom_defaults("wide")


# Step 4: Build every (metric, region) panel once; save World pages directly, ──
# and collect the "_gdp" regions into a grid figure per variable ────────────────

cat("\nSTEP 4: Build trajectory + spread-table panels\n")

for (met in unique(page_manifest$metric)) {
  pg <- metric_pages %>% filter(metric == met)
  regions_here <- page_manifest %>% filter(metric == met) %>% pull(region)
  panel_list <- list()

  for (reg in regions_here) {
    d <- metrics_levels_raw %>% filter(metric == met, region == reg)
    if (nrow(d) == 0) {
      cat("  [SKIP]", met, "|", reg, "-- no data\n")
      next
    }
    d <- d %>% mutate(ssp = factor(ssp, levels = SSP_LEVELS))
    if (reg == "World") write_csv(d, file.path(TRAJ_PARAM_DIR, paste0(met, "_world.csv")))

    # Emphasis: marker run, OR the only run reporting this SSP on this page
    # (otherwise indistinguishable from background noise -- e.g. SSP4 was a
    # real, complete series but the only one, drawn at non-marker weight).
    n_runs_ssp <- d %>% distinct(model, family, ssp) %>% count(ssp, name = "n_runs")
    d <- d %>%
      left_join(n_runs_ssp, by = "ssp") %>%
      mutate(emphasize = marker | n_runs == 1)

    # Runs with >1 year get a line; a run with exactly 1 year (geom_line
    # draws nothing for a single-point group) gets a point instead.
    n_years_run <- d %>% group_by(model, family, ssp) %>% mutate(n_years = n_distinct(year)) %>% ungroup()
    d_line <- n_years_run %>% filter(n_years > 1)
    d_point <- n_years_run %>% filter(n_years == 1)

    # Reference for the dashed RATIO_BASE_YEAR line and the secondary
    # (relative) axis: cross-run median RATIO_BASE_YEAR value (sec_axis must
    # be one linear transform of the primary axis, so a single constant is
    # used for both).
    ref_base <- median(d$value[d$year == RATIO_BASE_YEAR], na.rm = TRUE)

    # End-of-horizon (FORECAST_END) min-max range per SSP, for the end bars.
    # A range with only 1 run has ymin==ymax -- a zero-height geom_segment,
    # which draws nothing (this was SSP4's missing end-bar) -- so it is
    # split off and drawn as a point instead.
    end_by_ssp <- d %>%
      filter(year == END_YEAR) %>%
      group_by(ssp) %>%
      summarise(ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(x = END_YEAR + 2 + (as.integer(ssp) - 1) * 2.2)
    end_by_ssp_range <- end_by_ssp %>% filter(ymax > ymin)
    end_by_ssp_single <- end_by_ssp %>% filter(ymax == ymin)

    p_line <- ggplot(d, aes(year, value, colour = ssp, group = interaction(model, family, ssp))) +
      geom_hline(yintercept = ref_base, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
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
        sec.axis = sec_axis(
          as.formula(paste0("~ . / ", format(ref_base, digits = 17))), # value baked in: grids are built after the loop, when ref_base has moved on
          name = paste0("Index (", RATIO_BASE_YEAR, " = 1)"),
          labels = scales::label_number(accuracy = 0.1)
        )
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = pg$unit, title = if (reg == "World") pg$metric_label else paste0(pg$metric_label, " — ", reg)) +
      theme_pb_wide() +
      theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4))

    # RATIO_BASE_YEAR->FORECAST_END ratio per individual run, then two views:
    # "All Runs" = literal min/max across every run (matches what the eye
    # sees as the highest/lowest line on the plot); SSP/Model/Scenario =
    # one-way MARGINAL spread (average over the other two dimensions first,
    # then min/max/CV across the levels of the one dimension named in the
    # row) -- these two can legitimately disagree (a row's min/max is an
    # average-of-runs, not any single run's value), which is why both are
    # shown rather than only the marginal version.
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

    table_data <- tibble::tibble(
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
    ) %>%
      mutate(
        Min = sprintf("%.2f", Min),
        Max = sprintf("%.2f", Max),
        `CV%` = ifelse(is.na(CV_pct), "-", paste0(round(CV_pct, 1), "%"))
      ) %>%
      dplyr::select(Dimension, N, Min, Max, `CV%`)

    # Second table: the "SSP" row above collapsed to one marginal number per
    # SSP (averaged over model/family first); this breaks it back out to one
    # row per SSP, same columns, using the individual (unaveraged) runs
    # within that SSP -- shows which SSP actually drives the "SSP" row's spread.
    table_data2 <- rel_runs %>%
      group_by(ssp) %>%
      summarise(N = n(), Min = min(rel), Max = max(rel), CV_pct = sd(rel) / mean(rel) * 100, .groups = "drop") %>%
      mutate(
        Min = sprintf("%.2f", Min),
        Max = sprintf("%.2f", Max),
        `CV%` = ifelse(is.na(CV_pct), "-", paste0(round(CV_pct, 1), "%"))
      ) %>%
      transmute(SSP = ssp, N, Min, Max, `CV%`)

    # Ratio relative to RATIO_BASE_YEAR (1.10 = +10% by FORECAST_END), not %,
    # so trend comparisons read directly as multipliers. Compact row height
    # (padding); the two tables sit side by side BELOW the plot, together
    # spanning the same width as the plot, rather than beside it.
    TBL_THEME <- gridExtra::ttheme_minimal(
      base_size = 7,
      padding = grid::unit(c(2, 1.5), "mm"),
      core = list(fg_params = list(fontsize = 7, col = "#222222")),
      colhead = list(fg_params = list(fontsize = 7, fontface = "bold", col = "#222222"))
    )
    tbl_grob <- gridExtra::tableGrob(table_data, rows = NULL, theme = TBL_THEME)
    tbl_grob2 <- gridExtra::tableGrob(table_data2, rows = NULL, theme = TBL_THEME)

    p_combined <- (p_line / (patchwork::wrap_elements(full = tbl_grob) | patchwork::wrap_elements(full = tbl_grob2))) +
      patchwork::plot_layout(heights = c(3.3, 1.2)) +
      patchwork::plot_annotation(
        caption = paste0(
          "Min/Max/CV%: ", RATIO_BASE_YEAR, "->", END_YEAR, " ratio (value", END_YEAR, "/value", RATIO_BASE_YEAR,
          "). Left table: 'All Runs' = literal extremes across every run; SSP/Model/Scenario = one-way marginal ",
          "spread (each holding the other two fixed by averaging first), so its min/max need not match any single ",
          "run. Right table: the 'SSP' row broken back out to one row per SSP, using its individual (unaveraged) runs."
        ),
        theme = theme(plot.caption = element_text(size = 7, hjust = 0, colour = "#666666"))
      ) &
      theme(plot.background = element_rect(fill = "transparent", color = NA))

    panel_list[[reg]] <- p_combined

    if (reg == "World") {
      out_dir_v <- file.path(FIG_DIR, pg$folder)
      out_dir_v_svg <- file.path(FIG_DIR_SVG, pg$folder)
      dir.create(out_dir_v, showWarnings = FALSE, recursive = TRUE)
      dir.create(out_dir_v_svg, showWarnings = FALSE, recursive = TRUE)

      ggsave(file.path(out_dir_v, paste0(pg$filename, ".png")), p_combined, units = "cm", dpi = 600, width = 17, height = 12)
      ggsave(file.path(out_dir_v_svg, paste0(pg$filename, ".svg")), p_combined, units = "cm", width = 17, height = 12)
      group_svg_layers(file.path(out_dir_v_svg, paste0(pg$filename, ".svg")))
    }

    cat(
      "  [OK]", met, "|", reg, "| runs:", n_distinct(paste(d$model, d$family, d$ssp)),
      "| models:", n_distinct(d$model), "| families:", n_distinct(d$family), "\n"
    )
  }

  # By-region grid: only for the "_gdp" version (+ per-capita food_*), only
  # when more than the World panel was actually built (some region x metric
  # cells may be empty). Saved under {GDP,PerCapita}/ByRegion.
  if (pg$by_region && length(panel_list) > 1) {
    ordered_regions <- REGION_ORDER[REGION_ORDER %in% names(panel_list)]
    GRID_NCOL <- 4L
    p_grid <- patchwork::wrap_plots(panel_list[ordered_regions], ncol = GRID_NCOL) +
      patchwork::plot_annotation(
        title = paste0(unique(pg$metric_label), " by Region"),
        theme = theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5))
      )

    out_dir_r <- file.path(FIG_DIR, pg$folder, "ByRegion")
    out_dir_r_svg <- file.path(FIG_DIR_SVG, pg$folder, "ByRegion")
    dir.create(out_dir_r, showWarnings = FALSE, recursive = TRUE)
    dir.create(out_dir_r_svg, showWarnings = FALSE, recursive = TRUE)

    n_rows <- ceiling(length(ordered_regions) / GRID_NCOL)
    ggsave(
      file.path(out_dir_r, paste0(unique(pg$filename), ".png")), p_grid,
      units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 9.5 * n_rows
    )
    ggsave(
      file.path(out_dir_r_svg, paste0(unique(pg$filename), ".svg")), p_grid,
      units = "cm", width = 11 * GRID_NCOL, height = 9.5 * n_rows
    )
    group_svg_layers(file.path(out_dir_r_svg, paste0(unique(pg$filename), ".svg")))

    cat("  [OK] by-region grid:", met, "|", length(ordered_regions), "regions\n")
  }
}


# Summary ratio CSV moved to 04_summary_ratio.R (same inputs/ratio definition,
# plus the >10 World-substitution and SSP4<-SSP2 bound fixes).

cat("\nDone ->", FIG_DIR, "\n")

# EoF
