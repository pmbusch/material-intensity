## =============================================================================
## 04_summary_ratio.R
## Summary of the RATIO_BASE_YEAR->END_YEAR ratio (value2060 / value2025) of
## every GDP-intensity ("_gdp") metric, per (Variable, Region, SSP), across
## every individual run -- literal min/max + 90% CI (5th/95th percentile).
## Moved out of 03_trajectory_figures.R: same input (metrics_levels_raw.csv),
## same years and same ratio definition (runs with value_base <= 0 dropped),
## so it stays consistent with the per-panel tables in 03/03b/03c/03d (those
## tables keep showing the RAW, unfixed ratios).
##
## Two fixes applied to the summary (raw values kept in Min_raw/Max_raw, every
## touched cell marked in the Fix column):
##   1. Tiny-base blow-ups: a regional cell whose Max ratio > THRESHOLD (10)
##      -- typically a near-zero 2025 value (e.g. Pacific OECD coal) -- has
##      its Min/Max/CI90 replaced by the World cell of the same Variable x SSP
##      ("world_substituted"). A World cell itself > THRESHOLD is only flagged
##      ("world_above_threshold") and reported in the console.
##   2. SSP4 has a single run (one model, one scenario): its Min/Max/CI90 are
##      borrowed from the SSP2 cell of the same Variable x Region, AFTER fix 1
##      ("ssp2_bounds"). N keeps SSP4's own run count.
##
## Figure: horizontal min-max range bars of the fixed ratio for a selected set
## of /GDP variables, one panel per variable (4x2 grid, common x scale); y axis
## = SSP, each a tight cluster (World = thick black bar, then R10 regions as
## thin coloured bars). Regions labelled directly in the roundwood SSP3 cluster.
##
## Input:  Parameters/SSP_ScenarioMIP/metrics_levels_raw.csv, metric_units.csv (from 02)
## Outputs:
##   Parameters/SSP_ScenarioMIP/summary_ratio_{END_YEAR}_{RATIO_BASE_YEAR}.csv
##   Figures/IIASA/Trajectories/GDP/SummaryRatio/summary_ratio_{END_YEAR}_{RATIO_BASE_YEAR}.png (+ SVG mirror)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

RATIO_BASE_YEAR <- 2025L # reference year for all ratio/index calcs (2020 excluded: an anomalous year)
THRESHOLD <- 10 # regional Max ratio above this -> replaced by the World cell
OUT_DIR <- "Parameters/SSP_ScenarioMIP"
FIG_DIR <- "Figures/IIASA/Trajectories/GDP/SummaryRatio"
FIG_DIR_SVG <- "Figures/SVG/IIASA/Trajectories/GDP/SummaryRatio"

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR_SVG, showWarnings = FALSE, recursive = TRUE)


# Step 1: Load metric levels, keep GDP-intensity metrics -------------------------

cat("STEP 1: Load metric levels\n")

metrics_levels_raw <- read_csv(file.path(OUT_DIR, "metrics_levels_raw.csv"), show_col_types = FALSE)
metric_units <- read_csv(file.path(OUT_DIR, "metric_units.csv"), show_col_types = FALSE)

END_YEAR <- max(metrics_levels_raw$year) # FORECAST_END, read off the data itself
GDP_METRICS <- metric_units$metric[str_ends(metric_units$metric, "_gdp")]
cat("  /GDP metrics:", length(GDP_METRICS), "\n")


# Step 2: Raw min/max/90%CI of the ratio per (Variable, Region, SSP) ----------

cat("\nSTEP 2: Raw ratio summary\n")

summary_base <- metrics_levels_raw %>%
  filter(metric %in% GDP_METRICS, year == RATIO_BASE_YEAR) %>%
  dplyr::select(metric, region, model, family, ssp, value_base = value)

summary_raw <- metrics_levels_raw %>%
  filter(metric %in% GDP_METRICS, year == END_YEAR) %>%
  dplyr::select(metric, region, model, family, ssp, value_end = value) %>%
  inner_join(summary_base, by = c("metric", "region", "model", "family", "ssp")) %>%
  filter(value_base > 0) %>%
  mutate(rel = value_end / value_base) %>%
  group_by(metric, region, ssp) %>%
  summarise(
    N = n(),
    Min = min(rel), Max = max(rel),
    CI90_Lo = quantile(rel, 0.05, names = FALSE), CI90_Hi = quantile(rel, 0.95, names = FALSE),
    .groups = "drop"
  ) %>%
  mutate(Min_raw = Min, Max_raw = Max)

cat("  Cells:", nrow(summary_raw), "| regional cells with Max >", THRESHOLD, ":",
    sum(summary_raw$region != "World" & summary_raw$Max > THRESHOLD), "\n")


# Step 3: Fix 1 -- regional Max > THRESHOLD -> World cell (same Variable x SSP)

cat("\nSTEP 3: Replace regional cells with Max ratio >", THRESHOLD, "by the World cell\n")

world_cells <- summary_raw %>%
  filter(region == "World") %>%
  dplyr::select(metric, ssp, W_Min = Min, W_Max = Max, W_CI90_Lo = CI90_Lo, W_CI90_Hi = CI90_Hi)

summary_fix <- summary_raw %>%
  left_join(world_cells, by = c("metric", "ssp")) %>%
  mutate(
    sub_world = region != "World" & Max > THRESHOLD & !is.na(W_Max),
    Fix = case_when(
      sub_world & W_Max > THRESHOLD ~ "world_substituted; world_above_threshold",
      sub_world ~ "world_substituted",
      region != "World" & Max > THRESHOLD ~ "above_threshold_no_world_cell",
      region == "World" & Max > THRESHOLD ~ "world_above_threshold",
      TRUE ~ NA_character_
    ),
    Min = if_else(sub_world, W_Min, Min),
    Max = if_else(sub_world, W_Max, Max),
    CI90_Lo = if_else(sub_world, W_CI90_Lo, CI90_Lo),
    CI90_Hi = if_else(sub_world, W_CI90_Hi, CI90_Hi)
  ) %>%
  dplyr::select(-starts_with("W_"), -sub_world)

cat("  Substituted cells:", sum(str_detect(summary_fix$Fix, "world_substituted"), na.rm = TRUE), "\n")
print(
  summary_fix %>% filter(str_detect(Fix, "world_substituted")) %>%
    dplyr::select(metric, region, ssp, N, Max_raw, Max) %>% as.data.frame()
)

world_flag <- summary_fix %>% filter(region == "World", Max > THRESHOLD)
if (nrow(world_flag) > 0) {
  cat("\n[FLAG] World cells with Max ratio >", THRESHOLD, "(NOT replaced -- please review):\n")
  print(world_flag %>% dplyr::select(metric, ssp, N, Min, Max) %>% as.data.frame())
} else {
  cat("  No World cell above", THRESHOLD, "\n")
}


# Step 4: Fix 2 -- SSP4 (single run) borrows SSP2 bounds (same Variable x Region)

cat("\nSTEP 4: SSP4 <- SSP2 bounds\n")

ssp2_cells <- summary_fix %>%
  filter(ssp == "SSP2") %>%
  dplyr::select(metric, region, S2_Min = Min, S2_Max = Max, S2_CI90_Lo = CI90_Lo, S2_CI90_Hi = CI90_Hi, S2_Fix = Fix)

summary_fix <- summary_fix %>%
  left_join(ssp2_cells, by = c("metric", "region")) %>%
  mutate(
    borrow = ssp == "SSP4" & !is.na(S2_Max),
    Fix = case_when(
      borrow & !is.na(S2_Fix) ~ paste0("ssp2_bounds (SSP2: ", S2_Fix, ")"),
      borrow ~ "ssp2_bounds",
      TRUE ~ Fix
    ),
    Min = if_else(borrow, S2_Min, Min),
    Max = if_else(borrow, S2_Max, Max),
    CI90_Lo = if_else(borrow, S2_CI90_Lo, CI90_Lo),
    CI90_Hi = if_else(borrow, S2_CI90_Hi, CI90_Hi)
  )

cat("  SSP4 cells borrowing SSP2 bounds:", sum(summary_fix$borrow), "of", sum(summary_fix$ssp == "SSP4"), "\n")

summary_ratio <- summary_fix %>%
  transmute(Variable = metric, Region = region, SSP = ssp, N, Min, Max, CI90_Lo, CI90_Hi, Min_raw, Max_raw, Fix) %>%
  arrange(Variable, Region, SSP)

summary_file <- file.path(OUT_DIR, paste0("summary_ratio_", END_YEAR, "_", RATIO_BASE_YEAR, ".csv"))
write_csv(summary_ratio, summary_file)
cat("  Saved", summary_file, "(", nrow(summary_ratio), "rows )\n")


# Step 5: Range figure -- selected /GDP variables, semantic groups = columns ---

cat("\nSTEP 5: Summary ratio range figure\n")

FIG_VARS <- tibble::tribble(
  ~group, ~var, ~var_label,
  "Fossil fuels", "coal_extraction", "Coal extraction",
  "Fossil fuels", "gas_extraction", "Gas extraction",
  "Fossil fuels", "oil_extraction", "Oil extraction",
  "Materials", "steel", "Steel production",
  "Materials", "cement", "Cement production",
  "Biomass", "agri_crops", "Crop production",
  "Biomass", "agri_livestock", "Livestock production",
  "Biomass", "forestry_production", "Roundwood production"
) %>%
  mutate(Variable = paste0(var, "_gdp"), group = factor(group, levels = unique(group))) %>%
  group_by(group) %>%
  mutate(k = row_number()) %>%
  ungroup()

REGION_ORDER <- c(
  "World", "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Pacific OECD (R10)", "Reforming Economies (R10)", "Rest of Asia (R10)"
)

# Sort R10 regions by GDP/cap (median across runs, RATIO_BASE_YEAR), richest first; World stays on top
gdppc_order <- metrics_levels_raw %>%
  filter(metric == "gdp_pc", year == RATIO_BASE_YEAR, region %in% REGION_ORDER[-1]) %>%
  group_by(region) %>%
  summarise(gdppc = median(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(gdppc))
REGION_ORDER <- c("World", gdppc_order$region)
print(as.data.frame(gdppc_order))

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

SSP_LEVELS <- names(SSP_COLORS)

pb_set_geom_defaults("wide")

# Row positions (y, top-down): one panel per variable, one tight cluster per
# SSP centred on y = s (World first, then R10 regions), REGION_STEP apart.
REGION_STEP <- 0.045 # cluster height = 10 * 0.045 = 0.45 -> 0.55 gap between SSPs
LABEL_VAR <- "forestry_production_gdp" # panel carrying the direct region labels
LABEL_SSP <- "SSP3" # SSP cluster labelled within that panel

fig_data <- summary_ratio %>%
  inner_join(FIG_VARS, by = "Variable") %>%
  filter(Region %in% REGION_ORDER) %>%
  mutate(
    s = match(SSP, SSP_LEVELS),
    r = match(Region, REGION_ORDER),
    y = s + (r - (length(REGION_ORDER) + 1) / 2) * REGION_STEP,
    is_world = Region == "World",
    var_label = factor(var_label, levels = FIG_VARS$var_label)
  )

fig_region_labels <- fig_data %>%
  filter(Variable == LABEL_VAR, SSP == LABEL_SSP) %>%
  mutate(
    label = str_remove(Region, " \\(R10\\)"),
    lab_size = if_else(is_world, pb_annot_size("wide", 8), pb_annot_size("wide", 7))
  )

# Representative run per SSP: IIASA marker scenario; SSP2 has 3 markers -> keep
# the Medium one; SSP4 has no marker -> its single run. Cells substituted by the
# World range (tiny-base blow-ups) get no marker.
MARKER_FAMILY_SSP2 <- "Medium"

fig_marker <- metrics_levels_raw %>%
  filter(
    metric %in% FIG_VARS$Variable, region %in% REGION_ORDER, year %in% c(RATIO_BASE_YEAR, END_YEAR),
    marker | ssp == "SSP4", !(ssp == "SSP2" & family != MARKER_FAMILY_SSP2)
  ) %>%
  dplyr::select(metric, region, ssp, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "v") %>%
  mutate(rel = .data[[paste0("v", END_YEAR)]] / .data[[paste0("v", RATIO_BASE_YEAR)]]) %>%
  filter(.data[[paste0("v", RATIO_BASE_YEAR)]] > 0) %>%
  inner_join(fig_data, by = c("metric" = "Variable", "region" = "Region", "ssp" = "SSP")) %>%
  filter(!str_detect(coalesce(Fix, ""), "world_substituted")) %>%
  mutate(pt_size = if_else(is_world, 1.6, 1.0))

X_MAX <- 4 # fixed zoom via coord_cartesian: bars beyond it are clipped, not dropped

ggplot(fig_data) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_segment(
    data = fig_data %>% filter(Max > Min),
    aes(x = Min, xend = Max, y = y, yend = y, colour = Region, linewidth = is_world)
  ) +
  geom_point(
    data = fig_data %>% filter(Max == Min),
    aes(x = Min, y = y, colour = Region, size = is_world)
  ) +
  geom_point(
    data = fig_marker, aes(x = rel, y = y, colour = region),
    shape = 21, fill = "white", stroke = 0.5, size = fig_marker$pt_size
  ) +
  ggrepel::geom_text_repel(
    data = fig_region_labels, aes(x = Max, y = y, label = label, colour = Region),
    hjust = 0, nudge_x = 2.5 - fig_region_labels$Max, direction = "y", fontface = "bold",
    size = fig_region_labels$lab_size, segment.size = 0.2, segment.colour = "grey60",
    min.segment.length = 0, box.padding = 0.05, point.padding = 0, max.overlaps = Inf, seed = 1
  ) +
  scale_colour_manual(values = PALETTE_R10, guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.35), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.5), guide = "none") +
  scale_x_continuous(breaks = 0:X_MAX, labels = scales::label_number(accuracy = 1)) +
  scale_y_reverse(breaks = seq_along(SSP_LEVELS), labels = SSP_LEVELS) +
  coord_cartesian(xlim = c(0, X_MAX), ylim = c(length(SSP_LEVELS) + 0.4, 0.6), expand = FALSE) +
  facet_wrap(~var_label, ncol = 2, nrow = 4) +
  labs(
    x = paste0("Ratio ", END_YEAR, " / ", RATIO_BASE_YEAR, " of material intensity (min–max across runs)"), y = NULL,
    caption = paste0(
      "Bars: min–max of the per-run ratio. Per SSP: World (thick black) then R10 regions.\n",
      "Open circles: marker scenario (SSP2: ", MARKER_FAMILY_SSP2, " marker; SSP4: its single run).\n",
      "Dots: single value (min = max). Dashed vertical line: no change (ratio = 1)."
    )
  ) +
  theme_pb_wide() +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    plot.caption = element_text(size = 7, hjust = 0, colour = "#666666"),
    plot.margin = margin(t = 2, r = 4, b = 4, l = 4)
  )

fig_name <- paste0("summary_ratio_", END_YEAR, "_", RATIO_BASE_YEAR)
ggsave(file.path(FIG_DIR, paste0(fig_name, ".png")), ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 23)
ggsave(file.path(FIG_DIR_SVG, paste0(fig_name, ".svg")), ggplot2::last_plot(), units = "cm", width = 17, height = 23)
group_svg_layers(file.path(FIG_DIR_SVG, paste0(fig_name, ".svg")))

cat("  Saved", file.path(FIG_DIR, paste0(fig_name, ".png")), "\n")

# EoF
