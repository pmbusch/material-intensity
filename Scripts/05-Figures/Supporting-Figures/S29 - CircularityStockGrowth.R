## S29 - CircularityStockGrowth.R  -> Figures/Fig4 - CircularityStockGrowth.png (MAIN Figure 4; was SI S29)
## Degree of circularity vs. recovery, 2060, one column per material class:
##   X = share of end-of-life waste recovered (recycled / downcycled)
##       = (secondary + spilled) / waste = (waste - not recovered) / waste
##   Y = share of supply met by secondary material = secondary / (primary + secondary)
## One observation = one MC run, global (summed over regions and end uses);
## metals = Fe + Cu + Al + other non-Fe on pure-metal mass (*_Mt_pure; gold ore is a
## flow and is left out), so low-grade non-Fe ore
## does not dominate. Dashed diagonal Y = X: no stock growth (inflow = outflow,
## all recovered material absorbed); runs fall below it when stocks grow (inflow >
## waste) or when recovered material cannot be absorbed (minerals' room caps).
## Rows drawn: all runs (median + P2.5-P97.5 band, no run points), then median lines by end use.
## Also computed and saved to RawData/fig4_runs.csv (not drawn):
##   - stock-weighted mean lifetime (terciles; sampled lifetimes, weighted by the
##     run's own 2060 in-use stock per end use; power sector excluded: fixed lifetimes)
##   - GDP growth (ssp_label: SSP closest to the run's world GDP/cap growth)
##   - stock intensity (terciles of the realized 2060 S/G: world in-use stock /
##     the run's reconstructed world GDP, as in Figure 3)
## End use, "own loop": each end use's own recovered waste (pool rate x its
##     own waste) over its own supply -- the model pools recovered material per
##     region and reallocates it, so the realized per-end-use secondary share
##     would not reflect the end use's own waste. Minerals: only the end uses
##     that give and take downcycled material (buildings, civil, roads, power).
## Medians: X binned in BIN_W steps, bins with >= MIN_N runs, line through each
## bin's (median X, median Y).

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8") # LIFETIME_SAMPLE_PARAMS, LIFETIME_MIN, POWER_LIFE_CLASSES

pb_set_geom_defaults("wide")

cat("=== S29 - Circularity vs stock growth ===\n\n")

# Parameters ------------------------------------------------------------

FIG_YEAR <- 2060L
BIN_W <- 0.05 # X bin width for the median lines
MIN_N <- 15 # minimum runs per bin
MAT_LEVELS <- c("Metal ores", "Non-metallic minerals")
ROW_LEVELS <- c("All runs", "End use (own loop)") # rows drawn; lifetime / GDP / stock-intensity groupings are kept in RawData/fig4_runs.csv only
TERCILE_COLS <- c("#4B0055", "#00588B", "#009B95") # viridis-derived, low -> high

# sub_use key -> end-use label as stored in mc_results$material_key (03-RunSimulations.R)
SUB_USE_KEY_LABELS <- c(
  "residential" = "Residential",
  "non_residential" = "Non-residential",
  "roads" = "Roads",
  "civil_engineering" = "Civil engineering",
  "machinery_group" = "Machinery",
  "vehicles_group" = "Vehicles",
  "durables" = "Durables",
  "packaging" = "Packaging"
)
# End uses that give and receive downcycled minerals (03-RunSimulations.R allocate_eol)
MINERAL_RECOVERY_USES <- c("Residential", "Non-residential", "Roads", "Civil engineering", POWER_LIFE_CLASSES$label)


# Load data -------------------------------------------------------------

results_fy <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year == FIG_YEAR, material_group %in% STOCK_GROUPS) |>
  dplyr::select(
    run_id, region, material_group, material_key, ssp_label, ssp_lo, ssp_hi, ssp_share_lo, in_use_stock_Mt,
    primary_consumption_Mt_pure, secondary_supply_Mt_pure, waste_Mt_pure, not_recovered_Mt_pureMetal
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    material = dplyr::if_else(material_group == "nonmetallic_minerals", "Non-metallic minerals", "Metal ores"),
    waste = pmax(0, waste_Mt_pure),
    recovered = pmax(0, waste_Mt_pure - not_recovered_Mt_pureMetal), # secondary + spilled
    supply = primary_consumption_Mt_pure + secondary_supply_Mt_pure,
    end_use = dplyr::if_else(grepl("^Power", material_key), "Power sector", material_key)
  )
mc_input_matrix <- readr::read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
ssp_drivers <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)
gdp_region_hist <- readr::read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)


# Per-run circularity (global) ------------------------------------------

circ_run <- results_fy |>
  dplyr::group_by(run_id, material) |>
  dplyr::summarise(
    x = sum(recovered) / sum(waste),
    y = sum(secondary_supply_Mt_pure) / sum(supply),
    .groups = "drop"
  )

ssp_run <- results_fy |> dplyr::distinct(run_id, ssp_label)


# Grouping: stock-weighted mean lifetime --------------------------------
# Sampled mean lifetime per sub_use (same decoding as 03-RunSimulations.R /
# Figure 3), weighted by the run's own 2060 in-use stock of that end use

lifetime_dr <- mc_input_matrix |>
  dplyr::select(run_id, dplyr::matches("^lifetime_(mean|k)_")) |>
  tidyr::pivot_longer(-run_id, names_to = c("param", "super_cat"), names_pattern = "^lifetime_(mean|k)_(.+)") |>
  tidyr::pivot_wider(names_from = param, values_from = value) |>
  dplyr::rename(u_mean = mean, u_k = k) |>
  dplyr::left_join(LIFETIME_SAMPLE_PARAMS, by = c("super_cat" = "super_category"), relationship = "many-to-many") |> # several sub_uses per super_category
  dplyr::mutate(
    mean_life = pmax(LIFETIME_MIN, mean_life_min + u_mean * (mean_life_max - mean_life_min)),
    end_use = unname(SUB_USE_KEY_LABELS[sub_use])
  ) |>
  dplyr::select(run_id, end_use, mean_life)

lifetime_run <- results_fy |>
  dplyr::group_by(run_id, material, end_use) |>
  dplyr::summarise(stock = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(lifetime_dr, by = c("run_id", "end_use")) |>
  dplyr::group_by(run_id, material) |>
  dplyr::summarise(mean_life = stats::weighted.mean(mean_life, stock), .groups = "drop")


# Grouping: realized 2060 stock intensity --------------------------------
# World GDP at FIG_YEAR per run, same reconstruction as Figure 3 / 05-Decoupling.R:
# regional GDP_2024 x blended population index x blended GDP/cap index

gdp_2024_region <- gdp_region_hist |>
  dplyr::filter(year == 2024) |>
  dplyr::select(region = Region, gdp_2024 = GDP_PPP2017USD)
idx_fy <- ssp_drivers |>
  dplyr::filter(variable %in% c("Population", "GDP|PPP [per capita]"), year == FIG_YEAR) |>
  dplyr::mutate(variable = dplyr::if_else(variable == "Population", "pop_idx", "gdppc_idx")) |>
  dplyr::select(scenario, region, variable, index) |>
  tidyr::pivot_wider(names_from = variable, values_from = index)

world_gdp_run <- results_fy |>
  dplyr::distinct(run_id, region, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::left_join(idx_fy |> dplyr::rename(ssp_lo = scenario, pop_lo = pop_idx, gdppc_lo = gdppc_idx), by = c("ssp_lo", "region")) |>
  dplyr::left_join(idx_fy |> dplyr::rename(ssp_hi = scenario, pop_hi = pop_idx, gdppc_hi = gdppc_idx), by = c("ssp_hi", "region")) |>
  dplyr::left_join(gdp_2024_region, by = "region") |>
  dplyr::mutate(
    gdp_usd = gdp_2024 *
      (ssp_share_lo * pop_lo + (1 - ssp_share_lo) * pop_hi) *
      (ssp_share_lo * gdppc_lo + (1 - ssp_share_lo) * gdppc_hi)
  ) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(gdp_usd = sum(gdp_usd), .groups = "drop")

# S/G (kg of in-use stock per $ GDP) = world stock / world GDP
stock_int_run <- results_fy |>
  dplyr::group_by(run_id, material) |>
  dplyr::summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(world_gdp_run, by = "run_id") |>
  dplyr::transmute(run_id, material, sg_kg_usd = stock_Mt * 1e9 / gdp_usd)


# Long table: one row per run x material x panel row -------------------

run_groups <- circ_run |>
  dplyr::left_join(ssp_run, by = "run_id") |>
  dplyr::left_join(lifetime_run, by = c("run_id", "material")) |>
  dplyr::left_join(stock_int_run, by = c("run_id", "material")) |>
  dplyr::group_by(material) |>
  dplyr::mutate(life_t = dplyr::ntile(mean_life, 3), sg_t = dplyr::ntile(sg_kg_usd, 3)) |>
  dplyr::group_by(material, life_t) |>
  dplyr::mutate(life_lab = paste0(c("Short", "Mid", "Long")[life_t], " (", round(min(mean_life)), "–", round(max(mean_life)), " yr)")) |>
  dplyr::group_by(material, sg_t) |>
  dplyr::mutate(sg_lab = paste0(c("Low", "Mid", "High")[sg_t], " (", signif(min(sg_kg_usd), 2), "–", signif(max(sg_kg_usd), 2), " kg/$)")) |>
  dplyr::ungroup()

plot_long <- dplyr::bind_rows(
  run_groups |> dplyr::transmute(run_id, material, x, y, row = "All runs", group = material, col = unname(PALETTE_MATERIAL_GROUPS[material])),
  run_groups |> dplyr::transmute(run_id, material, x, y, row = "Mean lifetime", group = life_lab, col = TERCILE_COLS[life_t]),
  run_groups |> dplyr::transmute(run_id, material, x, y, row = "GDP growth", group = ssp_label, col = unname(SSP_COLORS[ssp_label])),
  run_groups |> dplyr::transmute(run_id, material, x, y, row = "Stock intensity", group = sg_lab, col = TERCILE_COLS[sg_t])
)


# End use, own loop -----------------------------------------------------
# Pool rate per run x region x material group = recovered / eligible waste;
# each end use's own recovered waste = pool rate x its own waste

eu_run <- results_fy |>
  dplyr::mutate(eligible = material_group != "nonmetallic_minerals" | material_key %in% MINERAL_RECOVERY_USES) |>
  dplyr::group_by(run_id, region, material_group) |>
  dplyr::mutate(pool_rate = sum(recovered) / sum(waste[eligible])) |>
  dplyr::ungroup() |>
  dplyr::filter(eligible) |>
  dplyr::mutate(own_recovered = dplyr::coalesce(pool_rate, 0) * waste) |>
  dplyr::group_by(run_id, material, end_use) |>
  dplyr::summarise(
    x = sum(own_recovered) / sum(waste),
    y = sum(own_recovered) / sum(supply),
    waste_Mt = sum(waste),
    supply_Mt = sum(supply),
    .groups = "drop"
  ) |>
  dplyr::filter(is.finite(x), is.finite(y))

plot_long <- dplyr::bind_rows(
  plot_long,
  eu_run |> dplyr::transmute(run_id, material, x, y, row = "End use (own loop)", group = end_use, col = unname(PALETTE_SUBENDUSE[end_use]))
) |>
  dplyr::filter(row %in% ROW_LEVELS) |>
  dplyr::mutate(
    material = factor(material, levels = MAT_LEVELS),
    row = factor(row, levels = ROW_LEVELS)
  )


# Binned medians ----------------------------------------------------------

med_df <- plot_long |>
  dplyr::mutate(bin = floor(pmin(x, 0.9999) / BIN_W)) |>
  dplyr::group_by(material, row, group, col, bin) |>
  dplyr::summarise(
    n = dplyr::n(),
    x_med = stats::median(x),
    y_med = stats::median(y),
    y_p25 = stats::quantile(y, 0.25),
    y_p75 = stats::quantile(y, 0.75),
    y_p025 = stats::quantile(y, 0.025),
    y_p975 = stats::quantile(y, 0.975),
    .groups = "drop"
  ) |>
  dplyr::filter(n >= MIN_N) |>
  dplyr::arrange(material, row, group, x_med)

# Direct labels at the right end of each line
lab_df <- med_df |>
  dplyr::group_by(material, row, group, col) |>
  dplyr::slice_max(x_med, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

# End-use labels carry the central mean lifetime (MC_Assumptions.xlsx "Lifetimes";
# power sector: range of its fixed finite lifetimes, hydro/nuclear never retire)
life_central <- dplyr::bind_rows(
  LIFETIME_SAMPLE_PARAMS |> dplyr::transmute(group = unname(SUB_USE_KEY_LABELS[sub_use]), life_txt = paste0(round(mean_life, ifelse(mean_life < 10, 1, 0)), " yr")),
  tibble::tibble(
    group = "Power sector",
    life_txt = paste0(min(POWER_LIFE_CLASSES$mean_life), "–", max(POWER_LIFE_CLASSES$mean_life[POWER_LIFE_CLASSES$mean_life < 1e3]), " yr")
  )
)
lab_df <- lab_df |>
  dplyr::left_join(life_central, by = "group") |>
  dplyr::mutate(group = dplyr::if_else(row == "End use (own loop)" & !is.na(life_txt), paste0(group, " (", life_txt, ")"), group))

# On-line labels (end-use panels): text along each median line, rotated to its
# slope; position along the line (hjust) and side (above / below) staggered so
# neighbouring lines' labels don't collide
EU_LABEL_POS <- tibble::tribble(
  ~material, ~group, ~hj, ~above,
  "Metal ores", "Packaging", 0.95, TRUE,
  "Metal ores", "Durables", 0.55, TRUE,
  "Metal ores", "Vehicles", 0.95, FALSE,
  "Metal ores", "Machinery", 0.5, FALSE,
  "Metal ores", "Power sector", 0.95, FALSE,
  "Metal ores", "Non-residential", 0.5, FALSE,
  "Metal ores", "Residential", 0.95, FALSE,
  "Metal ores", "Civil engineering", 0.6, FALSE,
  "Non-metallic minerals", "Roads", 0.9, TRUE,
  "Non-metallic minerals", "Non-residential", 0.9, TRUE,
  "Non-metallic minerals", "Power sector", 0.6, FALSE,
  "Non-metallic minerals", "Residential", 0.9, FALSE,
  "Non-metallic minerals", "Civil engineering", 0.9, FALSE
)
EU_DISPLAY <- c("Residential" = "Residential Bldg", "Non-residential" = "Non-residential Bldg")
eu_line_df <- med_df |>
  dplyr::filter(row == "End use (own loop)") |>
  dplyr::mutate(material = as.character(material)) |>
  dplyr::left_join(EU_LABEL_POS, by = c("material", "group")) |>
  dplyr::left_join(life_central, by = "group") |>
  dplyr::mutate(
    hj = dplyr::coalesce(hj, 0.9),
    vj = dplyr::if_else(dplyr::coalesce(above, TRUE), -0.35, 1.35),
    label = paste0(dplyr::coalesce(unname(EU_DISPLAY[group]), group), dplyr::if_else(is.na(life_txt), "", paste0(" (", life_txt, ")")))
  )

# Panel titles (tag + material + row) and the no-growth reference
# One figure row per material: (all runs, end use) side by side
panel_levels <- paste0(
  letters[seq_len(length(ROW_LEVELS) * length(MAT_LEVELS))], "  ",
  rep(MAT_LEVELS, each = length(ROW_LEVELS)),
  rep(c("All runs" = "", "End use (own loop)" = " · End use")[ROW_LEVELS], times = length(MAT_LEVELS))
)
panel_key <- tidyr::expand_grid(material = factor(MAT_LEVELS, levels = MAT_LEVELS), row = factor(ROW_LEVELS, levels = ROW_LEVELS)) |>
  dplyr::mutate(panel = factor(panel_levels, levels = panel_levels))
plot_long <- plot_long |> dplyr::left_join(panel_key, by = c("row", "material"))
med_df <- med_df |> dplyr::left_join(panel_key, by = c("row", "material"))
lab_df <- lab_df |> dplyr::left_join(panel_key, by = c("row", "material"))
eu_line_df <- eu_line_df |> dplyr::left_join(panel_key |> dplyr::mutate(material = as.character(material)), by = c("row", "material"))

cat("  Median (P25-P75) across runs, all runs:\n")
print(as.data.frame(
  plot_long |>
    dplyr::filter(row == "All runs") |>
    dplyr::group_by(material) |>
    dplyr::summarise(
      recovered = sprintf("%.0f%% (%.0f-%.0f%%)", 100 * median(x), 100 * quantile(x, 0.25), 100 * quantile(x, 0.75)),
      secondary = sprintf("%.0f%% (%.0f-%.0f%%)", 100 * median(y), 100 * quantile(y, 0.25), 100 * quantile(y, 0.75)),
      .groups = "drop"
    )
))


# Plot ------------------------------------------------------------------

# One ggplot per panel (same title, axis titles, tick labels and reference
# labels on every panel), assembled with patchwork.

Y_TITLE <- paste0("Secondary share of supply, ", FIG_YEAR)
X_TITLE <- paste0("Recovered share of retiring material, ", FIG_YEAR)

p_list <- list()
for (k in seq_len(nrow(panel_key))) {
  pk <- panel_key[k, ]
  pts_k <- plot_long |> dplyr::filter(panel == pk$panel, row == "All runs")
  med_k <- med_df |> dplyr::filter(panel == pk$panel)
  rib_k <- med_k |> dplyr::filter(row == "All runs")
  eu_k <- eu_line_df |> dplyr::filter(panel == pk$panel)

  p_list[[k]] <- ggplot() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.35) +
    annotate("text", x = 0.62, y = 0.62, label = "No stock growth", angle = 45, vjust = -0.5, colour = "grey30", size = pb_annot_size("wide", 7)) +
    # Panel tag inside the plot area, close to the top-left corner
    annotate("text", x = 0.025, y = 0.975, label = substr(as.character(pk$panel), 1, 1), hjust = 0, vjust = 1, fontface = "bold", size = pb_annot_size("wide", 9)) +
    # All-runs row: P2.5-P97.5 (95%) band of the median line, no run points (empty data elsewhere)
    geom_ribbon(data = rib_k, aes(x = x_med, ymin = y_p025, ymax = y_p975, fill = col, group = group), alpha = 0.25, colour = NA) +
    geom_line(data = med_k, aes(x = x_med, y = y_med, colour = col, group = group), linewidth = 0.6) +
    # End-use panels: label along each line (text only; the line is drawn above)
    geomtextpath::geom_textline(
      data = eu_k,
      aes(x = x_med, y = y_med, label = label, colour = col, group = group, hjust = hj, vjust = vj),
      text_only = TRUE, straight = TRUE, fontface = "bold", size = pb_annot_size("wide", 7)
    ) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(0, 1, 0.25)) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    labs(title = sub("^[a-z]  ", "", as.character(pk$panel)), x = X_TITLE, y = Y_TITLE) +
    theme_pb_wide() +
    theme(
      plot.title.position = "panel", # aligned with the panel, clear of the y-axis title
      plot.title = element_text(hjust = 0),
      plot.margin = margin(t = 4, r = 4, b = 6, l = 4, unit = "pt")
    )
}

patchwork::wrap_plots(p_list, ncol = 2)

# Main Figure 4 (was SI S29)
ggsave("Figures/Fig4 - CircularityStockGrowth.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 15.5, height = 15.5, bg = "white")
ggsave("Figures/SVG/Fig4 - CircularityStockGrowth.svg", ggplot2::last_plot(), units = "cm", width = 15.5, height = 15.5, bg = "transparent")
group_svg_layers("Figures/SVG/Fig4 - CircularityStockGrowth.svg") # fast now: no per-run points


# Save figure data ------------------------------------------------------

dir.create("Figures/RawData/", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(
  run_groups |> dplyr::select(run_id, material, recovered_share = x, secondary_share = y, ssp_label, mean_life, life_lab, sg_kg_usd, sg_lab),
  "Figures/RawData/fig4_runs.csv"
)
readr::write_csv(
  eu_run |> dplyr::rename(recovered_share = x, secondary_share_own_loop = y),
  "Figures/RawData/fig4_enduse_own_loop.csv"
)
readr::write_csv(
  med_df |> dplyr::select(panel, material, row, group, bin, n, x_med, y_med, y_p025, y_p25, y_p75, y_p975),
  "Figures/RawData/fig4_median_lines.csv"
)

cat("  Saved: Figures/Fig4 - CircularityStockGrowth.png (+ SVG, RawData/fig4_*.csv)\n")
cat("=== S29 done ===\n")

# EoF
