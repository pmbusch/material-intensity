## =============================================================================
## S18 - RegionalAnalysis.R  -> Supporting Figure S18
## 2 rows x 3 columns, all panels by region (8 regions, PALETTE_REGIONS), all at
## FIG_YEAR (2060). Kaya decomposition M = P x (G/P) x (M/G), shown as P, G/P,
## and M (primary material mass -- NOT divided by GDP) per material group:
##   Row 1: a) P (population), b) G/P (GDP per capita), c) M -- Biomass
##   Row 2: d) M -- Fossil fuels, e) M -- Metal ores, f) M -- Non-metallic minerals
## P and G/P don't vary by material (region-level demographic/economic values),
## so they each get ONE panel; M (primary material consumption, Mt) gets its
## own panel per material group -- 2 + 4 = 6 panels.
##
## Each panel: horizontal bars (one per region, sorted high-to-low, high at
## top), no y-axis text -- region name direct-labelled inside the bar (white
## text) if the bar is long, outside (dark text) if short. Bar = median across
## MC runs; error bar = 5th-95th percentile (90% CI); open-circle point = the
## same median statistic one year earlier (FIG_YEAR_BASE = 2025, the model's
## first simulated year) -- NOT a historical actual: Biomass/Fossil fuels have
## a clean 2024 primary-consumption anchor, but Metal ores/Non-metallic
## minerals only have a 2024 STOCK-intensity anchor (in-use stock/GDP) in the
## model's own calibration inputs, which is not comparable to these panels'
## primary-consumption-FLOW bars -- using the model's own year-2025 primary
## flow for every panel avoids that mismatch and keeps all 6 panels on
## identical footing. Every panel's bars are filled by region (PALETTE_REGIONS,
## same colour throughout) -- no material/end-use composition breakdown and no
## world/average reference line (a world total or average doesn't read
## sensibly once the bars are raw region-level mass rather than a ratio).
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

FIG_YEAR <- 2060L # this figure's snapshot year
FIG_YEAR_BASE <- 2025L # model's first simulated year -- this figure's "point" reference

pb_set_geom_defaults("largeFont")

cat("=== Figure 4 - Regional Analysis ===\n\n")

# STEP 1: Load data ------------------------------------------------------------

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  dplyr::filter(year %in% c(FIG_YEAR_BASE, FIG_YEAR))
gdp_region_hist <- readr::read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
pop_region_hist <- readr::read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE)
ssp_drivers <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)

cat("  Runs:", dplyr::n_distinct(results$run_id), "| Regions:", dplyr::n_distinct(results$region), "\n\n")

# STEP 2: Per-run, per-region population & GDP, both years --------------------
# Continuous SSP blend (same construction Figure 3 uses for its world total),
# kept at region level and at both FIG_YEAR_BASE/FIG_YEAR here.

gdp_2024_region <- gdp_region_hist |>
  dplyr::filter(year == 2024) |>
  dplyr::rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  dplyr::filter(year == 2024) |>
  dplyr::rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

run_ssp <- results |>
  dplyr::distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)

pop_idx_region <- ssp_drivers |>
  dplyr::filter(variable == "Population", year %in% c(FIG_YEAR_BASE, FIG_YEAR)) |>
  dplyr::select(scenario, region, year, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  dplyr::filter(variable == "GDP|PPP [per capita]", year %in% c(FIG_YEAR_BASE, FIG_YEAR)) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::left_join(
    pop_idx_region |> dplyr::rename(ssp_lo = scenario),
    by = "ssp_lo",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    pop_idx_region |> dplyr::rename(ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  dplyr::mutate(pop_idx_blend = ssp_share_lo * pop_idx + (1 - ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::left_join(
    gdppc_idx_region |> dplyr::rename(ssp_lo = scenario),
    by = "ssp_lo",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    gdppc_idx_region |> dplyr::rename(ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("ssp_hi", "region", "year")
  ) |>
  dplyr::mutate(gdppc_idx_blend = ssp_share_lo * gdppc_idx + (1 - ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

run_region_year <- pop_idx_blend |>
  dplyr::left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  dplyr::left_join(pop_2024_region, by = "region") |>
  dplyr::left_join(gdp_2024_region, by = "region") |>
  dplyr::mutate(
    pop_run = pop_2024 * pop_idx_blend,
    gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend
  ) |>
  dplyr::select(run_id, region, year, pop_run, gdp_run)

# STEP 3: Primary material mass by run/region/year, per material group --------
# "Primary" = primary_consumption_Mt only (secondary_supply_Mt excluded).
# Bars are coloured by region (not by material/end-use detail), so this is
# just each group's total mass -- no composition breakdown needed.

material_long <- dplyr::bind_rows(
  results |>
    dplyr::filter(material_group == "biomass") |>
    dplyr::transmute(run_id, region, year, panel = "Biomass", primary_Mt = primary_consumption_Mt),
  results |>
    dplyr::filter(material_group == "fossil_fuels") |>
    dplyr::transmute(run_id, region, year, panel = "Fossil fuels", primary_Mt = primary_consumption_Mt),
  results |>
    dplyr::filter(material_group %in% c("metal_fe", "metal_nonfe")) |>
    dplyr::transmute(run_id, region, year, panel = "Metal ores", primary_Mt = primary_consumption_Mt),
  results |>
    dplyr::filter(material_group == "nonmetallic_minerals") |>
    dplyr::transmute(run_id, region, year, panel = "Non-metallic minerals", primary_Mt = primary_consumption_Mt)
) |>
  dplyr::group_by(run_id, region, year, panel) |>
  dplyr::summarise(primary_Mt = sum(primary_Mt, na.rm = TRUE), .groups = "drop")

# STEP 4: Long panel table (all 6 panels), value in each panel's own display
# unit -- P: million people, G/P: '000 USD/person, M: Mt (primary mass) -------

panel_long <- dplyr::bind_rows(
  run_region_year |> dplyr::transmute(run_id, region, year, panel = "P", value = pop_run / 1e6),
  run_region_year |> dplyr::transmute(run_id, region, year, panel = "G/P", value = gdp_run / pop_run / 1e3),
  material_long |> dplyr::transmute(run_id, region, year, panel, value = primary_Mt)
)

# STEP 5: Region-level bar (median) + 90% CI (5th-95th pct), FIG_YEAR ----------

region_total_long <- panel_long |>
  dplyr::group_by(run_id, region, year, panel) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

region_total_stats <- region_total_long |>
  dplyr::filter(year == FIG_YEAR) |>
  dplyr::group_by(panel, region) |>
  dplyr::summarise(
    bar_med = median(value),
    p05 = quantile(value, 0.05),
    p95 = quantile(value, 0.95),
    .groups = "drop"
  )

# STEP 6: Region-level point -- same median statistic, FIG_YEAR_BASE ----------

region_point <- region_total_long |>
  dplyr::filter(year == FIG_YEAR_BASE) |>
  dplyr::group_by(panel, region) |>
  dplyr::summarise(point = median(value), .groups = "drop")

# STEP 7: Label placement -- inside (white) if the bar covers >= 60% of the
# panel's tallest bar, else outside (dark), positioned past the CI/point ------

region_total_stats <- region_total_stats |>
  dplyr::left_join(region_point, by = c("panel", "region")) |>
  dplyr::group_by(panel) |>
  dplyr::mutate(
    panel_max = max(bar_med, na.rm = TRUE),
    label_inside = bar_med >= 0.6 * panel_max,
    label_x = dplyr::if_else(
      label_inside,
      bar_med * 0.97,
      pmax(bar_med, p95, point, na.rm = TRUE) * 1.04
    ),
    label_hjust = dplyr::if_else(label_inside, 1, 0),
    label_col = dplyr::if_else(label_inside, "white", "#222222")
  ) |>
  dplyr::ungroup()

cat("  Panel rows -- region_total_stats:", nrow(region_total_stats), "\n\n")

# NOTE on region ordering: each panel sorts its own 8 regions independently
# (high-to-low). forcats::fct_reorder() is deliberately called separately for
# EACH panel's plot below (never once on the combined table) -- a factor
# column has one shared levels order, so computing it once across all 6
# panels stacked together would silently collapse every panel onto the same
# order instead of each panel's own.

# Plot --------------------------------------------------------------------

X_LAB <- c(
  "P" = "Population (million people)",
  "G/P" = "GDP per capita ('000 USD)",
  "Biomass" = "Primary biomass consumption (Mt)",
  "Fossil fuels" = "Primary fossil fuel consumption (Mt)",
  "Metal ores" = "Primary metal ore consumption (Mt)",
  "Non-metallic minerals" = "Primary mineral consumption (Mt)"
)

# Panel a: Population -- fill = region (direct labels carry region identity,
# so no legend).
totals_a <- region_total_stats |>
  dplyr::filter(panel == "P") |>
  dplyr::mutate(region_fct = forcats::fct_reorder(region, bar_med))
point_a <- region_point |>
  dplyr::filter(panel == "P") |>
  dplyr::mutate(region_fct = factor(region, levels = levels(totals_a$region_fct)))

p_a <- ggplot(totals_a, aes(x = bar_med, y = region_fct)) +
  geom_col(aes(fill = region), width = 0.7, colour = "black", linewidth = 0.15, show.legend = FALSE) +
  geom_errorbar(aes(xmin = p05, xmax = p95), width = 0.3, linewidth = 0.35) +
  geom_point(
    data = point_a,
    aes(x = point, y = region_fct),
    shape = 21,
    fill = NA,
    colour = "black",
    size = 1.8,
    stroke = 0.7
  ) +
  geom_text(aes(x = label_x, label = region, hjust = label_hjust, colour = label_col), size = pb_annot_size("largeFont")) +
  scale_fill_manual(values = PALETTE_REGIONS) +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::label_comma(accuracy = 1), expand = expansion(mult = c(0, 0.18))) +
  labs(tag = "a", title = "Population (P)", x = X_LAB[["P"]], y = NULL) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel"
  )

# Panel b: GDP per capita -- fill = region.
totals_b <- region_total_stats |>
  dplyr::filter(panel == "G/P") |>
  dplyr::mutate(region_fct = forcats::fct_reorder(region, bar_med))
point_b <- region_point |>
  dplyr::filter(panel == "G/P") |>
  dplyr::mutate(region_fct = factor(region, levels = levels(totals_b$region_fct)))

p_b <- ggplot(totals_b, aes(x = bar_med, y = region_fct)) +
  geom_col(aes(fill = region), width = 0.7, colour = "black", linewidth = 0.15, show.legend = FALSE) +
  geom_errorbar(aes(xmin = p05, xmax = p95), width = 0.3, linewidth = 0.35) +
  geom_point(
    data = point_b,
    aes(x = point, y = region_fct),
    shape = 21,
    fill = NA,
    colour = "black",
    size = 1.8,
    stroke = 0.7
  ) +
  geom_text(aes(x = label_x, label = region, hjust = label_hjust, colour = label_col), size = pb_annot_size("largeFont")) +
  scale_fill_manual(values = PALETTE_REGIONS) +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::label_comma(accuracy = 1), expand = expansion(mult = c(0, 0.18))) +
  labs(tag = "b", title = "GDP per capita (G/P)", x = X_LAB[["G/P"]], y = NULL) +
  theme_pb_large() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel"
  )

# Panels c-f: M -- fill = region (same as panels a/b).
mg_panels <- list(
  c(tag = "c", panel = "Biomass", title = "Biomass (M)"),
  c(tag = "d", panel = "Fossil fuels", title = "Fossil fuels (M)"),
  c(tag = "e", panel = "Metal ores", title = "Metal ores (M)"),
  c(tag = "f", panel = "Non-metallic minerals", title = "Non-metallic minerals (M)")
)

mg_plots <- list()
for (spec in mg_panels) {
  pnl <- spec[["panel"]]
  totals_pnl <- region_total_stats |>
    dplyr::filter(panel == pnl) |>
    dplyr::mutate(region_fct = forcats::fct_reorder(region, bar_med))
  point_pnl <- region_point |>
    dplyr::filter(panel == pnl) |>
    dplyr::mutate(region_fct = factor(region, levels = levels(totals_pnl$region_fct)))

  mg_plots[[pnl]] <- ggplot(totals_pnl, aes(x = bar_med, y = region_fct)) +
    geom_col(aes(fill = region), width = 0.7, colour = "black", linewidth = 0.15, show.legend = FALSE) +
    geom_errorbar(aes(xmin = p05, xmax = p95), width = 0.3, linewidth = 0.35) +
    geom_point(
      data = point_pnl,
      aes(x = point, y = region_fct),
      shape = 21,
      fill = NA,
      colour = "black",
      size = 1.8,
      stroke = 0.7
    ) +
    geom_text(aes(x = label_x, label = region, hjust = label_hjust, colour = label_col), size = pb_annot_size("largeFont")) +
    scale_fill_manual(values = PALETTE_REGIONS) +
    scale_colour_identity() +
    scale_x_continuous(labels = scales::label_comma(accuracy = 1), expand = expansion(mult = c(0, 0.18))) +
    labs(tag = spec[["tag"]], title = spec[["title"]], x = X_LAB[[pnl]], y = NULL) +
    theme_pb_large() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.tag = element_text(face = "bold"),
      plot.tag.location = "panel"
    )
}

fig <- (p_a + p_b + mg_plots[["Biomass"]] + mg_plots[["Fossil fuels"]] + mg_plots[["Metal ores"]] + mg_plots[["Non-metallic minerals"]]) +
  patchwork::plot_layout(ncol = 3, nrow = 2)

# Save ------------------------------------------------------------------

ggsave("Figures/Supporting-Figures/S18_RegionalAnalysis.png", fig, units = "cm", dpi = 600, width = 17.4, height = 13)
ggsave("Figures/SVG/Supporting-Figures/S18_RegionalAnalysis.svg", fig, units = "cm", width = 17.4, height = 13)
clean_svg("Figures/SVG/Supporting-Figures/S18_RegionalAnalysis.svg")

readr::write_csv(region_total_stats, "Figures/RawData/S18_region_totals.csv")
readr::write_csv(region_point, "Figures/RawData/S18_region_point.csv")

cat("  Saved: Figures/Supporting-Figures/S18_RegionalAnalysis.png, Figures/SVG/Supporting-Figures/S18_RegionalAnalysis.svg\n")
cat("=== Figure 4 - Regional Analysis done ===\n")

# EoF
