## Figure 3 - Contours-GrowthRate.R  -> Figures/Fig3 - Contours-GrowthRate.png (+ SI S17)
## Four-panel MC scatter, one panel per material group, all at FIG_YEAR (2060):
##   a) Biomass:              consumption per capita (kg/person; crops + grazed
##                            biomass/fodder + wood, summed)
##   b) Fossil fuels:         oil+coal+gas primary energy / GDP (MJ per $)
##   c) Metal ores:           secondary material supply share  [outline: ore grade]
##   d) Non-metallic minerals: secondary material supply share  [outline: average lifetime]
## Y axis (every panel, free scale): per-capita consumption annual growth rate,
## 2025-2060. Fill (every panel): decoupling status, collapsed to 3 classes --
## Peak decoupling is folded into Relative decoupling (both reflect a declining
## per-capita trend by 2060; see Scripts/04-Simulation/04-Decoupling.R). No
## legend is drawn for it (panel c's direct labels carry that meaning for all
## four panels); panels c/d additionally outline each point by ore grade /
## average lifetime, with its own legend inside the panel's lower-left corner.

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8") # LIFETIME_SAMPLE_PARAMS, LIFETIME_MIN

library(patchwork)

# This figure's own snapshot year/window -- independent of the model's global
# intensity-convergence TARGET_YEAR (2050, 04-Simulation/00-Parameters.R), which stays
# unchanged; mc_results.parquet already spans through FORECAST_END (2060).
FIG_YEAR <- 2060L
FIG_VARIANT_ID <- "window_2025_2060" # decoupling variant matching FIG_YEAR (was "main_2050")

pb_set_geom_defaults("largeFont")

cat("=== Figure 3 - Growth Decoupling ===\n\n")

# Load data --------------------------------------------------------------

results <- arrow::read_parquet("Results/MC/mc_results.parquet")
mc_input_matrix <- readr::read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
decoupling <- arrow::read_parquet("Results/MC/mc_decoupling.parquet")
ssp_drivers <- readr::read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)
gdp_region_hist <- readr::read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
pop_region_hist <- readr::read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE)
dmc_region_hist <- readr::read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
stock_2024_total <- readr::read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE)

results_target <- results |> dplyr::filter(year == FIG_YEAR)

DECOUPLE_LEVELS <- c("Absolute decoupling", "Relative decoupling", "No decoupling")
DECOUPLE_COLORS <- PALETTE_DECOUPLING[DECOUPLE_LEVELS] # subset of the project palette (00-CommonParameters.R)

# Outline-colour palettes for panels c/d (point fill stays the decoupling
# colour above; these colour the point's border instead of its shape).
ORE_GRADE_COLORS <- c("Low ore grade" = "#000000", "High ore grade" = "#D4A017") # black vs. gold, high contrast
LIFETIME_COLORS <- c("<60 yr" = "#FF7F00", "60-80 yr" = "#377EB8", ">80 yr" = "#984EA3") # distinct qualitative hues
# Historical star: yellow glyph drawn over a slightly larger black one (= black outline)
STAR_FILL <- "#F2C200"
STAR_SIZE <- 6
STAR_OUTLINE_SIZE <- 7.4

# Y axis + colour: per-capita CAGR & decoupling status, 3 classes -----------
# FIG_VARIANT_ID window (2025-2060) matches this figure's FIG_YEAR snapshot.
# Peak decoupling folded into Relative; rows with an invalid CAGR (non-positive
# start/end value) are dropped further down via tidyr::drop_na().

growth_decoupling <- decoupling |>
  dplyr::filter(
    variant_id == FIG_VARIANT_ID,
    material_group %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
  ) |>
  dplyr::mutate(
    decoupling_class = as.character(decoupling_class),
    decoupling_class = dplyr::if_else(decoupling_class == "Peak decoupling", "Relative decoupling", decoupling_class),
    decoupling_class = factor(decoupling_class, levels = DECOUPLE_LEVELS),
    material = as.character(material_group)
  ) |>
  dplyr::select(run_id, material, y = mf_percap_cagr, gdp_cagr = gdp_percap_cagr, decoupling_class)

# Total (population-inclusive) counterpart, used only by the contour version
# below (Fig3, the main output) -- the static Fig3_dots panels a-d above stay per-capita.
growth_decoupling_total <- decoupling |>
  dplyr::filter(
    variant_id == FIG_VARIANT_ID,
    material_group %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
  ) |>
  dplyr::mutate(
    decoupling_class = as.character(decoupling_class),
    decoupling_class = dplyr::if_else(decoupling_class == "Peak decoupling", "Relative decoupling", decoupling_class),
    decoupling_class = factor(decoupling_class, levels = DECOUPLE_LEVELS),
    material = as.character(material_group)
  ) |>
  dplyr::select(run_id, material, y = mf_total_cagr, gdp_cagr = gdp_total_cagr, decoupling_class)

# X (panel a, Biomass): total consumption per capita (kg/person), summed
# across simulated biomass sub-categories -----------------------------------
# Realized world consumption at FIG_YEAR from mc_results (intensity endpoints
# are region-specific, so there is no single global kg/$ draw to rescale),
# divided by this run's reconstructed world population at FIG_YEAR. "Crop
# residues" and "Other biomass" are excluded -- neither has its own sampled
# mc_input_matrix draw (residues follow crops; other biomass is fixed at
# 2024), and residues are additionally excluded on calorie-attribution
# grounds (near-zero calorie content unless burned, in which case it's energy,
# not material, consumption).

BIOMASS_MC_KEYS <- c("Crops" = "crops", "Grazed biomass and fodder crops" = "grazed_biomass", "Wood" = "wood")

# World GDP per capita at FIG_YEAR, per run -- same reconstruction as
# Scripts/04-Simulation/04-Decoupling.R STEP 2, restricted to one year.

gdp_2024_region <- gdp_region_hist |>
  dplyr::filter(year == 2024) |>
  dplyr::rename(region = Region, gdp_2024 = GDP_2015USD) |>
  dplyr::select(region, gdp_2024)

pop_2024_region <- pop_region_hist |>
  dplyr::filter(year == 2024) |>
  dplyr::rename(region = Region, pop_2024 = population) |>
  dplyr::select(region, pop_2024)

run_ssp <- results_target |>
  dplyr::distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)

pop_idx_region <- ssp_drivers |>
  dplyr::filter(variable == "Population", year == FIG_YEAR) |>
  dplyr::select(scenario, region, pop_idx = index)

gdppc_idx_region <- ssp_drivers |>
  dplyr::filter(variable == "GDP|PPP [per capita]", year == FIG_YEAR) |>
  dplyr::select(scenario, region, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::left_join(
    pop_idx_region |> dplyr::rename(ssp_lo = scenario),
    by = "ssp_lo",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    pop_idx_region |> dplyr::rename(ssp_hi = scenario, pop_idx_hi = pop_idx),
    by = c("ssp_hi", "region")
  ) |>
  dplyr::mutate(pop_idx_blend = ssp_share_lo * pop_idx + (1 - ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  dplyr::left_join(
    gdppc_idx_region |> dplyr::rename(ssp_lo = scenario),
    by = "ssp_lo",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    gdppc_idx_region |> dplyr::rename(ssp_hi = scenario, gdppc_idx_hi = gdppc_idx),
    by = c("ssp_hi", "region")
  ) |>
  dplyr::mutate(gdppc_idx_blend = ssp_share_lo * gdppc_idx + (1 - ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, gdppc_idx_blend)

world_gdp_percap_target <- pop_idx_blend |>
  dplyr::left_join(gdppc_idx_blend, by = c("run_id", "region")) |>
  dplyr::left_join(pop_2024_region, by = "region") |>
  dplyr::left_join(gdp_2024_region, by = "region") |>
  dplyr::mutate(pop = pop_2024 * pop_idx_blend, gdp_usd = gdp_2024 * pop_idx_blend * gdppc_idx_blend) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(gdp_percap_usd = sum(gdp_usd) / sum(pop), world_pop = sum(pop), .groups = "drop")

x_biomass <- results_target |>
  dplyr::filter(material_group == "biomass", material_key %in% names(BIOMASS_MC_KEYS)) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(biomass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(world_gdp_percap_target, by = "run_id") |>
  dplyr::mutate(x = biomass_Mt * 1e9 / world_pop, material = "Biomass") |>
  dplyr::select(run_id, x, material)

# X (panel b, Fossil fuels): primary energy / GDP (MJ per $) -----------------
# Realized world oil/coal/gas consumption at FIG_YEAR (mc_results), each
# converted to energy via a standard energy density (IPCC 2006 default net
# calorific values, GJ/tonne -- numerically equal to MJ/kg), summed and divided
# by this run's reconstructed world GDP at FIG_YEAR.

ENERGY_DENSITY_MJ_PER_KG <- c(coal = 25.8, oil = 42.3, gas = 48.0) # IPCC (2006) default NCVs
FOSSIL_MC_KEYS <- c("Coal" = "coal", "Natural Gas" = "gas", "Petroleum" = "oil")

x_fossil <- results_target |>
  dplyr::filter(material_group == "fossil_fuels", material_key %in% names(FOSSIL_MC_KEYS)) |>
  dplyr::mutate(mj = primary_consumption_Mt * 1e9 * ENERGY_DENSITY_MJ_PER_KG[FOSSIL_MC_KEYS[material_key]]) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(mj = sum(mj, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(world_gdp_percap_target, by = "run_id") |>
  dplyr::mutate(x = mj / (gdp_percap_usd * world_pop), material = "Fossil fuels") |>
  dplyr::select(run_id, x, material)

# X (panels c/d): realized secondary supply share, avg across end-use --------
# Metal ores pools Fe + NonFe by mass within each end-use category first.

ENDUSE_CATEGORY <- c(
  "Residential" = "buildings",
  "Non-residential" = "buildings",
  "Roads" = "civil",
  "Civil engineering" = "civil",
  "Machinery" = "machinery",
  "Vehicles" = "machinery"
)
results_target <- results_target |>
  dplyr::mutate(enduse_category = ENDUSE_CATEGORY[material_key]) |>
  dplyr::filter(!is.na(enduse_category))

x_metals <- results_target |>
  dplyr::filter(material_group %in% c("metal_fe", "metal_nonfe")) |>
  dplyr::group_by(run_id, enduse_category) |>
  dplyr::summarise(
    primary = sum(primary_consumption_Mt, na.rm = TRUE),
    secondary = sum(secondary_supply_Mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(secondary_share = secondary / (primary + secondary)) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(x = mean(secondary_share), .groups = "drop") |>
  dplyr::mutate(material = "Metal ores")

x_minerals <- results_target |>
  dplyr::filter(material_group == "nonmetallic_minerals", enduse_category %in% c("buildings", "civil")) |>
  dplyr::group_by(run_id, enduse_category) |>
  dplyr::summarise(
    primary = sum(primary_consumption_Mt, na.rm = TRUE),
    secondary = sum(secondary_supply_Mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(secondary_share = secondary / (primary + secondary)) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(x = mean(secondary_share), .groups = "drop") |>
  dplyr::mutate(material = "Non-metallic minerals")

# Shape (panel c, Metal ores): ore grade, High/Low, median split ------------
# Fe and NonFe each have their own sampled target grade draw (grade_ore_fe,
# grade_ore_nonfe, both in [0,1]); pooled per run as their simple average.

ore_grade_df <- mc_input_matrix |>
  dplyr::transmute(run_id, grade_u = (grade_ore_fe + grade_ore_nonfe) / 2) |>
  dplyr::mutate(
    ore_grade = factor(
      dplyr::if_else(grade_u >= median(grade_u), "High ore grade", "Low ore grade"),
      levels = c("Low ore grade", "High ore grade")
    )
  ) |>
  dplyr::select(run_id, ore_grade)

# Shape (panel d, Non-metallic minerals): average lifetime, 3 bins ----------
# Exact replication of Scripts/04-Simulation/02-RunSimulations.R lines 609-621,
# averaged across buildings + civil_infrastructure (the two super-categories
# feeding non-metallic minerals stock).

lifetime_dr <- mc_input_matrix |>
  dplyr::select(run_id, dplyr::matches("^lifetime_(mean|k)_")) |>
  tidyr::pivot_longer(-run_id, names_to = c("param", "super_cat"), names_pattern = "^lifetime_(mean|k)_(.+)") |>
  tidyr::pivot_wider(names_from = param, values_from = value) |>
  dplyr::rename(u_mean = mean, u_k = k) |>
  dplyr::left_join(LIFETIME_SAMPLE_PARAMS, by = c("super_cat" = "super_category")) |>
  dplyr::mutate(mean_life = pmax(LIFETIME_MIN, mean_life_min + u_mean * (mean_life_max - mean_life_min))) |>
  dplyr::select(run_id, sub_use, super_cat, mean_life)

lifetime_bin_df <- lifetime_dr |>
  dplyr::filter(super_cat %in% c("buildings", "civil_infrastructure")) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(mean_life = mean(mean_life), .groups = "drop") |>
  dplyr::mutate(
    lifetime_bin = cut(
      mean_life,
      breaks = c(-Inf, 60, 80, Inf),
      labels = c("<60 yr", "60-80 yr", ">80 yr"),
      right = FALSE
    )
  ) |>
  dplyr::select(run_id, mean_life, lifetime_bin) # mean_life (continuous) feeds the contour figure's panel f Y-axis

# Assemble per-panel plot data ------------------------------------------------

panel_a_df <- x_biomass |>
  dplyr::inner_join(growth_decoupling, by = c("run_id", "material")) |>
  tidyr::drop_na(y, decoupling_class)

panel_b_df <- x_fossil |>
  dplyr::inner_join(growth_decoupling, by = c("run_id", "material")) |>
  tidyr::drop_na(y, decoupling_class)

panel_c_df <- x_metals |>
  dplyr::inner_join(growth_decoupling, by = c("run_id", "material")) |>
  dplyr::left_join(ore_grade_df, by = "run_id") |>
  tidyr::drop_na(y, decoupling_class)

# Direct colour labels for panel c, placed at each class's median (x, y) --
# i.e. where most of that class's points sit -- replacing its colour legend.
# Nudged off the median so the text clears the densest point cluster: the
# Absolute decoupling label down/left, the other two up/right.
panel_c_labels <- panel_c_df |>
  dplyr::group_by(decoupling_class) |>
  dplyr::summarise(x = median(x), y = median(y), .groups = "drop") |>
  dplyr::mutate(
    nudge_sign = dplyr::if_else(decoupling_class == "Absolute decoupling", -1, 1),
    x = x + nudge_sign * 0.10 * diff(range(panel_c_df$x, na.rm = TRUE)),
    y = y + nudge_sign * 0.10 * diff(range(panel_c_df$y, na.rm = TRUE))
  ) |>
  dplyr::select(-nudge_sign)

panel_d_df <- x_minerals |>
  dplyr::inner_join(growth_decoupling, by = c("run_id", "material")) |>
  dplyr::left_join(lifetime_bin_df, by = "run_id") |>
  tidyr::drop_na(y, decoupling_class)

cat(
  "  Panel rows -- a:",
  nrow(panel_a_df),
  "| b:",
  nrow(panel_b_df),
  "| c:",
  nrow(panel_c_df),
  "| d:",
  nrow(panel_d_df),
  "\n\n"
)

# Plot ------------------------------------------------------------------

Y_LAB <- "Consumption per capita\nannual growth rate (2025-2060)" # two lines instead of "CAGR"
SHARE_LAB <- "Secondary material supply share (%)"

p_a <- ggplot(panel_a_df, aes(x = x, y = y, colour = decoupling_class)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(size = 0.7, alpha = 0.6) +
  scale_colour_manual(values = DECOUPLE_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_comma(), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0, 0)) +
  labs(tag = "a", title = "Biomass", x = "Biomass consumption per capita (kg/person)", y = Y_LAB) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Biomass"]))
  )

p_b <- ggplot(panel_b_df, aes(x = x, y = y, colour = decoupling_class)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(size = 0.7, alpha = 0.6) +
  scale_colour_manual(values = DECOUPLE_COLORS, guide = "none") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0, 0)) +
  labs(tag = "b", title = "Fossil fuels", x = "Primary fossil fuel energy intensity (MJ per $)", y = Y_LAB) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Fossil fuels"]))
  )

# Panel c: fill = decoupling status (no legend -- direct labels instead);
# outline colour = ore grade, its own legend, inside the panel (lower-left).
# Both the outline (ore grade) and the label text (decoupling_class) map to
# the "colour" aesthetic, so scale_colour_manual carries both palettes and
# `breaks` restricts the legend to just the ore-grade entries.
p_c <- ggplot(panel_c_df, aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(fill = decoupling_class, colour = ore_grade), shape = 21, size = 1.1, stroke = 0.6, alpha = 0.7) +
  geom_text(
    data = panel_c_labels,
    aes(x = x, y = y, label = decoupling_class, colour = decoupling_class),
    inherit.aes = FALSE,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = DECOUPLE_COLORS, guide = "none") +
  scale_colour_manual(
    values = c(DECOUPLE_COLORS, ORE_GRADE_COLORS),
    breaks = names(ORE_GRADE_COLORS),
    name = "Ore grade"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0, 0)) +
  labs(tag = "c", title = "Metal ores", x = SHARE_LAB, y = Y_LAB) +
  guides(colour = guide_legend(position = "inside", override.aes = list(fill = "white", shape = 21, size = 2))) +
  theme_pb_large() +
  theme(
    legend.position.inside = c(0.03, 0.03),
    legend.justification.inside = c(0, 0),
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]))
  )

# Panel d: fill = decoupling status (no legend); outline colour = average
# lifetime (sequential, short -> long), its own legend, inside the panel.
p_d <- ggplot(panel_d_df, aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(fill = decoupling_class, colour = lifetime_bin), shape = 21, size = 1.1, stroke = 0.6, alpha = 0.7) +
  scale_fill_manual(values = DECOUPLE_COLORS, guide = "none") +
  scale_colour_manual(values = LIFETIME_COLORS, name = "Average lifetime") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0, 0)) +
  labs(tag = "d", title = "Non-metallic minerals", x = SHARE_LAB, y = Y_LAB) +
  guides(colour = guide_legend(position = "inside", override.aes = list(fill = "white", shape = 21, size = 2))) +
  theme_pb_large() +
  theme(
    legend.position.inside = c(0.03, 0.03),
    legend.justification.inside = c(0, 0),
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Non-metallic minerals"]))
  )

fig <- (p_a + p_b + p_c + p_d) + patchwork::plot_layout(ncol = 2)

# Save ------------------------------------------------------------------

ggsave("Figures/Other/Fig3_dots.png", fig, units = "cm", dpi = 600, width = 17.4, height = 17.4)
ggsave("Figures/Other/SVG/Fig3_dots.svg", fig, units = "cm", width = 17.4, height = 17.4)
clean_svg("Figures/Other/SVG/Fig3_dots.svg")

cat("  Saved: Figures/Other/Fig3_dots.png, Figures/Other/SVG/Fig3_dots.svg\n")
cat("=== Figure 3 (dots version) done ===\n")

# Fig3 -- contour version (main output): GDP growth vs. circularity limits ---
## 12 panels (3 rows x 4 columns), ALL contour + fill-colour panels -- no
## stacked-area panels anywhere in this figure. Row-major reading order (tags
## a-l), each row carrying its own vertical row title --
##   Row 1 "Intensity": a) Biomass, b) Fossil fuels, c) Metal ores stock
##          intensity, d) Non-metallic minerals stock intensity -- all
##          X = GDP growth, Y = that material's own in-use STOCK intensity
##          per $ GDP (c/d) or consumption-based intensity (a/b).
##   Row 2 "Recycling/Downcycling": e) Metal ores recycling rate vs. GDP
##          growth, f) Metal ores recycling rate vs. stock-weighted average
##          lifetime, g) Non-metallic minerals downcycling rate vs. GDP
##          growth, h) Non-metallic minerals downcycling rate vs.
##          stock-weighted average lifetime.
##   Row 3 "Stock detail": i) Ferrous ore grade, j) Non-ferrous ore grade,
##          k) Buildings lifetime, l) Civil infrastructure lifetime -- all
##          X = GDP growth.
## Panels c/d give Metal ores / Non-metallic minerals a STOCK-intensity Y
## axis (in_use_stock_Mt / world GDP, at FIG_YEAR, X = GDP growth --
## analogous to Biomass's kg/person and Fossil's MJ/$ framing on the other
## row-1 panels). Panels f/h replace an earlier stock-per-GDP X axis (too
## redundant with c/d) with average lifetime across the material's own
## end-use categories, weighted by each end-use's in-use stock mass
## (buildings + civil_infrastructure + machinery for Metal ores; buildings +
## civil_infrastructure for Non-metallic minerals) -- not the plain
## per-sub_use mean k/l use for their own (different) Y axis.
##
## Every panel's colour + contour is still that panel's own sub-material's/
## sub-enduse's TOTAL (population-inclusive) primary-consumption CAGR,
## 2025-2060 -- the same quantity throughout the whole figure. Each panel
## gets its own custom diverging fill scale -- Spectral's characteristic blue
## at the low (decline) end, white at 0%, and that panel's own
## material/enduse colour (from PALETTE_MATERIAL_GROUPS/PALETTE_MATERIALS/
## PALETTE_ENDUSE, the same palettes Figure 1 uses, with two substitute
## colours -- see BLUE_SPECTRAL/NONMET_FILL_HIGH/BUILDINGS_FILL_HIGH below --
## where a category's own colour would be too close to the scale's blue low
## end) at the high (growth) end -- sized to ITS OWN observed range, with its
## own exclusive legend (not collected into one): the colourbar runs the
## full height of its own panel, title rotated vertically beside it, framed
## with a black outline, and labelled at ~4-5 "nice" breaks spanning the
## FULL symmetric c(-z_lim, z_lim) range the bar actually covers (brks_a,
## brks_b, ... below -- also reused as each panel's contour-line levels, so
## the legend's labelled ticks always land exactly on a drawn contour line),
## with one unlabelled tick at the midpoint between each labelled pair.
## Ticks sit OUTSIDE the bar, on the right only (CONTOUR_FILL_GUIDE's
## `legend.ticks.length = unit(c(-0.12, 0), "cm")` -- negative = outside,
## the 0 on the left side draws nothing there), replacing the default
## in-bar white ticks and ggplot2's normal decimal percent labels
## (CONTOUR_PCT_LABEL below renders whole numbers as "1%" not "1.0%"). The
## fill is a geom_raster drawn from the exact same GAM-interpolated grid the
## contour lines are traced from, so colour and lines are always
## pixel-consistent -- no separate interpolation. Contour lines all share
## the same linewidth (CONTOUR_LINEWIDTH), except the 0% line itself, which
## is slightly bolder (CONTOUR_LINEWIDTH_ZERO -- mapped per line via
## `linewidth = after_stat(...)`, same mechanism as the colour mapping below,
## paired with scale_linewidth_identity() so the literal values render as-is
## instead of being auto-rescaled the way a mapped continuous aesthetic
## normally would). Contour lines are always either pure
## black or pure white, never grey -- panels whose high-fill colour is light
## enough for black to read clearly stay black on both sides of 0% (fixed
## `colour = "black"`, a, d, g, h, j); panels with a dark high-fill colour
## map colour per line instead (b, c, e, f, i, k, l): black on the decline
## (z <= 0) side, since that half of every panel's scale is always the same
## white-to-blue low end regardless of panel, white on the growth (z > 0)
## side where black would wash out against the dark fill (see the
## `colour = after_stat(...)` inside each of those geom_textcontour()'s
## aes(), paired with scale_colour_identity() so the literal colour strings
## aren't treated as a discrete scale to train). The 0% line itself is
## always included in the black side (level <= 0, not < 0) -- level == 0
## sitting in the "positive" branch on a white-positive panel would put a
## white line on the fill scale's own white midpoint, invisible. Negative
## (declining) contour levels get an
## explicit unicode minus ("-1.2%"), positive an explicit "+" ("+1.2%"),
## both bold -- a plain ASCII "-" was easy to miss on a curved, rotated
## contour label; parentheses were tried and rejected in favour of just
## fixing the sign's legibility directly. The raw MC point cloud is hidden
## by default (SHOW_MC_POINTS below; too much visual noise once the raster
## fill and contour lines already carry the same information) --
## former per-run DOMINANT sub-material median labels are gone too, since
## with the points hidden they no longer had anything to anchor to. A star
## marks the real-world "today" situation on panels a/b/e/g (x = 2019-2024
## TOTAL world GDP CAGR, a 5-year window; text label -- moved here from
## panel b -- only on panel a, sized large enough to actually read) -- a real
## 2024 anchor -- and on panels i/j/k/l (an assumption-range midpoint, not an
## empirical measurement, since no real calibration anchor exists there:
## Ferrous/Non-ferrous both start from u = 0.5, the shared central
## assumption, converted to each metal's own real-unit grade; Buildings/
## Civil each use their own central lifetime assumption instead of the
## pooled average). Panels f/h also get a star: Y is the real 2024
## recycling_world_2024/downcycling_world_2024 (same value as panels e/g's
## own star), X is the unweighted mean of the same central lifetime
## assumptions i/j/k/l's stars use (no real 2024 stock-by-enduse breakdown
## exists to weight it the way each MC run's own X is weighted -- see
## metal_lt_star/nonmet_lt_star below). Panels c/d also get a real 2024 star
## -- Y is metal_int_2024/nonmet_int_2024, from actual 2024 in-use stock
## (Parameters/MISO-Stock/stock_2024_total.csv) over real 2024 world GDP, X is the same
## gdp_cagr_hist every other GDP-growth-axis panel's star uses.
## Panel a additionally gets a downward arrow + "Absolute decoupling" label
## below the 0% line, since that's the only panel where the two-way split
## (declining vs. growing per-capita consumption) plotted on Y is explained
## anywhere in the figure.
## Region points (each region's own historical point, same region palette as
## Figure 1, direct-labelled on panel e only) are OFF by default -- see
## SHOW_REGION_POINTS below -- and only exist for panels a/b/e/g (no
## region-level data exists for the other panels' variables). Each panel's
## axis range is the MC simulation's own min/max, extended on Y (and, for
## f/h, also on X) to always include the world star where one exists;
## panels a/b/e/g additionally extend X to always include East Asia/South
## Asia when SHOW_REGION_POINTS is on (other region points are dropped
## instead of shown if they still fall more than 10% outside that extended
## range).

cat("\n=== Figure 3 (contour version) ===\n\n")

library(mgcv)

# Set TRUE to draw each region's historical point (+ panel c's region labels);
# FALSE shows only the MC contour/point cloud and the world star.
SHOW_REGION_POINTS <- FALSE

# Set TRUE to scatter every individual MC run (light alpha) under the raster +
# contour on every panel below; FALSE (default) keeps the raw point cloud
# hidden, per this figure's own design note further down.
SHOW_MC_POINTS <- TRUE
MC_POINT_LAYER <- if (SHOW_MC_POINTS) {
  geom_point(size = 0.4, alpha = 0.12, colour = "grey20", show.legend = FALSE)
} else {
  NULL
}

# Smoothness of every panel's GAM-fitted contour surface (mgcv::gam(z_var ~
# te(x_var, y_var, k = GAM_SMOOTH_K), ...) below) -- lower = smoother/coarser,
# higher = more flexible and closer to following the raw MC scatter's noise.
# Optional command-line override for the SI smoothness comparison, e.g.
#   Rscript "Scripts/05-Figures/Figure 3 - Contours-GrowthRate.R" 5
# (non-default K saves only the main figure, as S24_Fig3_SmoothK<K>.png)
GAM_SMOOTH_K_DEFAULT <- 8L
GAM_SMOOTH_K <- if (length(commandArgs(TRUE)) > 0) as.integer(commandArgs(TRUE)[1]) else GAM_SMOOTH_K_DEFAULT

X_LAB_CONTOUR <- "GDP annual growth rate (2025-2060)"
RECYC_LAB <- "Recycling rate (%)"
DOWNCYC_LAB <- "Downcycling rate (%)"
# (GRADE_LAB/LIFETIME_LAB removed -- panel e/f are now split into per-
# sub-material/sub-enduse Y-axis labels set directly on each of the 4 new
# panels below, rather than one pooled label shared by a single panel.)

# Historical star -- GDP growth (last 5 yr) + 2024 model inputs -------------
## GDP growth: 2019->2024 CAGR of TOTAL world GDP (regions summed, not
## per-capita -- matches the contour panels' total-GDP x-axis), the shared
## x-value for all six panels' stars. Panel y-values use world 2024 DMC per
## capita or per GDP (biomass/fossil) or the 2024 "now" recycling/
## downcycling rate (metals/minerals) -- the calibrated "today" inputs the MC
## trajectories start from (BASE_YEAR), not a simulated draw. No source here
## extends past 2024, so "2020-2025" collapses to the single year 2024.

gdp_percap_world_hist <- gdp_region_hist |>
  dplyr::filter(year %in% c(2019, 2024)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(gdp = sum(GDP_2015USD), .groups = "drop") |>
  dplyr::left_join(
    pop_region_hist |>
      dplyr::filter(year %in% c(2019, 2024)) |>
      dplyr::group_by(year) |>
      dplyr::summarise(pop = sum(population), .groups = "drop"),
    by = "year"
  ) |>
  dplyr::mutate(gdp_percap = gdp / pop)

gdp_cagr_hist <- (gdp_percap_world_hist$gdp[gdp_percap_world_hist$year == 2024] /
  gdp_percap_world_hist$gdp[gdp_percap_world_hist$year == 2019])^(1 / 5) -
  1

gdp_percap_2024 <- gdp_percap_world_hist$gdp_percap[gdp_percap_world_hist$year == 2024]

# Star, panel a: biomass kg/person = world 2024 DMC (same crops/grazed
# biomass/wood scope as x_biomass) / world population 2024
biomass_kgpc_2024 <- dmc_region_hist |>
  dplyr::filter(year == 2024, material_category %in% names(BIOMASS_MC_KEYS)) |>
  dplyr::summarise(x = sum(DMC_Mt, na.rm = TRUE) * 1e9) |>
  dplyr::pull(x) /
  gdp_percap_world_hist$pop[gdp_percap_world_hist$year == 2024]

# Star, panel b: fossil MJ/$ = world 2024 DMC per fuel x energy density, summed / world GDP 2024
fossil_2024_mj_usd <- dmc_region_hist |>
  dplyr::filter(year == 2024, material_category %in% names(FOSSIL_MC_KEYS)) |>
  dplyr::mutate(mj = DMC_Mt * 1e9 * ENERGY_DENSITY_MJ_PER_KG[FOSSIL_MC_KEYS[material_category]]) |>
  dplyr::summarise(x = sum(mj, na.rm = TRUE)) |>
  dplyr::pull(x) /
  gdp_percap_world_hist$gdp[gdp_percap_world_hist$year == 2024]

# Star + region points, panels c/d: current (2024) recycling/downcycling rate,
# computed directly as recovered outflow mass / total outflow mass -- NOT a
# rate average. Inputs/Recycling_Assumptions.xlsx only has literature RATES
# (no mass columns -- some are literal text ranges like "~70-85%"), so the
# previous version GDP-weighted those rates across regions into a world
# figure, which fixed the wrong problem (the weighting, not the rates
# themselves). Parameters/Intermediate/historical_secondary_flows.csv
# (extended by Scripts/02-HistoricalStock/03c_HistoricalSecondaryFlows.R
# Step 7 to also export waste_Mt/recovered_Mt) already carries the real
# mass-based ratio: recovered_Mt = MISO waste x the implied recycling rate
# (wedge/waste), i.e. genuine recovered-outflow mass, not a literature
# estimate. CAVEAT: MISO's waste series is held flat 2017-2024 (bridge-year
# convention) and the reconstruction is flagged by 03c's own diagnostics as
# noisy/small for non-metallic minerals -- some regions may come out near 0%
# for panel d; that's the honest number given available data, not a bug.

secondary_flows_2024 <- readr::read_csv(
  "Parameters/Intermediate/historical_secondary_flows.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(year == 2024)

recycling_world_2024 <- secondary_flows_2024 |>
  dplyr::filter(material_group == "metal_ores") |>
  dplyr::summarise(rate = sum(recovered_Mt, na.rm = TRUE) / sum(waste_Mt, na.rm = TRUE)) |>
  dplyr::pull(rate)

downcycling_world_2024 <- secondary_flows_2024 |>
  dplyr::filter(material_group == "nonmetallic_minerals") |>
  dplyr::summarise(rate = sum(recovered_Mt, na.rm = TRUE) / sum(waste_Mt, na.rm = TRUE)) |>
  dplyr::pull(rate)

# Star, panels c/d (metal/mineral STOCK intensity): real 2024 in-use stock
# (stock_2024_total.csv, loaded above -- full scope, every super_category/
# sub_use, matching x_metals_intensity/x_minerals_intensity's own scope) /
# real 2024 world GDP (gdp_region_hist, same source used everywhere else in
# this figure for a GDP anchor).
metal_stock_2024_Mt <- stock_2024_total |>
  dplyr::filter(material %in% c("Metal_Fe", "Metal_NonFe")) |>
  dplyr::summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE)) |>
  dplyr::pull(stock_Mt)
nonmet_stock_2024_Mt <- stock_2024_total |>
  dplyr::filter(material == "Non-metallic minerals") |>
  dplyr::summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE)) |>
  dplyr::pull(stock_Mt)
gdp_world_2024 <- sum(gdp_2024_region$gdp_2024)

metal_int_2024 <- metal_stock_2024_Mt * 1e9 / gdp_world_2024
nonmet_int_2024 <- nonmet_stock_2024_Mt * 1e9 / gdp_world_2024

# star_df (all 6 panels, keyed by `panel` a-f) is assembled further below --
# panels e/f's "star" is an assumption-range midpoint, not a measured 2024
# value (no empirical anchor exists for ore grade/lifetime; see below).

# Region-level historical points (panels a-d only) -- same GDP-growth
# (2019-2024) x panel-variable (2024) construction as star_df, per region
# instead of world total; same 8 regions/palette as Figure 1 (PALETTE_REGIONS,
# Scripts/00-CommonParameters.R). Direct region-name labels are drawn on panel
# c only (see Plot section); East Asia/South Asia (the region points furthest
# out on the GDP-growth axis) always get the X range extended to include them
# when SHOW_REGION_POINTS is TRUE (see x_disp_rng_a..d below); other regions
# are dropped instead of shown if they still fall more than 10% outside a
# panel's own MC-plus-Asia/world range. Panels e/f have no region-level data
# for ore grade/lifetime, so they carry no region points at all.


# Name kept as gdp_percap_region_hist for minimal diff elsewhere, but x_var is
# now each region's TOTAL GDP CAGR (GDP_2015USD is already a region total, not
# per-capita) -- matches the contour panels' total-GDP x-axis.
gdp_percap_region_hist <- gdp_region_hist |>
  dplyr::filter(year %in% c(2019, 2024)) |>
  dplyr::rename(region = Region) |>
  dplyr::select(region, year, GDP_2015USD) |>
  tidyr::pivot_wider(names_from = year, values_from = GDP_2015USD, names_prefix = "y") |>
  dplyr::mutate(x_var = (y2024 / y2019)^(1 / 5) - 1) |>
  dplyr::select(region, x_var)

biomass_region_2024 <- dmc_region_hist |>
  dplyr::filter(year == 2024, material_category %in% names(BIOMASS_MC_KEYS)) |>
  dplyr::rename(region = Region) |>
  dplyr::group_by(region) |>
  dplyr::summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(pop_region_hist |> dplyr::filter(year == 2024) |> dplyr::rename(region = Region), by = "region") |>
  dplyr::mutate(y_var = DMC_Mt * 1e9 / population, panel = "a") |>
  dplyr::select(region, panel, y_var)

fossil_region_2024 <- dmc_region_hist |>
  dplyr::filter(year == 2024, material_category %in% c("Coal", "Natural Gas", "Petroleum")) |>
  dplyr::mutate(
    mat_key = dplyr::case_when(
      material_category == "Coal" ~ "coal",
      material_category == "Natural Gas" ~ "gas",
      material_category == "Petroleum" ~ "oil"
    )
  ) |>
  dplyr::rename(region = Region) |>
  dplyr::left_join(gdp_region_hist |> dplyr::filter(year == 2024) |> dplyr::rename(region = Region), by = "region") |>
  dplyr::mutate(mj = DMC_Mt * 1e9 * ENERGY_DENSITY_MJ_PER_KG[mat_key]) |>
  dplyr::group_by(region) |>
  dplyr::summarise(mj_total = sum(mj, na.rm = TRUE), gdp = dplyr::first(GDP_2015USD), .groups = "drop") |>
  dplyr::mutate(y_var = mj_total / gdp, panel = "b") |>
  dplyr::select(region, panel, y_var)

secondary_flows_region_2024 <- secondary_flows_2024 |> dplyr::mutate(rate = recovered_Mt / waste_Mt)

metal_region_2024 <- secondary_flows_region_2024 |>
  dplyr::filter(material_group == "metal_ores") |>
  dplyr::transmute(region = Region, y_var = rate, panel = "c")

nonmet_region_2024 <- secondary_flows_region_2024 |>
  dplyr::filter(material_group == "nonmetallic_minerals") |>
  dplyr::transmute(region = Region, y_var = rate, panel = "d")

# World assumption-midpoint anchor (panels e/f) -- ore grade and lifetime have
# no real 2024 empirical calibration anywhere in the live pipeline (no region
# breakdown, no measured "now" value): GRADE_ORE_FE/NONFE_MIN/CENTRAL/MAX (Inputs/
# MC_Assumptions.xlsx sheet "Parameters", loaded generically in
# Scripts/04-Simulation/00-Parameters.R) and LIFETIME_SAMPLE_PARAMS' own
# mean_life column (sheet "Lifetimes") both use their central assumption
# value as the deterministic "now" anchor -- which for both metals' ore grade
# sits EXACTLY at u = 0.5 under semi-uniform sampling (same u-space x_metal_grade
# below uses), so grade_u_star is hardcoded rather than recomputed; any
# mass-weighted average of two values that are both 0.5 is still 0.5, so this
# holds regardless of the Fe/NonFe mass weighting x_metal_grade applies.
# The star therefore marks a central assumption, not a measurement -- flagged
# as such in the plot (no "(2024)" label, unlike panels a-d).

# UPDATE: the 2024 grade (GRADE_ORE_*_2024, 00-CommonParameters.R) is no longer
# the central value; star = simple Fe/NonFe average of its semi-uniform u.
grade_u_star <- mean(c(
  dplyr::if_else(
    GRADE_ORE_FE_2024 < GRADE_ORE_FE_CENTRAL,
    0.5 * (GRADE_ORE_FE_2024 - GRADE_ORE_FE_MIN) / (GRADE_ORE_FE_CENTRAL - GRADE_ORE_FE_MIN),
    0.5 + 0.5 * (GRADE_ORE_FE_2024 - GRADE_ORE_FE_CENTRAL) / (GRADE_ORE_FE_MAX - GRADE_ORE_FE_CENTRAL)
  ),
  dplyr::if_else(
    GRADE_ORE_NONFE_2024 < GRADE_ORE_NONFE_CENTRAL,
    0.5 * (GRADE_ORE_NONFE_2024 - GRADE_ORE_NONFE_MIN) / (GRADE_ORE_NONFE_CENTRAL - GRADE_ORE_NONFE_MIN),
    0.5 + 0.5 * (GRADE_ORE_NONFE_2024 - GRADE_ORE_NONFE_CENTRAL) / (GRADE_ORE_NONFE_MAX - GRADE_ORE_NONFE_CENTRAL)
  )
))

lifetime_star <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category %in% c("buildings", "civil_infrastructure")) |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)

# Historical star (all 6 panels), keyed by `panel` a-f -- x is always the same
# world GDP-growth value; y is each panel's own 2024 model input (a-d) or
# assumption-range midpoint (e-f, computed above).

star_df <- tibble::tibble(
  panel = c("a", "b", "c", "d", "e", "f"),
  x_var = gdp_cagr_hist,
  y_var = c(
    biomass_kgpc_2024,
    fossil_2024_mj_usd,
    recycling_world_2024,
    downcycling_world_2024,
    grade_u_star,
    lifetime_star
  )
)

region_points_all <- dplyr::bind_rows(biomass_region_2024, fossil_region_2024, metal_region_2024, nonmet_region_2024) |>
  dplyr::inner_join(gdp_percap_region_hist, by = "region") |>
  dplyr::mutate(region_colour = unname(PALETTE_REGIONS[region]))

# Fe vs NonFe consumption mass at FIG_YEAR, per run (wide) -- feeds the
# dominant-metal classification used by panel c's point colour (further
# below). (Panel e's ore grade used to be a Fe/NonFe mass-weighted average
# built from this -- since panel e is now split into separate Ferrous/
# Non-ferrous contour panels, each just uses its own grade_ore_* draw
# directly; see panel_e_fe_contour_df/panel_e_nonfe_contour_df further down.)

metal_mass_by_type <- results_target |>
  dplyr::filter(material_group %in% c("metal_fe", "metal_nonfe")) |>
  dplyr::mutate(mass_Mt = primary_consumption_Mt + secondary_supply_Mt) |>
  dplyr::group_by(run_id, material_group) |>
  dplyr::summarise(mass_Mt = sum(mass_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = material_group, values_from = mass_Mt, values_fill = 0)

# Y (panels c/d, replacing Fig3_dots's secondary-share axis): each run's targeted
# recycling rate (metals) / downcycling rate (minerals), decoded from the same
# mc_input_matrix columns and min/central/max values Scripts/04-Simulation/
# 02-RunSimulations.R uses to build its recycling/downcycling endpoints
# (RECYCLING_RATE_FE_*, RECYCLING_RATE_NONFE_*, DOWNCYCLING_* -- all loaded via
# Scripts/04-Simulation/00-Parameters.R, sourced at the top of this script),
# with the same semi-uniform mapping (u < 0.5: min..central, else central..max).
# This is exactly the endpoint rate the simulation applies to every region
# (shared global value) -- it shows where each run's recycling ambition sits,
# without re-deriving the whole trajectory.

x_metals_recyc <- mc_input_matrix |>
  dplyr::transmute(
    run_id,
    recycling_fe = dplyr::if_else(
      recycling_rate_fe < 0.5,
      RECYCLING_RATE_FE_MIN + 2 * recycling_rate_fe * (RECYCLING_RATE_FE_CENTRAL - RECYCLING_RATE_FE_MIN),
      RECYCLING_RATE_FE_CENTRAL + 2 * (recycling_rate_fe - 0.5) * (RECYCLING_RATE_FE_MAX - RECYCLING_RATE_FE_CENTRAL)
    ),
    recycling_nonfe = dplyr::if_else(
      recycling_rate_nonfe < 0.5,
      RECYCLING_RATE_NONFE_MIN + 2 * recycling_rate_nonfe * (RECYCLING_RATE_NONFE_CENTRAL - RECYCLING_RATE_NONFE_MIN),
      RECYCLING_RATE_NONFE_CENTRAL + 2 * (recycling_rate_nonfe - 0.5) * (RECYCLING_RATE_NONFE_MAX - RECYCLING_RATE_NONFE_CENTRAL)
    ),
    x = (recycling_fe + recycling_nonfe) / 2,
    material = "Metal ores"
  ) |>
  dplyr::select(run_id, x, material)

x_minerals_downcyc <- mc_input_matrix |>
  dplyr::transmute(
    run_id,
    x = dplyr::if_else(
      downcycling < 0.5,
      DOWNCYCLING_MIN + 2 * downcycling * (DOWNCYCLING_CENTRAL - DOWNCYCLING_MIN),
      DOWNCYCLING_CENTRAL + 2 * (downcycling - 0.5) * (DOWNCYCLING_MAX - DOWNCYCLING_CENTRAL)
    ),
    material = "Non-metallic minerals"
  ) |>
  dplyr::select(run_id, x, material)

# Per-panel contour data: y = panel variable, x = GDP CAGR, z = material CAGR --

panel_a_contour_df <- x_biomass |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Biomass"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_b_contour_df <- x_fossil |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Fossil fuels"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_c_contour_df <- x_metals_recyc |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Metal ores"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_d_contour_df <- x_minerals_downcyc |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Non-metallic minerals"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

# Panel e is split into TWO contour panels (Ferrous / Non-ferrous ore grade)
# instead of pairing with a stacked-area companion, and panel f likewise
# splits into Buildings / Civil infrastructure (average lifetime). Both need
# each sub-material's/sub-enduse's OWN total (world, population-inclusive)
# primary-consumption CAGR -- 04-Decoupling.R pools metal_fe/metal_nonfe into
# "Metal ores" (and doesn't split non-metallic minerals by enduse at all)
# before computing any CAGR, so there is no pre-existing split series to
# reuse. Recomputed here directly from `results` (already loaded, full
# multi-year), same 2025-2060 window as the FIG_VARIANT_ID variant used
# everywhere else in this figure.

metal_fe_cagr <- results |>
  dplyr::filter(material_group == "metal_fe", year %in% c(2025L, FIG_YEAR)) |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(world_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = world_Mt, names_prefix = "yr") |>
  dplyr::filter(yr2025 > 0, yr2060 > 0) |>
  dplyr::transmute(run_id, z_var = (yr2060 / yr2025)^(1 / 35) - 1)

metal_nonfe_cagr <- results |>
  dplyr::filter(material_group == "metal_nonfe", year %in% c(2025L, FIG_YEAR)) |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(world_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = world_Mt, names_prefix = "yr") |>
  dplyr::filter(yr2025 > 0, yr2060 > 0) |>
  dplyr::transmute(run_id, z_var = (yr2060 / yr2025)^(1 / 35) - 1)

nonmet_buildings_cagr <- results |>
  dplyr::filter(material_group == "nonmetallic_minerals", year %in% c(2025L, FIG_YEAR)) |>
  dplyr::mutate(enduse_category = ENDUSE_CATEGORY[material_key]) |>
  dplyr::filter(enduse_category == "buildings") |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(world_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = world_Mt, names_prefix = "yr") |>
  dplyr::filter(yr2025 > 0, yr2060 > 0) |>
  dplyr::transmute(run_id, z_var = (yr2060 / yr2025)^(1 / 35) - 1)

nonmet_civil_cagr <- results |>
  dplyr::filter(material_group == "nonmetallic_minerals", year %in% c(2025L, FIG_YEAR)) |>
  dplyr::mutate(enduse_category = ENDUSE_CATEGORY[material_key]) |>
  dplyr::filter(enduse_category == "civil") |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(world_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = world_Mt, names_prefix = "yr") |>
  dplyr::filter(yr2025 > 0, yr2060 > 0) |>
  dplyr::transmute(run_id, z_var = (yr2060 / yr2025)^(1 / 35) - 1)

# Star anchors for the 4 new split panels -- ferrous/non-ferrous ore grade
# each convert the shared u = 0.5 central-assumption midpoint into that
# metal's OWN real-unit grade (GRADE_ORE_FE/NONFE_CENTRAL, loaded via
# Scripts/04-Simulation/00-Parameters.R) -- these differ a lot in real units
# (~0.4 vs ~0.016 metal fraction) even though both sit at u = 0.5; buildings/
# civil lifetime average LIFETIME_SAMPLE_PARAMS' own central "mean_life"
# value WITHIN each super_category (it has one row per sub_use, not one per
# super_category -- same fan-out lifetime_dr's join above relies on -- so
# each super_category's own sub_use rows still need averaging down to a
# single anchor, same idea as the pooled lifetime_star above, just scoped to
# one super_category instead of both).
grade_fe_star <- GRADE_ORE_FE_2024 # 2024 baseline (00-CommonParameters.R)
grade_nonfe_star <- GRADE_ORE_NONFE_2024
buildings_lifetime_star <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category == "buildings") |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)
civil_lifetime_star <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category == "civil_infrastructure") |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)
machinery_lifetime_star <- LIFETIME_SAMPLE_PARAMS |>
  dplyr::filter(super_category == "machinery") |>
  dplyr::summarise(mean_life = mean(mean_life, na.rm = TRUE)) |>
  dplyr::pull(mean_life)

star_df <- dplyr::bind_rows(
  star_df,
  tibble::tibble(
    panel = c("e_fe", "e_nonfe", "f_bldg", "f_civil"),
    x_var = gdp_cagr_hist,
    y_var = c(grade_fe_star, grade_nonfe_star, buildings_lifetime_star, civil_lifetime_star)
  )
)

# Star, panels f/h (metal recycling / mineral downcycling vs stock-weighted
# lifetime): Y is the same real 2024 recycling_world_2024/downcycling_world_2024
# already used for panels e/g's own star. X has no real 2024 stock-by-enduse
# breakdown to weight by (mc_results.parquet only starts at year 2025, no
# BASE_YEAR row to reconstruct 2024 stock mix from) -- so, like the i/j/k/l
# stars above, X is a central-assumption anchor instead of a measurement: the
# simple (unweighted) mean of each applicable end-use's own central lifetime
# assumption (buildings/civil/machinery for metals; buildings/civil for
# minerals) -- not stock-weighted the way each MC run's own X is, since no
# real weight exists for "today".
metal_lt_star <- mean(c(buildings_lifetime_star, civil_lifetime_star, machinery_lifetime_star))
nonmet_lt_star <- mean(c(buildings_lifetime_star, civil_lifetime_star))

star_df <- dplyr::bind_rows(
  star_df,
  tibble::tibble(
    panel = c("metals_lt", "nonmet_lt"),
    x_var = c(metal_lt_star, nonmet_lt_star),
    y_var = c(recycling_world_2024, downcycling_world_2024)
  ),
  tibble::tibble(
    panel = c("metals_int", "nonmet_int"),
    x_var = gdp_cagr_hist,
    y_var = c(metal_int_2024, nonmet_int_2024)
  )
)

# Y = the actual ore grade (real units, semi-uniform GRADE_ORE_*_MIN/CENTRAL/MAX), not the
# raw [0,1] sampling index -- Fe and NonFe each get their own real-unit scale
# rather than sharing the [0,1] u-space, since their real grades live on very
# different magnitudes (~0.4 vs ~0.016 metal fraction).
panel_e_fe_contour_df <- mc_input_matrix |>
  dplyr::transmute(
    run_id,
    y_var = dplyr::if_else(
      grade_ore_fe < 0.5,
      GRADE_ORE_FE_MIN + 2 * grade_ore_fe * (GRADE_ORE_FE_CENTRAL - GRADE_ORE_FE_MIN),
      GRADE_ORE_FE_CENTRAL + 2 * (grade_ore_fe - 0.5) * (GRADE_ORE_FE_MAX - GRADE_ORE_FE_CENTRAL)
    )
  ) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Metal ores"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr) |>
  dplyr::inner_join(metal_fe_cagr, by = "run_id") |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_e_nonfe_contour_df <- mc_input_matrix |>
  dplyr::transmute(
    run_id,
    y_var = dplyr::if_else(
      grade_ore_nonfe < 0.5,
      GRADE_ORE_NONFE_MIN + 2 * grade_ore_nonfe * (GRADE_ORE_NONFE_CENTRAL - GRADE_ORE_NONFE_MIN),
      GRADE_ORE_NONFE_CENTRAL + 2 * (grade_ore_nonfe - 0.5) * (GRADE_ORE_NONFE_MAX - GRADE_ORE_NONFE_CENTRAL)
    )
  ) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Metal ores"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr) |>
  dplyr::inner_join(metal_nonfe_cagr, by = "run_id") |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_f_bldg_contour_df <- lifetime_dr |>
  dplyr::filter(super_cat == "buildings") |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(y_var = mean(mean_life), .groups = "drop") |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Non-metallic minerals"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr) |>
  dplyr::inner_join(nonmet_buildings_cagr, by = "run_id") |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_f_civil_contour_df <- lifetime_dr |>
  dplyr::filter(super_cat == "civil_infrastructure") |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(y_var = mean(mean_life), .groups = "drop") |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Non-metallic minerals"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr) |>
  dplyr::inner_join(nonmet_civil_cagr, by = "run_id") |>
  tidyr::drop_na(x_var, y_var, z_var)

cat(
  "  Contour panel rows -- a:",
  nrow(panel_a_contour_df),
  "| b:",
  nrow(panel_b_contour_df),
  "| c:",
  nrow(panel_c_contour_df),
  "| d:",
  nrow(panel_d_contour_df),
  "| e (Fe):",
  nrow(panel_e_fe_contour_df),
  "| e (NonFe):",
  nrow(panel_e_nonfe_contour_df),
  "| f (Bldg):",
  nrow(panel_f_bldg_contour_df),
  "| f (Civil):",
  nrow(panel_f_civil_contour_df),
  "\n\n"
)

# Contour interpolation: scattered (x_var, y_var, z_var) -> smooth grid -----
## Raw scattered-point interpolation (e.g. akima's triangulation) passes
## exactly through every noisy point, so the contour lines inherit all of the
## MC scatter's point-to-point noise and come out jagged. A fitted smooth
## surface -- a thin-plate/tensor regression spline via mgcv::gam(), evaluated
## on a regular grid -- averages that noise away instead of interpolating
## through it, and te() handles x/y on very different scales (GDP CAGR vs.
## e.g. crop kg/person) natively, with no manual rescaling needed. `k`
## controls smoothness (lower = smoother); grid resolution is separate (100x100).
## The grid is padded 5% beyond the data range on each side (matching ggplot's
## own default axis expansion) so the fitted surface -- and hence the contour
## lines -- extrapolate all the way to the panel edges instead of stopping
## short of them (which otherwise left some contour labels with no visible
## line nearby, near the edge of the unpadded grid). te() smooths are
## unconstrained outside the data's support, so in that padded margin the
## prediction can shoot far past the real data's range; left alone, that blows
## out the min/max stat_contour uses to auto-pick its 6 levels, so almost all
## of them end up bunched in the (tiny, extreme) padded corners instead of
## spread across the actual data. Clipping each prediction back to the
## observed z_var range (right after predict(), below) keeps the levels
## sensible while still letting the (now flattened-off) surface extend
## smoothly all the way to the padded edges.

gam_a <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_a_contour_df)
x_rng_a <- range(panel_a_contour_df$x_var)
y_rng_a <- range(panel_a_contour_df$y_var)
grid_a_df <- expand.grid(
  x_var = seq(x_rng_a[1] - 0.05 * diff(x_rng_a), x_rng_a[2] + 0.05 * diff(x_rng_a), length.out = 100),
  y_var = seq(y_rng_a[1] - 0.05 * diff(y_rng_a), y_rng_a[2] + 0.05 * diff(y_rng_a), length.out = 100)
)
grid_a_df$z_var <- predict(gam_a, newdata = grid_a_df)
z_obs_rng_a <- range(panel_a_contour_df$z_var)
grid_a_df$z_var <- pmin(pmax(grid_a_df$z_var, z_obs_rng_a[1]), z_obs_rng_a[2])

gam_b <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_b_contour_df)
x_rng_b <- range(panel_b_contour_df$x_var)
y_rng_b <- range(panel_b_contour_df$y_var)
grid_b_df <- expand.grid(
  x_var = seq(x_rng_b[1] - 0.05 * diff(x_rng_b), x_rng_b[2] + 0.05 * diff(x_rng_b), length.out = 100),
  y_var = seq(y_rng_b[1] - 0.05 * diff(y_rng_b), y_rng_b[2] + 0.05 * diff(y_rng_b), length.out = 100)
)
grid_b_df$z_var <- predict(gam_b, newdata = grid_b_df)
z_obs_rng_b <- range(panel_b_contour_df$z_var)
grid_b_df$z_var <- pmin(pmax(grid_b_df$z_var, z_obs_rng_b[1]), z_obs_rng_b[2])

gam_c <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_c_contour_df)
x_rng_c <- range(panel_c_contour_df$x_var)
y_rng_c <- range(panel_c_contour_df$y_var)
grid_c_df <- expand.grid(
  x_var = seq(x_rng_c[1] - 0.05 * diff(x_rng_c), x_rng_c[2] + 0.05 * diff(x_rng_c), length.out = 100),
  y_var = seq(y_rng_c[1] - 0.05 * diff(y_rng_c), y_rng_c[2] + 0.05 * diff(y_rng_c), length.out = 100)
)
grid_c_df$z_var <- predict(gam_c, newdata = grid_c_df)
z_obs_rng_c <- range(panel_c_contour_df$z_var)
grid_c_df$z_var <- pmin(pmax(grid_c_df$z_var, z_obs_rng_c[1]), z_obs_rng_c[2])

gam_d <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_d_contour_df)
x_rng_d <- range(panel_d_contour_df$x_var)
y_rng_d <- range(panel_d_contour_df$y_var)
grid_d_df <- expand.grid(
  x_var = seq(x_rng_d[1] - 0.05 * diff(x_rng_d), x_rng_d[2] + 0.05 * diff(x_rng_d), length.out = 100),
  y_var = seq(y_rng_d[1] - 0.05 * diff(y_rng_d), y_rng_d[2] + 0.05 * diff(y_rng_d), length.out = 100)
)
grid_d_df$z_var <- predict(gam_d, newdata = grid_d_df)
z_obs_rng_d <- range(panel_d_contour_df$z_var)
grid_d_df$z_var <- pmin(pmax(grid_d_df$z_var, z_obs_rng_d[1]), z_obs_rng_d[2])

gam_e_fe <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_e_fe_contour_df)
x_rng_e_fe <- range(panel_e_fe_contour_df$x_var)
y_rng_e_fe <- range(panel_e_fe_contour_df$y_var)
grid_e_fe_df <- expand.grid(
  x_var = seq(x_rng_e_fe[1] - 0.05 * diff(x_rng_e_fe), x_rng_e_fe[2] + 0.05 * diff(x_rng_e_fe), length.out = 100),
  y_var = seq(y_rng_e_fe[1] - 0.05 * diff(y_rng_e_fe), y_rng_e_fe[2] + 0.05 * diff(y_rng_e_fe), length.out = 100)
)
grid_e_fe_df$z_var <- predict(gam_e_fe, newdata = grid_e_fe_df)
z_obs_rng_e_fe <- range(panel_e_fe_contour_df$z_var)
grid_e_fe_df$z_var <- pmin(pmax(grid_e_fe_df$z_var, z_obs_rng_e_fe[1]), z_obs_rng_e_fe[2])

gam_e_nonfe <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_e_nonfe_contour_df)
x_rng_e_nonfe <- range(panel_e_nonfe_contour_df$x_var)
y_rng_e_nonfe <- range(panel_e_nonfe_contour_df$y_var)
grid_e_nonfe_df <- expand.grid(
  x_var = seq(
    x_rng_e_nonfe[1] - 0.05 * diff(x_rng_e_nonfe),
    x_rng_e_nonfe[2] + 0.05 * diff(x_rng_e_nonfe),
    length.out = 100
  ),
  y_var = seq(
    y_rng_e_nonfe[1] - 0.05 * diff(y_rng_e_nonfe),
    y_rng_e_nonfe[2] + 0.05 * diff(y_rng_e_nonfe),
    length.out = 100
  )
)
grid_e_nonfe_df$z_var <- predict(gam_e_nonfe, newdata = grid_e_nonfe_df)
z_obs_rng_e_nonfe <- range(panel_e_nonfe_contour_df$z_var)
grid_e_nonfe_df$z_var <- pmin(pmax(grid_e_nonfe_df$z_var, z_obs_rng_e_nonfe[1]), z_obs_rng_e_nonfe[2])

gam_f_bldg <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_f_bldg_contour_df)
x_rng_f_bldg <- range(panel_f_bldg_contour_df$x_var)
y_rng_f_bldg <- range(panel_f_bldg_contour_df$y_var)
grid_f_bldg_df <- expand.grid(
  x_var = seq(
    x_rng_f_bldg[1] - 0.05 * diff(x_rng_f_bldg),
    x_rng_f_bldg[2] + 0.05 * diff(x_rng_f_bldg),
    length.out = 100
  ),
  y_var = seq(
    y_rng_f_bldg[1] - 0.05 * diff(y_rng_f_bldg),
    y_rng_f_bldg[2] + 0.05 * diff(y_rng_f_bldg),
    length.out = 100
  )
)
grid_f_bldg_df$z_var <- predict(gam_f_bldg, newdata = grid_f_bldg_df)
z_obs_rng_f_bldg <- range(panel_f_bldg_contour_df$z_var)
grid_f_bldg_df$z_var <- pmin(pmax(grid_f_bldg_df$z_var, z_obs_rng_f_bldg[1]), z_obs_rng_f_bldg[2])

gam_f_civil <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_f_civil_contour_df)
x_rng_f_civil <- range(panel_f_civil_contour_df$x_var)
y_rng_f_civil <- range(panel_f_civil_contour_df$y_var)
grid_f_civil_df <- expand.grid(
  x_var = seq(
    x_rng_f_civil[1] - 0.05 * diff(x_rng_f_civil),
    x_rng_f_civil[2] + 0.05 * diff(x_rng_f_civil),
    length.out = 100
  ),
  y_var = seq(
    y_rng_f_civil[1] - 0.05 * diff(y_rng_f_civil),
    y_rng_f_civil[2] + 0.05 * diff(y_rng_f_civil),
    length.out = 100
  )
)
grid_f_civil_df$z_var <- predict(gam_f_civil, newdata = grid_f_civil_df)
z_obs_rng_f_civil <- range(panel_f_civil_contour_df$z_var)
grid_f_civil_df$z_var <- pmin(pmax(grid_f_civil_df$z_var, z_obs_rng_f_civil[1]), z_obs_rng_f_civil[2])

# Dominant sub-material per run, by consumption LEVEL at FIG_YEAR --------
## Replaces the point cloud's previous shared Spectral growth-rate colour
## (redundant with the contour lines' own numeric labels) with a categorical
## colour + direct label showing which sub-material/end-use sector dominates
## that run's consumption. Colours reuse the project's own palettes
## (PALETTE_MATERIALS/PALETTE_ENDUSE, Scripts/00-CommonParameters.R) for
## consistency with every other figure.

# Panel a: dominant of {Crops, Grazed biomass and fodder crops, Wood} by
# realized world consumption (Mt) at FIG_YEAR -- same ranking as kg/person.
dominant_a <- results |>
  dplyr::filter(year == FIG_YEAR, material_group == "biomass", material_key %in% names(BIOMASS_MC_KEYS)) |>
  dplyr::group_by(run_id, material_key) |>
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::slice_max(mass_Mt, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(run_id, dominant = material_key)
DOMINANT_COLORS_A <- PALETTE_MATERIALS[unique(dominant_a$dominant)]

# Panel b: dominant of {Coal, Natural Gas, Petroleum} by realized MJ at FIG_YEAR.
dominant_b <- results |>
  dplyr::filter(year == FIG_YEAR, material_group == "fossil_fuels", material_key %in% names(FOSSIL_MC_KEYS)) |>
  dplyr::mutate(mj = primary_consumption_Mt * ENERGY_DENSITY_MJ_PER_KG[FOSSIL_MC_KEYS[material_key]]) |>
  dplyr::group_by(run_id, material_key) |>
  dplyr::summarise(mj = sum(mj, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::slice_max(mj, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(run_id, dominant = material_key)
DOMINANT_COLORS_B <- PALETTE_MATERIALS[unique(dominant_b$dominant)]

# Panels c & e (Metal ores): dominant of {Ferrous ores, Non-ferrous ores} by
# mass (primary + secondary Mt) at FIG_YEAR -- shared between both panels,
# same underlying material; reuses metal_mass_by_type (computed above,
# "Y (panel e)" section).
dominant_metal <- metal_mass_by_type |>
  dplyr::transmute(run_id, dominant = dplyr::if_else(metal_fe >= metal_nonfe, "Ferrous ores", "Non-ferrous ores"))
DOMINANT_COLORS_METAL <- PALETTE_MATERIALS[unique(dominant_metal$dominant)]

# Panels d & f (Non-metallic minerals): no material-sub-type split exists in
# the MC simulation (only historical DMC data has construction-vs-industrial;
# the simulation only tracks end-use sector) -- substitute dominant end-use
# sector {Buildings, Civil infrastructure} by mass, same buildings+civil scope
# already used by x_minerals/lifetime_dr. Shared between both panels.
dominant_nonmet <- results_target |>
  dplyr::filter(material_group == "nonmetallic_minerals", enduse_category %in% c("buildings", "civil")) |>
  dplyr::mutate(mass_Mt = primary_consumption_Mt + secondary_supply_Mt) |>
  dplyr::group_by(run_id, enduse_category) |>
  dplyr::summarise(mass_Mt = sum(mass_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::slice_max(mass_Mt, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    run_id,
    dominant = dplyr::recode(enduse_category, buildings = "Buildings", civil = "Civil infrastructure")
  )
DOMINANT_COLORS_NONMET <- PALETTE_ENDUSE[unique(dominant_nonmet$dominant)]

panel_a_contour_df <- panel_a_contour_df |> dplyr::inner_join(dominant_a, by = "run_id")
panel_b_contour_df <- panel_b_contour_df |> dplyr::inner_join(dominant_b, by = "run_id")
panel_c_contour_df <- panel_c_contour_df |> dplyr::inner_join(dominant_metal, by = "run_id")
panel_d_contour_df <- panel_d_contour_df |> dplyr::inner_join(dominant_nonmet, by = "run_id")
# (panels e/f no longer join `dominant` -- they're now split into per-sub-
# material/enduse contour panels, each already scoped to a single category,
# so a "dominant" classification has nothing left to distinguish.)

# One direct label per dominant category, at that category's point-cloud
# median (x_var, y_var) -- same idea as the static Fig3_dots panel c labels, just
# nudged upward via vjust rather than an x/y offset, since the categories
# already sit in visually distinct parts of each panel.
dominant_labels_a <- panel_a_contour_df |>
  dplyr::group_by(dominant) |>
  dplyr::summarise(x_var = median(x_var), y_var = median(y_var), .groups = "drop")
dominant_labels_b <- panel_b_contour_df |>
  dplyr::group_by(dominant) |>
  dplyr::summarise(x_var = median(x_var), y_var = median(y_var), .groups = "drop")
dominant_labels_c <- panel_c_contour_df |>
  dplyr::group_by(dominant) |>
  dplyr::summarise(x_var = median(x_var), y_var = median(y_var), .groups = "drop")
dominant_labels_d <- panel_d_contour_df |>
  dplyr::group_by(dominant) |>
  dplyr::summarise(x_var = median(x_var), y_var = median(y_var), .groups = "drop")

# Axis display ranges: the raw MC range (x_rng_*/y_rng_*, already computed
# above for the contour grids), extended on Y to always include the world
# star (every panel) and, for panels a-d, on X to always include East Asia
# and South Asia (the fastest-growing regions) when SHOW_REGION_POINTS is
# TRUE -- other region points are dropped instead of shown if they still fall
# more than 10% outside that extended range. Panels e/f never show region
# points (no region-level ore-grade/lifetime data exists), so their X range
# is just the MC range extended to include the star instead.

if (SHOW_REGION_POINTS) {
  asia_x_range <- range(region_points_all$x_var[region_points_all$region %in% c("East Asia", "South Asia")])
} else {
  asia_x_range <- c(NA_real_, NA_real_) # unused when SHOW_REGION_POINTS is FALSE
}

x_disp_rng_a <- if (SHOW_REGION_POINTS) range(c(x_rng_a, asia_x_range)) else x_rng_a
y_disp_rng_a <- range(c(y_rng_a, star_df$y_var[star_df$panel == "a"]))
x_disp_rng_b <- if (SHOW_REGION_POINTS) range(c(x_rng_b, asia_x_range)) else x_rng_b
y_disp_rng_b <- range(c(y_rng_b, star_df$y_var[star_df$panel == "b"]))
x_disp_rng_c <- if (SHOW_REGION_POINTS) range(c(x_rng_c, asia_x_range)) else x_rng_c
y_disp_rng_c <- range(c(y_rng_c, star_df$y_var[star_df$panel == "c"]))
x_disp_rng_d <- if (SHOW_REGION_POINTS) range(c(x_rng_d, asia_x_range)) else x_rng_d
y_disp_rng_d <- range(c(y_rng_d, star_df$y_var[star_df$panel == "d"]))
x_disp_rng_e_fe <- range(c(x_rng_e_fe, gdp_cagr_hist))
y_disp_rng_e_fe <- range(c(y_rng_e_fe, star_df$y_var[star_df$panel == "e_fe"]))
x_disp_rng_e_nonfe <- range(c(x_rng_e_nonfe, gdp_cagr_hist))
y_disp_rng_e_nonfe <- range(c(y_rng_e_nonfe, star_df$y_var[star_df$panel == "e_nonfe"]))
x_disp_rng_f_bldg <- range(c(x_rng_f_bldg, gdp_cagr_hist))
y_disp_rng_f_bldg <- range(c(y_rng_f_bldg, star_df$y_var[star_df$panel == "f_bldg"]))
x_disp_rng_f_civil <- range(c(x_rng_f_civil, gdp_cagr_hist))
y_disp_rng_f_civil <- range(c(y_rng_f_civil, star_df$y_var[star_df$panel == "f_civil"]))

region_points_a <- region_points_all |>
  dplyr::filter(
    panel == "a",
    x_var >= x_disp_rng_a[1] - 0.10 * diff(x_disp_rng_a),
    x_var <= x_disp_rng_a[2] + 0.10 * diff(x_disp_rng_a),
    y_var >= y_disp_rng_a[1] - 0.10 * diff(y_disp_rng_a),
    y_var <= y_disp_rng_a[2] + 0.10 * diff(y_disp_rng_a)
  )

region_points_b <- region_points_all |>
  dplyr::filter(
    panel == "b",
    x_var >= x_disp_rng_b[1] - 0.10 * diff(x_disp_rng_b),
    x_var <= x_disp_rng_b[2] + 0.10 * diff(x_disp_rng_b),
    y_var >= y_disp_rng_b[1] - 0.10 * diff(y_disp_rng_b),
    y_var <= y_disp_rng_b[2] + 0.10 * diff(y_disp_rng_b)
  )

region_points_c <- region_points_all |>
  dplyr::filter(
    panel == "c",
    x_var >= x_disp_rng_c[1] - 0.10 * diff(x_disp_rng_c),
    x_var <= x_disp_rng_c[2] + 0.10 * diff(x_disp_rng_c),
    y_var >= y_disp_rng_c[1] - 0.10 * diff(y_disp_rng_c),
    y_var <= y_disp_rng_c[2] + 0.10 * diff(y_disp_rng_c)
  ) |>
  # "most recent value" note -- attached to just one region (East Asia), since
  # every panel's own model input (Y) already uses 2024, the latest available.
  dplyr::mutate(label_text = dplyr::if_else(region == "East Asia", paste0(region, " (2024)"), region))

region_points_d <- region_points_all |>
  dplyr::filter(
    panel == "d",
    x_var >= x_disp_rng_d[1] - 0.10 * diff(x_disp_rng_d),
    x_var <= x_disp_rng_d[2] + 0.10 * diff(x_disp_rng_d),
    y_var >= y_disp_rng_d[1] - 0.10 * diff(y_disp_rng_d),
    y_var <= y_disp_rng_d[2] + 0.10 * diff(y_disp_rng_d)
  )

# 0% contour break -----------------------------------------------------------
## geom_textcontour's default bins = 6 auto-picks levels that may not include
## exactly 0; explicit breaks (extended_breaks(), the same algorithm ggplot's
## own axis breaks use) guarantee 0 is one of them, so the growth/decline
## boundary always gets its own signed numeric label even though every
## contour line now shares one equal linewidth (CONTOUR_LINEWIDTH below).
## Computed over the actual (often asymmetric) OBSERVED z range, same as the
## grid the lines are traced from -- a break outside that range has nothing
## in the (already range-clipped) grid data to cross, so no line gets drawn
## for it. These are contour-line levels only; the legend's own labelled
## ticks use a separate, wider break set spanning the fill scale's full
## symmetric range (brks_legend_a etc., computed alongside each z_lim_*
## below) -- conflating the two here previously made most contour lines
## silently vanish, since most of that wider range falls outside what the
## clipped grid can actually draw.

brks_a <- scales::extended_breaks(n = 6)(range(grid_a_df$z_var))
if (!0 %in% brks_a) {
  brks_a <- sort(c(brks_a, 0))
}
brks_b <- scales::extended_breaks(n = 6)(range(grid_b_df$z_var))
if (!0 %in% brks_b) {
  brks_b <- sort(c(brks_b, 0))
}
brks_c <- scales::extended_breaks(n = 6)(range(grid_c_df$z_var))
if (!0 %in% brks_c) {
  brks_c <- sort(c(brks_c, 0))
}
brks_d <- scales::extended_breaks(n = 6)(range(grid_d_df$z_var))
if (!0 %in% brks_d) {
  brks_d <- sort(c(brks_d, 0))
}
brks_e_fe <- scales::extended_breaks(n = 6)(range(grid_e_fe_df$z_var))
if (!0 %in% brks_e_fe) {
  brks_e_fe <- sort(c(brks_e_fe, 0))
}
brks_e_nonfe <- scales::extended_breaks(n = 6)(range(grid_e_nonfe_df$z_var))
if (!0 %in% brks_e_nonfe) {
  brks_e_nonfe <- sort(c(brks_e_nonfe, 0))
}
brks_f_bldg <- scales::extended_breaks(n = 6)(range(grid_f_bldg_df$z_var))
if (!0 %in% brks_f_bldg) {
  brks_f_bldg <- sort(c(brks_f_bldg, 0))
}
brks_f_civil <- scales::extended_breaks(n = 6)(range(grid_f_civil_df$z_var))
if (!0 %in% brks_f_civil) {
  brks_f_civil <- sort(c(brks_f_civil, 0))
}

CONTOUR_LINEWIDTH <- 0.18 # every contour line, every panel, except the 0% line
CONTOUR_LINEWIDTH_ZERO <- 0.4 # 0% line only -- slightly bolder so the growth/decline boundary stands out

# Background fill -- one EXCLUSIVE legend per panel, own observed range AND
# own custom diverging palette each --------------------------------------
## Raster drawn straight from the same GAM-interpolated grid each contour's
## lines are traced from (grid_a_df, grid_b_df, ..., grid_e_fe_df, etc.), so
## colour and lines are always pixel-consistent -- no separate interpolation
## step. Each panel gets its OWN scale sized to ITS OWN observed |z| range
## (not a shared range across all of them) -- panels genuinely differ in how
## fast their material grows, and forcing one shared scale washed out the
## panels with smaller swings.
## Custom diverging low/mid/high instead of the stock Spectral palette: low
## (decline) end is Spectral's own characteristic blue, 0% is white, and the
## high (growth) end is that panel's own material's colour from the project
## palette (PALETTE_MATERIAL_GROUPS for a-d, or PALETTE_MATERIALS/
## PALETTE_ENDUSE for e/f's split sub-panels -- the same palettes Figure 1
## uses) -- so the warm end of each panel's scale visually matches that
## category's colour everywhere else in the project instead of a generic
## red. Two exceptions: Non-metallic minerals' (#78909C) and Buildings'
## (#1B4F8A) own project colours are themselves blue-grey/blue, which would
## be nearly indistinguishable from the scale's own blue low end -- those two
## panels get a substitute warm colour instead (still semantically fitting --
## stone-tan for minerals, terracotta for buildings -- just not blue). Same
## guide styling (colourbar spans the full ~8.7cm panel height, title rotated
## vertically beside it) reused across all 8 panels -- a plain object, not a
## function -- since only the limits/high-colour vary per panel.
BLUE_SPECTRAL <- "#3288BD" # characteristic blue from the ColorBrewer/scientific Spectral palette
NONMET_FILL_HIGH <- "#A1887F" # stone-tan substitute for Non-metallic minerals' own blue-grey (#78909C)
BUILDINGS_FILL_HIGH <- "#C1440E" # terracotta substitute for Buildings' own blue (#1B4F8A)
CONTOUR_FILL_NAME <- "Primary material consumption\nannual growth rate (2025-2060)"
# Legend colourbar ticks/labels -- one labelled tick, one unlabelled tick,
# alternating (denser ruler without a cluttered axis), whole-number percents
# ("1%" not "1.0%") -- reused by every FILL_SCALE_* below. Labelled ticks use
# a DIFFERENT, wider break set than the contour lines' own brks_a/brks_b/...
# (computed just below, per panel, as brks_legend_a/brks_legend_b/... right
# alongside each z_lim_*): the legend's bar spans the FULL symmetric
# c(-z_lim, z_lim) range, while brks_a etc. are deliberately restricted to
# the actual (often asymmetric) observed z range so every contour break has
# real data to trace a line through. Reusing the narrower contour breaks for
# the legend too (tried first) left labels stopping short of one end of the
# bar; reusing the wider legend breaks for the contour lines instead made
# most lines silently vanish, since most of that wider range falls outside
# what the range-clipped grid can draw. Keeping the two separate is required,
# not just tidier.
CONTOUR_PCT_LABEL <- function(x) {
  v <- round(x * 100, 1)
  ifelse(v == round(v), paste0(round(v), "%"), paste0(format(v, nsmall = 1), "%"))
}
# General version of the same one-labelled/one-unlabelled tick rule, reused
# below for the Y axes of panels a/b/c/d (`label_fun` swapped in per panel --
# CONTOUR_PCT_LABEL for the legend, a units-conversion or scales::label_number
# for an axis).
axis_ticks <- function(major, label_fun) {
  minor <- (major[-1] + head(major, -1)) / 2
  brks <- sort(c(major, minor))
  list(breaks = brks, labels = ifelse(brks %in% major, label_fun(brks), ""))
}
legend_ticks <- function(major) axis_ticks(major, CONTOUR_PCT_LABEL)

# ticks.length is a length-2 unit (right side, left side) for a vertical bar
# -- negative = tick protrudes outside the bar, 0 = no tick drawn. c(-0.12, 0)
# therefore draws ticks ONLY on the outside right, none crossing the bar or
# poking out the left (GuideColourbar$build_ticks() draws each side as its
# own grob from this pair, confirmed empirically -- there's no simpler public
# arg for one-sided ticks).
CONTOUR_FILL_GUIDE <- guide_colourbar(
  barheight = unit(6.5, "cm"),
  barwidth = unit(0.3, "cm"),
  title.position = "right",
  title.theme = element_text(angle = -90, hjust = 0.5),
  frame.colour = "black",
  frame.linewidth = 0.3,
  ticks.colour = "black",
  ticks.linewidth = 0.4,
  theme = theme(legend.ticks.length = unit(c(-0.12, 0), "cm"))
)

# Common fill scale (v2 option) --------------------------------------------
# Set TRUE to use ONE shared diverging colour scale across every contour
# panel below -- fixed -6%/+6% range, blue (decline) - white (0%) - red
# (growth), the same blue/red Metal ores' own scale (panel c) already used --
# instead of each panel's own material-coloured scale sized to its own
# observed range. 4 evenly-spaced colour stops at -6/-3/0/3/6% mean colour
# changes fastest between -3%/+3% (white <-> the full saturated blue/red) and
# flattens out toward +-6% (that same saturated colour <-> a darker shade of
# it), so panels whose data stays inside +-3% still show strong contrast
# while the scale still accommodates outliers out to +-6%.
USE_COMMON_FILL_SCALE <- TRUE
COMMON_FILL_RANGE <- 0.06 # +-6%, same CAGR-fraction units as every z_var
COMMON_FILL_BLUE_DARK <- "#08306B" # darker shade of BLUE_SPECTRAL, for -6%
COMMON_FILL_RED_DARK <- "#67000D" # darker shade of Metal ores' red, for +6%
brks_legend_common <- scales::extended_breaks(n = 5)(c(-COMMON_FILL_RANGE, COMMON_FILL_RANGE))
if (!0 %in% brks_legend_common) {
  brks_legend_common <- sort(c(brks_legend_common, 0))
}
lt_common <- legend_ticks(brks_legend_common)
FILL_SCALE_COMMON <- scale_fill_gradientn(
  colours = c(COMMON_FILL_BLUE_DARK, BLUE_SPECTRAL, "white", unname(PALETTE_MATERIAL_GROUPS["Metal ores"]), COMMON_FILL_RED_DARK),
  limits = c(-COMMON_FILL_RANGE, COMMON_FILL_RANGE),
  oob = scales::squish, # clamp values beyond +-6% to the darkest blue/red instead of NA/grey
  breaks = lt_common$breaks,
  labels = lt_common$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)

z_lim_a <- max(abs(range(grid_a_df$z_var)))
brks_legend_a <- scales::extended_breaks(n = 5)(c(-z_lim_a, z_lim_a))
if (!0 %in% brks_legend_a) {
  brks_legend_a <- sort(c(brks_legend_a, 0))
}
lt_a <- legend_ticks(brks_legend_a)
FILL_SCALE_A <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIAL_GROUPS["Biomass"]),
  midpoint = 0,
  limits = c(-z_lim_a, z_lim_a),
  breaks = lt_a$breaks,
  labels = lt_a$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_b <- max(abs(range(grid_b_df$z_var)))
brks_legend_b <- scales::extended_breaks(n = 5)(c(-z_lim_b, z_lim_b))
if (!0 %in% brks_legend_b) {
  brks_legend_b <- sort(c(brks_legend_b, 0))
}
lt_b <- legend_ticks(brks_legend_b)
FILL_SCALE_B <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIAL_GROUPS["Fossil fuels"]),
  midpoint = 0,
  limits = c(-z_lim_b, z_lim_b),
  breaks = lt_b$breaks,
  labels = lt_b$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_c <- max(abs(range(grid_c_df$z_var)))
brks_legend_c <- scales::extended_breaks(n = 5)(c(-z_lim_c, z_lim_c))
if (!0 %in% brks_legend_c) {
  brks_legend_c <- sort(c(brks_legend_c, 0))
}
lt_c <- legend_ticks(brks_legend_c)
FILL_SCALE_C <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]),
  midpoint = 0,
  limits = c(-z_lim_c, z_lim_c),
  breaks = lt_c$breaks,
  labels = lt_c$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_d <- max(abs(range(grid_d_df$z_var)))
brks_legend_d <- scales::extended_breaks(n = 5)(c(-z_lim_d, z_lim_d))
if (!0 %in% brks_legend_d) {
  brks_legend_d <- sort(c(brks_legend_d, 0))
}
lt_d <- legend_ticks(brks_legend_d)
FILL_SCALE_D <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = NONMET_FILL_HIGH,
  midpoint = 0,
  limits = c(-z_lim_d, z_lim_d),
  breaks = lt_d$breaks,
  labels = lt_d$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)

# Panel e/f's 4 split sub-panels -- high colour is each one's OWN category
# (Ferrous/Non-ferrous ores from PALETTE_MATERIALS; Buildings/Civil
# infrastructure from PALETTE_ENDUSE, the same two palettes Figure 1 uses)
# rather than the pooled Metal ores / Non-metallic minerals group colour.
z_lim_e_fe <- max(abs(range(grid_e_fe_df$z_var)))
brks_legend_e_fe <- scales::extended_breaks(n = 5)(c(-z_lim_e_fe, z_lim_e_fe))
if (!0 %in% brks_legend_e_fe) {
  brks_legend_e_fe <- sort(c(brks_legend_e_fe, 0))
}
lt_e_fe <- legend_ticks(brks_legend_e_fe)
FILL_SCALE_E_FE <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIALS["Ferrous ores"]),
  midpoint = 0,
  limits = c(-z_lim_e_fe, z_lim_e_fe),
  breaks = lt_e_fe$breaks,
  labels = lt_e_fe$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_e_nonfe <- max(abs(range(grid_e_nonfe_df$z_var)))
brks_legend_e_nonfe <- scales::extended_breaks(n = 5)(c(-z_lim_e_nonfe, z_lim_e_nonfe))
if (!0 %in% brks_legend_e_nonfe) {
  brks_legend_e_nonfe <- sort(c(brks_legend_e_nonfe, 0))
}
lt_e_nonfe <- legend_ticks(brks_legend_e_nonfe)
FILL_SCALE_E_NONFE <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIALS["Non-ferrous ores"]),
  midpoint = 0,
  limits = c(-z_lim_e_nonfe, z_lim_e_nonfe),
  breaks = lt_e_nonfe$breaks,
  labels = lt_e_nonfe$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_f_bldg <- max(abs(range(grid_f_bldg_df$z_var)))
brks_legend_f_bldg <- scales::extended_breaks(n = 5)(c(-z_lim_f_bldg, z_lim_f_bldg))
if (!0 %in% brks_legend_f_bldg) {
  brks_legend_f_bldg <- sort(c(brks_legend_f_bldg, 0))
}
lt_f_bldg <- legend_ticks(brks_legend_f_bldg)
FILL_SCALE_F_BLDG <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = BUILDINGS_FILL_HIGH,
  midpoint = 0,
  limits = c(-z_lim_f_bldg, z_lim_f_bldg),
  breaks = lt_f_bldg$breaks,
  labels = lt_f_bldg$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_f_civil <- max(abs(range(grid_f_civil_df$z_var)))
brks_legend_f_civil <- scales::extended_breaks(n = 5)(c(-z_lim_f_civil, z_lim_f_civil))
if (!0 %in% brks_legend_f_civil) {
  brks_legend_f_civil <- sort(c(brks_legend_f_civil, 0))
}
lt_f_civil <- legend_ticks(brks_legend_f_civil)
FILL_SCALE_F_CIVIL <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_ENDUSE["Civil infrastructure"]),
  midpoint = 0,
  limits = c(-z_lim_f_civil, z_lim_f_civil),
  breaks = lt_f_civil$breaks,
  labels = lt_f_civil$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)

if (USE_COMMON_FILL_SCALE) {
  FILL_SCALE_A <- FILL_SCALE_COMMON
  FILL_SCALE_B <- FILL_SCALE_COMMON
  FILL_SCALE_C <- FILL_SCALE_COMMON
  FILL_SCALE_D <- FILL_SCALE_COMMON
  FILL_SCALE_E_FE <- FILL_SCALE_COMMON
  FILL_SCALE_E_NONFE <- FILL_SCALE_COMMON
  FILL_SCALE_F_BLDG <- FILL_SCALE_COMMON
  FILL_SCALE_F_CIVIL <- FILL_SCALE_COMMON
}

# Region-point layers (panels a-d only; NULL when SHOW_REGION_POINTS is
# FALSE, in which case `+ NULL` on a ggplot object is a harmless no-op).

region_layer_a <- if (SHOW_REGION_POINTS) {
  geom_point(
    data = region_points_a,
    aes(x = x_var, y = y_var, fill = I(region_colour)), shape = 21, size = 3.5, stroke = 0.8, colour = "black",
    inherit.aes = FALSE
  )
} else {
  NULL
}
region_layer_b <- if (SHOW_REGION_POINTS) {
  geom_point(
    data = region_points_b,
    aes(x = x_var, y = y_var, fill = I(region_colour)), shape = 21, size = 3.5, stroke = 0.8, colour = "black",
    inherit.aes = FALSE
  )
} else {
  NULL
}
region_layer_c <- if (SHOW_REGION_POINTS) {
  geom_point(
    data = region_points_c,
    aes(x = x_var, y = y_var, fill = I(region_colour)), shape = 21, size = 3.5, stroke = 0.8, colour = "black",
    inherit.aes = FALSE
  )
} else {
  NULL
}
region_label_layer_c <- if (SHOW_REGION_POINTS) {
  ggrepel::geom_text_repel(
    data = region_points_c,
    aes(x = x_var, y = y_var, label = label_text, colour = I(region_colour)),
    fontface = "bold",
    size = 2,
    segment.size = 0.3,
    seed = 42,
    show.legend = FALSE,
    inherit.aes = FALSE
  )
} else {
  NULL
}
region_layer_d <- if (SHOW_REGION_POINTS) {
  geom_point(
    data = region_points_d,
    aes(x = x_var, y = y_var, fill = I(region_colour)), shape = 21, size = 3.5, stroke = 0.8, colour = "black",
    inherit.aes = FALSE
  )
} else {
  NULL
}

# Panel-title text colour -- each panel's own material/end-use colour from
# the project palette (PALETTE_MATERIAL_GROUPS/PALETTE_MATERIALS/PALETTE_ENDUSE,
# the same palettes Figure 1 uses), independent of that panel's own fill-scale
# high colour -- which substitutes a different warm hue for Non-metallic
# minerals/Buildings (NONMET_FILL_HIGH/BUILDINGS_FILL_HIGH above) -- so the
# title always matches the material's real colour elsewhere in the project.
TITLE_COL_BIOMASS <- unname(PALETTE_MATERIAL_GROUPS["Biomass"])
TITLE_COL_FOSSIL <- unname(PALETTE_MATERIAL_GROUPS["Fossil fuels"])
TITLE_COL_METAL <- unname(PALETTE_MATERIAL_GROUPS["Metal ores"])
TITLE_COL_NONMET <- unname(PALETTE_MATERIAL_GROUPS["Non-metallic minerals"])
TITLE_COL_FE <- unname(PALETTE_MATERIALS["Ferrous ores"])
TITLE_COL_NONFE <- unname(PALETTE_MATERIALS["Non-ferrous ores"])
TITLE_COL_BUILDINGS <- unname(PALETTE_ENDUSE["Buildings"])
TITLE_COL_CIVIL <- unname(PALETTE_ENDUSE["Civil infrastructure"])

# Plot ------------------------------------------------------------------
# Panel a's Y axis: displayed in tons/person (data stays kg/person; only the
# label text converts) at whole numbers, same one-labelled/one-unlabelled
# tick rule as the legend -- major breaks picked directly in tons space
# (nicer round numbers there) then scaled back to kg for the actual break
# positions.
yt_a <- axis_ticks(scales::extended_breaks(n = 5)(y_disp_rng_a / 1000) * 1000, function(x) {
  scales::number(x / 1000, accuracy = 0.1) # 1 decimal (e.g. 3.5)
})

p_a2 <- ggplot(panel_a_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_a_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_a_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_a,
    straight = TRUE,
    colour = "black",
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  region_layer_a +
  geom_point(
    data = dplyr::filter(star_df, panel == "a"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "a"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  geom_text(
    data = dplyr::filter(star_df, panel == "a"),
    aes(x = x_var, y = y_var, label = "Historical\n(2019-2024)"),
    colour = "black", fontface = "bold", size = pb_annot_size("largeFont", pt = 11), vjust = -0.6, hjust = 0.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "segment",
    x = x_disp_rng_a[1] + 0.15 * diff(x_disp_rng_a),
    xend = x_disp_rng_a[1] + 0.15 * diff(x_disp_rng_a),
    y = 0,
    yend = y_disp_rng_a[1] * 0.35,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    linewidth = 0.4,
    colour = "black"
  ) +
  annotate(
    "text",
    x = x_disp_rng_a[1] + 0.15 * diff(x_disp_rng_a),
    y = y_disp_rng_a[1] * 0.35,
    label = "Absolute decoupling",
    vjust = 1.2,
    hjust = 0.5,
    fontface = "bold",
    size = pb_annot_size("largeFont", pt = 7),
    colour = "black"
  ) +
  FILL_SCALE_A +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = yt_a$breaks, labels = yt_a$labels) +
  labs(tag = "a", title = "Biomass", x = X_LAB_CONTOUR, y = "Biomass consumption per capita\n(tons/person)") +
  coord_cartesian(xlim = x_disp_rng_a, ylim = y_disp_rng_a, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_BIOMASS)
  )

# Panel b's Y axis: whole-number MJ per $ (no decimals), same tick rule.
yt_b <- axis_ticks(scales::extended_breaks(n = 5)(y_disp_rng_b), function(x) scales::number(x, accuracy = 1)) # 0 decimals

p_b2 <- ggplot(panel_b_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_b_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_b_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH)),
      # Manual label-position override (per user request) -- the +2.0% line's
      # label moves off-centre to 0.7; every other line stays at the default
      # 0.5 (middle).
      hjust = after_stat(ifelse(signif(level, 8) == signif(0.02, 8), 0.7, 0.5))
    ),
    breaks = brks_b,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  region_layer_b +
  geom_point(
    data = dplyr::filter(star_df, panel == "b"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "b"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_B +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = yt_b$breaks, labels = yt_b$labels) +
  labs(tag = "b", title = "Fossil fuels", x = X_LAB_CONTOUR, y = "Primary fossil fuel energy intensity (MJ per $)") +
  coord_cartesian(xlim = x_disp_rng_b, ylim = y_disp_rng_b, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_FOSSIL)
  )

p_c2 <- ggplot(panel_c_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_c_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_c_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH)),
      # Manual label-position override (per user request) -- the +2.0% line's
      # label moves off-centre to 0.3; every other line stays at the default
      # 0.5 (middle).
      hjust = after_stat(ifelse(signif(level, 8) == signif(0.02, 8), 0.3, 0.5))
    ),
    breaks = brks_c,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  region_layer_c +
  region_label_layer_c +
  geom_point(
    data = dplyr::filter(star_df, panel == "c"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "c"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_C +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(tag = "e", title = "Metal ores", x = X_LAB_CONTOUR, y = RECYC_LAB) +
  coord_cartesian(xlim = x_disp_rng_c, ylim = y_disp_rng_c, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.tag.position = "topright",
    plot.title = element_text(colour = TITLE_COL_METAL)
  )

p_d2 <- ggplot(panel_d_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_d_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_d_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_d,
    straight = TRUE,
    colour = "black",
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  region_layer_d +
  geom_point(
    data = dplyr::filter(star_df, panel == "d"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "d"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_D +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(tag = "g", title = "Non-metallic minerals", x = X_LAB_CONTOUR, y = DOWNCYC_LAB) +
  coord_cartesian(xlim = x_disp_rng_d, ylim = y_disp_rng_d, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_NONMET)
  )

# Panel e split into Ferrous / Non-ferrous ore grade contours (replacing the
# pooled mass-weighted version + its stacked-area companion) ----------------

p_e_fe <- ggplot(panel_e_fe_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_e_fe_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_e_fe_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH)),
      # Manual label-position override (per user request) -- the 0.0% line's
      # label moves off-centre to 0.3; every other line stays at the default
      # 0.5 (middle).
      hjust = after_stat(ifelse(signif(level, 8) == signif(0, 8), 0.3, 0.5))
    ),
    breaks = brks_e_fe,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "e_fe"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "e_fe"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_E_FE +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(tag = "i", title = "Ferrous ores", x = X_LAB_CONTOUR, y = "Ferrous ore grade (%)") +
  coord_cartesian(xlim = x_disp_rng_e_fe, ylim = y_disp_rng_e_fe, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_FE)
  )

p_e_nonfe <- ggplot(panel_e_nonfe_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_e_nonfe_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_e_nonfe_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH)),
      # Manual label-position override (per user request) -- the +2.0% line's
      # label moves off-centre to 0.3; every other line stays at the default
      # 0.5 (middle).
      hjust = after_stat(ifelse(signif(level, 8) == signif(0.02, 8), 0.3, 0.5))
    ),
    breaks = brks_e_nonfe,
    straight = TRUE,
    colour = "black",
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "e_nonfe"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "e_nonfe"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_E_NONFE +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(tag = "j", title = "Non-ferrous ores", x = X_LAB_CONTOUR, y = "Non-ferrous ore grade (%)") +
  coord_cartesian(xlim = x_disp_rng_e_nonfe, ylim = y_disp_rng_e_nonfe, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.tag.position = "topright",
    plot.title = element_text(colour = TITLE_COL_NONFE)
  )

# Panel f split into Buildings / Civil infrastructure lifetime contours
# (replacing the pooled average version + its stacked-area companion) -------

p_f_bldg <- ggplot(panel_f_bldg_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_f_bldg_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_f_bldg_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH)),
      # Manual label-position override (per user request) -- the +2.0% line's
      # label moves off-centre to 0.3; every other line stays at the default
      # 0.5 (middle).
      hjust = after_stat(ifelse(signif(level, 8) == signif(0.02, 8), 0.3, 0.5))
    ),
    breaks = brks_f_bldg,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "f_bldg"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "f_bldg"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_F_BLDG +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(tag = "k", title = "Buildings", x = X_LAB_CONTOUR, y = "Lifetime (years)") +
  coord_cartesian(xlim = x_disp_rng_f_bldg, ylim = y_disp_rng_f_bldg, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.tag.position = "topright",
    plot.title = element_text(colour = TITLE_COL_BUILDINGS)
  )

p_f_civil <- ggplot(panel_f_civil_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_f_civil_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_f_civil_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_f_civil,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "f_civil"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "f_civil"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_F_CIVIL +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(tag = "l", title = "Civil infrastructure", x = X_LAB_CONTOUR, y = "Lifetime (years)") +
  coord_cartesian(xlim = x_disp_rng_f_civil, ylim = y_disp_rng_f_civil, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.tag.position = "topright",
    plot.title = element_text(colour = TITLE_COL_CIVIL)
  )

# Row 1's 2 new panels: Metal ores / Non-metallic minerals STOCK intensity --
## In-use stock mass per $ GDP (kg/$) at FIG_YEAR (2060) -- accumulated
## material still in use, not an annual flow -- via world_gdp_percap_target
## (already reconstructed above for x_biomass's own per-capita conversion).
## Filtered from `results` directly (not results_target) -- results_target
## was already narrowed to the 6 enduse-mappable sub-use labels (dropping
## Durables/Packaging), which would silently undercount a "total" stock here.

x_metals_intensity <- results |>
  dplyr::filter(year == FIG_YEAR, material_group %in% c("metal_fe", "metal_nonfe")) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(world_gdp_percap_target, by = "run_id") |>
  dplyr::transmute(run_id, x = stock_Mt * 1e9 / (gdp_percap_usd * world_pop), material = "Metal ores")

x_minerals_intensity <- results |>
  dplyr::filter(year == FIG_YEAR, material_group == "nonmetallic_minerals") |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(world_gdp_percap_target, by = "run_id") |>
  dplyr::transmute(run_id, x = stock_Mt * 1e9 / (gdp_percap_usd * world_pop), material = "Non-metallic minerals")

# Contour data for the row-1 stock-intensity panels (c/d) -- fill/contour
# stays each material's own total primary-consumption CAGR, same quantity
# every other panel uses ------------------------------------------------

panel_metals_int_contour_df <- x_metals_intensity |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Metal ores"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_nonmet_int_contour_df <- x_minerals_intensity |>
  dplyr::select(run_id, y_var = x) |>
  dplyr::inner_join(growth_decoupling_total |> dplyr::filter(material == "Non-metallic minerals"), by = "run_id") |>
  dplyr::rename(x_var = gdp_cagr, z_var = y) |>
  tidyr::drop_na(x_var, y_var, z_var)

# GAM fit + grid, same recipe as every other panel (no star to extend the
# display range for on these 4, so coord_cartesian just uses the raw MC
# range directly) -------------------------------------------------------

gam_metals_int <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_metals_int_contour_df)
x_rng_metals_int <- range(panel_metals_int_contour_df$x_var)
y_rng_metals_int <- range(panel_metals_int_contour_df$y_var)
grid_metals_int_df <- expand.grid(
  x_var = seq(
    x_rng_metals_int[1] - 0.05 * diff(x_rng_metals_int),
    x_rng_metals_int[2] + 0.05 * diff(x_rng_metals_int),
    length.out = 100
  ),
  y_var = seq(
    y_rng_metals_int[1] - 0.05 * diff(y_rng_metals_int),
    y_rng_metals_int[2] + 0.05 * diff(y_rng_metals_int),
    length.out = 100
  )
)
grid_metals_int_df$z_var <- predict(gam_metals_int, newdata = grid_metals_int_df)
z_obs_rng_metals_int <- range(panel_metals_int_contour_df$z_var)
grid_metals_int_df$z_var <- pmin(pmax(grid_metals_int_df$z_var, z_obs_rng_metals_int[1]), z_obs_rng_metals_int[2])

gam_nonmet_int <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_nonmet_int_contour_df)
x_rng_nonmet_int <- range(panel_nonmet_int_contour_df$x_var)
y_rng_nonmet_int <- range(panel_nonmet_int_contour_df$y_var)
grid_nonmet_int_df <- expand.grid(
  x_var = seq(
    x_rng_nonmet_int[1] - 0.05 * diff(x_rng_nonmet_int),
    x_rng_nonmet_int[2] + 0.05 * diff(x_rng_nonmet_int),
    length.out = 100
  ),
  y_var = seq(
    y_rng_nonmet_int[1] - 0.05 * diff(y_rng_nonmet_int),
    y_rng_nonmet_int[2] + 0.05 * diff(y_rng_nonmet_int),
    length.out = 100
  )
)
grid_nonmet_int_df$z_var <- predict(gam_nonmet_int, newdata = grid_nonmet_int_df)
z_obs_rng_nonmet_int <- range(panel_nonmet_int_contour_df$z_var)
grid_nonmet_int_df$z_var <- pmin(pmax(grid_nonmet_int_df$z_var, z_obs_rng_nonmet_int[1]), z_obs_rng_nonmet_int[2])

brks_metals_int <- scales::extended_breaks(n = 6)(range(grid_metals_int_df$z_var))
if (!0 %in% brks_metals_int) {
  brks_metals_int <- sort(c(brks_metals_int, 0))
}
brks_nonmet_int <- scales::extended_breaks(n = 6)(range(grid_nonmet_int_df$z_var))
if (!0 %in% brks_nonmet_int) {
  brks_nonmet_int <- sort(c(brks_nonmet_int, 0))
}

z_lim_metals_int <- max(abs(range(grid_metals_int_df$z_var)))
brks_legend_metals_int <- scales::extended_breaks(n = 5)(c(-z_lim_metals_int, z_lim_metals_int))
if (!0 %in% brks_legend_metals_int) {
  brks_legend_metals_int <- sort(c(brks_legend_metals_int, 0))
}
lt_metals_int <- legend_ticks(brks_legend_metals_int)
FILL_SCALE_METALS_INT <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]),
  midpoint = 0,
  limits = c(-z_lim_metals_int, z_lim_metals_int),
  breaks = lt_metals_int$breaks,
  labels = lt_metals_int$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_nonmet_int <- max(abs(range(grid_nonmet_int_df$z_var)))
brks_legend_nonmet_int <- scales::extended_breaks(n = 5)(c(-z_lim_nonmet_int, z_lim_nonmet_int))
if (!0 %in% brks_legend_nonmet_int) {
  brks_legend_nonmet_int <- sort(c(brks_legend_nonmet_int, 0))
}
lt_nonmet_int <- legend_ticks(brks_legend_nonmet_int)
FILL_SCALE_NONMET_INT <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = NONMET_FILL_HIGH,
  midpoint = 0,
  limits = c(-z_lim_nonmet_int, z_lim_nonmet_int),
  breaks = lt_nonmet_int$breaks,
  labels = lt_nonmet_int$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)

if (USE_COMMON_FILL_SCALE) {
  FILL_SCALE_METALS_INT <- FILL_SCALE_COMMON
  FILL_SCALE_NONMET_INT <- FILL_SCALE_COMMON
}

# Display ranges extended to always include the real-2024 star (metal_int_2024/
# nonmet_int_2024, computed above alongside recycling_world_2024) -- same
# convention as every other starred panel in this figure.
x_disp_rng_metals_int <- range(c(x_rng_metals_int, gdp_cagr_hist))
y_disp_rng_metals_int <- range(c(y_rng_metals_int, metal_int_2024))
x_disp_rng_nonmet_int <- range(c(x_rng_nonmet_int, gdp_cagr_hist))
y_disp_rng_nonmet_int <- range(c(y_rng_nonmet_int, nonmet_int_2024))

# Panels c/d's Y axis: scales::label_number(accuracy = NULL) picks decimal
# precision from the actual major breaks each time it's called, instead of a
# fixed signif(x, 1) that collapsed distinct nearby breaks onto the same
# rounded label (e.g. 0.35/0.40 both -> "0.4", or 15/18/22 all -> "20").
yt_metals_int <- axis_ticks(
  scales::extended_breaks(n = 5)(y_disp_rng_metals_int),
  scales::label_number(accuracy = 0.01) # 2 decimals (e.g. 0.35)
)
yt_nonmet_int <- axis_ticks(
  scales::extended_breaks(n = 5)(y_disp_rng_nonmet_int),
  scales::label_number(accuracy = 0.1) # 1 decimal (e.g. 12.5)
)

p_metals_int <- ggplot(panel_metals_int_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_metals_int_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_metals_int_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_metals_int,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "metals_int"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "metals_int"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_METALS_INT +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = yt_metals_int$breaks, labels = yt_metals_int$labels) +
  labs(tag = "c", title = "Metal ores", x = X_LAB_CONTOUR, y = "Metal stock intensity (kg metal per $ GDP)") + # stock is metal mass, not ore
  coord_cartesian(xlim = x_disp_rng_metals_int, ylim = y_disp_rng_metals_int, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_METAL)
  )

p_nonmet_int <- ggplot(panel_nonmet_int_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_nonmet_int_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_nonmet_int_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_nonmet_int,
    straight = TRUE,
    colour = "black",
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "nonmet_int"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "nonmet_int"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_NONMET_INT +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = yt_nonmet_int$breaks, labels = yt_nonmet_int$labels) +
  labs(
    tag = "d",
    title = "Non-metallic minerals",
    x = X_LAB_CONTOUR,
    y = "Non-metallic minerals stock\nintensity (kg per $ GDP)"
  ) +
  coord_cartesian(xlim = x_disp_rng_nonmet_int, ylim = y_disp_rng_nonmet_int, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_NONMET)
  )

# Metal ores / Non-metallic minerals recycling rate vs STOCK-WEIGHTED average
# lifetime -- these two replace the old (now removed) f/h panels above,
# which paired recycling/downcycling rate with in-use stock per $ GDP; that
# X axis duplicated too much of what panels c/d already show, so f/h now use
# a genuinely new X instead --------------------------------------------
## X = average lifetime across the material's own end-use categories,
## weighted by each end-use's in-use stock mass (not a plain mean across
## sub_use like k/l above) -- buildings + civil_infrastructure + machinery
## for Metal ores (its full ENDUSE_CATEGORY scope), buildings +
## civil_infrastructure for Non-metallic minerals (matches x_minerals'
## own scope elsewhere in this figure). Y = the same recycling/downcycling
## rate already used by panels e/g (x_metals_recyc/x_minerals_downcyc).

ENDUSE_TO_SUPERCAT <- c(buildings = "buildings", civil = "civil_infrastructure", machinery = "machinery")

lifetime_supercat_df <- lifetime_dr |>
  dplyr::group_by(run_id, super_cat) |>
  dplyr::summarise(mean_life = mean(mean_life), .groups = "drop")

metal_lifetime_weighted <- results |>
  dplyr::filter(year == FIG_YEAR, material_group %in% c("metal_fe", "metal_nonfe")) |>
  dplyr::mutate(enduse_category = ENDUSE_CATEGORY[material_key]) |>
  dplyr::filter(!is.na(enduse_category)) |>
  dplyr::group_by(run_id, enduse_category) |>
  dplyr::summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(super_cat = ENDUSE_TO_SUPERCAT[enduse_category]) |>
  dplyr::inner_join(lifetime_supercat_df, by = c("run_id", "super_cat")) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(x = sum(mean_life * stock_Mt) / sum(stock_Mt), .groups = "drop") |>
  dplyr::mutate(material = "Metal ores")

nonmet_lifetime_weighted <- results |>
  dplyr::filter(year == FIG_YEAR, material_group == "nonmetallic_minerals") |>
  dplyr::mutate(enduse_category = ENDUSE_CATEGORY[material_key]) |>
  dplyr::filter(enduse_category %in% c("buildings", "civil")) |>
  dplyr::group_by(run_id, enduse_category) |>
  dplyr::summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(super_cat = ENDUSE_TO_SUPERCAT[enduse_category]) |>
  dplyr::inner_join(lifetime_supercat_df, by = c("run_id", "super_cat")) |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(x = sum(mean_life * stock_Mt) / sum(stock_Mt), .groups = "drop") |>
  dplyr::mutate(material = "Non-metallic minerals")

panel_metals_lt_contour_df <- metal_lifetime_weighted |>
  dplyr::select(run_id, x_var = x) |>
  dplyr::inner_join(x_metals_recyc |> dplyr::select(run_id, y_var = x), by = "run_id") |>
  dplyr::inner_join(
    growth_decoupling_total |> dplyr::filter(material == "Metal ores") |> dplyr::select(run_id, z_var = y),
    by = "run_id"
  ) |>
  tidyr::drop_na(x_var, y_var, z_var)

panel_nonmet_lt_contour_df <- nonmet_lifetime_weighted |>
  dplyr::select(run_id, x_var = x) |>
  dplyr::inner_join(x_minerals_downcyc |> dplyr::select(run_id, y_var = x), by = "run_id") |>
  dplyr::inner_join(
    growth_decoupling_total |> dplyr::filter(material == "Non-metallic minerals") |> dplyr::select(run_id, z_var = y),
    by = "run_id"
  ) |>
  tidyr::drop_na(x_var, y_var, z_var)

# GAM fit + grid, same recipe as every other panel ---------------------------

gam_metals_lt <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_metals_lt_contour_df)
x_rng_metals_lt <- range(panel_metals_lt_contour_df$x_var)
y_rng_metals_lt <- range(panel_metals_lt_contour_df$y_var)
grid_metals_lt_df <- expand.grid(
  x_var = seq(
    x_rng_metals_lt[1] - 0.05 * diff(x_rng_metals_lt),
    x_rng_metals_lt[2] + 0.05 * diff(x_rng_metals_lt),
    length.out = 100
  ),
  y_var = seq(
    y_rng_metals_lt[1] - 0.05 * diff(y_rng_metals_lt),
    y_rng_metals_lt[2] + 0.05 * diff(y_rng_metals_lt),
    length.out = 100
  )
)
grid_metals_lt_df$z_var <- predict(gam_metals_lt, newdata = grid_metals_lt_df)
z_obs_rng_metals_lt <- range(panel_metals_lt_contour_df$z_var)
grid_metals_lt_df$z_var <- pmin(pmax(grid_metals_lt_df$z_var, z_obs_rng_metals_lt[1]), z_obs_rng_metals_lt[2])

gam_nonmet_lt <- mgcv::gam(z_var ~ te(x_var, y_var, k = GAM_SMOOTH_K), data = panel_nonmet_lt_contour_df)
x_rng_nonmet_lt <- range(panel_nonmet_lt_contour_df$x_var)
y_rng_nonmet_lt <- range(panel_nonmet_lt_contour_df$y_var)
grid_nonmet_lt_df <- expand.grid(
  x_var = seq(
    x_rng_nonmet_lt[1] - 0.05 * diff(x_rng_nonmet_lt),
    x_rng_nonmet_lt[2] + 0.05 * diff(x_rng_nonmet_lt),
    length.out = 100
  ),
  y_var = seq(
    y_rng_nonmet_lt[1] - 0.05 * diff(y_rng_nonmet_lt),
    y_rng_nonmet_lt[2] + 0.05 * diff(y_rng_nonmet_lt),
    length.out = 100
  )
)
grid_nonmet_lt_df$z_var <- predict(gam_nonmet_lt, newdata = grid_nonmet_lt_df)
z_obs_rng_nonmet_lt <- range(panel_nonmet_lt_contour_df$z_var)
grid_nonmet_lt_df$z_var <- pmin(pmax(grid_nonmet_lt_df$z_var, z_obs_rng_nonmet_lt[1]), z_obs_rng_nonmet_lt[2])

brks_metals_lt <- scales::extended_breaks(n = 6)(range(grid_metals_lt_df$z_var))
if (!0 %in% brks_metals_lt) {
  brks_metals_lt <- sort(c(brks_metals_lt, 0))
}
brks_nonmet_lt <- scales::extended_breaks(n = 6)(range(grid_nonmet_lt_df$z_var))
if (!0 %in% brks_nonmet_lt) {
  brks_nonmet_lt <- sort(c(brks_nonmet_lt, 0))
}

z_lim_metals_lt <- max(abs(range(grid_metals_lt_df$z_var)))
brks_legend_metals_lt <- scales::extended_breaks(n = 5)(c(-z_lim_metals_lt, z_lim_metals_lt))
if (!0 %in% brks_legend_metals_lt) {
  brks_legend_metals_lt <- sort(c(brks_legend_metals_lt, 0))
}
lt_metals_lt <- legend_ticks(brks_legend_metals_lt)
FILL_SCALE_METALS_LT <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]),
  midpoint = 0,
  limits = c(-z_lim_metals_lt, z_lim_metals_lt),
  breaks = lt_metals_lt$breaks,
  labels = lt_metals_lt$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)
z_lim_nonmet_lt <- max(abs(range(grid_nonmet_lt_df$z_var)))
brks_legend_nonmet_lt <- scales::extended_breaks(n = 5)(c(-z_lim_nonmet_lt, z_lim_nonmet_lt))
if (!0 %in% brks_legend_nonmet_lt) {
  brks_legend_nonmet_lt <- sort(c(brks_legend_nonmet_lt, 0))
}
lt_nonmet_lt <- legend_ticks(brks_legend_nonmet_lt)
FILL_SCALE_NONMET_LT <- scale_fill_gradient2(
  low = BLUE_SPECTRAL,
  mid = "white",
  high = NONMET_FILL_HIGH,
  midpoint = 0,
  limits = c(-z_lim_nonmet_lt, z_lim_nonmet_lt),
  breaks = lt_nonmet_lt$breaks,
  labels = lt_nonmet_lt$labels,
  name = CONTOUR_FILL_NAME,
  guide = CONTOUR_FILL_GUIDE
)

if (USE_COMMON_FILL_SCALE) {
  FILL_SCALE_METALS_LT <- FILL_SCALE_COMMON
  FILL_SCALE_NONMET_LT <- FILL_SCALE_COMMON
}

x_disp_rng_metals_lt <- range(c(x_rng_metals_lt, metal_lt_star))
y_disp_rng_metals_lt <- range(c(y_rng_metals_lt, recycling_world_2024))
x_disp_rng_nonmet_lt <- range(c(x_rng_nonmet_lt, nonmet_lt_star))
y_disp_rng_nonmet_lt <- range(c(y_rng_nonmet_lt, downcycling_world_2024))

p_metals_lt <- ggplot(panel_metals_lt_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_metals_lt_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_metals_lt_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      colour = after_stat(ifelse(level <= 0, "black", "white")),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_metals_lt,
    straight = TRUE,
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "metals_lt"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "metals_lt"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_METALS_LT +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(tag = "f", title = "Metal ores", x = "Lifetime (years)", y = RECYC_LAB) +
  coord_cartesian(xlim = x_disp_rng_metals_lt, ylim = y_disp_rng_metals_lt, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = TITLE_COL_METAL)
  )

p_nonmet_lt <- ggplot(panel_nonmet_lt_contour_df, aes(x = x_var, y = y_var)) +
  geom_raster(data = grid_nonmet_lt_df, aes(x = x_var, y = y_var, fill = z_var), inherit.aes = FALSE) +
  MC_POINT_LAYER +
  geomtextpath::geom_textcontour(
    data = grid_nonmet_lt_df,
    aes(
      x = x_var,
      y = y_var,
      z = z_var,
      label = after_stat(ifelse(
        level < 0,
        paste0("−", sprintf("%.1f%%", abs(level * 100))),
        paste0("+", sprintf("%.1f%%", level * 100))
      )),
      linewidth = after_stat(ifelse(level == 0, CONTOUR_LINEWIDTH_ZERO, CONTOUR_LINEWIDTH))
    ),
    breaks = brks_nonmet_lt,
    straight = TRUE,
    colour = "black",
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  geom_point(
    data = dplyr::filter(star_df, panel == "nonmet_lt"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_OUTLINE_SIZE, colour = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = dplyr::filter(star_df, panel == "nonmet_lt"),
    aes(x = x_var, y = y_var), shape = "★", size = STAR_SIZE, colour = STAR_FILL, inherit.aes = FALSE
  ) +
  FILL_SCALE_NONMET_LT +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(tag = "h", title = "Non-metallic minerals", x = "Lifetime (years)", y = DOWNCYC_LAB) +
  coord_cartesian(xlim = x_disp_rng_nonmet_lt, ylim = y_disp_rng_nonmet_lt, expand = FALSE) +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.tag.position = "topright",
    plot.title = element_text(colour = TITLE_COL_NONMET)
  )

# Assemble: 12 panels, 3 rows x 4 columns, row-major reading order a-l (see
# tags set on each labs() call above); each contour panel keeps its OWN
# Spectral-derived legend (guides are NOT collected). f/h are now
# p_metals_lt/p_nonmet_lt (stock-weighted lifetime), replacing the removed
# stock-per-GDP panels that used to sit there. Tags a-l keep their original
# topleft position except e/h/j/k/l (moved topright above, clear of
# star/legend/dense-cloud content in that corner). Tag/axis-title/legend-
# title font sizes bumped for every panel via the shared `&` theme below.
# A narrow rotated-text column labels each row (Intensity / Recycling-
# Downcycling / Stock detail); rows are built + widthed individually so that
# label column aligns across all three, then stacked with `/`.

ROW_TITLE_SIZE <- pb_annot_size("largeFont", pt = 13)
row_title_intensity <- ggplot() +
  theme_void() +
  annotate("text", x = 0, y = 0, label = "Intensity", angle = 90, fontface = "bold", size = ROW_TITLE_SIZE)
row_title_recycling <- ggplot() +
  theme_void() +
  annotate("text", x = 0, y = 0, label = "Recycling/Downcycling", angle = 90, fontface = "bold", size = ROW_TITLE_SIZE)
row_title_stock <- ggplot() +
  theme_void() +
  annotate("text", x = 0, y = 0, label = "Stock detail", angle = 90, fontface = "bold", size = ROW_TITLE_SIZE)

ROW_WIDTHS <- c(0.12, 1, 1, 1, 1)
fig_row1 <- (row_title_intensity + p_a2 + p_b2 + p_metals_int + p_nonmet_int) +
  patchwork::plot_layout(nrow = 1, widths = ROW_WIDTHS)
fig_row2 <- (row_title_recycling + p_c2 + p_metals_lt + p_d2 + p_nonmet_lt) +
  patchwork::plot_layout(nrow = 1, widths = ROW_WIDTHS)
fig_row3 <- (row_title_stock + p_e_fe + p_e_nonfe + p_f_bldg + p_f_civil) +
  patchwork::plot_layout(nrow = 1, widths = ROW_WIDTHS)

fig_contour <- (fig_row1 / fig_row2 / fig_row3) &
  theme(
    legend.position = "right",
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    plot.tag = element_text(size = 12, face = "bold", margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt"))
  )

# Main figure (6 panels, 2 cols x 3 rows, max width 18cm) -------------------
## Compact main-text version: only the 4 Intensity-vs-GDP panels (Biomass,
## Fossil fuels, Metal ores, Non-metallic minerals -- rows 1-2 of the SI figure
## above) plus Metal ores recycling / Non-metallic minerals downcycling vs.
## GDP (SI panels e/g) -- dropping the lifetime-based recycling/downcycling
## panels (SI f/h) and the whole "Stock detail" row (SI i/j/k/l). Re-tagged
## a-f in reading order: p_a2/p_b2/p_metals_int/p_nonmet_int/p_c2 keep their SI
## tags (a-e) unchanged; only p_d2's SI tag "g" becomes "f" here, since e-l
## aren't all present in this compact layout.
p_d2_main <- p_d2 + labs(tag = "f")

fig_main <- (p_a2 + p_b2 + p_metals_int + p_nonmet_int + p_c2 + p_d2_main) +
  patchwork::plot_layout(ncol = 2, byrow = TRUE) &
  theme(
    legend.position = "right",
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    plot.tag = element_text(size = 12, face = "bold", margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt"))
  )

# Save ------------------------------------------------------------------
# SI: all 12 panels (fig_contour above, unchanged) -- width accounts for the
# added row-title column (each row's relative widths sum to 4.12 instead of 4;
# scaling total width by that ratio keeps every panel at its original ~8.7cm).
# Main text: the compact 6-panel fig_main, capped at 18cm width (design
# pre-prompt max) instead of the SI's 4-column ~34cm.

if (GAM_SMOOTH_K == GAM_SMOOTH_K_DEFAULT) {
  fig3_si_png <- "Figures/Supporting-Figures/S17_GrowthDecoupling_Contours.png"
  fig3_si_svg <- "Figures/SVG/Supporting-Figures/S17_GrowthDecoupling_Contours.svg"
  ggsave(fig3_si_png, fig_contour, units = "cm", dpi = 600, width = 8.7 * 4.12, height = 8.7 * 3)
  ggsave(fig3_si_svg, fig_contour, units = "cm", width = 8.7 * 4.12, height = 8.7 * 3)
  clean_svg(fig3_si_svg)
  cat("  Saved:", fig3_si_png, ",", fig3_si_svg, "\n")
  fig3_png <- "Figures/Fig3 - Contours-GrowthRate.png"
  fig3_svg <- "Figures/SVG/Fig3 - Contours-GrowthRate.svg"
} else {
  # SI smoothness comparison: main figure only, at this K
  fig3_png <- paste0("Figures/Supporting-Figures/S24_Fig3_SmoothK", GAM_SMOOTH_K, ".png")
  fig3_svg <- paste0("Figures/SVG/Supporting-Figures/S24_Fig3_SmoothK", GAM_SMOOTH_K, ".svg")
}
ggsave(fig3_png, fig_main, units = "cm", dpi = 600, width = 18, height = 8.7 * 3)
ggsave(fig3_svg, fig_main, units = "cm", width = 18, height = 8.7 * 3)
clean_svg(fig3_svg)
cat("  Saved:", fig3_png, ",", fig3_svg, "\n")

cat("=== Figure 3 (contour version) done ===\n")

# EoF
