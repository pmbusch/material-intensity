## S19 - CumulativeExtraction.R  -> Supporting Figure S19
## Four-panel MC figure, one panel per material group, X = year (2025-2060):
##   a) Biomass:               ANNUAL primary consumption (Gt/yr, not
##                              cumulative -- flow-through material, no stock
##                              to accumulate against). Secondary axis: land
##                              equivalent (Gha/yr), converting crops and all
##                              other biomass sub-categories via their own
##                              t/ha yield assumption (PARAMS below).
##   b) Fossil fuels:          CUMULATIVE primary consumption (Gt, oil-
##                              equivalent energy basis) so it is directly
##                              comparable to a horizontal line marking global
##                              proved reserves (converted to the same Gt
##                              energy-equivalent basis). Secondary axis:
##                              share of those reserves (0% to >100%).
##   c) Metal ores:             CUMULATIVE primary consumption (Gt). Secondary
##                              axis: share of a 35-year (1990-2025) cumulative
##                              historical benchmark (world Domestic
##                              Extraction) -- matches this figure's own
##                              35-year forecast window length.
##   d) Non-metallic minerals:  same as (c).
## Every panel: MC median line + 90% CI shaded ribbon (5th-95th percentile).
## Colours: PALETTE_MATERIAL_GROUPS (Scripts/00-CommonParameters.R).

source("Scripts/00-Libraries.R", encoding = "UTF-8")

library(patchwork)

pb_set_geom_defaults("largeFont")

cat("=== Figure 5 - Cumulative Primary Extraction ===\n\n")

# Parameters ------------------------------------------------------------

FIG_START <- 2025L
FIG_END <- FORECAST_END # 2060 (Scripts/00-CommonParameters.R)
CI_LO <- 0.05 # 90% CI = 5th-95th percentile
CI_HI <- 0.95

# Biomass land-equivalency (panel a secondary axis): global-average yield
# (t dry matter / ha / yr) used to convert consumed mass into an equivalent
# harvested area -- ASSUMPTION, literature-typical FAOSTAT world averages.
# Crops get their own (higher-yield) factor; every other biomass
# sub-category (grazed biomass/fodder, wood, crop residues, other biomass)
# shares a single lower-yield factor representative of pasture/forestry.
CROP_YIELD_T_HA <- 4.0
OTHER_BIOMASS_YIELD_T_HA <- 2.0

# Fossil fuel reserves (panel b): global PROVED reserves (Energy Institute
# Statistical Review of World Energy, ~2023 data), converted to gigatonnes of
# oil equivalent (Gtoe). Update these three constants if a newer/alternate
# reserves estimate is preferred.
OIL_RESERVES_GTOE <- 244 # ~1.73 trillion barrels
GAS_RESERVES_GTOE <- 169 # ~188 trillion m3 (~0.90 toe / 1,000 m3)
COAL_RESERVES_GTOE <- 752 # ~1,074 Gt (~0.70 toe / t coal)
FOSSIL_RESERVES_GTOE <- OIL_RESERVES_GTOE + GAS_RESERVES_GTOE + COAL_RESERVES_GTOE

# Energy densities (MJ/kg) to convert fossil fuel mass to energy -- coal/oil/
# gas match Figure 3's IPCC (2006) default net calorific values; "Other
# fossil fuels" (refined products/plastics) has no NCV of its own, so it
# borrows Petroleum's -- ASSUMPTION.
FOSSIL_ENERGY_DENSITY_MJ_KG <- c(
  "Coal" = 25.8, "Natural Gas" = 48.0, "Petroleum" = 42.3, "Other fossil fuels" = 42.3
)
TOE_MJ <- 41868 # 1 tonne of oil equivalent, IEA standard definition

# Historical benchmark window (panels c/d): same length (35 yr) as this
# figure's own forecast window (FIG_START-FIG_END) -- world Domestic
# Extraction data only runs through 2024, so 2025 contributes nothing.
HIST_START <- 1990L
HIST_END <- 2025L

# Historical benchmark categories (panels c/d), world Domestic Extraction --
# same material_category grouping Figure 1 uses for material_group.
METAL_HIST_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_HIST_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant",
  "Products mainly from non-metallic minerals"
)

# Load data ---------------------------------------------------------------

mc_results <- arrow::read_parquet("Results/MC/mc_results.parquet")
de_world_hist <- readr::read_csv("Parameters/UNEP-Materials/materials_world_DE.csv", show_col_types = FALSE)

results_fig <- mc_results |> dplyr::filter(year >= FIG_START, year <= FIG_END)

# Panel a -- Biomass: annual consumption + land equivalent ------------------

biomass_annual <- results_fig |>
  dplyr::filter(material_group == "biomass") |>
  dplyr::mutate(bio_class = dplyr::if_else(material_key == "Crops", "crops", "other")) |>
  dplyr::group_by(run_id, year, bio_class) |>
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = bio_class, values_from = mass_Mt, values_fill = 0) |>
  # Land equivalent: each sub-category's own mass (Mt -> t) over its own
  # yield assumption (t/ha), summed -- hectares actually harvested that year.
  dplyr::mutate(
    total_Mt = crops + other,
    land_ha = crops * 1e6 / CROP_YIELD_T_HA + other * 1e6 / OTHER_BIOMASS_YIELD_T_HA
  )

panel_a_df <- biomass_annual |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    med_Gt = median(total_Mt) / 1e3,
    lo_Gt = quantile(total_Mt, CI_LO, names = FALSE) / 1e3,
    hi_Gt = quantile(total_Mt, CI_HI, names = FALSE) / 1e3,
    med_land_Gha = median(land_ha) / 1e9,
    .groups = "drop"
  )

# ggplot's sec_axis must be a FIXED function of the primary axis, so the two
# yield factors above are blended into one effective ha-per-Gt constant here,
# using the ensemble median trajectory's own crops/other mass split across
# the whole figure window (rather than a single hardcoded blend).
LAND_GHA_PER_GT_BIOMASS <- sum(panel_a_df$med_land_Gha) / sum(panel_a_df$med_Gt)

# Panel b -- Fossil fuels: cumulative energy vs. reserves --------------------

fossil_annual <- results_fig |>
  dplyr::filter(material_group == "fossil_fuels") |>
  dplyr::mutate(
    energy_MJ = primary_consumption_Mt * 1e9 * FOSSIL_ENERGY_DENSITY_MJ_KG[material_key]
  ) |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(energy_Gtoe = sum(energy_MJ, na.rm = TRUE) / TOE_MJ / 1e9, .groups = "drop") |>
  dplyr::arrange(run_id, year) |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(cum_Gtoe = cumsum(energy_Gtoe)) |>
  dplyr::ungroup()

panel_b_df <- fossil_annual |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    med = median(cum_Gtoe),
    lo = quantile(cum_Gtoe, CI_LO, names = FALSE),
    hi = quantile(cum_Gtoe, CI_HI, names = FALSE),
    .groups = "drop"
  )

# Panel c -- Metal ores: cumulative mass vs. historical benchmark -----------

metal_annual <- results_fig |>
  dplyr::filter(material_group %in% c("metal_fe", "metal_nonfe")) |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(run_id, year) |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(cum_Gt = cumsum(mass_Mt) / 1e3) |>
  dplyr::ungroup()

panel_c_df <- metal_annual |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    med = median(cum_Gt),
    lo = quantile(cum_Gt, CI_LO, names = FALSE),
    hi = quantile(cum_Gt, CI_HI, names = FALSE),
    .groups = "drop"
  )

metal_hist_filtered <- de_world_hist |>
  dplyr::filter(material_category %in% METAL_HIST_CATS, year >= HIST_START, year <= HIST_END)
metal_hist_benchmark_Gt <- sum(metal_hist_filtered$DE_Mt, na.rm = TRUE) / 1e3
metal_hist_label <- paste0(HIST_START, "-", max(metal_hist_filtered$year), " cumulative extraction")

# Panel d -- Non-metallic minerals: cumulative mass vs. historical benchmark -

nonmet_annual <- results_fig |>
  dplyr::filter(material_group == "nonmetallic_minerals") |>
  dplyr::group_by(run_id, year) |>
  dplyr::summarise(mass_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(run_id, year) |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(cum_Gt = cumsum(mass_Mt) / 1e3) |>
  dplyr::ungroup()

panel_d_df <- nonmet_annual |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    med = median(cum_Gt),
    lo = quantile(cum_Gt, CI_LO, names = FALSE),
    hi = quantile(cum_Gt, CI_HI, names = FALSE),
    .groups = "drop"
  )

nonmet_hist_filtered <- de_world_hist |>
  dplyr::filter(material_category %in% NONMET_HIST_CATS, year >= HIST_START, year <= HIST_END)
nonmet_hist_benchmark_Gt <- sum(nonmet_hist_filtered$DE_Mt, na.rm = TRUE) / 1e3
nonmet_hist_label <- paste0(HIST_START, "-", max(nonmet_hist_filtered$year), " cumulative extraction")

cat(
  "  Reserves -- Fossil fuels:", round(FOSSIL_RESERVES_GTOE), "Gt | Metal ores", metal_hist_label, ":",
  round(metal_hist_benchmark_Gt), "Gt | Non-metallic minerals", nonmet_hist_label, ":",
  round(nonmet_hist_benchmark_Gt), "Gt\n\n"
)

# Plot ----------------------------------------------------------------------

X_BREAKS <- c(2025, 2030, 2040, 2050, 2060)

p_a <- ggplot(panel_a_df, aes(x = year)) +
  geom_ribbon(aes(ymin = lo_Gt, ymax = hi_Gt), fill = PALETTE_MATERIAL_GROUPS["Biomass"], alpha = 0.25) +
  geom_line(aes(y = med_Gt), colour = PALETTE_MATERIAL_GROUPS["Biomass"], linewidth = 0.8) +
  scale_x_continuous(breaks = X_BREAKS) +
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(~ . * LAND_GHA_PER_GT_BIOMASS, name = "Land equivalent (Gha/yr)")
  ) +
  coord_cartesian(expand = FALSE) +
  labs(tag = "a", title = "Biomass", x = "Year", y = "Annual primary consumption (Gt/yr)") +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Biomass"]))
  )

p_b <- ggplot(panel_b_df, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = PALETTE_MATERIAL_GROUPS["Fossil fuels"], alpha = 0.25) +
  geom_line(aes(y = med), colour = PALETTE_MATERIAL_GROUPS["Fossil fuels"], linewidth = 0.8) +
  geom_hline(yintercept = FOSSIL_RESERVES_GTOE, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  annotate(
    "text",
    x = FIG_START, y = FOSSIL_RESERVES_GTOE, label = "Proved reserves",
    colour = "grey30", hjust = 0, vjust = 1.5, size = pb_annot_size("largeFont")
  ) +
  scale_x_continuous(breaks = X_BREAKS) +
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / FOSSIL_RESERVES_GTOE, name = "Share of global reserves", labels = scales::percent_format(accuracy = 1))
  ) +
  coord_cartesian(expand = FALSE) +
  labs(tag = "b", title = "Fossil fuels", x = "Year", y = "Cumulative primary consumption (Gt)") +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Fossil fuels"]))
  )

p_c <- ggplot(panel_c_df, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = PALETTE_MATERIAL_GROUPS["Metal ores"], alpha = 0.25) +
  geom_line(aes(y = med), colour = PALETTE_MATERIAL_GROUPS["Metal ores"], linewidth = 0.8) +
  geom_hline(yintercept = metal_hist_benchmark_Gt, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  annotate(
    "text",
    x = FIG_START, y = metal_hist_benchmark_Gt, label = metal_hist_label,
    colour = "grey30", hjust = 0, vjust = 1.5, size = pb_annot_size("largeFont")
  ) +
  scale_x_continuous(breaks = X_BREAKS) +
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(
      ~ . / metal_hist_benchmark_Gt,
      name = paste0("Share of ", metal_hist_label),
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  coord_cartesian(expand = FALSE) +
  labs(tag = "c", title = "Metal ores", x = "Year", y = "Cumulative primary consumption (Gt)") +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]))
  )

p_d <- ggplot(panel_d_df, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = PALETTE_MATERIAL_GROUPS["Non-metallic minerals"], alpha = 0.25) +
  geom_line(aes(y = med), colour = PALETTE_MATERIAL_GROUPS["Non-metallic minerals"], linewidth = 0.8) +
  geom_hline(yintercept = nonmet_hist_benchmark_Gt, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  annotate(
    "text",
    x = FIG_START, y = nonmet_hist_benchmark_Gt, label = nonmet_hist_label,
    colour = "grey30", hjust = 0, vjust = 1.5, size = pb_annot_size("largeFont")
  ) +
  scale_x_continuous(breaks = X_BREAKS) +
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(
      ~ . / nonmet_hist_benchmark_Gt,
      name = paste0("Share of ", nonmet_hist_label),
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  coord_cartesian(expand = FALSE) +
  labs(tag = "d", title = "Non-metallic minerals", x = "Year", y = "Cumulative primary consumption (Gt)") +
  theme_pb_large() +
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.location = "panel",
    plot.title = element_text(colour = unname(PALETTE_MATERIAL_GROUPS["Non-metallic minerals"]))
  )

fig5 <- (p_a + p_b + p_c + p_d) + patchwork::plot_layout(ncol = 2)

# Save ------------------------------------------------------------------

ggsave("Figures/Supporting-Figures/S19_CumulativeExtraction.png", fig5, units = "cm", dpi = 600, width = 17.4, height = 17.4)
ggsave("Figures/SVG/Supporting-Figures/S19_CumulativeExtraction.svg", fig5, units = "cm", width = 17.4, height = 17.4)
clean_svg("Figures/SVG/Supporting-Figures/S19_CumulativeExtraction.svg")

cat("  Saved: Figures/Supporting-Figures/S19_CumulativeExtraction.png, Figures/SVG/Supporting-Figures/S19_CumulativeExtraction.svg\n")

## Save figure data, one file per panel ----------------------------------

dir.create("Figures/RawData/", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(panel_a_df, "Figures/RawData/S19a_biomass.csv")
readr::write_csv(panel_b_df, "Figures/RawData/S19b_fossil.csv")
readr::write_csv(panel_c_df, "Figures/RawData/S19c_metals.csv")
readr::write_csv(panel_d_df, "Figures/RawData/S19d_minerals.csv")
cat("  Saved: Figures/RawData/S19a_biomass.csv, fig5b_fossil.csv, fig5c_metals.csv, fig5d_minerals.csv\n")

cat("=== Figure 5 done ===\n")

# EoF
