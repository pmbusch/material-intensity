## =============================================================================
## Figure 2 Detail - Material.R
## Material-group detail behind panels c-f of Figure 2: one file per material
## group (Biomass, Fossil fuels, Metal ores, Non-metallic minerals), faceted
## by sub-material / end-use, with one coloured line per region (solid =
## historical, dashed = MC median projection). No MC uncertainty ribbons --
## with 8 overlapping regions per facet, bands would be unreadable; only the
## median trajectory is shown per region.
## Saved to Figures/Figure 2 Detail/Material/<group>.png.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")
library(patchwork)

# ── Constants ----------------------------------------------------------------

HIST_END <- 2024L
PROJ_END <- FORECAST_END

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood", "Other biomass")
BIOMASS_CATS_NAMED <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum", "Other fossil fuels")
FOSSIL_CATS_NAMED <- c("Coal", "Natural Gas", "Petroleum")

SUB_USE_LABELS <- c(
  "residential" = "Residential Bldg",
  "non_residential" = "Non-residential Bldg",
  "roads" = "Roads",
  "civil_engineering" = "Civil eng.",
  "machinery_group" = "Machinery",
  "vehicles_group" = "Vehicles",
  "durables" = "Durables",
  "packaging" = "Packaging"
)
ENDUSE_ORDER <- unname(SUB_USE_LABELS)

# mc_results$material_key uses raw end-use names for metal/non-metallic stock;
# relabel to match SUB_USE_LABELS so the 2024 historical->projection splice
# doesn't fall into a separate (unconnected) colour group.
RESULTS_ENDUSE_RELABEL <- c(
  "Residential" = "Residential Bldg",
  "Non-residential" = "Non-residential Bldg",
  "Civil engineering" = "Civil eng."
)

HIST_LW <- 0.55
PROJ_LW <- 0.55

x_sc <- scale_x_continuous(breaks = seq(1970, PROJ_END, 20))
present_line <- geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted")

pb_set_geom_defaults("wide")


# ── SECTION A: Load historical + MC data --------------------------------------

cat("A: Loading historical and MC data\n")

gdp_region_hist <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE) |> rename(region = Region)
dmc_hist <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE) |> rename(region = Region)
stock_subenduse_hist <- read_csv("Parameters/Intermediate/stock_trajectory_subenduse.csv", show_col_types = FALSE) |>
  rename(region = Region)

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(
    material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group),
    # Power-sector end uses folded into civil engineering, the 2024 stock they were carved out of
    material_key = ifelse(startsWith(material_key, "Power: "), "Civil engineering", material_key)
  )

ssp_drivers <- read_csv("Parameters/IIASA-Trajectories/ssp_drivers.csv", show_col_types = FALSE)
gdp_2024_region <- gdp_region_hist |> filter(year == HIST_END) |> transmute(region, gdp_2024 = GDP_2015USD)

run_ssp <- results |> distinct(run_id, ssp_lo, ssp_hi, ssp_share_lo)

pop_idx_region <- ssp_drivers |>
  filter(variable == "Population", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, pop_idx = index)
gdppc_idx_region <- ssp_drivers |>
  filter(variable == "GDP|PPP [per capita]", year >= HIST_END, year <= PROJ_END) |>
  dplyr::select(scenario, region, year, gdppc_idx = index)

pop_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(pop_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(pop_idx_region |> rename(ssp_hi = scenario, pop_idx_hi = pop_idx), by = c("ssp_hi", "region", "year")) |>
  mutate(pop_idx_blend = ssp_share_lo * pop_idx + (1 - ssp_share_lo) * pop_idx_hi) |>
  dplyr::select(run_id, region, year, pop_idx_blend)

gdppc_idx_blend <- run_ssp |>
  dplyr::select(run_id, ssp_lo, ssp_hi, ssp_share_lo) |>
  left_join(gdppc_idx_region |> rename(ssp_lo = scenario), by = "ssp_lo", relationship = "many-to-many") |>
  left_join(gdppc_idx_region |> rename(ssp_hi = scenario, gdppc_idx_hi = gdppc_idx), by = c("ssp_hi", "region", "year")) |>
  mutate(gdppc_idx_blend = ssp_share_lo * gdppc_idx + (1 - ssp_share_lo) * gdppc_idx_hi) |>
  dplyr::select(run_id, region, year, gdppc_idx_blend)

gdp_by_run_region <- pop_idx_blend |>
  left_join(gdppc_idx_blend, by = c("run_id", "region", "year")) |>
  left_join(gdp_2024_region, by = "region") |>
  transmute(run_id, region, year, gdp_run = gdp_2024 * pop_idx_blend * gdppc_idx_blend)


# ── SECTION B: Biomass (M/G) ---------------------------------------------------

cat("B: Biomass\n")

biomass_hist <- dmc_hist |>
  filter(
    material_category %in%
      c(BIOMASS_CATS_NAMED, "Wild catch and harvest", "Non-wild animal products", "Products mainly from biomass nec.")
  ) |>
  mutate(material_category = if_else(material_category %in% BIOMASS_CATS_NAMED, material_category, "Other biomass")) |>
  group_by(region, year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  transmute(region, material_key = material_category, year, mg = DMC_Mt * 1e9 / GDP_2015USD)

biomass_proj <- results |>
  filter(material_group == "biomass", material_key %in% BIOMASS_CATS, year > HIST_END) |>
  group_by(run_id, region, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = M_Mt * 1e9 / gdp_run) |>
  group_by(region, material_key, year) |>
  summarise(mg = median(mg, na.rm = TRUE), .groups = "drop")

biomass_hist <- biomass_hist |> mutate(material_key = factor(material_key, levels = BIOMASS_CATS))
biomass_proj <- biomass_proj |>
  mutate(material_key = factor(material_key, levels = BIOMASS_CATS)) |>
  bind_rows(biomass_hist |> filter(year == HIST_END) |> dplyr::select(region, material_key, year, mg))


# ── SECTION C: Fossil fuels (M/G) -----------------------------------------------

cat("C: Fossil fuels\n")

fossil_hist <- dmc_hist |>
  filter(
    material_category %in%
      c(
        FOSSIL_CATS_NAMED,
        "Oil shale and tar sands",
        "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
        "Other products mainly from fossil fuels e.g. plastics"
      )
  ) |>
  mutate(material_category = if_else(material_category %in% FOSSIL_CATS_NAMED, material_category, "Other fossil fuels")) |>
  group_by(region, year, material_category) |>
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  transmute(region, material_key = material_category, year, mg = DMC_Mt * 1e9 / GDP_2015USD)

fossil_proj <- results |>
  filter(material_group == "fossil_fuels", material_key %in% FOSSIL_CATS, year > HIST_END) |>
  group_by(run_id, region, material_key, year) |>
  summarise(M_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = M_Mt * 1e9 / gdp_run) |>
  group_by(region, material_key, year) |>
  summarise(mg = median(mg, na.rm = TRUE), .groups = "drop")

fossil_hist <- fossil_hist |> mutate(material_key = factor(material_key, levels = FOSSIL_CATS))
fossil_proj <- fossil_proj |>
  mutate(material_key = factor(material_key, levels = FOSSIL_CATS)) |>
  bind_rows(fossil_hist |> filter(year == HIST_END) |> dplyr::select(region, material_key, year, mg))


# ── SECTION D: Metal ores (S/G by end-use) ---------------------------------------

cat("D: Metal ores\n")

metal_hist <- stock_subenduse_hist |>
  filter(material %in% c("Metal_Fe", "Metal_NonFe")) |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(region, year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  transmute(region, end_use_label, year, mg = stock_Mt * 1e9 / GDP_2015USD)

metal_proj <- results |>
  filter(material_group == "metal_ores", year > HIST_END) |>
  group_by(run_id, region, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = stock_Mt * 1e9 / gdp_run) |>
  rename(end_use_label = material_key) |>
  mutate(end_use_label = dplyr::if_else(
    end_use_label %in% names(RESULTS_ENDUSE_RELABEL), RESULTS_ENDUSE_RELABEL[end_use_label], end_use_label
  )) |>
  group_by(region, end_use_label, year) |>
  summarise(mg = median(mg, na.rm = TRUE), .groups = "drop")

metal_hist <- metal_hist |> mutate(end_use_label = factor(end_use_label, levels = ENDUSE_ORDER))
metal_proj <- metal_proj |>
  mutate(end_use_label = factor(end_use_label, levels = ENDUSE_ORDER)) |>
  bind_rows(metal_hist |> filter(year == HIST_END) |> dplyr::select(region, end_use_label, year, mg))


# ── SECTION E: Non-metallic minerals (S/G by end-use) -----------------------------

cat("E: Non-metallic minerals\n")

nonmet_hist <- stock_subenduse_hist |>
  filter(material == "Non-metallic minerals") |>
  mutate(end_use_label = SUB_USE_LABELS[sub_use]) |>
  group_by(region, year, end_use_label) |>
  summarise(stock_Mt = sum(stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_region_hist, by = c("region", "year")) |>
  filter(!is.na(GDP_2015USD), year <= HIST_END) |>
  transmute(region, end_use_label, year, mg = stock_Mt * 1e9 / GDP_2015USD)

nonmet_proj <- results |>
  filter(material_group == "nonmetallic_minerals", year > HIST_END) |>
  group_by(run_id, region, material_key, year) |>
  summarise(stock_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp_by_run_region, by = c("run_id", "region", "year")) |>
  mutate(mg = stock_Mt * 1e9 / gdp_run) |>
  rename(end_use_label = material_key) |>
  mutate(end_use_label = dplyr::if_else(
    end_use_label %in% names(RESULTS_ENDUSE_RELABEL), RESULTS_ENDUSE_RELABEL[end_use_label], end_use_label
  )) |>
  group_by(region, end_use_label, year) |>
  summarise(mg = median(mg, na.rm = TRUE), .groups = "drop")

nonmet_hist <- nonmet_hist |> mutate(end_use_label = factor(end_use_label, levels = ENDUSE_ORDER))
nonmet_proj <- nonmet_proj |>
  mutate(end_use_label = factor(end_use_label, levels = ENDUSE_ORDER)) |>
  bind_rows(nonmet_hist |> filter(year == HIST_END) |> dplyr::select(region, end_use_label, year, mg))


# ── SECTION F: Build + save one facet figure per material group -------------

cat("F: Building material-group figures\n")

dir.create("Figures/Figure 2 Detail/Material", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures/SVG/Figure 2 Detail/Material", recursive = TRUE, showWarnings = FALSE)

region_legend <- guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1)))
legend_bottom <- theme(legend.position = "bottom", legend.title = element_blank())

p_biomass <- ggplot() +
  geom_line(data = biomass_hist, aes(x = year, y = mg, colour = region), linewidth = HIST_LW) +
  geom_line(data = biomass_proj, aes(x = year, y = mg, colour = region), linewidth = PROJ_LW, linetype = "dashed") +
  present_line +
  facet_wrap(~material_key, ncol = 2, scales = "free_y") +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_colour_manual(values = PALETTE_REGIONS) +
  region_legend +
  labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Biomass — by region") +
  theme_pb_wide() +
  legend_bottom

p_fossil <- ggplot() +
  geom_line(data = fossil_hist, aes(x = year, y = mg, colour = region), linewidth = HIST_LW) +
  geom_line(data = fossil_proj, aes(x = year, y = mg, colour = region), linewidth = PROJ_LW, linetype = "dashed") +
  present_line +
  facet_wrap(~material_key, ncol = 2, scales = "free_y") +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_colour_manual(values = PALETTE_REGIONS) +
  region_legend +
  labs(x = NULL, y = "Material consumption per GDP (kg/$)", title = "M/G Fossil fuels — by region") +
  theme_pb_wide() +
  legend_bottom

p_metal <- ggplot() +
  geom_line(data = metal_hist, aes(x = year, y = mg, colour = region), linewidth = HIST_LW) +
  geom_line(data = metal_proj, aes(x = year, y = mg, colour = region), linewidth = PROJ_LW, linetype = "dashed") +
  present_line +
  facet_wrap(~end_use_label, ncol = 4, scales = "free_y") +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_colour_manual(values = PALETTE_REGIONS) +
  region_legend +
  labs(x = NULL, y = "Stock per GDP (kg/$)", title = "S/G Metal ores — by region") +
  theme_pb_wide() +
  legend_bottom

p_nonmet <- ggplot() +
  geom_line(data = nonmet_hist, aes(x = year, y = mg, colour = region), linewidth = HIST_LW) +
  geom_line(data = nonmet_proj, aes(x = year, y = mg, colour = region), linewidth = PROJ_LW, linetype = "dashed") +
  present_line +
  facet_wrap(~end_use_label, ncol = 4, scales = "free_y") +
  x_sc +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_colour_manual(values = PALETTE_REGIONS) +
  region_legend +
  labs(x = NULL, y = "Stock per GDP (kg/$)", title = "S/G Non-metallic minerals — by region") +
  theme_pb_wide() +
  legend_bottom

material_figs <- list(
  Biomass = list(plot = p_biomass, width = 17.4, height = 18.4),
  Fossil_fuels = list(plot = p_fossil, width = 17.4, height = 18.4),
  Metal_ores = list(plot = p_metal, width = 17.4, height = 11),
  Non_metallic_minerals = list(plot = p_nonmet, width = 17.4, height = 11)
)

for (nm in names(material_figs)) {
  cat("  -", nm, "\n")
  spec <- material_figs[[nm]]
  fig <- spec$plot & theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
  ggsave(paste0("Figures/Figure 2 Detail/Material/", nm, ".png"), fig, units = "cm", dpi = 600, width = spec$width, height = spec$height)
  ggsave(paste0("Figures/SVG/Figure 2 Detail/Material/", nm, ".svg"), fig, units = "cm", width = spec$width, height = spec$height)
  group_svg_layers(paste0("Figures/SVG/Figure 2 Detail/Material/", nm, ".svg"))
}

cat("  Saved: Figures/Figure 2 Detail/Material/\n")

# EoF
