## =============================================================================
## S16 - Density.R  -> Supporting Figure S16
## Density / distribution plots of global flows and in-use stock
## across MC runs at TARGET_YEAR. Four figures:
##   (A) 07_density_global       – global totals (primary, secondary, stock)
##   (B) 07_density_region       – violin by world region (primary + stock)
##   (C) 07_density_material_*   – density by material group (primary + stock)
##   (D) 07_conditional_density  – conditional density per MC parameter (PDF)
##
## Present-level reference lines/dots use 2024 historical data:
##   primary consumption → Parameters/UNEP-Materials/materials_region_DMC.csv
##                         (global, regional and by material group)
##   in-use stock        → Parameters/MISO-Stock/stock_2024_total.csv
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)

cat("=== Density Plots ===\n\n")

results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))

cat("  Target year:", TARGET_YEAR, "\n")

# Filter to target year and convert Mt → Gt
res_target <- results |>
  filter(year == TARGET_YEAR) |>
  mutate(
    primary_Gt   = primary_consumption_Mt / 1e3,
    secondary_Gt = secondary_supply_Mt    / 1e3,
    stock_Gt     = in_use_stock_Mt        / 1e3
  )

# =============================================================================
# 2024 historical reference levels
# =============================================================================

# UNEP DMC 2024 by region x material group (Mt)
dmc_2024 <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE) |>
  filter(year == 2024) |>
  left_join(
    readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") |> select(Material_22, Material_group),
    by = c("material_category" = "Material_22")
  ) |>
  filter(Material_group %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")) |>
  select(Region, material_group = Material_group, DMC_Mt)

present_global_primary <- sum(dmc_2024$DMC_Mt, na.rm = TRUE) / 1e3

present_global_stock <- read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |>
  summarise(Gt = sum(stock_Mt, na.rm = TRUE) / 1e3) |>
  pull(Gt)

# Regional present levels
regional_primary_2024 <- dmc_2024 |>
  group_by(region = Region) |>
  summarise(present_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

regional_stock_2024 <- read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |>
  group_by(region = Region) |>
  summarise(present_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Material group present levels (primary, 2024)
mat_primary_2024 <- dmc_2024 |>
  group_by(material_group) |>
  summarise(present_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(material_group = factor(
    material_group,
    levels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
  ))

# Material group present levels (stock, 2024)
mat_stock_2024 <- read_csv("Parameters/MISO-Stock/stock_2024_total.csv", show_col_types = FALSE) |>
  mutate(material_group = ifelse(grepl("^Metal", material), "Metal ores", "Non-metallic minerals")) |>
  group_by(material_group) |>
  summarise(present_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(material_group = factor(
    material_group,
    levels = c("Metal ores", "Non-metallic minerals")
  ))

# =============================================================================
# (A) Global density
# =============================================================================

cat("  (A) Global density\n")

global_target <- res_target |>
  group_by(run_id) |>
  summarise(
    `Primary consumption` = sum(primary_Gt,   na.rm = TRUE),
    `Secondary supply`    = sum(secondary_Gt, na.rm = TRUE),
    `In-use stock`        = sum(stock_Gt,     na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-run_id, names_to = "flow", values_to = "Gt") |>
  mutate(flow = factor(flow, levels = c("Primary consumption", "Secondary supply", "In-use stock")))

flow_colours <- c(
  "Primary consumption" = "#e31a1c",
  "Secondary supply"    = "#33a02c",
  "In-use stock"        = "#1f78b4"
)

# Median lines
global_median <- global_target |>
  group_by(flow) |>
  summarise(med = median(Gt), .groups = "drop")

# Present level (2024): only primary and stock have historical data
global_present <- tibble(
  flow        = factor(c("Primary consumption", "In-use stock"),
                       levels = c("Primary consumption", "Secondary supply", "In-use stock")),
  present_Gt  = c(present_global_primary, present_global_stock)
)

ggplot(global_target, aes(x = Gt, fill = flow, colour = flow)) +
  geom_density(alpha = 0.35, linewidth = 0.4) +
  geom_vline(
    data = global_median,
    aes(xintercept = med, colour = flow),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_vline(
    data = global_present,
    aes(xintercept = present_Gt),
    linetype = "dashed", colour = "grey50", linewidth = 0.5
  ) +
  facet_wrap(~flow, scales = "free", ncol = 1) +
  scale_fill_manual(values  = flow_colours, guide = "none") +
  scale_colour_manual(values = flow_colours, guide = "none") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = paste0("Global distribution at ", TARGET_YEAR),
    subtitle = "Coloured dashed = median; grey dashed = 2024 present level (primary & stock only)",
    x        = "Gt / yr  (stock: Gt)",
    y        = "Density"
  ) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Simulation/07_density_global.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7 * 2)
cat("    Saved: Figures/Simulation/07_density_global.png\n")

# =============================================================================
# (B) By region (violin)
# =============================================================================

cat("  (B) By region\n")

region_target <- res_target |>
  group_by(run_id, region) |>
  summarise(
    primary_Gt = sum(primary_Gt, na.rm = TRUE),
    stock_Gt   = sum(stock_Gt,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(c(primary_Gt, stock_Gt), names_to = "flow", values_to = "Gt") |>
  mutate(
    flow = factor(flow,
                  levels = c("primary_Gt", "stock_Gt"),
                  labels = c("Primary consumption (Gt/yr)", "In-use stock (Gt)"))
  )

region_order <- region_target |>
  filter(flow == "Primary consumption (Gt/yr)") |>
  group_by(region) |>
  summarise(med = median(Gt), .groups = "drop") |>
  arrange(med) |>
  pull(region)

region_target <- region_target |>
  mutate(region = factor(region, levels = region_order))

# Present-level dots by region and flow
present_regional <- bind_rows(
  regional_primary_2024 |> mutate(flow = "Primary consumption (Gt/yr)"),
  regional_stock_2024   |> mutate(flow = "In-use stock (Gt)")
) |>
  mutate(
    flow   = factor(flow, levels = c("Primary consumption (Gt/yr)", "In-use stock (Gt)")),
    region = factor(region, levels = region_order)
  )

ggplot(region_target, aes(x = Gt, y = region, fill = region)) +
  geom_violin(alpha = 0.65, linewidth = 0.3, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.3, outlier.alpha = 0.3,
               fill = "white", linewidth = 0.3) +
  geom_point(
    data = present_regional,
    aes(x = present_Gt, y = region),
    shape = 21, size = 1.8, fill = "black", colour = "white", stroke = 0.4,
    inherit.aes = FALSE
  ) +
  facet_wrap(~flow, scales = "free_x", ncol = 2) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = paste0("Distribution by region at ", TARGET_YEAR),
    subtitle = "Black dot = 2024 present level",
    x     = NULL,
    y     = NULL
  ) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Supporting-Figures/S16_DensityByRegion.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 1.5)
ggsave("Figures/SVG/Supporting-Figures/S16_DensityByRegion.svg", ggplot2::last_plot(), units = "cm", width = 8.7 * 2, height = 8.7 * 1.5)
clean_svg("Figures/SVG/Supporting-Figures/S16_DensityByRegion.svg")
cat("    Saved: Figures/Supporting-Figures/S16_DensityByRegion.png\n")

# =============================================================================
# (C) By material group (density)
# =============================================================================

cat("  (C) By material group\n")

mat_target <- res_target |>
  group_by(run_id, material_group) |>
  summarise(
    primary_Gt = sum(primary_Gt, na.rm = TRUE),
    stock_Gt   = sum(stock_Gt,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    material_group = factor(
      material_group,
      levels = c("biomass", "fossil_fuels", "metal_ores", "nonmetallic_minerals"),
      labels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    )
  )

mat_colours <- c(
  "Biomass"                 = "#4daf4a",
  "Fossil fuels"            = "#984ea3",
  "Metal ores"              = "#e41a1c",
  "Non-metallic minerals"   = "#ff7f00"
)

# Medians per material group (primary)
mat_median_primary <- mat_target |>
  group_by(material_group) |>
  summarise(med = median(primary_Gt), .groups = "drop")

p_mat_primary <- ggplot(mat_target, aes(x = primary_Gt, fill = material_group, colour = material_group)) +
  geom_density(alpha = 0.45, linewidth = 0.4) +
  geom_vline(
    data = mat_median_primary,
    aes(xintercept = med, colour = material_group),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_vline(
    data = mat_primary_2024,
    aes(xintercept = present_Gt),
    linetype = "dashed", colour = "grey50", linewidth = 0.5
  ) +
  facet_wrap(~material_group, scales = "free", ncol = 2) +
  scale_fill_manual(values   = mat_colours, guide = "none") +
  scale_colour_manual(values = mat_colours, guide = "none") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = paste0("Primary consumption by material group at ", TARGET_YEAR),
    subtitle = "Coloured dashed = median; grey dashed = 2024 present level",
    x = "Gt / yr", y = "Density"
  ) +
  theme_pb_large()

# Medians per material group (stock)
mat_median_stock <- mat_target |>
  filter(material_group %in% c("Metal ores", "Non-metallic minerals")) |>
  group_by(material_group) |>
  summarise(med = median(stock_Gt), .groups = "drop")

p_mat_stock <- ggplot(
    mat_target |> filter(material_group %in% c("Metal ores", "Non-metallic minerals")),
    aes(x = stock_Gt, fill = material_group, colour = material_group)
  ) +
  geom_density(alpha = 0.45, linewidth = 0.4) +
  geom_vline(
    data = mat_median_stock,
    aes(xintercept = med, colour = material_group),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_vline(
    data = mat_stock_2024,
    aes(xintercept = present_Gt),
    linetype = "dashed", colour = "grey50", linewidth = 0.5
  ) +
  facet_wrap(~material_group, scales = "free", ncol = 2) +
  scale_fill_manual(values   = mat_colours, guide = "none") +
  scale_colour_manual(values = mat_colours, guide = "none") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = paste0("In-use stock by material group at ", TARGET_YEAR),
    subtitle = "Coloured dashed = median; grey dashed = 2024 present level",
    x = "Gt", y = "Density"
  ) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Simulation/07_density_material_primary.png", p_mat_primary, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
# fmt: skip
ggsave("Figures/Simulation/07_density_material_stock.png",   p_mat_stock,   units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("    Saved: Figures/Simulation/07_density_material_*.png\n")

cat("=== Density done ===\n\n")


# =============================================================================
# (D) Conditional density per MC parameter
# =============================================================================

mc_input <- readr::read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)

dmc_target <- results |>
  filter(year == TARGET_YEAR) |>
  group_by(run_id) |>
  summarise(primary_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

base_df <- dmc_target |>
  left_join(mc_input, by = "run_id")

cat("  (D) Conditional density — Runs:", nrow(base_df), "| Target year:", TARGET_YEAR, "\n\n")

param_labels <- c(
  ssp_u                               = "SSP position",
  target_year_u                           = "Intensity convergence year",
  intensity_crops_global                  = "Crops intensity",
  intensity_grazed_biomass_global         = "Grazed biomass intensity",
  intensity_wood_global                   = "Wood intensity",
  intensity_coal_global                   = "Coal intensity",
  intensity_gas_global                    = "Natural gas intensity",
  intensity_oil_global                    = "Petroleum intensity",
  intensity_buildings_metalOres_global    = "Metal ores – Buildings intensity",
  intensity_buildings_nonMetallic_global  = "Non-metallic – Buildings intensity",
  intensity_civil_metalOres_global        = "Metal ores – Civil infra intensity",
  intensity_civil_nonMetallic_global      = "Non-metallic – Civil infra intensity",
  intensity_machinery_metalOres_global    = "Metal ores – Machinery intensity",
  intensity_sl_products_metalOres_global  = "Metal ores – Short-lived products intensity",
  recycling_Fe_global                     = "Recycling rate endpoint – Fe",
  recycling_NonFe_global                  = "Recycling rate endpoint – NonFe",
  recyc_convergence_yr_global             = "Recycling convergence year",
  downcycling_buildings_global            = "Downcycling rate – Buildings",
  downcycling_civil_infrastructure_global = "Downcycling rate – Civil infra",
  sub_factor_recycling_same               = "Substitution factor – Same-use recycling",
  sub_factor_recycling_same_civil         = "Substitution factor – Same-use recycling (civil)",
  max_secondary_roads                     = "Max secondary material for roads",
  sub_factor_downcycling_roads            = "Substitution factor – Downcycling to roads",
  grade_ore_fe_u                          = "Ore grade – Fe",
  grade_ore_nonfe_u                       = "Ore grade – NonFe",
  lifetime_mean_buildings                 = "Lifetime mean – Buildings",
  lifetime_mean_civil_infrastructure      = "Lifetime mean – Civil infra",
  lifetime_mean_machinery                 = "Lifetime mean – Machinery",
  lifetime_mean_short_lived               = "Lifetime mean – Short-lived products",
  lifetime_k_buildings                    = "Lifetime shape k – Buildings",
  lifetime_k_civil_infrastructure         = "Lifetime shape k – Civil infra",
  lifetime_k_machinery                    = "Lifetime shape k – Machinery",
  lifetime_k_short_lived                  = "Lifetime shape k – Short-lived products"
)

make_label <- function(col) {
  if (col %in% names(param_labels)) param_labels[[col]] else col
}

quartile_colors <- c(
  "Q1 (low)"  = "#2166ac",
  "Q2"        = "#92c5de",
  "Q3"        = "#f4a582",
  "Q4 (high)" = "#b2182b"
)
quartile_labels <- names(quartile_colors)

param_cols <- setdiff(names(mc_input), "run_id")
cat("  Generating", length(param_cols), "pages...\n")

dir.create("Figures/Simulation", recursive = TRUE, showWarnings = FALSE)
out_pdf <- "Figures/Simulation/07_conditional_density.pdf"

pdf(out_pdf, width = 8.7 / 2.54, height = 8.7 / 2.54)

for (col in param_cols) {
  label <- make_label(col)
  cat("    ", col, "\n")

  is_categorical <- is.character(base_df[[col]]) || is.factor(base_df[[col]])

  df_plot <- base_df |>
    dplyr::select(primary_Gt, group = dplyr::all_of(col))

  if (is_categorical) {
    df_plot <- df_plot |>
      mutate(group = factor(group))

    cats <- levels(df_plot$group)
    fill_vals <- setNames(scales::hue_pal()(length(cats)), cats)
    legend_title <- label

  } else {
    breaks <- quantile(df_plot$group, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
    df_plot <- df_plot |>
      mutate(group = cut(group, breaks = breaks, labels = quartile_labels, include.lowest = TRUE))
    fill_vals     <- quartile_colors
    legend_title  <- paste0(label, "\n(quartile)")
  }

  p <- ggplot(df_plot, aes(x = primary_Gt, colour = group)) +
    geom_density(fill = NA, linewidth = 0.9) +
    scale_colour_manual(values = fill_vals, name = legend_title) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title    = paste0("Primary consumption at ", TARGET_YEAR),
      subtitle = paste0("Conditioned on: ", label),
      x        = "Primary consumption (Gt / yr)",
      y        = "Density"
    ) +
    theme_pb_large() +
    theme(legend.position = c(0.82, 0.72))

  print(p)
}

dev.off()

cat("\nSaved", length(param_cols), "pages to:", out_pdf, "\n")
cat("=== Density done ===\n")

# EoF
