## =============================================================================
## S13-S14 - MaterialContribution.R  -> Supporting Figures S13, S14
## Material composition figures across the MC uncertainty space.
##
## Part A — Contribution by DMC level: stacked area showing how material
##   composition shifts with total DMC level (2050 and cumulative 2025–2060),
##   for 4 groups and 16 sub-categories.
##
## Part B — Material mix shift over time: stacked area and absolute line chart
##   of global primary DMC by material group across the projection period.
##
## Figures saved to Figures/Simulation/ with prefix 05_
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)

cat("=== Material Contribution ===\n\n")

# =============================================================================
# Load
# =============================================================================

results <- arrow::read_parquet("Results/MC/mc_results.parquet")
cat("  Rows:", format(nrow(results), big.mark = ","), "\n")

# =============================================================================
# Palettes and factor levels
# =============================================================================

GROUP_LEVELS_4 <- c("Non-metallic minerals", "Biomass", "Fossil fuels", "Metal ores")

COLS_4GROUP <- c(
  "Non-metallic minerals" = "#78909C",
  "Biomass" = "#558B2F",
  "Fossil fuels" = "#5D4037",
  "Metal ores" = "#B71C1C"
)

LABEL_COL_4 <- c(
  "Non-metallic minerals" = "black",
  "Biomass" = "white",
  "Fossil fuels" = "white",
  "Metal ores" = "white"
)

COLS_DETAIL <- c(
  "Coal" = "#3E2723",
  "Natural Gas" = "#6D4C41",
  "Petroleum" = "#A1887F",
  "Other fossil fuels" = "#D7CCC8",
  "Crops" = "#1B5E20",
  "Crop Residues" = "#388E3C",
  "Wood" = "#558B2F",
  "Grazed biomass" = "#8BC34A",
  "Other biomass" = "#DCEDC8",
  "Metals – Buildings" = "#B71C1C",
  "Metals – Civil infra" = "#D32F2F",
  "Metals – Machinery" = "#E57373",
  "Metals – Short-lived" = "#FFCDD2",
  "Metals – Power sector" = "#7F0000",
  "Minerals – Buildings" = "#455A64",
  "Minerals – Civil infra" = "#607D8B",
  "Minerals – Machinery" = "#90A4AE",
  "Minerals – Short-lived" = "#CFD8DC",
  "Minerals – Power sector" = "#263238"
)

LABEL_COL_DETAIL <- c(
  "Coal" = "white",
  "Natural Gas" = "white",
  "Petroleum" = "black",
  "Other fossil fuels" = "black",
  "Crops" = "white",
  "Crop Residues" = "white",
  "Wood" = "white",
  "Grazed biomass" = "black",
  "Other biomass" = "black",
  "Metals – Buildings" = "white",
  "Metals – Civil infra" = "white",
  "Metals – Machinery" = "black",
  "Metals – Short-lived" = "black",
  "Metals – Power sector" = "white",
  "Minerals – Buildings" = "white",
  "Minerals – Civil infra" = "white",
  "Minerals – Machinery" = "black",
  "Minerals – Short-lived" = "black",
  "Minerals – Power sector" = "white"
)

N_BINS <- 50L

# =============================================================================
# BASE AGGREGATION — 2050 snapshot
# =============================================================================

cat("Aggregating 2050 snapshot...\n")

run_2050 <- results |>
  filter(year == 2050L) |>
  group_by(run_id, material_group, material_key) |>
  summarise(total_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(
    cat_4 = case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group %in% c("metal_fe", "metal_nonfe") ~ "Metal ores",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals",
      TRUE ~ material_group
    ),
    cat_detail = case_when(
      material_group == "fossil_fuels" & material_key == "coal" ~ "Coal",
      material_group == "fossil_fuels" & material_key == "gas" ~ "Natural Gas",
      material_group == "fossil_fuels" & material_key == "oil" ~ "Petroleum",
      material_group == "fossil_fuels" & material_key == "other_fossil" ~ "Other fossil fuels",
      material_group == "biomass" & material_key == "crops" ~ "Crops",
      material_group == "biomass" & material_key == "wood" ~ "Wood",
      material_group == "biomass" & str_detect(material_key, "Grazed") ~ "Grazed biomass",
      material_group == "biomass" & material_key == "other_biomass" ~ "Other biomass",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Residential", "Non-residential") ~ "Metals – Buildings",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Roads", "Civil engineering") ~ "Metals – Civil infra",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Machinery", "Vehicles") ~ "Metals – Machinery",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Durables", "Packaging") ~ "Metals – Short-lived",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Residential", "Non-residential") ~ "Minerals – Buildings",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Roads", "Civil engineering") ~ "Minerals – Civil infra",
      material_group == "nonmetallic_minerals" & material_key %in% c("Machinery", "Vehicles") ~ "Minerals – Machinery",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Durables", "Packaging") ~ "Minerals – Short-lived",
      # Power-sector end uses (generation + batteries) as their own category
      material_group %in% c("metal_fe", "metal_nonfe") & startsWith(material_key, "Power: ") ~ "Metals – Power sector",
      material_group == "nonmetallic_minerals" & startsWith(material_key, "Power: ") ~ "Minerals – Power sector",
      material_group == "nonmetallic_minerals" ~ paste0("Minerals – ", material_key),
      material_group %in% c("metal_fe", "metal_nonfe") ~ paste0("Metals – ", material_key),
      TRUE ~ material_key
    )
  )

# =============================================================================
# FIGURE 1 — 2050 × 4 groups
# =============================================================================

cat("Plotting 2050 x 4 groups...\n")

shares_2050_4 <- run_2050 |>
  group_by(run_id, cat_label = cat_4) |>
  summarise(total_Mt = sum(total_Mt), .groups = "drop") |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_2050_4 <- shares_2050_4 |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_2050_4 <- shares_2050_4 |>
  left_join(run_bins_2050_4 |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, cat_label) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup()

binned_2050_4 <- binned_2050_4 |> mutate(cat_label = factor(cat_label, levels = rev(GROUP_LEVELS_4)))

labels_4g_2050 <- binned_2050_4 |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(cat_label)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2)

x_breaks_2050_4 <- range(binned_2050_4$x_mid)

ggplot(binned_2050_4, aes(x = x_mid, y = share_pct, fill = cat_label)) +
  geom_area(colour = "black", linewidth = 0.5, alpha = 1) +
  geom_text(
    data = labels_4g_2050,
    aes(x = x_mid, y = label_y, label = cat_label, colour = cat_label),
    size = 2.2, hjust = 0.5, fontface = "bold"
  ) +
  scale_fill_manual(values = COLS_4GROUP, name = NULL, breaks = rev(GROUP_LEVELS_4)) +
  scale_colour_manual(values = LABEL_COL_4, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_2050_4) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off", expand = F, ylim = c(0, 100)) +
  theme_pb_large() +
  labs(x = "2050 Material Consumption (Gt/yr)", y = "Share (%)") +
  theme(
    legend.position = "none",
    legend.key.size = unit(0.35, "cm"),
    legend.text = element_text(size = 6),
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA)
  )

# fmt: skip
ggsave("Figures/Simulation/05_material_contribution_2050_4group.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2,height = 8.7)

# =============================================================================
# FIGURE 2 — 2050 × 16 sub-categories
# =============================================================================

cat("Plotting 2050 x 16 sub-categories...\n")

shares_2050_d <- run_2050 |>
  group_by(run_id, cat_4, cat_label = cat_detail) |>
  summarise(total_Mt = sum(total_Mt), .groups = "drop") |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_2050_d <- shares_2050_d |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_2050_d <- shares_2050_d |>
  left_join(run_bins_2050_d |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, cat_4, cat_label) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup()

cat_order_2050_d <- binned_2050_d |>
  group_by(cat_label, cat_4) |>
  summarise(mean_share = mean(share_pct), .groups = "drop") |>
  mutate(cat_4 = factor(cat_4, levels = GROUP_LEVELS_4)) |>
  arrange(cat_4, desc(mean_share)) |>
  pull(cat_label)

binned_2050_d <- binned_2050_d |> mutate(cat_label = factor(cat_label, levels = rev(cat_order_2050_d)))

labels_d_2050 <- binned_2050_d |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(cat_label)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  filter(share_pct >= 0.1)

nice_breaks <- function(x, n = 5) {
  lo <- ceiling(min(x, na.rm = TRUE) / 5) * 5
  hi <- floor(max(x, na.rm = TRUE) / 5) * 5
  mid <- pretty(c(lo, hi), n = n)
  mid <- mid[mid > lo & mid < hi]
  sort(unique(c(lo, mid, hi)))
}

x_breaks_2050_d <- nice_breaks(binned_2050_d$x_mid)

grp_bounds_2050_d <- binned_2050_d |>
  group_by(bin, x_mid, mat_group = cat_4) |>
  summarise(grp_share = sum(share_pct), .groups = "drop") |>
  mutate(mat_group = factor(mat_group, levels = GROUP_LEVELS_4)) |>
  arrange(bin, mat_group) |>
  group_by(bin) |>
  mutate(boundary_y = cumsum(grp_share)) |>
  ungroup() |>
  filter(mat_group != "Metal ores")

ggplot(binned_2050_d, aes(x = x_mid, y = share_pct, fill = cat_label)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 1) +
  geom_line(
    data = grp_bounds_2050_d,
    aes(x = x_mid, y = boundary_y, group = mat_group),
    colour = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_d_2050,
    aes(x = x_mid, y = label_y, label = cat_label, colour = cat_label),
    size = 1.8, hjust = 0.5
  ) +
  scale_fill_manual(values = COLS_DETAIL, name = NULL, breaks = rev(cat_order_2050_d), guide = guide_legend(ncol = 1)) +
  scale_colour_manual(values = LABEL_COL_DETAIL, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_2050_d) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off", expand = F, ylim = c(0, 100)) +
  theme_pb_large() +
  labs(x = "2050 Material Consumption (Gt/yr)", y = "Share (%)") +
  theme(
    legend.position = "none",
    legend.key.size = unit(0.28, "cm"),
    legend.text = element_text(size = 5.5),
    legend.spacing.y = unit(0.05, "cm")
  )

# fmt: skip
ggsave("Figures/Supporting-Figures/S13_MaterialContribution_2050.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2.3,height = 8.7)
ggsave("Figures/SVG/Supporting-Figures/S13_MaterialContribution_2050.svg",ggplot2::last_plot(),units = "cm",width = 8.7 * 2.3,height = 8.7)
clean_svg("Figures/SVG/Supporting-Figures/S13_MaterialContribution_2050.svg")

# =============================================================================
# BASE AGGREGATION — cumulative 2025–2060
# =============================================================================

cat("\nAggregating cumulative 2025-2060...\n")

run_cumul <- results |>
  filter(year >= 2025L, year <= 2060L) |>
  group_by(run_id, material_group, material_key) |>
  summarise(total_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  mutate(
    cat_4 = case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group %in% c("metal_fe", "metal_nonfe") ~ "Metal ores",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals",
      TRUE ~ material_group
    ),
    cat_detail = case_when(
      material_group == "fossil_fuels" & material_key == "coal" ~ "Coal",
      material_group == "fossil_fuels" & material_key == "gas" ~ "Natural Gas",
      material_group == "fossil_fuels" & material_key == "oil" ~ "Petroleum",
      material_group == "fossil_fuels" & material_key == "other_fossil" ~ "Other fossil fuels",
      material_group == "biomass" & material_key == "crops" ~ "Crops",
      material_group == "biomass" & material_key == "wood" ~ "Wood",
      material_group == "biomass" & str_detect(material_key, "Grazed") ~ "Grazed biomass",
      material_group == "biomass" & material_key == "other_biomass" ~ "Other biomass",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Residential", "Non-residential") ~ "Metals – Buildings",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Roads", "Civil engineering") ~ "Metals – Civil infra",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Machinery", "Vehicles") ~ "Metals – Machinery",
      material_group %in%
        c("metal_fe", "metal_nonfe") &
        material_key %in% c("Durables", "Packaging") ~ "Metals – Short-lived",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Residential", "Non-residential") ~ "Minerals – Buildings",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Roads", "Civil engineering") ~ "Minerals – Civil infra",
      material_group == "nonmetallic_minerals" & material_key %in% c("Machinery", "Vehicles") ~ "Minerals – Machinery",
      material_group == "nonmetallic_minerals" &
        material_key %in% c("Durables", "Packaging") ~ "Minerals – Short-lived",
      # Power-sector end uses (generation + batteries) as their own category
      material_group %in% c("metal_fe", "metal_nonfe") & startsWith(material_key, "Power: ") ~ "Metals – Power sector",
      material_group == "nonmetallic_minerals" & startsWith(material_key, "Power: ") ~ "Minerals – Power sector",
      material_group == "nonmetallic_minerals" ~ paste0("Minerals – ", material_key),
      material_group %in% c("metal_fe", "metal_nonfe") ~ paste0("Metals – ", material_key),
      TRUE ~ material_key
    )
  )

# =============================================================================
# FIGURE 3 — Cumulative × 4 groups
# =============================================================================

cat("Plotting cumulative x 4 groups...\n")

shares_cumul_4 <- run_cumul |>
  group_by(run_id, cat_label = cat_4) |>
  summarise(total_Mt = sum(total_Mt), .groups = "drop") |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_cumul_4 <- shares_cumul_4 |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_cumul_4 <- shares_cumul_4 |>
  left_join(run_bins_cumul_4 |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, cat_label) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup()

binned_cumul_4 <- binned_cumul_4 |> mutate(cat_label = factor(cat_label, levels = rev(GROUP_LEVELS_4)))

labels_4g_cumul <- binned_cumul_4 |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(cat_label)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2)

x_breaks_cumul_4 <- nice_breaks(binned_cumul_4$x_mid, n = 5)[c(-1, -9)]

ggplot(binned_cumul_4, aes(x = x_mid, y = share_pct, fill = cat_label)) +
  geom_area(colour = "black", linewidth = 0.5, alpha = 1) +
  geom_text(
    data = labels_4g_cumul,
    aes(x = x_mid, y = label_y, label = cat_label, colour = cat_label),
    size = 2.2, hjust = 0.5, fontface = "bold"
  ) +
  scale_fill_manual(values = COLS_4GROUP, name = NULL, breaks = rev(GROUP_LEVELS_4)) +
  scale_colour_manual(values = LABEL_COL_4, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_cumul_4) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off", expand = F, ylim = c(0, 100)) +
  theme_pb_large() +
  labs(x = "2025-2060 Material Consumption (Gt)", y = "Share (%)") +
  theme(
    legend.position = "none",
    legend.key.size = unit(0.35, "cm"),
    legend.text = element_text(size = 6),
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA)
  )

# fmt: skip
ggsave("Figures/Simulation/05_material_contribution_cumul_4group.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2,height = 8.7)

# =============================================================================
# FIGURE 4 — Cumulative × 16 sub-categories
# =============================================================================

cat("Plotting cumulative x 16 sub-categories...\n")

shares_cumul_d <- run_cumul |>
  group_by(run_id, cat_4, cat_label = cat_detail) |>
  summarise(total_Mt = sum(total_Mt), .groups = "drop") |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_cumul_d <- shares_cumul_d |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_cumul_d <- shares_cumul_d |>
  left_join(run_bins_cumul_d |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, cat_4, cat_label) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup()

cat_order_cumul_d <- binned_cumul_d |>
  group_by(cat_label, cat_4) |>
  summarise(mean_share = mean(share_pct), .groups = "drop") |>
  mutate(cat_4 = factor(cat_4, levels = GROUP_LEVELS_4)) |>
  arrange(cat_4, desc(mean_share)) |>
  pull(cat_label)

binned_cumul_d <- binned_cumul_d |> mutate(cat_label = factor(cat_label, levels = rev(cat_order_cumul_d)))

labels_d_cumul <- binned_cumul_d |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(cat_label)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2) |>
  filter(share_pct >= 0.1)

x_breaks_cumul_d <- range(binned_cumul_d$x_mid)

grp_bounds_cumul_d <- binned_cumul_d |>
  group_by(bin, x_mid, mat_group = cat_4) |>
  summarise(grp_share = sum(share_pct), .groups = "drop") |>
  mutate(mat_group = factor(mat_group, levels = GROUP_LEVELS_4)) |>
  arrange(bin, mat_group) |>
  group_by(bin) |>
  mutate(boundary_y = cumsum(grp_share)) |>
  ungroup() |>
  filter(mat_group != "Metal ores")

ggplot(binned_cumul_d, aes(x = x_mid, y = share_pct, fill = cat_label)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 1) +
  geom_line(
    data = grp_bounds_cumul_d,
    aes(x = x_mid, y = boundary_y, group = mat_group),
    colour = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_d_cumul,
    aes(x = x_mid, y = label_y, label = cat_label, colour = cat_label),
    size = 1.8, hjust = 0.5
  ) +
  scale_fill_manual(
    values = COLS_DETAIL,
    name = NULL,
    breaks = rev(cat_order_cumul_d),
    guide = guide_legend(ncol = 1)
  ) +
  scale_colour_manual(values = LABEL_COL_DETAIL, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_cumul_d) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off", expand = F, ylim = c(0, 100)) +
  theme_pb_large() +
  labs(x = "Cumulative Material Consumption 2025-2060 (Gt)", y = "Share (%)") +
  theme(
    legend.position = "none",
    legend.key.size = unit(0.28, "cm"),
    legend.text = element_text(size = 5.5),
    legend.spacing.y = unit(0.05, "cm")
  )

# fmt: skip
ggsave("Figures/Supporting-Figures/S14_MaterialContribution_Cumulative.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2.3,height = 8.7)
ggsave("Figures/SVG/Supporting-Figures/S14_MaterialContribution_Cumulative.svg",ggplot2::last_plot(),units = "cm",width = 8.7 * 2.3,height = 8.7)
clean_svg("Figures/SVG/Supporting-Figures/S14_MaterialContribution_Cumulative.svg")

cat("\n=== Material Contribution done ===\n\n")


# =============================================================================
# PART B — Material mix shift over time
# =============================================================================

cat("=== Material Mix Shift ===\n\n")

results_mat <- results |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))

mat_levels <- c("biomass", "fossil_fuels", "metal_ores", "nonmetallic_minerals")
mat_labels <- c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")

# Global total per run × material_group × year
mat_global <- results_mat |>
  group_by(run_id, material_group, year) |>
  summarise(primary_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

run_totals <- mat_global |>
  group_by(run_id, year) |>
  summarise(total_Gt = sum(primary_Gt), .groups = "drop")

mat_shares <- mat_global |>
  left_join(run_totals, by = c("run_id", "year")) |>
  mutate(share = primary_Gt / total_Gt) |>
  mutate(material_group = factor(material_group, levels = mat_levels, labels = mat_labels))

# Quantile envelope per material group × year
share_env <- mat_shares |>
  arrange(desc(material_group), year) |>
  group_by(material_group, year) |>
  summarise(p25 = quantile(share, 0.25), p50 = quantile(share, 0.50), p75 = quantile(share, 0.75), .groups = "drop") |>
  mutate(material_group = factor(material_group, levels = mat_labels))

abs_env <- mat_global |>
  mutate(material_group = factor(material_group, levels = mat_levels, labels = mat_labels)) |>
  group_by(material_group, year) |>
  summarise(
    p25 = quantile(primary_Gt, 0.25),
    p50 = quantile(primary_Gt, 0.50),
    p75 = quantile(primary_Gt, 0.75),
    .groups = "drop"
  )

# =============================================================================
# FIGURE 5 — Stacked area of median shares + P25–P75 uncertainty ribbons
# =============================================================================

share_stack <- share_env |>
  arrange(desc(material_group), year) |>
  group_by(year) |>
  mutate(cum_p50 = cumsum(p50), lag_p50 = lag(cum_p50, default = 0)) |>
  ungroup()

ggplot(share_stack, aes(x = year, fill = material_group, colour = material_group)) +
  geom_ribbon(aes(ymin = lag_p50, ymax = cum_p50), alpha = 0.85, linewidth = 0.3) +
  geom_text(
    data = share_stack |> filter(year == max(year)),
    aes(y = (cum_p50 + lag_p50) / 2, label = material_group),
    x = max(share_stack$year), hjust = 1, nudge_x = -0.3,
    size = 2.4, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = COLS_4GROUP, guide = "none") +
  scale_colour_manual(values = COLS_4GROUP, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Material composition of global primary DMC",
    subtitle = "Stacked median shares; translucent bands = P25–P75 uncertainty",
    x = "Year",
    y = "Share of global primary DMC"
  ) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Simulation/05_material_mix_shares.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("  Saved: Figures/Simulation/05_material_mix_shares.png\n")

# =============================================================================
# FIGURE 6 — Absolute Gt/yr with P25–P75 band per material
# =============================================================================

ggplot(abs_env, aes(x = year, colour = material_group, fill = material_group)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25, colour = NA) +
  geom_line(aes(y = p50), linewidth = 0.7) +
  geom_text(
    data = abs_env |> filter(year == max(year)),
    aes(y = p50, label = material_group),
    hjust = 0, nudge_x = 0.5, size = 2.3
  ) +
  scale_colour_manual(values = COLS_4GROUP, guide = "none") +
  scale_fill_manual(values = COLS_4GROUP, guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Global primary DMC by material group (absolute)",
    subtitle = "Median line and P25–P75 band",
    x = "Year",
    y = "Primary consumption (Gt/yr)"
  ) +
  theme_pb_large() +
  theme(plot.margin = margin(5.5, 55, 5.5, 5.5))

# fmt: skip
ggsave("Figures/Simulation/05_material_mix_absolute.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("  Saved: Figures/Simulation/05_material_mix_absolute.png\n")

cat("=== Material Contribution done ===\n")

# EoF
