## =============================================================================
## S15 - RegionContribution.R  -> Supporting Figure S15
## Regional composition figures across the MC uncertainty space.
## Same structure as S13-S14 - MaterialContribution.R, but stacked fill = region
## (PALETTE_REGIONS, 8 levels) instead of material category.
##
## Part A — Contribution by DMC level: stacked area showing how the regional
##   split of consumption shifts with total DMC level (2050 and cumulative
##   2025–2060).
##
## Part B — Regional mix shift over time: stacked area and absolute line chart
##   of global primary DMC by region across the projection period.
##
## Figures saved to Figures/Simulation/ with prefix 05b_
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

library(arrow)

cat("=== Region Contribution ===\n\n")

# =============================================================================
# Load
# =============================================================================

results <- arrow::read_parquet("Results/MC/mc_results.parquet")
cat("  Rows:", format(nrow(results), big.mark = ","), "\n")

# =============================================================================
# Palettes and factor levels
# =============================================================================

REGION_LEVELS <- names(PALETTE_REGIONS)

# Label colour per region (white on dark fills, black on light fills)
LABEL_COL_REGION <- c(
  "East Asia" = "white",
  "Europe & Russia" = "white",
  "North America" = "white",
  "Latin America" = "white",
  "Sub-Saharan Africa" = "black",
  "Middle East & North Africa" = "black",
  "South Asia" = "white",
  "Oceania" = "black"
)

N_BINS <- 50L

nice_breaks <- function(x, n = 5) {
  lo <- ceiling(min(x, na.rm = TRUE) / 5) * 5
  hi <- floor(max(x, na.rm = TRUE) / 5) * 5
  mid <- pretty(c(lo, hi), n = n)
  mid <- mid[mid > lo & mid < hi]
  sort(unique(c(lo, mid, hi)))
}

# =============================================================================
# BASE AGGREGATION — 2050 snapshot
# =============================================================================

cat("Aggregating 2050 snapshot...\n")

run_2050 <- results |>
  filter(year == 2050L) |>
  group_by(run_id, region) |>
  summarise(total_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop")

# =============================================================================
# FIGURE 1 — 2050 × region
# =============================================================================

cat("Plotting 2050 x region...\n")

shares_2050 <- run_2050 |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_2050 <- shares_2050 |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_2050 <- shares_2050 |>
  left_join(run_bins_2050 |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, region) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup() |>
  mutate(region = factor(region, levels = rev(REGION_LEVELS)))

labels_2050 <- binned_2050 |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(region)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2)

x_breaks_2050 <- range(binned_2050$x_mid)

ggplot(binned_2050, aes(x = x_mid, y = share_pct, fill = region)) +
  geom_area(colour = "black", linewidth = 0.3, alpha = 1) +
  geom_text(
    data = labels_2050,
    aes(x = x_mid, y = label_y, label = region, colour = region),
    size = 1.9, hjust = 0.5, fontface = "bold"
  ) +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL, breaks = rev(REGION_LEVELS)) +
  scale_colour_manual(values = LABEL_COL_REGION, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_2050) +
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
ggsave("Figures/Simulation/05b_region_contribution_2050.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2,height = 8.7)

# =============================================================================
# BASE AGGREGATION — cumulative 2025–2060
# =============================================================================

cat("\nAggregating cumulative 2025-2060...\n")

run_cumul <- results |>
  filter(year >= 2025L, year <= 2060L) |>
  group_by(run_id, region) |>
  summarise(total_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop")

# =============================================================================
# FIGURE 2 — Cumulative × region
# =============================================================================

cat("Plotting cumulative x region...\n")

shares_cumul <- run_cumul |>
  group_by(run_id) |>
  mutate(x_Gt = sum(total_Mt) / 1e3) |>
  ungroup() |>
  mutate(share = total_Mt / (x_Gt * 1e3))

run_bins_cumul <- shares_cumul |> distinct(run_id, x_Gt) |> mutate(bin = ntile(x_Gt, N_BINS))

binned_cumul <- shares_cumul |>
  left_join(run_bins_cumul |> dplyr::select(run_id, bin), by = "run_id") |>
  group_by(bin, region) |>
  summarise(x_mid = median(x_Gt), share_med = median(share), .groups = "drop") |>
  group_by(bin) |>
  mutate(share_pct = share_med / sum(share_med) * 100) |>
  ungroup() |>
  mutate(region = factor(region, levels = rev(REGION_LEVELS)))

labels_cumul <- binned_cumul |>
  filter(bin == N_BINS %/% 2) |>
  arrange(desc(region)) |>
  mutate(cum_top = cumsum(share_pct), cum_bot = lag(cum_top, default = 0), label_y = (cum_top + cum_bot) / 2)

x_breaks_cumul <- nice_breaks(binned_cumul$x_mid, n = 5)[c(-1, -9)]

ggplot(binned_cumul, aes(x = x_mid, y = share_pct, fill = region)) +
  geom_area(colour = "black", linewidth = 0.3, alpha = 1) +
  geom_text(
    data = labels_cumul,
    aes(x = x_mid, y = label_y, label = region, colour = region),
    size = 1.9, hjust = 0.5, fontface = "bold"
  ) +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL, breaks = rev(REGION_LEVELS)) +
  scale_colour_manual(values = LABEL_COL_REGION, guide = "none") +
  scale_x_continuous(labels = scales::comma, breaks = x_breaks_cumul) +
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
ggsave("Figures/Simulation/05b_region_contribution_cumul.png",ggplot2::last_plot(),units = "cm",dpi = 600,width = 8.7 * 2,height = 8.7)

cat("\n=== Region Contribution done ===\n\n")

# =============================================================================
# PART B — Regional mix shift over time
# =============================================================================

cat("=== Region Mix Shift ===\n\n")

# Global total per run × region × year
reg_global <- results |>
  group_by(run_id, region, year) |>
  summarise(primary_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

run_totals <- reg_global |>
  group_by(run_id, year) |>
  summarise(total_Gt = sum(primary_Gt), .groups = "drop")

reg_shares <- reg_global |>
  left_join(run_totals, by = c("run_id", "year")) |>
  mutate(share = primary_Gt / total_Gt) |>
  mutate(region = factor(region, levels = REGION_LEVELS))

# Quantile envelope per region × year
share_env <- reg_shares |>
  arrange(desc(region), year) |>
  group_by(region, year) |>
  summarise(p25 = quantile(share, 0.25), p50 = quantile(share, 0.50), p75 = quantile(share, 0.75), .groups = "drop") |>
  mutate(region = factor(region, levels = REGION_LEVELS))

abs_env <- reg_global |>
  mutate(region = factor(region, levels = REGION_LEVELS)) |>
  group_by(region, year) |>
  summarise(
    p25 = quantile(primary_Gt, 0.25),
    p50 = quantile(primary_Gt, 0.50),
    p75 = quantile(primary_Gt, 0.75),
    .groups = "drop"
  )

# =============================================================================
# FIGURE 3 — Stacked area of median shares + P25–P75 uncertainty ribbons
# =============================================================================

share_stack <- share_env |>
  arrange(desc(region), year) |>
  group_by(year) |>
  mutate(cum_p50 = cumsum(p50), lag_p50 = lag(cum_p50, default = 0)) |>
  ungroup()

ggplot(share_stack, aes(x = year, fill = region, colour = region)) +
  geom_ribbon(aes(ymin = lag_p50, ymax = cum_p50), alpha = 0.85, linewidth = 0.3) +
  geom_text(
    data = share_stack |> filter(year == max(year)),
    aes(y = (cum_p50 + lag_p50) / 2, label = region, colour = region),
    x = max(share_stack$year), hjust = 1, nudge_x = -0.3,
    size = 2.1, fontface = "bold"
  ) +
  scale_fill_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_colour_manual(values = LABEL_COL_REGION, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Regional composition of global primary DMC",
    subtitle = "Stacked median shares; translucent bands = P25–P75 uncertainty",
    x = "Year",
    y = "Share of global primary DMC"
  ) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Supporting-Figures/S15_RegionMixShares.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/SVG/Supporting-Figures/S15_RegionMixShares.svg", ggplot2::last_plot(), units = "cm", width = 8.7 * 2, height = 8.7)
clean_svg("Figures/SVG/Supporting-Figures/S15_RegionMixShares.svg")
cat("  Saved: Figures/Supporting-Figures/S15_RegionMixShares.png\n")

# =============================================================================
# FIGURE 4 — Absolute Gt/yr with P25–P75 band per region
# =============================================================================

ggplot(abs_env, aes(x = year, colour = region, fill = region)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25, colour = NA) +
  geom_line(aes(y = p50), linewidth = 0.7) +
  geom_text(
    data = abs_env |> filter(year == max(year)),
    aes(y = p50, label = region),
    hjust = 0, nudge_x = 0.5, size = 2.3
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_fill_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Global primary DMC by region (absolute)",
    subtitle = "Median line and P25–P75 band",
    x = "Year",
    y = "Primary consumption (Gt/yr)"
  ) +
  theme_pb_large() +
  theme(plot.margin = margin(5.5, 55, 5.5, 5.5))

# fmt: skip
ggsave("Figures/Simulation/05b_region_mix_absolute.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("  Saved: Figures/Simulation/05b_region_mix_absolute.png\n")

cat("=== Region Contribution done ===\n")

# EoF
