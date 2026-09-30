## =============================================================================
## S11 - FlowTimeSeries.R  -> Supporting Figure S11
## Envelope time series of global total DMC, primary consumption,
## secondary supply, and in-use stock across MC runs.
## Saves: Figures/Supporting-Figures/S11_FlowTimeSeries.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

cat("=== Flow Time Series ===\n\n")

cat("Loading MC results...\n")
results <- arrow::read_parquet("Results/MC/mc_results.parquet")

cat("  Rows:", format(nrow(results), big.mark = ","), "\n")

# =============================================================================
# Aggregate: global totals per run × year, convert Mt → Gt
# =============================================================================

ts_global <- results |>
  group_by(run_id, year) |>
  summarise(
    dmc_Gt = sum(total_inflow_Mt, na.rm = TRUE) / 1e3,
    primary_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3,
    secondary_Gt = sum(secondary_supply_Mt, na.rm = TRUE) / 1e3,
    stock_Gt = sum(in_use_stock_Mt, na.rm = TRUE) / 1e3,
    .groups = "drop"
  )

# =============================================================================
# Percentile envelopes
# =============================================================================

ts_env <- ts_global |>
  pivot_longer(c(dmc_Gt, primary_Gt, secondary_Gt, stock_Gt), names_to = "flow_type", values_to = "Gt") |>
  group_by(flow_type, year) |>
  summarise(
    p05 = quantile(Gt, 0.05, na.rm = TRUE),
    p25 = quantile(Gt, 0.25, na.rm = TRUE),
    p50 = quantile(Gt, 0.50, na.rm = TRUE),
    p75 = quantile(Gt, 0.75, na.rm = TRUE),
    p95 = quantile(Gt, 0.95, na.rm = TRUE),
    p_min = min(Gt, na.rm = TRUE),
    p_max = max(Gt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    flow_type = factor(
      flow_type,
      levels = c("dmc_Gt", "primary_Gt", "secondary_Gt", "stock_Gt"),
      labels = c("Total DMC", "Primary consumption", "Secondary supply", "In-use stock")
    )
  )

cat("  Year range:", min(ts_env$year), "-", max(ts_env$year), "\n\n")

# =============================================================================
# Plot: 2×2 envelope panels
# =============================================================================

label_dat <- ts_env |> filter(year == max(year)) |> dplyr::select(flow_type, p_max, p95, p50)

ggplot(ts_env, aes(x = year)) +
  geom_ribbon(aes(ymin = p_min, ymax = p_max), fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p05, ymax = p95), fill = "steelblue", alpha = 0.45) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "steelblue", alpha = 0.55) +
  geom_line(aes(y = p50), colour = "#1a1a1a", linewidth = 0.7) +
  geom_text(
    data = label_dat, aes(x = max(ts_env$year), y = p_max, label = "Min–Max"),
    hjust = 0, nudge_x = 0.5, size = 2.1, colour = "grey50"
  ) +
  geom_text(
    data = label_dat, aes(x = max(ts_env$year), y = p95, label = "P5–P95"),
    hjust = 0, nudge_x = 0.5, size = 2.1, colour = "steelblue"
  ) +
  geom_text(
    data = label_dat, aes(x = max(ts_env$year), y = p50, label = "P50"),
    hjust = 0, nudge_x = 0.5, size = 2.1, colour = "#1a1a1a"
  ) +
  facet_wrap(~flow_type, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = "MC output envelope: global material flows and stock", x = "Year", y = "Gt / yr  (stock: Gt)") +
  theme_pb_large() +
  theme(plot.margin = margin(5.5, 42, 5.5, 5.5))

# fmt: skip
ggsave("Figures/Supporting-Figures/S11_FlowTimeSeries.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave("Figures/SVG/Supporting-Figures/S11_FlowTimeSeries.svg", ggplot2::last_plot(), units = "cm", width = 8.7 * 2, height = 8.7 * 2)
clean_svg("Figures/SVG/Supporting-Figures/S11_FlowTimeSeries.svg")
cat("  Saved: Figures/Supporting-Figures/S11_FlowTimeSeries.png\n")
cat("=== Done ===\n")

# EoF
