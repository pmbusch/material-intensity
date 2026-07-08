## =============================================================================
## 03_diagnostics.R
## Post-processing diagnostics for Monte Carlo results.
##
## Run AFTER mc_runner.R. Reads:
##   Results/MC/mc_results.parquet
##   Results/MC/mc_input_matrix.csv
##
## Produces three diagnostics:
##   1. Convergence  -- running P01/P50/P99 vs cumulative N at TARGET_YEAR
##   2. SRRC         -- standardised rank regression coefficients (sensitivity)
##   3. Envelope     -- P5/P25/P50/P75/P95 band per material category × year
##
## Figures saved to Figures/MC/
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/04-Simulation/00-Parameters.R", encoding = "UTF-8")

cat("=== MC Diagnostics ===\n\n")

# =============================================================================
# Load results
# =============================================================================

cat("Loading MC results...\n")
results <- arrow::read_parquet("Results/MC/mc_results.parquet") |>
  mutate(material_group = ifelse(material_group %in% c("metal_fe", "metal_nonfe"), "metal_ores", material_group))
input_matrix <- read_csv("Parameters/MC/mc_input_matrix.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

n_runs <- n_distinct(results$run_id)
cat("  Runs:", n_runs, "| Rows:", format(nrow(results), big.mark = ","), "\n")
cat("  Material categories:", paste(sort(unique(results$material_key)), collapse = ", "), "\n")
cat("  Year range:", min(results$year), "-", max(results$year), "\n\n")

# Global primary consumption per run at TARGET_YEAR (main scalar output)
global_at_target <- results |>
  filter(year == TARGET_YEAR) |>
  group_by(run_id) |>
  summarise(total_Mt = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop") |>
  arrange(run_id)


# =============================================================================
# DIAGNOSTIC 1: Convergence
# =============================================================================

cat("DIAGNOSTIC 1: Convergence\n")

# Compute running P01/P50/P99 at checkpoints
checkpoints <- unique(c(seq(100L, n_runs, by = 100L), n_runs))

conv_data <- purrr::map_dfr(checkpoints, function(n) {
  vals <- global_at_target$total_Mt[seq_len(n)]
  tibble(
    n = n,
    p01 = quantile(vals, 0.01, names = FALSE),
    p50 = quantile(vals, 0.50, names = FALSE),
    p99 = quantile(vals, 0.99, names = FALSE)
  )
})

conv_long <- conv_data |>
  pivot_longer(c(p01, p50, p99), names_to = "percentile", values_to = "Mt") |>
  mutate(Mt = Mt / 1e3) |> #Gt
  mutate(percentile = factor(percentile, levels = c("p99", "p50", "p01"), labels = c("P99", "P50", "P01")))

ggplot(conv_long, aes(n, Mt, colour = percentile)) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = c("P99" = "#d73027", "P50" = "#1a1a1a", "P01" = "#4575b4"), name = NULL) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "MC convergence: global total primary consumption at 2050",
    x = "Cumulative runs (N)",
    y = "Primary consumption (Gt/yr)"
  ) +
  theme_pb_large() +
  theme(legend.position = c(0.88, 0.5))

# fmt: skip
ggsave("Figures/MC/03_convergence.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# fmt: skip
cat("  P01 / P50 / P99 at N =",n_runs,":",round(tail(conv_data$p01, 1)/1e3, 0),"/",round(tail(conv_data$p50, 1)/1e3, 0),"/",round(tail(conv_data$p99, 1)/1e3, 0),"Gt\n\n")


# =============================================================================
# DIAGNOSTIC 2: SRRC sensitivity
# =============================================================================

cat("DIAGNOSTIC 2: SRRC sensitivity\n")

# pop_ssp_u / gdppc_ssp_u are already continuous [0,1] draws -> used as-is
inputs_num <- input_matrix |>
  arrange(run_id) |>
  dplyr::select(-run_id)

# Rank-transform inputs and output
x_rnk <- apply(as.matrix(inputs_num), 2, rank) / nrow(inputs_num) # normalised [0,1]
y_rnk <- rank(global_at_target$total_Mt) / nrow(global_at_target)

# Partial SRRC via OLS on normalised ranks
mod <- lm(y_rnk ~ x_rnk)
summary(mod)
coef_sum <- summary(mod)$coefficients[-1, ] # drop intercept

srrc <- tibble(parameter = rownames(coef_sum), srrc = coef_sum[, "Estimate"], p_value = coef_sum[, "Pr(>|t|)"]) |>
  mutate(
    abs_srrc = abs(srrc),
    # Classify parameter family for colour
    family = dplyr::case_when(
      parameter %in% c("x_rnkpop_ssp_u", "x_rnkgdppc_ssp_u") ~ "SSP choice",
      stringr::str_detect(parameter, "rho") ~ "Variance split (rho)",
      stringr::str_detect(parameter, "target_year|intensity|g_int|delta_int") ~ "Intensity",
      stringr::str_detect(parameter, "circ|conv_year") ~ "Circularity",
      stringr::str_detect(parameter, "lifetime") ~ "Lifetime",
      TRUE ~ "Other"
    ),
    # Readable short labels (strip rank-matrix prefix)
    label = stringr::str_remove(parameter, "^x_rnk"),
    significant = p_value < 0.05
  ) |>
  arrange(desc(abs_srrc))

# Top 20 by |SRRC|
top_srrc <- head(srrc, 20) |> mutate(label = factor(label, levels = rev(label)))

srrc_colours <- c(
  "SSP choice" = "#1f78b4",
  "Variance split (rho)" = "#6a3d9a",
  "Intensity" = "#e31a1c",
  "Circularity" = "#33a02c",
  "Lifetime" = "#ff7f00",
  "Other" = "#999999"
)

ggplot(top_srrc, aes(srrc, label, fill = family)) +
  geom_col(colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = srrc_colours, name = "Parameter family") +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(
    title = "SRRC sensitivity: global total primary consumption at 2050",
    x = "Standardised Rank Regression Coefficient",
    y = NULL
  ) +
  theme_pb_large() +
  theme(legend.position = "right", axis.text.y = element_text(size = 7))

# fmt: skip
ggsave("Figures/MC/03_srrc_sensitivity.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2.5, height = 8.7 * 2)
cat("  Saved: Figures/MC/srrc_sensitivity.png\n")
cat("  Top 5 parameters by |SRRC|:\n")
print(head(srrc |> dplyr::select(parameter, srrc, p_value), 5))
cat("\n")


# =============================================================================
# DIAGNOSTIC 3: Output envelope
# =============================================================================

cat("DIAGNOSTIC 3: Output envelope\n")

HIST_END <- 2024L

# Historical DMC aggregated to material group × year
hist_envelope <- dmc_hist |>
  mutate(
    material_group = case_when(
      material_category %in%
        c(
          "Crops",
          "Crop Residues",
          "Grazed biomass and fodder crops",
          "Wood",
          "Other biomass",
          "Wild catch and harvest",
          "Non-wild animal products",
          "Products mainly from biomass nec."
        ) ~ "Biomass",
      material_category %in%
        c(
          "Coal",
          "Natural Gas",
          "Petroleum",
          "Oil shale and tar sands",
          "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
          "Other products mainly from fossil fuels e.g. plastics"
        ) ~ "Fossil fuels",
      material_category %in% c("Ferrous ores", "Non-ferrous ores") ~ "Metal ores",
      material_category %in%
        c(
          "Non-metallic minerals - construction dominant",
          "Non-metallic minerals - industrial or agricultural dominant"
        ) ~ "Non-metallic minerals",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(material_group), year <= HIST_END) |>
  group_by(year, material_group) |>
  summarise(global_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

envelope <- results |>
  group_by(material_group, year) |>
  mutate(primary_consumption_Gt = primary_consumption_Mt / 1e3) |>
  summarise(
    p05 = quantile(primary_consumption_Gt, 0.05, na.rm = TRUE),
    p25 = quantile(primary_consumption_Gt, 0.25, na.rm = TRUE),
    p50 = quantile(primary_consumption_Gt, 0.50, na.rm = TRUE),
    p75 = quantile(primary_consumption_Gt, 0.75, na.rm = TRUE),
    p95 = quantile(primary_consumption_Gt, 0.95, na.rm = TRUE),
    p_min = min(primary_consumption_Gt, na.rm = TRUE),
    p_max = max(primary_consumption_Gt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    material_group = factor(
      material_group,
      levels = c("biomass", "fossil_fuels", "metal_ores", "nonmetallic_minerals"),
      labels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    )
  )

# Aggregate across regions: sum to global per run x material_group x year, then quantile
envelope_global <- results |>
  group_by(run_id, material_group, year) |>
  summarise(global_Gt = sum(primary_consumption_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  group_by(material_group, year) |>
  summarise(
    p05 = quantile(global_Gt, 0.05),
    p25 = quantile(global_Gt, 0.25),
    p50 = quantile(global_Gt, 0.50),
    p75 = quantile(global_Gt, 0.75),
    p95 = quantile(global_Gt, 0.95),
    p_min = min(global_Gt),
    p_max = max(global_Gt),
    .groups = "drop"
  ) |>
  mutate(
    material_group = factor(
      material_group,
      levels = c("biomass", "fossil_fuels", "metal_ores", "nonmetallic_minerals"),
      labels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    )
  )

# Anchor MC envelope to the actual 2024 historical value (per material group) so the
# projected median/band connects explicitly instead of jumping from the MC's own
# reconstructed 2024 estimate
envelope_global <- envelope_global |>
  mutate(material_group = as.character(material_group)) |>
  filter(year > HIST_END) |>
  bind_rows(
    hist_envelope |>
      filter(year == HIST_END) |>
      transmute(
        material_group, year,
        p05 = global_Gt, p25 = global_Gt, p50 = global_Gt, p75 = global_Gt, p95 = global_Gt,
        p_min = global_Gt, p_max = global_Gt
      )
  ) |>
  mutate(material_group = factor(material_group, levels = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")))

ggplot() +
  # MC uncertainty bands (projection)
  geom_ribbon(data = envelope_global, aes(x = year, ymin = p_min, ymax = p_max), fill = "grey85", alpha = 0.6) +
  geom_ribbon(data = envelope_global, aes(x = year, ymin = p05, ymax = p95), fill = "steelblue", alpha = 0.45) +
  geom_ribbon(data = envelope_global, aes(x = year, ymin = p25, ymax = p75), fill = "steelblue", alpha = 0.55) +
  # MC median (dashed, projection style)
  geom_line(data = envelope_global, aes(x = year, y = p50), colour = "#1a1a1a", linewidth = 0.7, linetype = "dashed") +
  # Historical solid line
  geom_line(data = hist_envelope, aes(x = year, y = global_Gt), colour = "steelblue", linewidth = 0.65) +
  # Present boundary
  geom_vline(xintercept = HIST_END, colour = "grey45", linewidth = 0.3, linetype = "dotted") +
  annotate(
    "text",
    x = HIST_END - 2,
    y = Inf,
    label = "Present (2024)",
    hjust = 1.05,
    vjust = 0.5,
    size = 2.2,
    colour = "grey45",
    angle = 90
  ) +
  # Band labels (right margin of last year)
  geom_text(
    data = envelope_global |> filter(year == max(year)),
    aes(x = year, y = p_max, label = "Min–Max"), hjust = 0, size = 2.2, nudge_x = 0.5, colour = "grey50"
  ) +
  geom_text(
    data = envelope_global |> filter(year == max(year)),
    aes(x = year, y = p95, label = "P5–P95"), hjust = 0, size = 2.2, nudge_x = 0.5, colour = "steelblue"
  ) +
  geom_text(
    data = envelope_global |> filter(year == max(year)),
    aes(x = year, y = p50, label = "P50"), hjust = 0, size = 2.2, nudge_x = 0.5, colour = "#1a1a1a"
  ) +
  facet_wrap(~material_group, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1970, FORECAST_END, 20)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = paste0("MC output envelope: global primary consumption 1970-", FORECAST_END),
    x = "Year",
    y = "Primary consumption (Gt/yr)"
  ) +
  theme_pb_large() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

# fmt: skip
ggsave("Figures/MC/03_output_envelope.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
cat("  Saved: Figures/MC/output_envelope.png\n")

# Save envelope data
write_csv(envelope_global, "Results/MC/mc_envelope.csv")
cat("  Saved: Results/MC/mc_envelope.csv\n")

# Save full SRRC table
write_csv(srrc, "Results/MC/mc_srrc.csv")
cat("  Saved: Results/MC/mc_srrc.csv\n\n")

cat("=== Diagnostics complete ===\n")

# EoF
