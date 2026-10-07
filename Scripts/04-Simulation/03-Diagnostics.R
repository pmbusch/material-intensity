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
## Figures saved to Figures/Simulation/
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
input_matrix <- read_csv("Parameters/Simulation/mc_input_matrix.csv", show_col_types = FALSE)
dmc_hist <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)

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
ggsave("Figures/Simulation/03_convergence.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# fmt: skip
cat("  P01 / P50 / P99 at N =",n_runs,":",round(tail(conv_data$p01, 1)/1e3, 0),"/",round(tail(conv_data$p50, 1)/1e3, 0),"/",round(tail(conv_data$p99, 1)/1e3, 0),"Gt\n\n")


# =============================================================================
# DIAGNOSTIC 2: SRRC sensitivity
# =============================================================================

cat("DIAGNOSTIC 2: SRRC sensitivity\n")

# ssp_u is already a continuous [0,1] draw -> used as-is
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
      parameter %in% c("x_rnkssp_u") ~ "SSP choice",
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
ggsave("Figures/Simulation/03_srrc_sensitivity.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2.5, height = 8.7 * 2)
cat("  Saved: Figures/Simulation/srrc_sensitivity.png\n")
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
ggsave("Figures/Simulation/03_output_envelope.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
cat("  Saved: Figures/Simulation/output_envelope.png\n")

# Save envelope data
write_csv(envelope_global, "Results/MC/mc_envelope.csv")
cat("  Saved: Results/MC/mc_envelope.csv\n")

# Save full SRRC table
write_csv(srrc, "Results/MC/mc_srrc.csv")
cat("  Saved: Results/MC/mc_srrc.csv\n\n")

# =============================================================================
# DIAGNOSTIC 4: Secondary recovery vs drawn recycling / downcycling rates
# =============================================================================
# FORECAST_END is past every run's recovery convergence year (RECYC_CONVERGENCE_YR_MAX),
# so the EOL recovery achieved there must equal the drawn endpoint rate exactly:
#   metals:   1 - sum(not_recovered) / sum(waste) over all end uses   = recycling_rate_fe / _nonfe
#   minerals: same over the giving sectors (buildings, roads, civil eng., power) = downcycling
# Tests (all region x year x material unless stated):
#   T1 mass balance        waste = not_recovered + secondary + spilled (ore-equivalent for metals)
#   T2 achieved = drawn    FORECAST_END achieved recovery rate vs drawn endpoint, per run x region
#   T3 demand cap          secondary <= inflow per row (primary >= 0)
#   T4 room cap (minerals) secondary <= room share x inflow per row (drawn room parameters)
#   T5 response            Spearman(drawn rate, FORECAST_END world secondary share of inflow) > 0

cat("DIAGNOSTIC 4: Secondary recovery vs drawn recycling rates\n")

TOL <- 1e-6
stopifnot(RECYC_CONVERGENCE_YR_MAX < FORECAST_END)
GIVING_SECTORS <- c("Residential", "Non-residential", "Roads", "Civil engineering", POWER_LIFE_CLASSES$label)

rec <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  filter(material_group %in% c("metal_fe", "metal_nonfe", "nonmetallic_minerals")) |>
  dplyr::select(
    run_id, region, material_group, material_key, year, total_inflow_Mt, primary_consumption_Mt,
    secondary_supply_Mt, secondary_supply_Mt_pure, secondary_spilled_Mt, not_recovered_Mt, waste_Mt, waste_Mt_pure
  ) |>
  collect()

# Drawn endpoint values in real units (same semi-uniform mapping as 02-RunSimulations.R)
drawn <- input_matrix |>
  dplyr::select(run_id, all_of(MC_PARAMS$col)) |>
  pivot_longer(-run_id, names_to = "col", values_to = "u") |>
  left_join(MC_PARAMS, by = "col") |>
  mutate(value = if_else(u < 0.5, min + 2 * u * (central - min), central + 2 * (u - 0.5) * (max - central))) |>
  dplyr::select(run_id, col, value) |>
  pivot_wider(names_from = col, values_from = value)
drawn_rate <- bind_rows(
  drawn |> transmute(run_id, material_group = "metal_fe", rate_drawn = recycling_rate_fe),
  drawn |> transmute(run_id, material_group = "metal_nonfe", rate_drawn = recycling_rate_nonfe),
  drawn |> transmute(run_id, material_group = "nonmetallic_minerals", rate_drawn = downcycling)
)

# T1: mass balance per region x year x material
t1 <- rec |>
  group_by(run_id, region, material_group, year) |>
  summarise(
    waste = sum(waste_Mt),
    resid = sum(waste_Mt) - sum(not_recovered_Mt + secondary_supply_Mt + secondary_spilled_Mt),
    .groups = "drop"
  ) |>
  mutate(rel_resid = abs(resid) / pmax(waste, 1e-9))

# T2: achieved EOL recovery at FORECAST_END = drawn endpoint
t2 <- rec |>
  filter(year == FORECAST_END, material_group != "nonmetallic_minerals" | material_key %in% GIVING_SECTORS) |>
  group_by(run_id, region, material_group) |>
  summarise(rate_achieved = 1 - sum(not_recovered_Mt) / sum(waste_Mt), .groups = "drop") |>
  left_join(drawn_rate, by = c("run_id", "material_group")) |>
  mutate(diff = rate_achieved - rate_drawn)

# T3: secondary never exceeds demand (metal mass: inflow is metal, *_pure is metal)
t3 <- rec |> mutate(excess = secondary_supply_Mt_pure - total_inflow_Mt)

# T4: mineral secondary never exceeds its room (power sector receives like civil engineering)
t4 <- rec |>
  filter(material_group == "nonmetallic_minerals") |>
  left_join(drawn, by = "run_id") |>
  mutate(
    room_share = case_when(
      material_key %in% c("Residential", "Non-residential") ~ max_secondary_build_civil * share_concrete_buildings * share_agg_concrete,
      material_key %in% c("Civil engineering", POWER_LIFE_CLASSES$label) ~ max_secondary_build_civil * share_concrete_civil * share_agg_concrete,
      material_key == "Roads" ~ max_secondary_roads * share_granular_road,
      TRUE ~ 0
    ),
    excess = secondary_supply_Mt - room_share * total_inflow_Mt
  )

# T5: response of the realised world secondary share to the drawn rate
t5_data <- rec |>
  filter(year == FORECAST_END) |>
  group_by(run_id, material_group) |>
  summarise(
    secondary_share = sum(secondary_supply_Mt_pure) / sum(total_inflow_Mt), # metal mass / metal mass
    spill_share = sum(secondary_spilled_Mt) / sum(waste_Mt),
    .groups = "drop"
  ) |>
  left_join(drawn_rate, by = c("run_id", "material_group"))
t5 <- t5_data |>
  group_by(material_group) |>
  summarise(spearman = cor(rate_drawn, secondary_share, method = "spearman"), runs_with_spill = sum(spill_share > 1e-9), .groups = "drop")

recycling_tests <- tibble(
  test = c(
    "T1 mass balance (max relative residual)",
    "T2 achieved vs drawn EOL rate at FORECAST_END (max |diff|)",
    "T3 secondary <= inflow (max excess, Mt)",
    "T4 mineral secondary <= room (max excess, Mt)",
    paste0("T5 Spearman(drawn rate, secondary share) - ", t5$material_group)
  ),
  value = c(max(t1$rel_resid), max(abs(t2$diff)), max(t3$excess), max(t4$excess), t5$spearman),
  pass = c(value[1:4] < c(TOL, TOL, TOL, TOL), t5$spearman > 0)
)
print(recycling_tests |> mutate(value = signif(value, 3)) |> as.data.frame())
cat("  Runs with recovered surplus (spill) at", FORECAST_END, ":\n")
print(as.data.frame(t5 |> dplyr::select(material_group, runs_with_spill)))
if (!all(recycling_tests$pass)) {
  warning("Recycling tests failed -- see table above")
}
write_csv(recycling_tests, "Results/MC/mc_recycling_tests.csv")

# Figure: drawn endpoint rate vs achieved EOL recovery (should sit on 1:1) and vs
# realised secondary share of inflow (below the rate: waste < demand, room caps)
t5_plot <- t2 |>
  group_by(run_id, material_group, rate_drawn) |>
  summarise(rate_achieved = mean(rate_achieved), .groups = "drop") |>
  left_join(t5_data |> dplyr::select(run_id, material_group, secondary_share), by = c("run_id", "material_group")) |>
  pivot_longer(c(rate_achieved, secondary_share), names_to = "metric", values_to = "y") |>
  mutate(
    metric = recode(metric, rate_achieved = "EOL recovery achieved", secondary_share = "Secondary share of inflow"),
    material_group = recode(material_group, metal_fe = "Fe metals (recycling)", metal_nonfe = "Non-Fe metals (recycling)", nonmetallic_minerals = "Minerals (downcycling)")
  )

pb_set_geom_defaults("wide")
ggplot(t5_plot, aes(rate_drawn, y, colour = metric)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_wrap(~material_group, nrow = 1) +
  scale_colour_manual(values = c("EOL recovery achieved" = "#1B4F8A", "Secondary share of inflow" = "#B8896A"), name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 1), expand = FALSE) +
  labs(
    x = paste0("Drawn endpoint rate (reached by ", RECYC_CONVERGENCE_YR_MAX, " at the latest)"),
    y = NULL,
    title = paste0("Secondary recovery in ", FORECAST_END, " vs drawn rate (world, 1 point = 1 run)")
  ) +
  theme_pb_wide() +
  theme(legend.position = "inside", legend.position.inside = c(0.15, 0.85))

ggsave("Figures/Simulation/03_recycling_tests.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 8.7)
cat("  Saved: Figures/Simulation/03_recycling_tests.png, Results/MC/mc_recycling_tests.csv\n\n")

cat("=== Diagnostics complete ===\n")

# EoF
