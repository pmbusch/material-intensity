## =============================================================================
## Figure 5 - Alt1 - PrepareData.R  (run for "Figure 5 - Alt1.R")
## Data for Figure 5 alternative 1 (panel c = growth decomposition; panels a, b,
## d unchanged) and SI S22. Sources "Figure 5 - Sensitivity - PrepareData.R"
## for its objects (growth_df, real_matrix, input_matrix, feature_cols,
## world_by_run, results_fig) -- that script's own outputs are rewritten unchanged.
##
## Panel c: per run, additive (LMDI) decomposition of total material growth,
##   ln(M_2060 / M_2025) / T, in percentage points of annual growth (%/yr):
##   M = sum_g M_g over g = biomass, fossil, Fe, NonFe, minerals;
##   ln(M_60/M_25) = sum_g w_g ln(M_g,60 / M_g,25),
##   w_g = L(M_g,60, M_g,25) / L(M_60, M_25), L(a,b) = (a - b) / (ln a - ln b);
##   flows    (biomass, fossil): M_g = P * (G/P) * (M_g/G)
##   metals   (Fe, NonFe each):  Ore = P * (G/P) * (S/G) * (In/S) * (Prim/In) * (Ore/Prim)
##   minerals:                   Prim = P * (G/P) * (S/G) * (In/S) * (Prim/In)
##   S in-use stock, In inflow, Prim primary (= inflow - secondary), metals in
##   metal mass; Ore = ore extracted. Fe and NonFe are separate groups, so the
##   ore terms (Fe, NonFe) are the change in ore grade only (not the Fe/NonFe mix).
##   Each log change times w_g is one term; the terms sum exactly to the total.
##   Terms are then rescaled per run by CAGR / log rate, so they sum to the
##   run's CAGR (compound annual growth rate) instead of its log growth rate.
## S22: GDP elasticity per material group: lm(CAGR_g ~ gdp_cagr + other levers,
##   real units, ssp_u excluded); slope on gdp_cagr.
##
## Output (Parameters/Intermediate/):
##   Figure5_Alt1_Decomposition.csv (run_id, term, pct_yr), FigureS22_GDPElasticity.csv
## =============================================================================

source("Scripts/05-Figures/Figure 5 - Sensitivity - PrepareData.R", encoding = "UTF-8")

cat("\n=== Figure 5 - Alt1 - Prepare Data ===\n\n")

T_YRS <- FORECAST_END - 2025L

growth_run <- growth_df |> dplyr::select(run_id, mat_cagr, gdp_cagr) |> dplyr::arrange(run_id)
R <- real_matrix |> dplyr::filter(run_id %in% growth_run$run_id) |> dplyr::arrange(run_id)


# STEP 1: LMDI growth decomposition per run --------------------------------------

cat("STEP 1: growth decomposition\n")

dec_raw <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year %in% c(2025L, FORECAST_END)) |>
  dplyr::select(
    run_id,
    material_group,
    year,
    primary_consumption_Mt,
    primary_consumption_Mt_pure,
    total_inflow_Mt,
    in_use_stock_Mt
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    grp = dplyr::case_when(
      material_group == "biomass" ~ "bio",
      material_group == "fossil_fuels" ~ "fos",
      material_group == "metal_fe" ~ "fe",
      material_group == "metal_nonfe" ~ "nfe",
      material_group == "nonmetallic_minerals" ~ "min"
    )
  ) |>
  dplyr::group_by(run_id, grp, year) |>
  dplyr::summarise(
    M = sum(primary_consumption_Mt, na.rm = TRUE), # metals: ore
    Prim = sum(primary_consumption_Mt_pure, na.rm = TRUE), # metals: metal mass
    In = sum(total_inflow_Mt, na.rm = TRUE),
    S = sum(in_use_stock_Mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(world_by_run, by = c("run_id", "year")) |>
  dplyr::mutate(yr = dplyr::if_else(year == 2025L, "a", "b")) |>
  dplyr::select(-year) |>
  tidyr::pivot_wider(names_from = yr, values_from = c(M, Prim, In, S, world_pop, world_gdp))

# Logarithmic mean L(a, b) and LMDI group weights w_g = L(M_g) / L(M_total)
dec <- dec_raw |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(Mtot_a = sum(M_a), Mtot_b = sum(M_b)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    L_g = dplyr::if_else(abs(M_b - M_a) < 1e-12, M_a, (M_b - M_a) / (log(M_b) - log(M_a))),
    L_tot = dplyr::if_else(abs(Mtot_b - Mtot_a) < 1e-12, Mtot_a, (Mtot_b - Mtot_a) / (log(Mtot_b) - log(Mtot_a))),
    k = L_g / L_tot * 100 / T_YRS, # w_g -> %/yr
    d_pop = log(world_pop_b / world_pop_a),
    d_gpc = log((world_gdp_b / world_pop_b) / (world_gdp_a / world_pop_a)),
    d_int = log((M_b / world_gdp_b) / (M_a / world_gdp_a)), # flows: M/G
    d_sg = log((S_b / world_gdp_b) / (S_a / world_gdp_a)), # stocks: S/G
    d_turn = log((In_b / S_b) / (In_a / S_a)), # stocks: inflow / stock (turnover)
    d_sec = log((Prim_b / In_b) / (Prim_a / In_a)), # stocks: primary share of inflow (1 - secondary share)
    d_ore = log((M_b / Prim_b) / (M_a / Prim_a)) # Fe, NonFe: ore per t metal (1 / grade)
  )

met <- c("fe", "nfe")
c3 <- dec |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(
    `Population` = sum(k * d_pop),
    `GDP per capita` = sum(k * d_gpc),
    `Biomass per GDP` = sum((k * d_int)[grp == "bio"]),
    `Fossil fuels per GDP` = sum((k * d_int)[grp == "fos"]),
    `Metal stock per GDP` = sum((k * d_sg)[grp %in% met]),
    `Mineral stock per GDP` = sum((k * d_sg)[grp == "min"]),
    `Metal inflow / stock` = sum((k * d_turn)[grp %in% met]),
    `Mineral inflow / stock` = sum((k * d_turn)[grp == "min"]),
    `Metal recycling` = sum((k * d_sec)[grp %in% met]),
    `Mineral downcycling` = sum((k * d_sec)[grp == "min"]),
    `Fe ore grade` = sum((k * d_ore)[grp == "fe"]),
    `Non-Fe ore grade` = sum((k * d_ore)[grp == "nfe"]),
    Total = 100 / T_YRS * log(dplyr::first(Mtot_b) / dplyr::first(Mtot_a)),
    .groups = "drop"
  )
# Log growth -> CAGR: every term of a run scaled by CAGR / log rate, so the
# terms still sum exactly to the total, now the run's CAGR = exp(log rate) - 1
c3 <- c3 |>
  dplyr::mutate(
    cagr_scale = dplyr::if_else(abs(Total) < 1e-12, 1, (exp(Total / 100) - 1) / (Total / 100)),
    dplyr::across(-c(run_id, cagr_scale), \(v) v * cagr_scale)
  ) |>
  dplyr::select(-cagr_scale)
# Inflow / stock (yearly inflow as % of in-use stock) and stock growth, median across runs
cat("  Inflow / stock and stock growth (median across runs, %/yr):\n")
print(
  dec |>
    dplyr::filter(grp %in% c("fe", "nfe", "min")) |>
    dplyr::mutate(grp = dplyr::if_else(grp == "min", "minerals", "metals")) |>
    dplyr::group_by(run_id, grp) |>
    dplyr::summarise(In_a = sum(In_a), In_b = sum(In_b), S_a = sum(S_a), S_b = sum(S_b), .groups = "drop") |>
    dplyr::group_by(grp) |>
    dplyr::summarise(
      inflow_per_stock_2025 = round(100 * stats::median(In_a / S_a), 2),
      inflow_per_stock_2060 = round(100 * stats::median(In_b / S_b), 2),
      stock_growth_2025_2060 = round(100 * stats::median((S_b / S_a)^(1 / T_YRS) - 1), 2), # CAGR
      .groups = "drop"
    ) |>
    as.data.frame()
)
resid <- c3$Total - rowSums(c3 |> dplyr::select(-run_id, -Total))
cat(sprintf("  Identity check: max |sum of terms - total| = %.2e %%/yr\n", max(abs(resid))))
c3_long <- c3 |> tidyr::pivot_longer(-run_id, names_to = "term", values_to = "pct_yr")
print(
  c3_long |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      p05 = quantile(pct_yr, .05),
      median = median(pct_yr),
      p95 = quantile(pct_yr, .95),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(-term, ~ round(.x, 2))) |>
    as.data.frame()
)
cat(sprintf(
  "  Sum of term medians = %.2f vs median total = %.2f %%/yr\n",
  sum(
    c3_long |>
      dplyr::filter(term != "Total") |>
      dplyr::group_by(term) |>
      dplyr::summarise(m = median(pct_yr)) |>
      dplyr::pull(m)
  ),
  median(c3$Total)
))


# STEP 2: S22 -- GDP elasticity per material group --------------------------------

cat("STEP 2: S22 -- GDP elasticity per material group\n")

grp_cagr <- results_fig |>
  dplyr::mutate(
    group = dplyr::recode(
      material_group,
      biomass = "Biomass",
      fossil_fuels = "Fossil fuels",
      metal_fe = "Metal ores Fe",
      metal_nonfe = "Metal ores NonFe",
      nonmetallic_minerals = "Non-metallic minerals"
    )
  ) |>
  dplyr::group_by(run_id, group, year) |>
  dplyr::summarise(M = sum(primary_consumption_Mt, na.rm = TRUE), .groups = "drop")
grp_cagr <- dplyr::bind_rows(
  grp_cagr,
  grp_cagr |>
    dplyr::group_by(run_id, year) |>
    dplyr::summarise(M = sum(M), .groups = "drop") |>
    dplyr::mutate(group = "Total")
) |>
  tidyr::pivot_wider(names_from = year, values_from = M, names_prefix = "M") |>
  dplyr::mutate(cagr = (.data[[paste0("M", FORECAST_END)]] / M2025)^(1 / T_YRS) - 1) |>
  dplyr::select(run_id, group, cagr)

s22 <- tibble::tibble(group = sort(unique(grp_cagr$group)), elasticity = NA_real_, r2 = NA_real_, n = NA_integer_)
for (j in seq_len(nrow(s22))) {
  df <- grp_cagr |>
    dplyr::filter(group == s22$group[j]) |>
    dplyr::inner_join(growth_run |> dplyr::select(run_id, gdp_cagr), by = "run_id") |>
    dplyr::inner_join(R |> dplyr::select(run_id, dplyr::all_of(setdiff(feature_cols, "ssp_u"))), by = "run_id")
  m <- lm(cagr ~ ., data = df |> dplyr::select(-run_id, -group))
  s22$elasticity[j] <- unname(coef(m)["gdp_cagr"])
  s22$r2[j] <- summary(m)$r.squared
  s22$n[j] <- nrow(df)
}
print(s22 |> dplyr::mutate(dplyr::across(c(elasticity, r2), ~ round(.x, 3))) |> as.data.frame())


# STEP 3: Save -------------------------------------------------------------------

readr::write_csv(c3_long, "Parameters/Intermediate/Figure5_Alt1_Decomposition.csv")
readr::write_csv(s22, "Parameters/Intermediate/FigureS22_GDPElasticity.csv")
cat("\n  Saved: Figure5_Alt1_Decomposition.csv, FigureS22_GDPElasticity.csv\n")
cat("=== Figure 5 - Alt1 - Prepare Data done ===\n")

# EoF
