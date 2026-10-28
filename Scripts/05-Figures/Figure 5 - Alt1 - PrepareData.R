## =============================================================================
## Figure 5 - Alt1 - PrepareData.R  (run for "Figure 5 - Alt1.R")
## Data for Figure 5 alternative 1 (panel c = growth decomposition; panels a, b,
## d unchanged) and SI S22. Sources "Figure 5 - Sensitivity - PrepareData.R"
## for its objects (growth_df, real_matrix, input_matrix, feature_cols,
## world_by_run, results_fig) -- that script's own outputs are rewritten unchanged.
##
## Panel c: per run, additive (LMDI) decomposition of total material growth,
##   ln(M_2060 / M_2025) / T, in percentage points of annual growth (%/yr):
##   M = sum_g M_g over g = biomass, fossil, Fe, Cu, Al, other non-Fe, gold, minerals;
##   ln(M_60/M_25) = sum_g w_g ln(M_g,60 / M_g,25),
##   w_g = L(M_g,60, M_g,25) / L(M_60, M_25), L(a,b) = (a - b) / (ln a - ln b);
##   flows    (biomass, fossil): M_g = P * (G/P) * (M_g/G)
##   gold ore (flow):            M_g = P * (G/P) * (M_g/G)
##   metals   (Fe, Cu, Al, other non-Fe each):  Ore = P * (G/P) * (S/G) * (Out/S) * (In/Out) * (Prim/In) * (Ore/Prim)
##   minerals:                   Prim = P * (G/P) * (S/G) * (Out/S) * (In/Out) * (PrimU/In) * (Prim/PrimU)
##   S in-use stock, In inflow, Out outflow (end-of-life waste), Prim primary
##   (= inflow - secondary), metals in metal mass; Ore = ore extracted.
##   Out/S = lifetime retirement rate; In/Out = stock growth (In/Out = 1 + dS/Out).
##   Minerals: PrimU = In - (secondary + spilled surplus) = primary if every
##   sector could absorb all downcycled material, so PrimU/In = downcycling rate
##   and Prim/PrimU = absorption caps (max secondary share x concrete / granular
##   shares). Each metal is a separate group, so the ore terms (Fe; non-Fe = Cu +
##   Al + other) are the change in ore grade only (not the metal mix). Gold's
##   M/G change (regional mix only) is counted in "Metal stock per GDP".
##   Each log change times w_g is one term; the terms sum exactly to the total.
##   Terms are then rescaled per run by CAGR / log rate, so they sum to the
##   run's CAGR (compound annual growth rate) instead of its log growth rate.
## S25: the same decomposition within each material group (its own total growth),
##   with intensity split by sub-material (biomass, fossil) or S/G by end use
##   (metals, minerals).
## S22: GDP elasticity per material group: lm(CAGR_g ~ gdp_cagr + other levers,
##   real units, ssp_u excluded); slope on gdp_cagr.
##
## Output (Parameters/Intermediate/):
##   Figure5_Alt1_Decomposition.csv (run_id, term, pct_yr),
##   FigureS25_DecompositionByMaterial.csv (run_id, material, term, pct_yr),
##   FigureS22_GDPElasticity.csv
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
    in_use_stock_Mt,
    waste_Mt_pure,
    secondary_supply_Mt_pure,
    secondary_spilled_Mt
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    grp = dplyr::case_when(
      material_group == "biomass" ~ "bio",
      material_group == "fossil_fuels" ~ "fos",
      material_group == "metal_fe" ~ "fe",
      material_group == "metal_cu" ~ "cu",
      material_group == "metal_al" ~ "al",
      material_group == "metal_othernonfe" ~ "oth",
      material_group == "metal_gold" ~ "au", # flow: fixed regional intensity per GDP
      material_group == "nonmetallic_minerals" ~ "min"
    )
  ) |>
  dplyr::group_by(run_id, grp, year) |>
  dplyr::summarise(
    M = sum(primary_consumption_Mt, na.rm = TRUE), # metals: ore
    Prim = sum(primary_consumption_Mt_pure, na.rm = TRUE), # metals: metal mass
    In = sum(total_inflow_Mt, na.rm = TRUE),
    S = sum(in_use_stock_Mt, na.rm = TRUE),
    Out = sum(waste_Mt_pure, na.rm = TRUE), # end-of-life outflow (metals: metal mass)
    Pool = sum(secondary_supply_Mt_pure + secondary_spilled_Mt, na.rm = TRUE), # minerals: downcycled pool before absorption caps
    .groups = "drop"
  ) |>
  dplyr::left_join(world_by_run, by = c("run_id", "year")) |>
  dplyr::mutate(yr = dplyr::if_else(year == 2025L, "a", "b")) |>
  dplyr::select(-year) |>
  tidyr::pivot_wider(names_from = yr, values_from = c(M, Prim, In, S, Out, Pool, world_pop, world_gdp))

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
    d_ret = log((Out_b / S_b) / (Out_a / S_a)), # stocks: outflow / stock (lifetime retirement rate)
    d_sgr = log((In_b / Out_b) / (In_a / Out_a)), # stocks: inflow / outflow (stock growth)
    d_sec = log((Prim_b / In_b) / (Prim_a / In_a)), # stocks: primary share of inflow (1 - secondary share)
    d_ore = log((M_b / Prim_b) / (M_a / Prim_a)), # Fe, NonFe: ore per t metal (1 / grade)
    # Minerals: d_sec = downcycling rate (uncapped primary PrimU / In) + absorption caps (Prim / PrimU)
    PrimU_a = In_a - Pool_a,
    PrimU_b = In_b - Pool_b,
    cap_ok = grp == "min" & PrimU_a > 0 & PrimU_b > 0,
    d_drate = dplyr::if_else(cap_ok, log((PrimU_b / In_b) / (PrimU_a / In_a)), d_sec),
    d_cap = d_sec - d_drate
  )
# Guard: runs where the uncapped primary would be <= 0 keep the whole d_sec in the downcycling-rate term
cat(sprintf(
  "  Minerals runs without a downcycling rate / caps split (uncapped primary <= 0): %d\n",
  sum(dec$grp == "min" & !dec$cap_ok)
))

met <- c("fe", "cu", "al", "oth") # stock metals
nfe <- c("cu", "al", "oth")
c3 <- dec |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(
    `Population` = sum(k * d_pop),
    `GDP per capita` = sum(k * d_gpc),
    `Biomass per GDP` = sum((k * d_int)[grp == "bio"]),
    `Fossil fuels per GDP` = sum((k * d_int)[grp == "fos"]),
    # gold (a flow) only changes its world intensity through the regional mix;
    # that small term is counted with metal stock per GDP
    `Metal stock per GDP` = sum((k * d_sg)[grp %in% met]) + sum((k * d_int)[grp == "au"]),
    `Mineral stock per GDP` = sum((k * d_sg)[grp == "min"]),
    `Metal lifetime retirement rate` = sum((k * d_ret)[grp %in% met]),
    `Metal stock growth` = sum((k * d_sgr)[grp %in% met]),
    `Mineral lifetime retirement rate` = sum((k * d_ret)[grp == "min"]),
    `Mineral stock growth` = sum((k * d_sgr)[grp == "min"]),
    `Ferrous recycling` = sum((k * d_sec)[grp == "fe"]),
    `Non-ferrous recycling` = sum((k * d_sec)[grp %in% nfe]),
    `Mineral downcycling rate` = sum((k * d_drate)[grp == "min"]),
    `Mineral absorption caps` = sum((k * d_cap)[grp == "min"]),
    `Fe ore grade` = sum((k * d_ore)[grp == "fe"]),
    `Non-Fe ore grade` = sum((k * d_ore)[grp %in% nfe]),
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
    dplyr::filter(grp %in% c(met, "min")) |>
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


# STEP 1b: S25 -- LMDI growth decomposition within each material group -----------
# Same identities as STEP 1, but the total is each material group's own primary
# consumption (biomass, fossil fuels, metal ores = Fe + Cu + Al + other + gold ore, minerals):
#   biomass / fossil: one component per sub-material c, M_c = P (G/P) (M_c/G)
#   gold ore: a flow component (M/G: Gold) within metal ores
#   metals / minerals: one component per metal type t (Fe, Cu, Al, other; minerals = 1)
#     with the STEP 1 factors; S/G further split by end use e via LMDI on the
#     sum S_t = sum_e S_t,e:  dln(S_t/G) = sum_e [L(S_t,e) / L(S_t)] dln(S_t,e/G)
# Component weights k = L(M_c) / L(M_material) x 100 / T; terms rescaled per run
# and material by CAGR / log rate, so they sum to that material's CAGR.

cat("STEP 1b: S25 -- growth decomposition within each material group\n")

ENDUSE_OF_KEY <- c(
  "Residential" = "Buildings", "Non-residential" = "Buildings",
  "Roads" = "Civil infrastructure", "Civil engineering" = "Civil infrastructure",
  "Machinery" = "Machinery", "Vehicles" = "Machinery",
  "Durables" = "Short-lived products", "Packaging" = "Short-lived products"
)

mat_raw <- arrow::open_dataset("Results/MC/mc_results.parquet") |>
  dplyr::filter(year %in% c(2025L, FORECAST_END)) |>
  dplyr::select(
    run_id, material_group, material_key, year,
    primary_consumption_Mt, primary_consumption_Mt_pure, total_inflow_Mt, in_use_stock_Mt,
    waste_Mt_pure, secondary_supply_Mt_pure, secondary_spilled_Mt
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    material = dplyr::case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group %in% METAL_GROUPS_ALL ~ "Metal ores",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals"
    ),
    end_use = dplyr::if_else(startsWith(material_key, "Power: "), "Power sector", unname(ENDUSE_OF_KEY[material_key]))
  ) |>
  dplyr::left_join(world_by_run, by = c("run_id", "year")) |>
  dplyr::mutate(yr = dplyr::if_else(year == 2025L, "a", "b")) |>
  dplyr::select(-year)

# Flow components (biomass, fossil; gold ore within metal ores): one per sub-material
flow_c <- mat_raw |>
  dplyr::filter(material %in% c("Biomass", "Fossil fuels") | material_group == "metal_gold") |>
  dplyr::group_by(run_id, material, comp = material_key, yr) |>
  dplyr::summarise(M = sum(primary_consumption_Mt, na.rm = TRUE), pop = dplyr::first(world_pop), gdp = dplyr::first(world_gdp), .groups = "drop") |>
  tidyr::pivot_wider(names_from = yr, values_from = c(M, pop, gdp)) |>
  dplyr::filter(M_a > 0, M_b > 0)

# Stock components (metals, minerals): one per metal type, all end uses pooled
stock_t <- mat_raw |>
  dplyr::filter(material %in% c("Metal ores", "Non-metallic minerals"), material_group != "metal_gold") |>
  dplyr::group_by(run_id, material, comp = material_group, yr) |>
  dplyr::summarise(
    M = sum(primary_consumption_Mt, na.rm = TRUE), # metals: ore
    Prim = sum(primary_consumption_Mt_pure, na.rm = TRUE),
    In = sum(total_inflow_Mt, na.rm = TRUE),
    S = sum(in_use_stock_Mt, na.rm = TRUE),
    Out = sum(waste_Mt_pure, na.rm = TRUE),
    Pool = sum(secondary_supply_Mt_pure + secondary_spilled_Mt, na.rm = TRUE),
    pop = dplyr::first(world_pop),
    gdp = dplyr::first(world_gdp),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(names_from = yr, values_from = c(M, Prim, In, S, Out, Pool, pop, gdp))

# Stock by metal type x end use (for the S/G split)
stock_te <- mat_raw |>
  dplyr::filter(material %in% c("Metal ores", "Non-metallic minerals"), material_group != "metal_gold") |>
  dplyr::group_by(run_id, material, comp = material_group, end_use, yr) |>
  dplyr::summarise(S = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = yr, values_from = S, names_prefix = "S_") |>
  dplyr::filter(S_a > 0, S_b > 0)

# Component weights within each material
mat_comp <- dplyr::bind_rows(
  flow_c |> dplyr::select(run_id, material, comp, M_a, M_b),
  stock_t |> dplyr::select(run_id, material, comp, M_a, M_b)
) |>
  dplyr::group_by(run_id, material) |>
  dplyr::mutate(Mtot_a = sum(M_a), Mtot_b = sum(M_b)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    L_c = dplyr::if_else(abs(M_b - M_a) < 1e-12, M_a, (M_b - M_a) / (log(M_b) - log(M_a))),
    L_tot = dplyr::if_else(abs(Mtot_b - Mtot_a) < 1e-12, Mtot_a, (Mtot_b - Mtot_a) / (log(Mtot_b) - log(Mtot_a))),
    k = L_c / L_tot * 100 / T_YRS
  ) |>
  dplyr::select(run_id, material, comp, k, Mtot_a, Mtot_b)

flow_terms <- flow_c |>
  dplyr::inner_join(mat_comp |> dplyr::select(run_id, material, comp, k), by = c("run_id", "material", "comp")) |>
  dplyr::transmute(
    run_id, material, comp,
    Population = k * log(pop_b / pop_a),
    `GDP per capita` = k * log((gdp_b / pop_b) / (gdp_a / pop_a)),
    int = k * log((M_b / gdp_b) / (M_a / gdp_a))
  ) |>
  tidyr::pivot_longer(c(Population, `GDP per capita`, int), names_to = "term", values_to = "v") |>
  dplyr::mutate(term = dplyr::if_else(term == "int", paste0("M/G: ", comp), term)) |>
  dplyr::select(run_id, material, term, v)

stock_terms <- stock_t |>
  dplyr::inner_join(mat_comp |> dplyr::select(run_id, material, comp, k), by = c("run_id", "material", "comp")) |>
  dplyr::mutate(
    PrimU_a = In_a - Pool_a,
    PrimU_b = In_b - Pool_b,
    cap_ok = material == "Non-metallic minerals" & PrimU_a > 0 & PrimU_b > 0,
    d_sec = log((Prim_b / In_b) / (Prim_a / In_a)),
    d_drate = dplyr::if_else(cap_ok, log((PrimU_b / In_b) / (PrimU_a / In_a)), d_sec)
  ) |>
  dplyr::transmute(
    run_id, material, comp,
    Population = k * log(pop_b / pop_a),
    `GDP per capita` = k * log((gdp_b / pop_b) / (gdp_a / pop_a)),
    `Lifetime retirement rate` = k * log((Out_b / S_b) / (Out_a / S_a)),
    `Stock growth` = k * log((In_b / Out_b) / (In_a / Out_a)),
    rec = k * d_drate,
    `Absorption caps` = k * (d_sec - d_drate),
    grade = k * log((M_b / Prim_b) / (M_a / Prim_a))
  ) |>
  tidyr::pivot_longer(-c(run_id, material, comp), names_to = "term", values_to = "v") |>
  dplyr::mutate(
    term = dplyr::case_when(
      term == "rec" & comp == "metal_fe" ~ "Ferrous recycling",
      term == "rec" & comp == "metal_cu" ~ "Cu recycling",
      term == "rec" & comp == "metal_al" ~ "Al recycling",
      term == "rec" & comp == "metal_othernonfe" ~ "Other non-Fe recycling",
      term == "rec" ~ "Downcycling rate",
      term == "grade" & comp == "metal_fe" ~ "Ferrous ore grade",
      term == "grade" & comp == "metal_cu" ~ "Cu ore grade",
      term == "grade" & comp == "metal_al" ~ "Al ore grade",
      term == "grade" & comp == "metal_othernonfe" ~ "Other non-Fe ore grade",
      TRUE ~ term
    )
  ) |>
  # metals have no caps term; minerals have no ore-grade term (both identically 0);
  # minerals' caps term is replaced by its end-use split (caps_terms below)
  dplyr::filter(term != "Absorption caps", term != "grade") |>
  dplyr::select(run_id, material, term, v)

sg_terms <- stock_te |>
  dplyr::inner_join(
    stock_t |> dplyr::select(run_id, material, comp, St_a = S_a, St_b = S_b, gdp_a, gdp_b),
    by = c("run_id", "material", "comp")
  ) |>
  dplyr::inner_join(mat_comp |> dplyr::select(run_id, material, comp, k), by = c("run_id", "material", "comp")) |>
  dplyr::mutate(
    L_e = dplyr::if_else(abs(S_b - S_a) < 1e-12, S_a, (S_b - S_a) / (log(S_b) - log(S_a))),
    L_t = dplyr::if_else(abs(St_b - St_a) < 1e-12, St_a, (St_b - St_a) / (log(St_b) - log(St_a)))
  ) |>
  # Stock part: sum_e L(S_e)/L(S_t) dln(S_e) = dln(S_t) exactly; GDP part allocated with
  # normalised weights L(S_e) / sum_e L(S_e) (the L(S_e)/L(S_t) weights do not sum to 1)
  dplyr::group_by(run_id, material, comp) |>
  dplyr::mutate(L_sum = sum(L_e)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    v = k * (L_e / L_t * log(S_b / S_a) - L_e / L_sum * log(gdp_b / gdp_a)),
    term = paste0("S/G: ", end_use)
  ) |>
  dplyr::select(run_id, material, term, v)

# Minerals absorption caps by end use: caps = dln(Prim) - dln(PrimU), PrimU = In - Pool.
# Additive LMDI split, exact: dln(X) = sum_e (X_e,b - X_e,a) / L(X) for X = Prim, PrimU
caps_terms <- mat_raw |>
  dplyr::filter(material == "Non-metallic minerals") |>
  dplyr::group_by(run_id, material, comp = material_group, end_use, yr) |>
  dplyr::summarise(
    Prim_e = sum(primary_consumption_Mt_pure, na.rm = TRUE),
    PrimU_e = sum(total_inflow_Mt, na.rm = TRUE) - sum(secondary_supply_Mt_pure + secondary_spilled_Mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(names_from = yr, values_from = c(Prim_e, PrimU_e), values_fill = 0) |>
  dplyr::inner_join(
    stock_t |> dplyr::transmute(run_id, material, comp, Prim_a, Prim_b, PrimU_a = In_a - Pool_a, PrimU_b = In_b - Pool_b),
    by = c("run_id", "material", "comp")
  ) |>
  dplyr::inner_join(mat_comp |> dplyr::select(run_id, material, comp, k), by = c("run_id", "material", "comp")) |>
  dplyr::mutate(
    cap_ok = PrimU_a > 0 & PrimU_b > 0,
    L_prim = dplyr::if_else(abs(Prim_b - Prim_a) < 1e-12, Prim_a, (Prim_b - Prim_a) / (log(Prim_b) - log(Prim_a))),
    L_primu = dplyr::if_else(abs(PrimU_b - PrimU_a) < 1e-12, PrimU_a, (PrimU_b - PrimU_a) / (log(PrimU_b) - log(PrimU_a))),
    v = dplyr::if_else(cap_ok, k * ((Prim_e_b - Prim_e_a) / L_prim - (PrimU_e_b - PrimU_e_a) / L_primu), 0),
    term = paste0("Absorption cap, ", tolower(dplyr::recode(end_use, "Civil infrastructure" = "civil")))
  ) |>
  dplyr::select(run_id, material, term, v)

s25 <- dplyr::bind_rows(flow_terms, stock_terms, sg_terms, caps_terms) |>
  dplyr::group_by(run_id, material, term) |>
  dplyr::summarise(pct_yr = sum(v), .groups = "drop") |>
  dplyr::bind_rows(
    mat_comp |>
      dplyr::distinct(run_id, material, Mtot_a, Mtot_b) |>
      dplyr::transmute(run_id, material, term = "Total", pct_yr = 100 / T_YRS * log(Mtot_b / Mtot_a))
  )

# Log growth -> CAGR, per run x material (as in STEP 1)
s25 <- s25 |>
  dplyr::group_by(run_id, material) |>
  dplyr::mutate(
    tot = pct_yr[term == "Total"],
    pct_yr = pct_yr * dplyr::if_else(abs(tot) < 1e-12, 1, (exp(tot / 100) - 1) / (tot / 100))
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-tot)

cat(sprintf(
  "  Minerals runs without a downcycling rate / caps split: %d\n",
  sum(stock_t$material == "Non-metallic minerals" & !(stock_t$In_a - stock_t$Pool_a > 0 & stock_t$In_b - stock_t$Pool_b > 0))
))
resid25 <- s25 |>
  dplyr::group_by(run_id, material) |>
  dplyr::summarise(r = sum(pct_yr[term != "Total"]) - pct_yr[term == "Total"], .groups = "drop")
cat(sprintf("  S25 identity check: max |sum of terms - total| = %.2e %%/yr\n", max(abs(resid25$r))))
print(
  s25 |>
    dplyr::group_by(material, term) |>
    dplyr::summarise(median = round(stats::median(pct_yr), 2), .groups = "drop") |>
    as.data.frame()
)


# STEP 2: S22 -- GDP elasticity per material group --------------------------------

cat("STEP 2: S22 -- GDP elasticity per material group\n")

grp_cagr <- results_fig |>
  dplyr::mutate(
    group = dplyr::recode(
      material_group,
      biomass = "Biomass",
      fossil_fuels = "Fossil fuels",
      metal_fe = "Metal ores Fe",
      metal_cu = "Metal ores NonFe",
      metal_al = "Metal ores NonFe",
      metal_othernonfe = "Metal ores NonFe",
      metal_gold = "Metal ores NonFe", # gold ore is a non-ferrous ore (UNEP)
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
readr::write_csv(s25, "Parameters/Intermediate/FigureS25_DecompositionByMaterial.csv")
readr::write_csv(s22, "Parameters/Intermediate/FigureS22_GDPElasticity.csv")
cat("\n  Saved: Figure5_Alt1_Decomposition.csv, FigureS25_DecompositionByMaterial.csv, FigureS22_GDPElasticity.csv\n")
cat("=== Figure 5 - Alt1 - Prepare Data done ===\n")

# EoF
