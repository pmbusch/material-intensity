# 02-HistoricalStock

Rebuilds the historical (1970-2024) in-use stock of metals and non-metallic minerals. These are the starting conditions of the Monte Carlo simulation.

UNEP domestic material consumption (DMC) is split into end uses and sub-uses, and ore is converted to metal. Metal inflows are corrected to MISO2 scope. A Weibull dynamic stock model (DSM) is then run and calibrated to MISO2 stocks in 2016. Biomass and fossil fuels are not modelled here: they have no stock.

Run order: `01` → `01b` → `02` → `02b` → `03` → `03b` → `03c` → `04`. Each script reads files written by earlier ones.

## `01_UNEP_enduse_shares.R`: DMC by end use

1. Maps the 22 UNEP categories to 4 groups and keeps metal ores and non-metallic minerals. Mixed and Waste are mapped but then dropped by the scope filter.
2. End-use shares = MISO2 gross additions of each end use / total, per region × material × year. Stops if the shares do not sum to 1.
3. Extends the 2016 shares to 2017-2024 (last observation carried forward, LOCF).
4. DMC × share → flow by end use. Checks that the sum over end uses equals the original DMC.
5. Prints region × material pairs where UNEP and MISO2 average flows (1970-2016) differ by more than about 30%.
6. Plots the end-use shares (world and by region).

- **Inputs**: `Parameters/UNEP-Materials/materials_region_DMC.csv`, `Parameters/MISO-Stock/MISO_flows_regional.csv`.
- **Outputs**: `Parameters/Intermediate/UNEP_flows_enduse.csv` (Region, material, end_use, year, flow_Mt); figures S03, S04.

## `01b_UNEP_enduse_detail.R`: 4 end uses → 8 sub-uses

1. Reads the Wiedenhofer et al. (2024) global stocks by sector (1900-2016, sheets 3a/3c) and groups them into 8 sub-uses, each with a fixed mean lifetime.
2. Reads the 2016 material composition of each sub-use stock (sheets 3b/3d):
   - metal fraction = Metals / total
   - mineral fraction = (aggregates + asphalt + bricks + concrete + glass) / total
3. Recovers inflows from stocks by inversion, for CV = 0.2, 0.3 and 0.5:
   - `I(t) = max(0, S(t) − Σ I(k) · ℓ(t − k))`, with ℓ = lognormal survival.
   - `σ² = ln(1 + CV²)`; CV = 0.3 is the main case.
4. Prints diagnostics: inversion shares vs ΔS shares vs S/τ shares, and shares for each CV (1990-2010 means).
5. Material-specific inflow shares = sub-use inflow × its 2016 material fraction, normalised within super-category × material × year. LOCF to 2024, then normalised again so they sum to exactly 1.
6. UNEP end-use flow × share → sub-use flow. Cells with zero share (e.g. metal in roads) are dropped. Checks that sub-uses sum back to the end-use flow.
7. Plots the shares per super-category and the CV sensitivity.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_enduse.csv`, `Inputs/MISO/SI_Wiedenhofer2024_globalStocks.xlsx`.
- **Outputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv` (year, Region, material, super_category, sub_use, mean_life, inflow_share, flow_Mt); figures in `Figures/Inputs/MISO/01b-*.png`.

## `02_OreGrade_Factor_g.R`: ore-to-metal factor g

Recycled metal is removed from MISO2 metal inflows, and the rest is divided by UNEP ore:

`g(t) = [share · M(t) − rr · Out(t−1)] / Ore(t)`

- M = MISO2 total metal inflow
- Out = MISO2 metal outflow, lagged one year
- Ore = UNEP DMC of ferrous or non-ferrous ores
- share_Fe = 0.9; recycling rates rr_Fe = 0.30, rr_NonFe = 0.10

1. Sums MISO2 metal inflow and outflow, and UNEP Fe and non-Fe ore DMC, by region and world.
2. Computes global g_Fe and g_NonFe for every year where MISO2 and UNEP overlap. Flags values outside [0, 1].
3. Sensitivity: the same calculation with rr from 0 to 0.6.
4. Regional g, for diagnostics only. Values above 1 are expected for ore exporters, because UNEP counts extraction while MISO2 counts use.
5. Saves the global series. The first year (no lagged outflow) is filled from the next year.

- **Inputs**: `Parameters/MISO-Stock/MISO_flows_regional.csv`, `MISO_outflows_regional.csv`, `Parameters/UNEP-Materials/materials_region_DMC.csv`.
- **Outputs**: `Parameters/MISO-Stock/metal_grade_ore.csv` (year, group = Ferrous/Non-ferrous, g); figure S05; `Figures/Inputs/MISO/02-ore_grade_g_{sensitivity,regional}.png`.

## `02b_MISO_scope_factor.R`: scope factor A for metal inflows

UNEP DMC covers primary ore only. MISO2 inflows also include recycled scrap and metal embodied in traded goods. A closes this gap empirically, without separating recycling from trade.

1. UNEP metal inflow in metal mass: sub-use ore flow × [fe_share · g_Fe + (1 − fe_share) · g_NonFe].
   - fe_share = regional Fe / (Fe + NonFe) ore DMC.
   - g is held at its first and last observed values outside its range.
2. MISO2 metal inflow per region × end use × year.
3. A = MISO2 / UNEP, set to 1 when MISO2 data is missing or UNEP is near zero.
4. Smooths A with a centred 5-year mean, clips it to [0.5, 4], and holds the 2016 value for 2017-2024.
5. Checks: number of clipped cells; 2016 A by region; Σ UNEP × A compared with Σ MISO2.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv`, `Parameters/UNEP-Materials/materials_region_DMC.csv`, `Parameters/MISO-Stock/metal_grade_ore.csv`, `MISO_flows_regional.csv`.
- **Outputs**: `Parameters/Intermediate/miso_unep_scope_factor_A.csv` (Region, super_category, year, A, unep_inflow_Mt, miso_inflow_Mt); `Figures/Stocks/02b-scope_factor_A.png`.

## `03_UNEP_stock_flow_model.R`: historical DSM 1970-2024

1. **Inputs to the DSM**
   - Metal ore flows are split into `Metal_Fe` and `Metal_NonFe` (fe_share) and converted to metal with g.
   - g after 2016 ramps linearly to `GRADE_ORE_*_2024` in 2024.
   - Metal flows are multiplied by A (step 1c). Non-metallic minerals are not corrected.
2. **Cohorts before 1970**: positive 5-year increments of the MISO2 stock (1900-1970), split by the 1970 sub-use shares and the 1970 Fe share.
3. **Simulation input**: flows 1970-2024 (LOCF if UNEP ends earlier), plus sub-use lifetimes (mean, Weibull k) from `MC_Assumptions.xlsx`, sheet `Lifetimes`.
4. **DSM per Region × material × sub-use**: stock(t) = Σ inflow × Weibull survival + surviving pre-1970 cohorts; outflow = stock(t−1) − stock(t) + inflow(t).
5. **Calibration**: λ = MISO2 stock 2016 / simulated stock 2016, per Region × material × end use. Inflows and pre-1970 cohorts are scaled by λ and the DSM is run again. Prints λ values outside [0.7, 1.3] and metal groups still far from 1.
6. **Non-primary share 2024** (step 5b): `s_2024 = 1 − primary metal / inflow`, with primary metal = UNEP ore DMC 2024 × g_2024.
7. **2024 age profile**: surviving stock of each cohort in 2024, including the pre-1970 cohorts.
8. Plots the stock (S06), stock by end use, and outflows, and prints summaries.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv`, `miso_unep_scope_factor_A.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/MISO_stock_regional.csv`, `metal_grade_ore.csv`; `Inputs/MC_Assumptions.xlsx` (`Lifetimes`).
- **Outputs**:
  - `Parameters/Intermediate/stock_trajectory_subenduse.csv`: calibrated stock by sub-use, Fe/NonFe separate.
  - `Parameters/Intermediate/stock_trajectory_1970_2024.csv`: stock by Region × material × end use, Fe+NonFe merged.
  - `Parameters/Intermediate/flow_trajectory_subenduse.csv`: calibrated inflow, outflow and ore grade per year.
  - `Parameters/Intermediate/nonprimary_share_2024.csv`.
  - `Parameters/MISO-Stock/stock_2024_age_profile.csv`: surviving stock per cohort in 2024.
  - `Parameters/MISO-Stock/stock_2024_total.csv`: 2024 stock per Region × material × sub-use.
  - Figure S06; `Figures/Stocks/stock_trajectory_endUSE_1970_2024.png`, `outflow_trajectory.png`.

## `03b_AgePyramid.R`: 2024 age pyramids (figures only)

1. Bins the 2024 cohorts into 5-year age classes (55+ grouped).
2. Non-metallic minerals: buildings (left) vs civil infrastructure (right), stacked by region → S07.
3. Machinery (left) vs short-lived products (right) → S08.

A third version (one panel per region) is built but not saved.

- **Inputs**: `Parameters/MISO-Stock/stock_2024_age_profile.csv`.
- **Outputs**: figures S07, S08.

## `03c_HistoricalSecondaryFlows.R`: historical secondary flows

No historical series of recycling exists, so one is built from the gap between MISO2 and UNEP inflows.

1. Metal A comes from `02b`. For non-metallic minerals, an equivalent A is computed here (same smoothing, clipping and hold).
2. Non-primary inflow = UNEP inflow × (A − 1). Negative values (A < 1, net exporters) are counted and printed.
3. Waste = MISO2 outflows, held at 2016 values for 2017-2024.
4. Implied recycling rate = non-primary inflow / waste, clamped to [0, 1]; secondary = waste × rate.
5. Metal secondary flows are divided by the blended ore grade, so they are in ore equivalent like primary DMC.
6. Sums to Region × material group × year and adds primary = UNEP DMC (ores or non-metallic minerals).
7. Checks: 2024 metal secondary vs `nonprimary_share_2024.csv`; global mineral secondary share.

- **Inputs**: `Parameters/Intermediate/miso_unep_scope_factor_A.csv`, `UNEP_flows_subenduse.csv`, `nonprimary_share_2024.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/MISO_flows_regional.csv`, `MISO_outflows_regional.csv`, `metal_grade_ore.csv`.
- **Outputs**: `Parameters/Intermediate/historical_secondary_flows.csv` (Region, material_group, year, primary_Mt, secondary_Mt, waste_Mt, recovered_Mt). Used by Figure 3 (2024 recycling/downcycling rate) and S12.

## `04_stock_intensity_analysis.R`: stock intensity (figures only)

1. Stock intensity = stock (kg) / GDP (2015 USD), by region and world.
2. Four breakdowns: total, by material, by end use, material × end use. Prints the world intensity in 1970, 2000 and 2024.
3. Repeats the calculation by material (Fe, NonFe, minerals) × end use and × sub-use.

- **Inputs**: `Parameters/Intermediate/stock_trajectory_1970_2024.csv`, `stock_trajectory_subenduse.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`.
- **Outputs**: `Figures/Stocks/stock_intensity_*.png` (6 figures).

## Scripts not in the pipeline (`98*`, `99*`)

Exploratory diagnostics. Not called by `00-RunAll.R` and not used by the model.

- `98a_MineralsMetals_Ratio.R`: ratio of mineral to metal inflow in MISO2, by end use.
- `98b_OreGrade_Bridge.R`: solves for the non-ferrous ore factor with a fixed ferrous factor; writes `Parameters/MISO-Stock/phi_nonFe_sweep.csv`.
- `98c_OreIntensity_Decomp.R`: Fe and non-Fe shares of ore DMC by region.
- `98d_OreGrade_Regression.R`: panel regression of ore-to-metal factors; writes `Parameters/MISO-Stock/ore_grade_regression_*` and `phi_trajectories_model_c.csv`.
- `99_ImpliedLifetime_Diagnostic.R`: lifetimes implied by MISO2 inflows and outflows (non-negative deconvolution, Weibull and lognormal fits); writes `Parameters/Intermediate/lifetime_fit_results.csv`.
