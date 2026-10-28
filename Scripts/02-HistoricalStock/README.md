# 02-HistoricalStock

Builds the historical (1970-2024) in-use stock of metals and non-metallic minerals from UNEP material flows, using MISO2 stocks for calibration. The simulation's stock-flow model needs these stocks and their age profiles by material, end use and region.

UNEP domestic material consumption (DMC) is split into end uses and sub-uses, and ore is converted to metal. Metal inflows are corrected to MISO2 scope. A Weibull dynamic stock model (DSM) is then run and calibrated to MISO2 stocks in 2016. Biomass and fossil fuels are not modelled here: they are flows, not stocks.

**Metals**: `Metal_Fe`, and non-ferrous metal split into `Metal_Cu`, `Metal_Al` and `Metal_OtherNonFe`. Gold ore is removed from the UNEP DMC (`01-PrepareData/07`) and modelled as a flow in 04-Simulation.

**Data-source tags** used below:
- **[UNEP]**: UNEP IRP DMC (gold excluded: `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`)
- **[MISO2]**: MISO2 regional stocks and flows, 4 end uses (`01-PrepareData/04`)
- **[W2024]**: Wiedenhofer et al. (2024) Supporting Information, global sector detail from the MISO2 model
- **[A]**: project assumption

Run order by number. Each script reads files written by earlier ones.

## `01-UNEP_enduse_shares.R`: material use by end use

1. Maps the 22 UNEP categories to 4 groups and keeps metal ores and non-metallic minerals only. Mixed and Waste are mapped but then dropped by the scope filter.
2. End-use shares = [MISO2] gross additions of each end use / total, per region × material × year. Stops if the shares do not sum to 1.
3. Holds the 2016 shares fixed for 2017-2024.
4. [UNEP] DMC × share → flow by end use. Checks that the sum over end uses equals the original DMC.
5. Prints region × material pairs where UNEP and MISO2 average flows (1970-2016) differ by more than about 30%.
6. Plots the end-use shares (world and by region).

- **Inputs**: `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`, `Parameters/MISO-Stock/MISO_flows_regional.csv`.
- **Outputs**: `Parameters/Intermediate/UNEP_flows_enduse.csv` (Region, material, end_use, year, flow_Mt); figures S03, S04.

## `02-UNEP_enduse_detail.R`: 4 end uses → 8 sub-uses

1. Reads the [W2024] global stocks by sector (1900-2016, sheets 3a/3c) and groups them into 8 sub-uses, each with a fixed mean lifetime:
   - From sheet 3a: residential (80 yr), non-residential (50), roads (33), civil engineering (120).
   - From sheet 3c: machinery & equipment (25; machinery, electrical equipment, other transport), vehicles (14; motor vehicles, computers), durables (12; furniture, printed matter, products nec), packaging (0.5; food packaging).
2. Reads the [W2024] 2016 material composition of each sub-use stock (sheets 3b/3d):
   - metal fraction = Metals / total
   - mineral fraction = (aggregates + asphalt + bricks + concrete + glass) / total (sheet 3d: glass only)
3. Recovers sub-use inflows from the [W2024] stock series by inversion, for CV = 0.2, 0.3 and 0.5:
   - `I(t) = max(0, S(t) − Σ I(k) · ℓ(t − k))`, with ℓ = lognormal survival.
   - `σ² = ln(1 + CV²)`; CV = 0.3 is the main case.
   - **Why invert**: [W2024] reports stocks by sub-use but no inflows, and [MISO2] reports inflows only for the 4 end uses. Inflow shares are needed rather than stock shares, because stock ≈ inflow × lifetime over-weights long-lived sub-uses.
   - The inversion uses a lognormal lifetime with the fixed means above. It is separate from the Weibull lifetimes used in `05`.
4. Prints diagnostics: inversion shares vs ΔS shares vs S/τ shares, and shares for each CV (1990-2010 means).
5. Sub-use inflow shares:
   - share = [W2024] inverted inflow × [W2024] 2016 material fraction of the sub-use, normalised within end use × material × year.
   - These are global shares, applied to every region.
   - They are held at 2016 values to 2024 and renormalised to sum to 1.
6. [UNEP] end-use flow (from `01`) × share → sub-use flow. Cells with zero share (e.g. metal in roads) are dropped. Checks that sub-uses sum back to the end-use flow.
7. Plots the shares per end use and the CV sensitivity.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_enduse.csv`, `Inputs/MISO/SI_Wiedenhofer2024_globalStocks.xlsx`.
- **Outputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv` (year, Region, material, super_category, sub_use, mean_life, inflow_share, flow_Mt); figures in `Figures/Inputs/MISO/01b-*.png`.

## `03-OreGrade_Factor_g.R`: ore-to-metal factor g

MISO2 reports stocks as pure metal, while UNEP reports metal ores. This script estimates g, the factor that converts ore to metal (t metal per t ore), for ferrous and non-ferrous ores.

Recycled metal is removed from [MISO2] metal inflows, and the rest is divided by [UNEP] ore:

`g_Fe(t) = [share_Fe · M(t) − rr_Fe · Out(t−1)] / Ore_Fe(t)`
`g_NonFe(t) = [(1 − share_Fe) · M(t) − rr_NonFe · Out(t−1)] / Ore_NonFe(t)`

- M = [MISO2] total metal inflow (gross additions to stock).
- Out = [MISO2] total metal end-of-life outflow, lagged one year.
- Ore = [UNEP] DMC of ferrous or non-ferrous ores (non-ferrous without gold).
- share_Fe = 0.9: the ferrous share of [MISO2] total metal inflow.
- rr = the fraction of the previous year's [MISO2] total metal outflow that returns as secondary metal inflow; rr_Fe = 0.30, rr_NonFe = 0.10.

**About these values**:
- share_Fe, rr_Fe and rr_NonFe are [A] calibration values, hard-coded in the script with no external source.
- The MISO2 data has no metal detail, so it cannot check them. The model's 2024 metal inflow (about 90% Fe by mass) is consistent with share_Fe.
- rr is not the end-of-life recycling rate of `Recycling_Assumptions.xlsx` (the recovered share of each metal's scrap), so the values differ.

**What g affects**:
- the Fe / non-Fe split of historical metal;
- the 2016 starting point of the ramp to the 2024 grades (`05`).

Total metal is rescaled to MISO2 by A (`04`) and calibrated by λ (`05`), and projections start from the 2024 grades. Totals therefore do not depend on these values; the Fe / non-Fe split does.

1. Sums [MISO2] metal inflow and outflow, and [UNEP] Fe and non-Fe ore DMC, by region and world.
2. Computes global g_Fe and g_NonFe for every year where MISO2 and UNEP overlap. Flags values outside [0, 1].
3. Sensitivity: the same calculation with rr from 0 to 0.6.
4. Regional g, for diagnostics only. Values above 1 are expected for ore exporters, because UNEP counts extraction while MISO2 counts use.
5. Saves the global series. The first year (no lagged outflow) is filled from the next year.

- **Inputs**: `Parameters/MISO-Stock/MISO_flows_regional.csv`, `MISO_outflows_regional.csv`, `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`.
- **Outputs**: `Parameters/MISO-Stock/metal_grade_ore.csv` (year, group = Ferrous/Non-ferrous, g); figure S05; `Figures/Inputs/MISO/02-ore_grade_g_{sensitivity,regional}.png`.

## `04-MISO_scope_factor.R`: scope factor A for metal inflows

UNEP DMC covers primary ore only. MISO2 inflows also include recycled scrap and metal embodied in traded goods. Scope factor A closes this gap empirically, without separating recycling from trade.

For region r, end use e and year t (1970-2016):

`A(r, e, t) = M_MISO(r, e, t) / M_UNEP(r, e, t)`

- M_MISO = [MISO2] metal gross additions to stock.
- M_UNEP = Σ over the sub-uses of e of the [UNEP] metal-ore flow × [fe_share(r,t) · g_Fe(t) + (1 − fe_share(r,t)) · g_NonFe(t)].
  - fe_share = regional Fe / (Fe + non-Fe) ore DMC.
  - g is from `03`, held at its first and last observed values outside its range.

1. Computes M_UNEP and M_MISO per region × end use × year.
2. A = M_MISO / M_UNEP. A = 1 when MISO2 data is missing or M_UNEP is near zero. One A per region × end use is shared by all metals, because MISO2 has no metal detail.
3. Smooths A with a centred 5-year mean, clips it to [0.5, 4], and holds the 2016 value for 2017-2024.
4. Checks: number of clipped cells; 2016 A by region; Σ UNEP × A compared with Σ MISO2.

**Why only metals**:
- Recycled scrap and metal in traded goods are a large part of metal use, so UNEP primary ore understates it.
- Non-metallic minerals are mostly used where they are extracted, with little recycling into new products, and the DSM already fits their MISO2 stock through λ (`05`).
- `07` computes a mineral A only to estimate historical secondary flows; it is not applied to mineral inflows.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv`, `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`, `Parameters/MISO-Stock/metal_grade_ore.csv`, `MISO_flows_regional.csv`.
- **Outputs**: `Parameters/Intermediate/miso_unep_scope_factor_A.csv` (Region, super_category, year, A, unep_inflow_Mt, miso_inflow_Mt); `Figures/Stocks/02b-scope_factor_A.png`.

## `05-UNEP_stock_flow_model.R`: historical DSM 1970-2024

Turns [UNEP] sub-use flows into metal and mineral inflows, and runs a Weibull dynamic stock model per region × material × sub-use for 1970-2024. The pre-1970 [MISO2] stock enters as starting cohorts, and the model is calibrated to the [MISO2] 2016 stock. The outputs are the historical stock, inflow and outflow series, plus the 2024 stock and age profile that start the Monte Carlo.

1. **Inputs to the DSM**
   - [UNEP] metal ore flows are split into Fe and non-Fe with fe_share, then converted to metal mass with **g, the ore-to-metal factor from `03`**.
   - **Non-Fe metal is split into `Metal_Cu`, `Metal_Al` and `Metal_OtherNonFe`** with each year's world metal shares:
     - share = world ore DMC of copper, bauxite and other non-ferrous (`01-PrepareData/07`) × the metal's 2024 grade (`GRADE_ORE_CU_2024` = 0.0075, `GRADE_ORE_AL_2024` = 0.165, `GRADE_ORE_OTHERNONFE_2024` = 0.03), normalised.
     - The same shares apply in every region and sub-use. MISO2 has no metal detail, and metal use is far more uniform across regions than mining, because concentrates and metal are traded. Regional mining mixes gave, for example, South Asia almost no copper in use.
     - Per-metal grade g_m(t) = c(t) × the metal's 2024 grade, with c(t) set so that the three metals add up to g_NonFe × non-Fe ore (world ore mix).
   - After 2016 (end of MISO2), g_Fe ramps linearly to `GRADE_ORE_FE_2024` (0.4) and c(t) to 1 in 2024. The per-metal grades therefore reach their 2024 values.
   - Metal flows are multiplied by A from `04` (why minerals are not corrected is explained there).
2. **Cohorts before 1970** [MISO2 stock, annual]:
   - Each year's positive stock increase, S(t) − S(t−1) for 1901-1969, is that year's cohort; the 1900 stock is the 1900 cohort.
   - 1970 is left out because [UNEP] 1970 inflows already enter the DSM.
   - Increases are net of retirements, so early inflows are understated; λ (step 5) rescales them.
   - They are split by sub-use with the 1970 [W2024] shares, and into metals with the region's 1970 Fe share and metal shares.
   - They are kept in metal mass (no g or A).
3. **Simulation input**: flows 1970-2024 (LOCF if UNEP ends earlier), plus [A] sub-use lifetimes (mean, Weibull k) from `MC_Assumptions.xlsx`, sheet `Lifetimes`.
4. **DSM per Region × material × sub-use**:
   - stock(t) = Σ_c [UNEP] inflow(c) × Weibull survival(t − c) + surviving [MISO2] pre-1970 cohorts
   - outflow(t) = stock(t−1) − stock(t) + inflow(t)
5. **Calibration**:
   - λ = [MISO2] stock 2016 / simulated stock 2016, per region × end use, for all metals together and for minerals. All metals get the same λ.
   - Inflows and pre-1970 cohorts are scaled by λ and the DSM is run again.
   - Prints λ values outside [0.7, 1.3] and metal groups still far from 1.
6. **Non-primary share 2024** (step 5b, diagnostic): `s_2024 = 1 − primary metal / inflow` per metal, with primary metal = 2024 ore DMC × 2024 grade.
7. **2024 age profile**: surviving stock of each cohort in 2024, including the pre-1970 cohorts.
8. Plots the stock (S06), stock by end use, and outflows, and prints summaries.

- **Inputs**: `Parameters/Intermediate/UNEP_flows_subenduse.csv`, `miso_unep_scope_factor_A.csv`; `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`, `nonfe_split_region.csv`; `Parameters/MISO-Stock/MISO_stock_regional.csv`, `metal_grade_ore.csv`; `Inputs/MC_Assumptions.xlsx` (`Lifetimes`).
- **Outputs** (Fe, Cu, Al and other non-Fe separate unless stated):
  - `Parameters/Intermediate/stock_trajectory_subenduse.csv`: calibrated stock by sub-use.
  - `Parameters/Intermediate/stock_trajectory_1970_2024.csv`: stock by Region × material × end use, metals merged into "Metal ores".
  - `Parameters/Intermediate/flow_trajectory_subenduse.csv`: calibrated inflow, outflow and ore grade per year.
  - `Parameters/Intermediate/nonprimary_share_2024.csv`.
  - `Parameters/MISO-Stock/stock_2024_age_profile.csv`: surviving stock per cohort in 2024.
  - `Parameters/MISO-Stock/stock_2024_total.csv`: 2024 stock per Region × material × sub-use.
  - Figure S06; `Figures/Stocks/stock_trajectory_endUSE_1970_2024.png`, `outflow_trajectory.png`.

## `06-AgePyramid.R`: 2024 age pyramids (figures only)

1. Bins the 2024 cohorts by age: 5-year classes up to 55, then 55-64, 65-79, 80-99 and 100+.
2. Non-metallic minerals: buildings (left) vs civil infrastructure (right), stacked by region → S07.
3. Machinery (left) vs short-lived products (right) → S08.
4. A third version (one panel per region, buildings vs civil infrastructure) is saved as an exploratory figure, not a supporting figure.

- **Inputs**: `Parameters/MISO-Stock/stock_2024_age_profile.csv`.
- **Outputs**: figures S07, S08; `Figures/Stocks/AgePyramid_2024_Region.png`.

## `07-HistoricalSecondaryFlows.R`: historical secondary flows

No historical series of recycling exists, so one is built from the gap between MISO2 and UNEP inflows.

1. Metal A comes from `04`. For non-metallic minerals, an equivalent A is computed here (same smoothing, clipping and hold).
2. Non-primary inflow = UNEP inflow × (A − 1). Negative values (A < 1, net exporters) are counted and printed.
3. Waste = MISO2 outflows, held at 2016 values for 2017-2024.
4. Implied recycling rate = non-primary inflow / waste, clamped to [0, 1]; secondary = waste × rate.
5. Metal secondary flows are divided by the blended ore grade, fe_share · g_Fe + (1 − fe_share) · g_NonFe. This puts them in ore equivalent, like primary DMC. The blend is consistent with the Cu + Al + other split of `05`: for the world ore mix, those metals add up to g_NonFe.
6. Sums to Region × material group × year and adds primary = UNEP DMC (ores without gold, or non-metallic minerals).
7. Checks: 2024 metal secondary vs `nonprimary_share_2024.csv`; global mineral secondary share.

- **Inputs**: `Parameters/Intermediate/miso_unep_scope_factor_A.csv`, `UNEP_flows_subenduse.csv`, `nonprimary_share_2024.csv`; `Parameters/MaterialFlows/materials_region_DMC_exGold.csv`; `Parameters/MISO-Stock/MISO_flows_regional.csv`, `MISO_outflows_regional.csv`, `metal_grade_ore.csv`.
- **Outputs**: `Parameters/Intermediate/historical_secondary_flows.csv` (Region, material_group, year, primary_Mt, secondary_Mt, waste_Mt, recovered_Mt). Used by Figure 3 (2024 recycling/downcycling rate) and S12.

## `08-stock_intensity_analysis.R`: stock intensity (figures only)

1. Stock intensity = stock (kg) / GDP (PPP, constant 2017 intl $), by region and world.
2. Four breakdowns: total, by material, by end use, and material × end use. Prints the world intensity in 1970, 2000 and 2024.
3. Repeats the calculation by material (Fe; non-ferrous = Cu + Al + other; minerals) × end use and × sub-use.

- **Inputs**: `Parameters/Intermediate/stock_trajectory_1970_2024.csv`, `stock_trajectory_subenduse.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`.
- **Outputs**: `Figures/Stocks/stock_intensity_*.png` (6 figures).
