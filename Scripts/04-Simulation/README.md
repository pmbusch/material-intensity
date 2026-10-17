# 04-Simulation

Monte Carlo projection of global material consumption, 2025-2060, by region (8), material and end use. Each run combines:

- **Flow materials** (biomass, fossil fuels), Kaya identity: M = M_2024 × population index × GDP-per-capita index × intensity index.
- **Stock materials** (Fe metals, non-Fe metals, non-metallic minerals): target stock = S_2024 × population index × GDP-per-capita index × stock-intensity index. A forward dynamic stock model (DSM) turns the target stock into inflows and end-of-life waste. Waste is recycled (metals) or downcycled (minerals) into secondary supply, and primary = inflow − secondary.
- **Power sector**: generation capacity (13 technologies) and batteries as explicit stocks. They follow the run's fossil-fuel draws and add no new sampled parameters.

Run order: `01-Sampling.R` → `01b-PowerSector.R` → `02-RunSimulations.R` → `03-Diagnostics.R` → `04-Decoupling.R`. `00-Parameters.R` is sourced by all of them.

## `00-Parameters.R`: configuration

No calculations. Holds every model constant and reads the sampling ranges from Excel.

- Years: `BASE_YEAR = 2024`; `TARGET_YEAR = 2050` (default intensity convergence; sampled per run); `SNAPSHOT_YEARS`.
- Run control: `N_RUNS = 1000`, `GLOBAL_SEED`, `SSP_SHARE_TOL`.
- Scalar parameters (`MC_PARAM_NAMES`), read from `Inputs/MC_Assumptions.xlsx` sheet `Parameters` as `<NAME>_MIN/_CENTRAL/_MAX`:
  - recovery: recycling rate Fe and NonFe, downcycling rate, convergence year
  - room for secondary minerals: maximum secondary share (buildings/civil, roads); concrete shares (buildings, civil); aggregate share of concrete; granular share of roads
  - ore grade Fe and NonFe
  - target year of intensity convergence
- Lifetimes: `lifetime_params` (central values, 8 sub-uses) and `LIFETIME_SAMPLE_PARAMS` (min/max), sheet `Lifetimes`.
- `STOCK_GROWTH_BLEND_YRS = 3`: years over which stock growth blends from the 2024 historical rate into the model rate.
- Power sector:
  - 4 lifetime classes (thermal 40 yr; solar & wind 27 yr; hydro & nuclear 10,000 yr, i.e. no retirement before 2060; batteries 15 yr). Each class borrows the age profile of a "donor" sub-use with a similar lifetime.
  - Technology map to Wang (2023) material intensities (`POWER_TECHS`).
  - Battery sizing: GWh = 4 h × 0.1 × (PV + wind GW).
  - Material-to-group maps; 7 t aggregates per t cement.
  - Fossil energy densities (MJ/kg).
  - `POWER_CARVEOUT_SUB_USE = civil_engineering`; `POWER_CALIB_MAX = 5`.

## `01-Sampling.R`: sampling bounds and Latin hypercube draws

1. **Flow-intensity bounds**:
   - Takes the fixed 2060/2025 ratio min/max per R10 region × SSP (`summary_ratio_2060_2025.csv`) for coal, oil, gas, wood, crops and livestock (→ grazed biomass). Keeps SSP1, 2, 3, 5.
   - Weights = share of each model region's 2024 GDP falling in each R10 region (World Bank country GDP, latest year ≤ 2024).
   - Model-region bounds = GDP-weighted mean of the R10 min and of the R10 max, per material × SSP.
2. **Stock-intensity bounds**: ratio min/max per region × material × end use, from `MC_Assumptions.xlsx` sheet `Stock_Bounds`. They do not depend on the SSP.
3. **World bounds for figures**: Σ (2024 GDP share × 2024 intensity × ratio bound), in kg/$. Used only to label figures.
4. **Latin hypercube** (`lhs::randomLHS`, seed `GLOBAL_SEED`): `N_RUNS` × one column in [0, 1] per sampled parameter:
   - `ssp_u`: position between SSPs
   - one intensity draw per material, shared by all regions: crops, grazed biomass, wood, coal, gas, oil; metal stock for 4 end uses; mineral stock for buildings and civil (other biomass, other fossil and mineral machinery/short-lived products stay at 2024 values)
   - the scalar parameters
   - lifetime mean and Weibull k per super-category

- **Inputs**: `Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Inputs/WorldBank/API_NY.GDP.MKTP.KD_*.xls`; `Inputs/Dict_Countries.xlsx` (`R10_Agg`, `UNEP_Agg`); `Inputs/MC_Assumptions.xlsx`; `Inputs/MatIntensity_Assumptions.xlsx`.
- **Outputs** (`Parameters/Simulation/`): `mc_input_matrix.csv`, `flow_ratio_bounds.csv`, `stock_ratio_bounds.csv`, `r10_region_weights.csv` (with `gdp`), `intensity_world_bounds.csv`.

## `01b-PowerSector.R`: power-sector inputs

Fixed inputs that link generation capacity to each run's fossil-fuel draws.

1. **Scenario set**: ScenarioMIP runs (model × family × SSP) that report coal, gas and oil extraction per GDP at R10 level. These are the runs behind the fossil bounds. Prints which capacity variables each run reports.
2. **Capacity paths**: capacity by R10 × technology (missing = 0) and GDP|MER, interpolated linearly from 5-year to annual values (2025-2060). Prints 2025 → 2030 jumps above 1.5×.
3. **R10 → model regions**: GW/GDP intensity = GDP-weighted mean over R10.
   - The 2025 anchor capacity = median 2025 GW across the scenario set per R10, split to model regions by GDP share.
   - Capacity factor (to be multiplied by the run's population and GDP-per-capita indices):
     - ratio form: anchor × I(t) / I(2025)
     - level form: I(t) × scenario 2025 GDP, used when the anchor or I(2025) is zero, or when anchor / scenario's own 2025 GW is outside [1/5, 5]
4. **Fossil index** per scenario × region = Σ over coal/gas/oil of 2024 energy share × the scenario's 2060/2025 extraction-per-GDP ratio.
   - Energy shares come from UNEP DMC × energy density.
   - R10 cells replaced by World in `04_summary_ratio.R` use the scenario's World ratio.
   - World index = regional indices weighted by 2024 fossil energy.
   - Prints the index range of scenarios against the range implied by the sampling bounds.
5. **Warming**: 2060 median global surface temperature of each scenario (MAGICC).
6. **Material intensities**: Wang (2023) t/GW, averaged per technology × material, plus aggregates = 7 × cement. Batteries: mean kg/kWh of LFP and NMC 811, converted to t/GWh. Grouped into Fe, NonFe and non-metallic minerals.
7. **Carve-out**: the 2025 power-sector stock (anchor GW and GWh × t/unit) is subtracted from the 2024 civil-engineering stock. Stops if any stock becomes negative.
8. Saves. Capacity factors are shifted one year earlier (scenario 2025 = model 2024) and the last year is held to 2060.

- **Inputs**: `Parameters/IIASA-Trajectories/metrics_levels_raw.csv`, `ssp_long_clean.csv`, `summary_ratio_2060_2025.csv`; `Parameters/Simulation/r10_region_weights.csv`, `flow_ratio_bounds.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`; climate CSV; `Inputs/Wang2023_material_intensity_technology.xlsx`; `Inputs/Battery_intensity.xlsx`.
- **Outputs** (`Parameters/Simulation/`): `power_scenarios.csv`, `power_scenario_index.csv`, `power_fossil_shares.csv`, `power_capacity_factor.csv`, `power_capacity_anchor.csv`, `power_capacity_check_2025.csv`, `power_material_intensity.csv`, `power_carveout_2025.csv`.

## `02-RunSimulations.R`: Monte Carlo runner

**STEP 1-2. Load draws and 2024 values.** LHS matrix; 2024 intensities by region (`MatIntensity_Assumptions.xlsx`: Biomass, FossilFuels, MetalOres, NonMetallicMinerals); 2024 recycling rates Fe/NonFe (`Recycling_EOL`) and downcycling rates (`Downcycling`) from `Recycling_Assumptions.xlsx`; flow and stock ratio bounds.

**STEP 3. Static data.**
- SSP population and GDP-per-capita indices, as region × year matrices per SSP.
- 2024 UNEP DMC anchors for flow materials (minor categories grouped as "Other biomass" and "Other fossil fuels").
- 2024 stock and age profile per region × material × sub-use, with the power carve-out removed. The 5-year cohorts are spread evenly over single years.
- 2023 → 2024 historical stock growth (for the seam blend).
- Power-sector arrays: capacity factors, anchor, material intensity.
- SSPs ranked by their world GDP-per-capita growth 2024 → 2060.

**STEP 4. Per-run parameters (all runs at once).**
- **SSP blend**:
  - `ssp_u` sets a target world GDP-per-capita growth, evenly spaced between the slowest and fastest SSP.
  - The two SSPs that bracket the target are blended with the share `ssp_share_lo` that reproduces it. The share is found on a fine grid; stops if growth is not monotone in the share.
  - The same pair and share drive population, GDP per capita, flow-intensity bounds and the power-sector bracket.
  - `ssp_label` = SSP with the closest growth. Warns if label counts depart from an equal split by more than `SSP_SHARE_TOL`.
- **Intensity endpoints**: endpoint = int_2024 × ratio, with ratio = min + u × (max − min) from each region's own bounds. For flows, the ratio is computed under both bracketing SSPs and blended.
- **Power bracket**:
  - Under each of the run's two SSPs, the run's coal/gas/oil draws give a fossil index per region.
  - The two ScenarioMIP scenarios whose index brackets it are interpolated linearly (clamped at the ends), then the two SSPs are blended.
  - The world index gives the run's 2060 warming the same way.
- **Scalar parameters**: semi-uniform around the central value: u < 0.5 maps min → central, u ≥ 0.5 maps central → max.
- **Recovery rates**: each region ramps from its 2024 rate to the run's sampled global endpoint by the convergence year (smoothstep), then holds.
- **Lifetimes**: mean = max(0.3, min + u (max − min)); k likewise (≥ 0.1). One draw per super-category, applied to its sub-uses.
- **Ore grade**: smoothstep from the 2024 value to the sampled target, same timing as intensity.

**STEP 5. One run (`run_one`).**
1. Population and GDP-per-capita indices = blend of the two SSP matrices.
2. Time weight α = smoothstep from 2024 (0) to the run's target year (1), then 1. Intensity index = (endpoint / int_2024)^α, a log-linear path with zero slope at both ends.
3. **Power sector**: for each region and technology, target GW = weighted scenario capacity factor × population index × GDP-per-capita index; batteries = 0.4 × (PV + wind) GWh. One DSM per technology with fixed lifetimes. Materials = units × t/unit, summed to the 4 power end uses.
4. **Biomass and fossil fuels**: M = M_2024 × population × GDP per capita × intensity index. Crop residues follow the crops index; other biomass and other fossil stay at the 2024 intensity.
5. **Metals (Fe and NonFe separately)**, for each region × sub-use:
   - Target stock = S_2024 × population × GDP per capita × stock-intensity index × a Hermite term. The Hermite term starts the path at the 2024 historical growth slope.
   - Over the first 3 years, stock log-growth blends from the 2024 rate to the model rate.
   - `run_forward_dsm_fast` with the sampled lifetime.
   - The power end uses join the same pool.
   - `allocate_eol` with room = demand: secondary = min(recycling rate × waste pool, demand).
   - Base `*_Mt` columns are divided by the ore grade (ore equivalent); `*_Mt_pure` columns are metal mass.
6. **Non-metallic minerals**: same DSM. Only residential, non-residential, roads, civil engineering and power give and receive downcycled material. Room = demand × room share:
   - buildings: max secondary (buildings/civil) × concrete share (buildings) × aggregate share
   - civil and power: max secondary (buildings/civil) × concrete share (civil) × aggregate share
   - roads: max secondary (roads) × granular share
7. Adds the SSP blend columns and per-run flags: negative primary, total surplus ("spill").

**STEP 6. Parallel execution.** `furrr::future_map` over runs, all cores but one.

**STEP 7. Save and checks.** Results, spill summary, power-sector outputs, and four checks:
1. 2025 world capacity vs the input file.
2. Power share of in-use stock.
3. Correlation between fossil index and cumulative power inflows (expected negative).
4. No negative stocks.

**STEP 8. Historical flows on the model basis.** Applies `allocate_eol` to the calibrated historical DSM flows (1970-2024), using each region's 2024 recovery rate and the central room parameters. This gives historical primary and secondary flows on the same basis as the projection (metals in ore equivalent).

- **Inputs**: `Parameters/Simulation/*` (from `01`, `01b`); `Parameters/IIASA-Trajectories/ssp_drivers.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Parameters/UN-Population/population_region_historical.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`, `stock_2024_age_profile.csv`; `Parameters/Intermediate/stock_trajectory_subenduse.csv`, `flow_trajectory_subenduse.csv`; `Inputs/MatIntensity_Assumptions.xlsx`, `Recycling_Assumptions.xlsx`, `MC_Assumptions.xlsx`, `Dict_Materials.xlsx`.
- **Outputs** (`Results/MC/`):
  - `mc_results.parquet`: one row per run × region × material group × material key × year, 2025-2060. Columns: total inflow, in-use stock, primary and secondary (ore equivalent and `_pure`), new additions, replacement, waste, surplus (`secondary_spilled_Mt`), not recovered, target stock, SSP blend (`ssp_lo`, `ssp_hi`, `ssp_share_lo`, `ssp_label`), run flags.
  - `mc_ssp_assignment.csv`: per run, target and achieved world GDP-per-capita growth, population growth, SSP blend and label.
  - `mc_power_capacity.csv`, `mc_power_materials.csv`, `mc_power_run_link.csv` (fossil index, 2060 warming), `mc_power_run_brackets.csv`.
  - `hist_flows_model_basis.csv`.

## `03-Diagnostics.R`: Monte Carlo checks

1. **Convergence**: P01/P50/P99 of global primary consumption in `TARGET_YEAR` (2050) as the number of runs grows.
2. **Standardised rank regression coefficients (SRRC)**: regression of the rank of 2050 global primary consumption on the ranks of all inputs; top 20 plotted.
3. **Envelope**: P5-P95, P25-P75 and min-max of global primary consumption by material group, joined to UNEP history at 2024.
4. **Recycling tests** at 2060, after every run's convergence year:
   - T1: mass balance closes
   - T2: achieved recovery = drawn rate
   - T3: secondary ≤ inflow
   - T4: mineral secondary ≤ room
   - T5: drawn rate and secondary share are positively correlated

   Warns if any test fails.

- **Inputs**: `Results/MC/mc_results.parquet`; `Parameters/Simulation/mc_input_matrix.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`.
- **Outputs**: `Results/MC/mc_envelope.csv`, `mc_srrc.csv`, `mc_recycling_tests.csv`; `Figures/Simulation/03_{convergence,srrc_sensitivity,output_envelope,recycling_tests}.png`.

## `04-Decoupling.R`: decoupling classification

1. Loads primary consumption 2025-2060 (Fe and NonFe merged into metal ores).
2. Rebuilds each run's regional population and GDP from its SSP blend: GDP = GDP_2024 × population index × GDP-per-capita index.
3. World consumption per capita for each material group and for the total. Two weightings: actual population, and regional population shares fixed at 2025.
4. For each window variant: CAGR of consumption per capita and of GDP per capita (also total-mass CAGRs), and the peak year. Class, first match wins:
   - **Absolute decoupling**: material CAGR < 0 and GDP CAGR > 0.
   - **Peak decoupling**: peaked before the window end and is below the peak at the end, GDP CAGR > 0.
   - **Relative decoupling**: 0 ≤ material CAGR < GDP CAGR.
   - **No decoupling**: everything else.

   Rows with non-positive start or end values are flagged, not dropped.

Variants:

| `variant_id` | Window | Population weights |
|---|---|---|
| `main_actual` | 2040-2060 | actual |
| `window_2030_2060` | 2030-2060 | actual |
| `window_2025_2060` | 2025-2060 | actual (used by Figures 3 and 5) |
| `main_fixed2025w` | 2040-2060 | fixed 2025 |
| `main_2050` | 2025-2050 | actual |

- **Inputs**: `Results/MC/mc_results.parquet`; `Parameters/Simulation/mc_input_matrix.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Parameters/UN-Population/population_region_historical.csv`; `Parameters/IIASA-Trajectories/ssp_drivers.csv`.
- **Outputs**: `Results/MC/mc_decoupling.parquet` (one row per run × material group × variant).
