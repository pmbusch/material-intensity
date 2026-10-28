# 04-Simulation

Monte Carlo simulation with Latin hypercube sampling: `N_RUNS` = 1,000 runs, each with one draw of every uncertain parameter. It projects global material consumption, 2025-2060, by region (8), material and end use. Each run combines:

- **Flow materials** (biomass, fossil fuels), Kaya identity: M = M_2024 × population index × GDP-per-capita index × intensity index.
- **Gold ore**: a flow with fixed 2024 world intensity per world GDP (M_r = M_2024,r × world GDP index; regional 2024 shares held), reported inside metal ores.
- **Stock materials**: metals (Fe, Cu, Al and other non-Fe, each separately) and non-metallic minerals.
  - Target stock = S_2024 × population index × GDP-per-capita index × stock-intensity index.
  - A forward dynamic stock model (DSM) turns the target stock into inflows and end-of-life waste.
  - Waste is recycled (metals) or downcycled (minerals) into secondary supply, and primary = inflow − secondary.
- **Power sector**: generation capacity (13 technologies) and batteries as explicit stocks. They follow the run's fossil-fuel draws and add no new sampled parameters.

Run order: `01-Sampling.R` → `02-PowerSector.R` → `03-RunSimulations.R` → `04-Diagnostics.R` → `05-Decoupling.R`. `00-Parameters.R` is sourced by all of them.

## `00-Parameters.R`: configuration

No calculations. Holds every model constant and reads the sampling ranges from Excel.

- Years: `BASE_YEAR = 2024`; `TARGET_YEAR = 2050` (default intensity convergence; sampled per run); `SNAPSHOT_YEARS`.
- Run control: `N_RUNS = 1000`, `GLOBAL_SEED`, `SSP_SHARE_TOL`.
- Scalar parameters (`MC_PARAM_NAMES`), read from `Inputs/MC_Assumptions.xlsx` sheet `Parameters` as `<NAME>_MIN/_CENTRAL/_MAX`. The script stops and names any missing row.
  - recovery: recycling rate of Fe, Cu, Al and other non-Fe; downcycling rate; convergence year
  - room for secondary minerals: maximum secondary share (buildings/civil, roads); concrete shares (buildings, civil); aggregate share of concrete; granular share of roads
  - ore grade of Fe, Cu, Al and other non-Fe
  - target year of intensity convergence
- `METAL_PARAM_SUFFIX`: stock-metal group → suffix of its recycling and grade parameters (`metal_cu` → `recycling_rate_cu`, `grade_ore_cu`).
- Lifetimes: `lifetime_params` (central values, 8 sub-uses) and `LIFETIME_SAMPLE_PARAMS` (min/max), sheet `Lifetimes`.
- `STOCK_GROWTH_BLEND_YRS = 3`: years over which stock growth blends from the 2024 historical rate into the model rate.
- Power sector:
  - 4 lifetime classes: thermal 40 yr; solar & wind 27 yr; hydro & nuclear 10,000 yr (no retirement before 2060); batteries 15 yr. Each class borrows the age profile of a "donor" sub-use with a similar lifetime.
  - Technology map to Wang (2023) material intensities (`POWER_TECHS`).
  - Battery sizing: GWh = 4 h × 0.1 × (PV + wind GW).
  - Material-to-group maps:
    - Wang Al → `metal_al`, Cu → `metal_cu`.
    - Battery Al → `metal_al`, Cu → `metal_cu`; cobalt, lithium, manganese, nickel → `metal_othernonfe`.
    - 7 t aggregates per t cement.
  - Fossil energy densities (MJ/kg).
  - `POWER_CARVEOUT_SUB_USE = civil_engineering`; `POWER_CALIB_MAX = 5`.

## `01-Sampling.R`: sampling bounds and Latin hypercube draws

0. **2024 intensities** (`intensity_2024.csv`, kg per $ GDP PPP) per region × material: UNEP DMC 2024 / GDP for flows (crops, grazed biomass, wood, coal, gas, oil; remaining biomass and fossil categories pooled as other biomass / other fossil, Crop Residues excluded); MISO 2024 in-use stock / GDP for stocks (buildings, civil, machinery, short-lived; metals as metal mass). The simulation uses them only as the starting level of each intensity path (it samples ratios to them).
1. **Flow-intensity bounds**:
   - Takes the fixed 2060/2025 ratio min/max per R10 region × SSP (`summary_ratio_2060_2025.csv`) for coal, oil, gas, wood, crops and livestock (→ grazed biomass). Keeps SSP1, 2, 3 and 5.
   - **R10 → model regions**: ScenarioMIP reports the 10 IIASA R10 regions, which do not match the 8 model regions.
     - Each country belongs to one region of each kind (sheet `R10_Agg`).
     - weight(model region, R10) = share of the model region's GDP produced in countries of that R10 region (World Bank GDP PPP, constant 2021 intl $, latest year ≤ 2024).
     - Weights sum to 1 per model region. For example, a model region with 70% of its GDP in one R10 region and 30% in another gets weights 0.7 and 0.3.
   - **Model-region bounds**: min = Σ weight × R10 min, and max = Σ weight × R10 max, per material × SSP. These are GDP-weighted averages of the R10 bounds.
2. **Stock-intensity bounds**: ratio min/max per region × material × end use, from `MC_Assumptions.xlsx` sheet `Stock_Bounds`. They do not depend on the SSP. The metal bounds are shared by Fe, Cu, Al and other non-Fe.
3. **World bounds for figures**: Σ (2024 GDP share × 2024 intensity × ratio bound), in kg/$. Used only to label figures.
4. **Latin hypercube** (`lhs::randomLHS`, seed `GLOBAL_SEED`): `N_RUNS` × one column in [0, 1] per sampled parameter:
   - `ssp_u`: position between SSPs
   - one intensity draw per material, shared by all regions: crops, grazed biomass, wood, coal, gas, oil; metal stock for 4 end uses (shared by all metals); mineral stock for buildings and civil. Other biomass, other fossil, gold, and mineral machinery/short-lived products stay at 2024 values.
   - the scalar parameters, including recycling rate and ore grade for each of Fe, Cu, Al and other non-Fe
   - lifetime mean and Weibull k per super-category

**Why a Latin hypercube**: each parameter's range is cut into `N_RUNS` equal-probability strata, and every stratum is drawn exactly once, so every range is covered evenly. For the same number of runs, percentiles and sensitivity estimates converge faster than with plain random sampling. Columns are drawn independently (no correlation imposed).

- **Inputs**: `Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`; `Inputs/WorldBank/API_NY.GDP.MKTP.PP.KD_*.xls`; `Inputs/Dict_Countries.xlsx` (`R10_Agg`, `UNEP_Agg`); `Inputs/Dict_Materials.xlsx`; `Inputs/MC_Assumptions.xlsx`.
- **Outputs** (`Parameters/Simulation/`): `intensity_2024.csv`, `mc_input_matrix.csv`, `flow_ratio_bounds.csv`, `stock_ratio_bounds.csv`, `r10_region_weights.csv` (with `gdp`), `intensity_world_bounds.csv`.

## `02-PowerSector.R`: power-sector inputs

Fixed inputs that link generation capacity to each run's fossil-fuel draws.

Capacity follows real ScenarioMIP scenarios, not a rule:
- Each run's coal, gas and oil draws give it a fossil index: the 2024-energy-weighted 2060/2025 change in fossil extraction per GDP.
- In each region, the run sits between the two scenarios of its SSP with the nearest lower and higher index, and takes their interpolated capacity of all 13 technologies (`03-RunSimulations.R`).
- High-fossil runs therefore tend to follow scenarios with less renewable capacity, and vice versa. This comes from the scenarios, not from an imposed complement.

1. **Scenario set**: ScenarioMIP runs (model × family × SSP) that report coal, gas and oil extraction per GDP at R10 level. These are the runs behind the fossil bounds. Prints which capacity variables each run reports.
2. **Capacity paths**: capacity by R10 × technology (missing = 0) and GDP|PPP, interpolated linearly from 5-year to annual values (2025-2060). Prints 2025 → 2030 jumps above 1.5×.
3. **R10 → model regions**:
   - A model region's capacity intensity (GW per $ GDP) = the GDP-weighted mean of the R10 intensities, using the weights from `01`.
   - Absolute amounts (2025 GW, GDP) are split the other way: each R10 value is divided among model regions by their share of that R10's GDP.
   - The 2025 anchor capacity = median 2025 GW across the scenario set per R10, split to model regions by GDP share.
   - Capacity factor (to be multiplied by the run's population and GDP-per-capita indices):
     - ratio form: anchor × I(t) / I(2025)
     - level form: I(t) × scenario 2025 GDP. Used when the anchor or I(2025) is zero, or when anchor / the scenario's own 2025 GW is outside [1/5, 5].
4. **Fossil index** per scenario × region = Σ over coal/gas/oil of 2024 energy share × the scenario's 2060/2025 extraction-per-GDP ratio.
   - Energy shares come from UNEP DMC × energy density.
   - R10 cells replaced by World in `08-summary_ratio.R` use the scenario's World ratio.
   - World index = regional indices weighted by 2024 fossil energy.
   - Prints the index range of scenarios against the range implied by the sampling bounds.
5. **Warming**: 2060 median global surface temperature of each scenario (MAGICC).
6. **Material intensities**: Wang (2023) t/GW, averaged per technology × material, plus aggregates = 7 × cement. Batteries: mean kg/kWh of LFP and NMC 811, converted to t/GWh. Grouped into Fe, Cu, Al, other non-Fe and non-metallic minerals.
7. **Carve-out**: MISO2's civil-engineering stock already contains today's power plants. Because the power sector is modelled as its own stock:
   - its 2025 material is subtracted from the 2024 civil-engineering stock, per region and material (Fe, Cu, Al, other non-Fe, minerals);
   - `03` scales the civil-engineering cohorts down by the same factor;
   - the 2025 material = anchor GW and battery GWh × t per unit.
   Without this, power infrastructure would be counted twice. The script stops if any stock becomes negative.
8. Saves. Capacity factors are shifted one year earlier (scenario 2025 = model 2024), and the last year is held to 2060.

- **Inputs**: `Parameters/IIASA-Trajectories/metrics_levels_raw.csv`, `ssp_long_clean.csv`, `summary_ratio_2060_2025.csv`; `Parameters/Simulation/r10_region_weights.csv`, `flow_ratio_bounds.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`; climate CSV; `Inputs/Wang2023_material_intensity_technology.xlsx`; `Inputs/Battery_intensity.xlsx`.
- **Outputs** (`Parameters/Simulation/`): `power_scenarios.csv`, `power_scenario_index.csv`, `power_fossil_shares.csv`, `power_capacity_factor.csv`, `power_capacity_anchor.csv`, `power_capacity_check_2025.csv`, `power_material_intensity.csv`, `power_carveout_2025.csv`.

## `03-RunSimulations.R`: Monte Carlo runner

**STEP 1-2. Load draws and 2024 values.**
- LHS matrix.
- 2024 intensities by region × material (`intensity_2024.csv`, from `01`).
- 2024 recycling rates from `Recycling_Assumptions.xlsx` sheet `Recycling_EOL`: `Recycling_rate_Fe`, `Copper EOL-RR`, `Aluminum EOL-RR`, `OtherNonFE EOL-RR`. The script stops if the last column is missing.
- Downcycling rates (`Downcycling`).
- Flow and stock ratio bounds.

**STEP 3. Static data.**
- SSP population and GDP-per-capita indices, as region × year matrices per SSP.
- 2024 UNEP DMC anchors for flow materials (minor categories grouped as "Other biomass" and "Other fossil fuels"), and 2024 gold ore DMC (`nonfe_split_region.csv`).
- 2024 stock and age profile per region × material × sub-use, with the power carve-out removed. Cohorts are annual; the first (1900) cohort, which holds all pre-1900 stock, is spread evenly over 1896-1900.
- 2023 → 2024 historical stock growth (for the seam blend).
- Power-sector arrays: capacity factors, anchor, material intensity.
- SSPs ranked by their world GDP-per-capita growth 2024 → 2060.

**STEP 4. Per-run parameters (all runs at once).**
- **SSP blend**:
  - `ssp_u` sets a target world GDP-per-capita growth, evenly spaced between the slowest and fastest SSP.
  - The two SSPs that bracket the target are blended with the share `ssp_share_lo` that reproduces it. The share is found on a fine grid; the script stops if growth is not monotone in the share.
  - The same pair and share drive population, GDP per capita, flow-intensity bounds and the power-sector bracket.
  - `ssp_label` = SSP with the closest growth. Warns if label counts depart from an equal split by more than `SSP_SHARE_TOL`.
- **Intensity endpoints**: endpoint = int_2024 × ratio, with ratio = min + u × (max − min) from each region's own bounds. For flows, the ratio is computed under both bracketing SSPs and blended.
- **Power bracket**:
  - Under each of the run's two SSPs, the run's coal/gas/oil draws give a fossil index per region.
  - The two ScenarioMIP scenarios whose index brackets it are interpolated linearly (clamped at the ends), then the two SSPs are blended.
  - The world index gives the run's 2060 warming the same way.
- **Scalar parameters**: semi-uniform around the central value: u < 0.5 maps min → central, u ≥ 0.5 maps central → max.
- **Recovery rates** (per metal, and downcycling): each region ramps from its 2024 rate to the run's sampled global endpoint by the convergence year (smoothstep), then holds.
- **Lifetimes**: mean = max(0.3, min + u (max − min)); k likewise (≥ 0.1). One draw per super-category, applied to its sub-uses.
- **Ore grade** (per metal): smoothstep from the 2024 value (`GRADE_ORE_*_2024`) to the sampled target, with the same timing as intensity.

**STEP 5. One run (`run_one`).**
1. Population and GDP-per-capita indices = blend of the two SSP matrices.
2. Time weight α = smoothstep from 2024 (0) to the run's target year (1), then 1. **Intensity index = (endpoint / int_2024)^α**, a log-linear path with zero slope at both ends.
3. **Power sector**: for each region and technology, target GW = weighted scenario capacity factor × population index × GDP-per-capita index; batteries = 0.4 × (PV + wind) GWh. One DSM per technology with fixed lifetimes. Materials = units × t/unit, summed to the 4 power end uses.
4. **Biomass and fossil fuels**: M = M_2024 × population × GDP per capita × intensity index. Crop residues follow the crops index; other biomass and other fossil stay at the 2024 intensity.
5. **Gold ore** (`metal_gold`): M_r = gold ore DMC 2024 of region r × world GDP index (constant 2024 world intensity; regional shares fixed at 2024, since gold ore is mined for a global market). No stock, recycling or grade.
6. **Metals** (Fe, Cu, Al and other non-Fe separately; they share each end use's metal stock-intensity draw), for each region × sub-use:
   - Target stock = S_2024 × population × GDP per capita × stock-intensity index × a Hermite term.
   - The Hermite term lets the path start at the 2024 historical growth rate instead of flat. With s = (t − 2024) / (target year − 2024):
     - log intensity = log I_2024 + h01(s) · log(endpoint / I_2024) + h10(s) · g · (target year − 2024)
     - h01 = 3s² − 2s³ (the smoothstep) and h10 = s³ − 2s² + s are cubic Hermite basis functions.
     - h10 is 0 at both ends and has slope 1 at the start, so the path starts with slope g and ends flat at the endpoint. With g = 0 it reduces to the smoothstep.
     - g = 2024 historical stock log-growth − the run's 2025 GDP log-growth.
   - Over the first 3 years, stock log-growth blends from the 2024 rate to the model rate.
   - `run_forward_dsm_fast` with the sampled lifetime.
   - The power end uses of that metal join the same pool.
   - `allocate_eol` with room = demand: secondary = min(recycling rate × waste pool, demand).
   - Base `*_Mt` columns are divided by the metal's ore grade (ore equivalent); `*_Mt_pure` columns are metal mass.
7. **Non-metallic minerals**: same DSM. Only residential, non-residential, roads, civil engineering and power give and receive downcycled material. Room = demand × room share:
   - buildings: max secondary (buildings/civil) × concrete share (buildings) × aggregate share
   - civil and power: max secondary (buildings/civil) × concrete share (civil) × aggregate share
   - roads: max secondary (roads) × granular share
8. Adds the SSP blend columns and per-run flags: negative primary, total surplus ("spill").

**STEP 6. Parallel execution.** `furrr::future_map` over runs, using all cores but one.

**STEP 7. Save and checks.** Results, spill summary, power-sector outputs, and four checks:
1. 2025 world capacity vs the input file.
2. Power share of in-use stock, per metal and minerals.
3. Correlation between fossil index and cumulative power inflows (expected negative).
4. No negative stocks.

**STEP 8. Historical flows on the model basis.**
- Applies `allocate_eol` to the calibrated historical DSM flows (1970-2024), per metal, using each region's 2024 recovery rate and the central room parameters.
- This gives historical primary and secondary flows on the same basis as the projection (metals in ore equivalent).
- Gold has no DSM history.

- **Inputs**: `Parameters/Simulation/*` (from `01`, `02`); `Parameters/IIASA-Trajectories/ssp_drivers.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Parameters/UN-Population/population_region_historical.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MaterialFlows/nonfe_split_region.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`, `stock_2024_age_profile.csv`; `Parameters/Intermediate/stock_trajectory_subenduse.csv`, `flow_trajectory_subenduse.csv`; `Inputs/Recycling_Assumptions.xlsx`, `MC_Assumptions.xlsx`, `Dict_Materials.xlsx`.
- **Outputs** (`Results/MC/`):
  - `mc_results.parquet`: one row per run × region × material group × material key × year, 2025-2060.
    - Material groups: `biomass`, `fossil_fuels`, `metal_fe`, `metal_cu`, `metal_al`, `metal_othernonfe`, `metal_gold`, `nonmetallic_minerals`.
    - Columns: total inflow, in-use stock, primary and secondary (ore equivalent and `_pure`), new additions, replacement, waste, surplus (`secondary_spilled_Mt`), not recovered, target stock, SSP blend (`ssp_lo`, `ssp_hi`, `ssp_share_lo`, `ssp_label`), run flags.
  - `mc_ssp_assignment.csv`: per run, target and achieved world GDP-per-capita growth, population growth, SSP blend and label.
  - `mc_power_capacity.csv`, `mc_power_materials.csv`, `mc_power_run_link.csv` (fossil index, 2060 warming), `mc_power_run_brackets.csv`.
  - `hist_flows_model_basis.csv`.

## `04-Diagnostics.R`: Monte Carlo checks

All metal groups (gold included) are pooled into metal ores.

1. **Convergence**: P01/P50/P99 of global primary consumption in `TARGET_YEAR` (2050) as the number of runs grows.
2. **Standardised rank regression coefficients (SRRC)**: regression of the rank of 2050 global primary consumption on the ranks of all inputs; top 20 plotted.
3. **Envelope**: P5-P95, P25-P75 and min-max of global primary consumption by material group, joined to UNEP history at 2024.
4. **Recycling tests** at 2060, after every run's convergence year, per stock metal (Fe, Cu, Al, other non-Fe; gold excluded) and for minerals:
   - T1: mass balance closes
   - T2: achieved recovery = drawn rate
   - T3: secondary ≤ inflow
   - T4: mineral secondary ≤ room
   - T5: drawn rate and secondary share are positively correlated

   Warns if any test fails.

- **Inputs**: `Results/MC/mc_results.parquet`; `Parameters/Simulation/mc_input_matrix.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`.
- **Outputs**: `Results/MC/mc_envelope.csv`, `mc_srrc.csv`, `mc_recycling_tests.csv`; `Figures/Simulation/03_{convergence,srrc_sensitivity,output_envelope,recycling_tests}.png`.

## `05-Decoupling.R`: decoupling classification

**Goal**: classify each run, for the total and for each material group, by how world per-capita primary consumption moves against GDP per capita: absolute decoupling (consumption falls), peak, relative (consumption grows slower than GDP) or none. Figures 3 and 5 and S12 use these classes.

1. Loads primary consumption 2025-2060, with all metals (Fe, Cu, Al, other non-Fe, gold) merged into metal ores.
2. Rebuilds each run's regional population and GDP from its SSP blend: GDP = GDP_2024 × population index × GDP-per-capita index.
3. World consumption per capita for each material group and for the total. Two weightings: actual population, and regional population shares fixed at 2025.
4. For each window variant: the CAGR of consumption per capita and of GDP per capita (also total-mass CAGRs), and the peak year. Class, first match wins:
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
