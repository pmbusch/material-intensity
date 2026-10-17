# 03-SSP Trajectories

Prepares the future-scenario inputs for the Monte Carlo simulation:

1. **SSP drivers** (`00`): population and GDP per capita by model region, 2024-2100, indexed to 2024 = 1.
2. **ScenarioMIP intensity ratios** (`01`, `02`, `04`): for each material, the 2060/2025 ratio of material intensity (per GDP) across the CMIP7 ScenarioMIP integrated assessment model runs, summarised as min-max bounds per R10 region × SSP. These are the sampling bounds of the flow intensities (biomass, fossil fuels).
3. **Exploratory figures** (`03`, `03b`, `03c`, `03d`, `05`): trajectories of ScenarioMIP variables. Only `04` and `05` write supporting figures (S09, S20, S21).

Run order: `00` → `01` → `02` → `03*` → `04` → `05`. ScenarioMIP regions are the IIASA R10 regions plus World. They are mapped to model regions later, in `04-Simulation/01-Sampling.R`.

ScenarioMIP files used: `Inputs/IIASA_SSP/2026-MIP-CMIP7/{region,iamc,food,energy,capacity,climate}_iamc_data-*.csv`.

## `00_preprocess_ssp_drivers.R`: SSP population and GDP

1. Reads the IIASA SSP basic drivers (all columns as text, then converts the years to numbers). Keeps Population, GDP|PPP and GDP|PPP per capita for the historical scenario and SSP1-5.
2. Uses the historical scenario up to 2024 for every SSP, then each SSP's own projection after 2024.
3. Maps countries to model regions (`IIASA_Agg`) and sums population and GDP. GDP per capita is recomputed from the regional sums.
4. Interpolates the 5-year values to annual values with a monotone cubic spline on log(value). Prints the interpolated vs original GDP at the 5-year points for SSP2.
5. Index = value / value in 2024.
6. Plots population and GDP per capita by region and SSP.

- **Inputs**: `Inputs/IIASA_SSP/ssp_basic_drivers_release_3.2_full.xlsx` (sheet `data`), `Inputs/Dict_Countries.xlsx` (`IIASA_Agg`).
- **Outputs**: `Parameters/IIASA-Trajectories/ssp_drivers.csv` (scenario, region, variable, year, value, index); `Figures/Inputs/IIASA/BasicDrivers.png`.

## `01_load_and_check.R`: ScenarioMIP clean-up

1. Stacks the R10, World, food, energy and capacity extracts and drops exact duplicate rows. The energy extract repeats some primary energy rows.
2. Pivots to long format and keeps the 5-year grid 2020-2060.
3. Parses each scenario name into forcing family (e.g. "Medium"), SSP, and marker flag.
4. Checks for marker and non-marker runs of the same model × family × SSP. If found, keeps the marker run.
5. Coverage tables: models per family × SSP × variable × region.
6. Share of World in "Other (R10)" for each variable (logged; this region is then dropped).
7. Tests whether GDP and population are the same across forcing families (exogenous) or change with policy. Reports the coefficient of variation (CV) across models and across families.
8. Saves the cleaned panel, without "Other (R10)", and an assumptions log.

- **Inputs**: the five ScenarioMIP extracts (`region`, `iamc` (World), `food`, `energy`, `capacity`).
- **Outputs** (`Parameters/IIASA-Trajectories/`): `ssp_long_clean.csv`, `coverage_model_scenario_variable_region.csv`, `coverage_model_count_by_family_ssp.csv`, `other_r10_share.csv`, `gdp_pop_consistency.csv`, `assumptions_log.txt`.

## `02_compute_metrics.R`: intensity metrics

1. Pivots so each run × region × year is one row with one column per variable. Numerator and denominator therefore always come from the same run.
2. Computes about 70 metrics:
   - Per GDP (GDP|MER): primary energy by source (MJ/$), electricity capacity by technology (W/$), coal/gas/oil extraction (MJ/$), steel, cement, aluminium, crops, livestock and roundwood production, floor space.
   - Per capita: the same production variables, plus food intake and food waste.
   - Raw levels (`_total`).
3. Long table of levels; `metric_units.csv` holds the unit of each metric.
4. Compound annual growth rate (CAGR) per run, 2020 → 2060.
5. Base-year-harmonized levels (each run rescaled to the cross-run median in 2020), used only for plotting.

- **Inputs**: `Parameters/IIASA-Trajectories/ssp_long_clean.csv`.
- **Outputs** (`Parameters/IIASA-Trajectories/`): `metric_units.csv`, `metrics_levels_raw.csv`, `metrics_cagr.csv`, `metrics_levels_harmonized.csv`.

## `03_trajectory_figures.R`, `03b_trajectory_by_ssp_region.R`, `03c_primary_energy_figures.R`, `03d_capacity_figures.R`: trajectory figures

Exploratory figures with a shared design: every run is a line coloured by SSP (marker runs thicker), a dashed reference at 2025, a min-max bar per SSP at 2060, and tables of the 2060/2025 ratio (min, max, CV) underneath.

- `03`: 14 focus variables (fossil extraction; steel, aluminium, cement; floor space; crops, livestock, roundwood; food) as totals, per GDP and per capita for World, plus a grid of R10 regions for the per-GDP versions. Also writes the data behind each World figure.
- `03b`: the same per-GDP figures with one panel per SSP and one colour per region.
- `03c`: primary energy per GDP, one figure per energy source (World, by region, by SSP, and all sources together).
- `03d`: electricity capacity per GDP, one figure per technology (same layout as `03c`).

- **Inputs**: `Parameters/IIASA-Trajectories/metrics_levels_raw.csv`, `metric_units.csv`.
- **Outputs**: `Figures/Inputs/IIASA/Trajectories/...` (+ SVG copies); `Parameters/IIASA-Trajectories/trajectories/{metric}_world.csv` (`03` only).

## `04_summary_ratio.R`: intensity ratio bounds (sampling input)

1. For every per-GDP metric, region and SSP: per-run ratio = value 2060 / value 2025 (runs with a 2025 value ≤ 0 are dropped). Summarised as N, min, max and 5th-95th percentile.
2. **Fix 1 (very small 2025 values)**: a regional cell with max ratio above 5 takes the World cell of the same variable × SSP. World cells above 5 are only flagged.
3. **Fix 2 (SSP4)**: SSP4 has a single run, so it takes the SSP2 bounds of the same variable × region.
4. Raw min/max and the applied fix are kept in the output.
5. Figure S09: min-max bars of the fixed ratio for 8 variables. Rows are SSPs; World and R10 regions are sorted by GDP per capita; open circles mark the marker run.

- **Inputs**: `Parameters/IIASA-Trajectories/metrics_levels_raw.csv`, `metric_units.csv`.
- **Outputs**: `Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv` (Variable, Region, SSP, N, Min, Max, CI90_Lo, CI90_Hi, Min_raw, Max_raw, Fix); figure S09.

## `05_climate_figures.R`: ScenarioMIP climate outcomes

1. Reads the World climate assessment (MAGICC), annual 2020-2060, and parses family, SSP and marker.
2. For forcing, temperature (33rd/median/67th percentile) and emissions (CO₂, CH₄, N₂O, Kyoto gases), plots all runs coloured by SSP and, separately, by forcing family.
3. Each panel's table: 2060 level (min/max) and change since 2025. Temperature panels also show the share of runs whose peak warming exceeds 1.5 °C and 2 °C.

- **Inputs**: `Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-*.csv`.
- **Outputs**: figures S20 (temperature by SSP) and S21 (by family); other figures in `Figures/Inputs/IIASA/Climate/`; `Parameters/IIASA-Trajectories/climate_summary_2060.csv`.
