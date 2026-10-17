# 05-Figures

Main-text figures, Table 1 and supporting figures. Figure scripts only read results; they do not change the model.

Each figure is saved as PNG in `Figures/` and as SVG in `Figures/SVG/` (cleaned with `clean_svg()` or `group_svg_layers()`). Where noted, the data behind a figure is written to `Figures/RawData/`. Expensive steps are cached in `Parameters/Intermediate/Figure*_*.csv` by the `PrepareData` scripts.

Run order matters (see `00-RunAll.R`):

- `Figure 5 - Sensitivity - PrepareData.R` must run before `Figure 4 - Climate.R`, because Figure 4 panel a reads `Figure5_Scatter.csv`.
- `Figure 5 - Alt1 - PrepareData.R` sources (re-runs) `Figure 5 - Sensitivity - PrepareData.R`.
- `Figure 5 - Alt1.R` sources `Figure 5 - Sensitivity.R`.

## Recurring calculations

- **Per-run world population and GDP**: `results` only stores each run's SSP blend (`ssp_lo`, `ssp_hi`, `ssp_share_lo`). Scripts rebuild regional population = pop_2024 × blended population index, and GDP = GDP_2024 × population index × GDP-per-capita index, then sum to the world. Used by Figures 2, 3, 5 and S18.
- **Splice at 2024**: projected envelopes start from the observed 2024 value, so history and projection meet.
- **Intensity**: M/G = consumption per GDP (kg/$); S/G = in-use stock per GDP (kg/$).

## Main figures and table

### `Figure 1 - HistoricalTimeseries.R` → `Fig1 - Historical Timeseries.png`

Historical only (UNEP DMC, 1970-2024).

- **(a)** World DMC stacked by region.
- **(b)** World DMC stacked by material; categories grouped into the 4 material groups.
- **(c)** Material per GDP vs GDP per capita, one path per region and the world, with lines of equal t/capita.
- **(d)** Same for 3 region groups (advanced economies, Asia, rest of world) × 4 material groups.

Paths in c/d are smoothed with LOWESS; the axis ranges are fixed.

- **Inputs**: `Parameters/UNEP-Materials/materials_region_DMC.csv`, `Parameters/Worldbank-GDP/gdp_region.csv`, `Parameters/UN-Population/population_region_historical.csv`.
- **Outputs**: the figure; `Figures/RawData/fig1{a,b,c,d}.csv`.

### `Figure 2 - Assumptions.R` → `Fig2 - Assumptions.png`

Eight Kaya panels, history 1970-2024 and Monte Carlo projection 2025-2060. Projections show the median, the interquartile band (P25-P75) and the min-max band across runs.

- **(a)** World population. **(b)** World GDP per capita.
- **(c)** M/G of biomass sub-materials. **(d)** M/G of fossil sub-materials.
- **(e)** S/G of metals by sub-use. **(f)** S/G of minerals by sub-use.
- **(g)** Total primary consumption. **(h)** Total in-use stock.

Steps:

1. Rebuilds world population and GDP per run (sections C, C2).
2. Historical M/G from UNEP DMC; historical S/G from the sub-use DSM stock (section D).
3. Historical power-sector stock = EIA capacity, split into model technologies with each region's 2025 anchor mix, × model t/GW (and batteries). It is shown as its own line, removed from historical civil engineering (same carve-out as the model), and extended back from 1980 to 1970 with the 1980-1990 trend.
4. Projected envelopes per run (section E). Panel g also prints the 2025 → 2060 growth of total consumption.
5. Panel g history: biomass and fossil from UNEP DMC; metals and minerals from `hist_flows_model_basis.csv`, so they are on the same basis as the projection (section F).
6. Splices the 2024 values (F2). Adds a bar at 2060 showing the composition of the run closest to the median (F3).

- **Inputs**: `Parameters/UN-Population/population_{world,region}_historical.csv`, `Parameters/Worldbank-GDP/gdp_{world,region}.csv`, `Parameters/IIASA-Trajectories/ssp_drivers.csv`, `Parameters/UNEP-Materials/materials_region_DMC.csv`, `Parameters/Intermediate/stock_trajectory_{1970_2024,subenduse}.csv`, `Parameters/EIA-Capacity/capacity_region_historical.csv`, `Parameters/Simulation/power_{capacity_anchor,material_intensity,carveout_2025}.csv`, `Results/MC/mc_results.parquet`, `Results/MC/hist_flows_model_basis.csv`.
- **Outputs**: the figure.

### `Figure 2 Detail - Material.R`, `Figure 2 Detail - Region.R`, `Figure 2 Detail - Region Summary.R`

Region-level detail behind Figure 2.

- **Material**: one figure per material group. Facets are sub-materials or sub-uses; one line per region (history solid, Monte Carlo median dashed); no bands.
- **Region**: the 8-panel Figure 2 rebuilt for each region, using that region's own population and GDP.
- **Region Summary**: 2024 snapshot of M/G (4 groups) and S/G (metals, minerals) by region: stacked bars, an M/G scatter and an S/G scatter. Historical data only.

- **Inputs**: the same historical files as Figure 2 (Region Summary also reads `Parameters/Intermediate/UNEP_flows_enduse.csv`); `mc_results.parquet` (Material, Region).
- **Outputs**: `Figures/Figure 2 Detail/Material/<group>.png`, `Figures/Figure 2 Detail/Region/<region>.png`, `Figures/Figure 2 Detail/Region 2024 Intensity - {Stacked,Scatter MG,Scatter SG}.png`.

### `Figure 3 - Contours-GrowthRate.R` → `Fig3 - Contours-GrowthRate.png` (+ S17)

Growth of material consumption as a function of GDP growth and one material-specific lever, at 2060.

1. **Dot version** (`Figures/Other/Fig3_dots.png`):
   - Y = per-capita consumption CAGR 2025-2060 (`mc_decoupling`, variant `window_2025_2060`); colour = decoupling class (peak decoupling merged into relative).
   - X per panel:
     - biomass: kg/person (crops, grazed biomass, wood)
     - fossil fuels: MJ/$ (DMC × energy density / GDP)
     - metals and minerals: secondary share of inflow
   - Point outlines: ore grade (metals) and lifetime (minerals).
2. **Contour version** (main output):
   - Each panel plots X = total GDP CAGR 2025-2060 against a lever on Y. Colour and contours = total primary-consumption CAGR of that material, interpolated with a smooth surface (`mgcv::gam(z ~ te(x, y, k = 8))`) on a 100 × 100 grid. Predictions are clipped to the observed range.
   - Y levers: biomass kg/person; fossil MJ/$; metal and mineral S/G; recycling and downcycling rates; ore grades (Fe, NonFe); building and civil lifetimes; recovery rate vs stock intensity.
   - A star marks today's situation: 2019-2024 world GDP CAGR and 2024 values. The 2024 recycling/downcycling rates = recovered / waste from `historical_secondary_flows.csv`.
   - An optional command-line `k` (smoothness) saves only the main figure as `S24_Fig3_SmoothK<k>.png`.
3. **Main figure**: 6 panels (4 intensity panels, metal recycling, mineral downcycling). **S17**: all 16 panels in 4 rows.

- **Inputs**: `Results/MC/mc_results.parquet`, `mc_decoupling.parquet`; `Parameters/Simulation/mc_input_matrix.csv`; `Parameters/IIASA-Trajectories/ssp_drivers.csv`; `Parameters/Worldbank-GDP/gdp_region.csv`; `Parameters/UN-Population/population_region_historical.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`; `Parameters/Intermediate/historical_secondary_flows.csv`.
- **Outputs**: `Fig3 - Contours-GrowthRate.png`, S17, `Figures/Other/Fig3_dots.png`.

### `Figure 4 - Climate - PrepareData.R`

Gives each run a 2060 warming by matching its flow-intensity draws to ScenarioMIP scenarios.

1. Each run's SSP = `ssp_label`. Its six flow-intensity draws u (coal, oil, gas, wood, crops, grazed biomass) are its position within the SSP's bounds.
2. Each ScenarioMIP scenario's World 2060/2025 ratio is put in the same space: u = (ratio − Min) / (Max − Min), using the World bounds of its SSP.
3. Distance = root mean square of the u differences, over scenarios of the run's SSP that report all six materials and have climate data.
4. Run warming = inverse-distance-weighted mean (power 2) of the 3 nearest scenarios, for the 2060 median, 33rd and 67th percentile.

- **Inputs**: `Results/MC/mc_results.parquet`; `Parameters/Simulation/mc_input_matrix.csv`; `Parameters/IIASA-Trajectories/summary_ratio_2060_2025.csv`, `metrics_levels_raw.csv`; climate CSV.
- **Outputs**: `Parameters/Intermediate/Figure4_RunScenarioMap.csv` (run_id, ssp, t_med, t_p33, t_p67, nearest model/family/dist, t_nearest).

### `Figure 4 - Climate.R` → `Fig4 - Climate.png` (+ S23, S24)

1. Cumulative 2025-2060 primary consumption per run and material group. Reference lines: the 2025 level held constant, and growing 2%/yr.
2. **(a)** Material vs GDP growth per run (from `Figure5_Scatter.csv`), with density margins by SSP.
3. **(b)** Cumulative consumption vs 2060 median warming (from the run-scenario map): scatter with 33%/66% density contours, 1.5 °C/2 °C lines, and on the right the warming range of each SSP's scenarios.
4. **S23**: panel b at the 33rd and 67th percentile warming. **S24**: cumulative material shares by SSP.

- **Inputs**: `Results/MC/mc_results.parquet`; climate CSV; `Parameters/Intermediate/Figure4_RunScenarioMap.csv`, `Figure5_Scatter.csv`.
- **Outputs**: the figure; S23 (`S23_Climate_Percentiles.png`); S24 (`S24_MaterialShare_SSP.png`).

### `Figure 5 - Sensitivity - PrepareData.R`

Shared growth outcome per run: total material CAGR and total GDP CAGR, 2025-2060 (`mc_decoupling`, `window_2025_2060`, Total). Absolute decoupling = material CAGR < 0.

1. **STEP 2**: growth table per run, with SSP label, fossil index and 2060 warming, growth bin (<0%, 0-1%, 1-2%, >2%), and a "selected" flag for runs more than 1 residual standard error below the line `lm(mat_cagr ~ gdp_cagr)`.
2. **STEPS 3-5**: converts each u draw to real units (`intensity_world_bounds.csv` and the Excel ranges); labels and families for each lever.
3. **STEP 6, importance**:
   - LightGBM model of material CAGR on all levers, with `ssp_u` replaced by achieved population and GDP-per-capita growth.
   - 80/20 split; a larger model is fit if held-out R² < 0.8.
   - Mean |SHAP| per growth bin, normalised to 100%. Shows population, GDP per capita, the top 8 other levers, and "Other".
4. **STEP 7, lever effects**: `lm(mat_cagr ~ all levers, real units)`; effect = coefficient × a fixed real-unit change per lever (e.g. +1%/yr GDP, +0.1 kg/$, +20 yr lifetime), with a 95% interval.
5. **STEP 8**: raw u draws per lever, and mean u among decoupling runs and among selected runs.
6. **STEP 8b**: 2060 world consumption per capita by material. Runs are split into Low and High groups by a 2060 metric (biomass t/cap, fossil MJ/$, metal and mineral S/G; metals and minerals also by recovery rate). Reference lines show 0% and +2.5%/yr growth from 2025.

- **Inputs**: `Parameters/Simulation/mc_input_matrix.csv`, `intensity_world_bounds.csv`; `Results/MC/mc_decoupling.parquet`, `mc_ssp_assignment.csv`, `mc_power_run_link.csv`, `mc_results.parquet`; `Parameters/IIASA-Trajectories/ssp_drivers.csv`; GDP and population 2024; `Inputs/*_Assumptions.xlsx`.
- **Outputs** (`Parameters/Intermediate/`): `Figure5_GrowthImportance.csv`, `Figure5_Scatter.csv`, `Figure5_LeverEffects.csv`, `Figure5_LeverSamples.csv`, `Figure5_LeverDiamonds.csv`, `Figure5_Densities.csv`, `Figure5_DensityLines.csv`.

### `Figure 5 - Sensitivity.R` → `Fig5 - Sensitivity_alt1.png` (alternative layout)

Plotting only, from the Figure5 CSVs:

- **(a)** SHAP importance by growth bin.
- **(b)** Material vs GDP growth scatter with regression line and margins.
- **(c)** Lever effects with 95% intervals.
- **(d)** 2060 per-capita consumption densities, Low vs High groups.

### `Figure 5 - Alt1 - PrepareData.R`

Sources `Figure 5 - Sensitivity - PrepareData.R`, then:

1. **Additive (LMDI) decomposition of total growth per run**, ln(M_2060 / M_2025):
   - **Flows**: M = P × G/P × M/G.
   - **Metals** (Fe, NonFe): Ore = P × G/P × S/G × Out/S × In/Out × Prim/In × Ore/Prim.
   - **Minerals**: Prim = P × G/P × S/G × Out/S × In/Out × PrimU/In × Prim/PrimU. Here PrimU = inflow − (secondary + surplus), so PrimU/In reflects the downcycling rate and Prim/PrimU the absorption caps.
   - Out/S is the retirement rate (lifetime) and In/Out the stock growth.
   - Terms are weighted by logarithmic means, then rescaled so they sum to the run's CAGR (%/yr).
2. **S25**: the same decomposition within each material group, with intensity split by sub-material (flows) or S/G split by end use (metals, minerals).
3. **S22**: GDP elasticity per material group = slope on GDP CAGR in `lm(group CAGR ~ gdp_cagr + other levers)`, excluding `ssp_u`.

- **Inputs**: as above, plus `Results/MC/mc_results.parquet`.
- **Outputs** (`Parameters/Intermediate/`): `Figure5_Alt1_Decomposition.csv`, `FigureS25_DecompositionByMaterial.csv`, `FigureS22_GDPElasticity.csv`.

### `Figure 5 - Alt1.R` → `Fig5 - Sensitivity.png` (main Figure 5) (+ S22, S25, S26)

Sources `Figure 5 - Sensitivity.R` for panel a.

- **(a)** SHAP importance.
- **(b)** Decomposition terms: median contribution of each term as a cascade, with the spread across runs as range + interquartile box.
- **(c-f)** One panel per material: 2060 consumption densities for GDP growth below and above 2%/yr, each split into runs with favourable or unfavourable levers. The score is the mean percentile rank of the material's levers; the top and bottom quartiles are used, widened until each group is large enough.

Also saves `Fig5 - Sensitivity_v1.png` (densities instead of boxes), S25 (decomposition by material), S26 (jittered runs) and S22 (GDP elasticities).

- **Inputs**: `Parameters/Intermediate/Figure5_*.csv`, `FigureS22_GDPElasticity.csv`, `FigureS25_DecompositionByMaterial.csv`.
- **Outputs**: `Fig5 - Sensitivity.png`, `Fig5 - Sensitivity_v1.png`, S22, S25, S26.

### `Table 1 - Results.R` → `Table1_Results.html`

Stock, flow and outflow (Gt or Gt/yr) for 2024 and 2060, by material group and detail (biomass and fossil sub-materials; Fe and NonFe metals; mineral end uses, with the power sector counted under civil infrastructure).

- **2024**: stock from `stock_2024_total.csv`; biomass and fossil flows from UNEP DMC; metal and mineral flow and outflow from `hist_flows_model_basis.csv`.
- **2060**: Monte Carlo median with a 95% interval (2.5-97.5 percentile).
- Flow = primary + secondary (ore equivalent for metals); outflow = waste.

Rendered with `gt`.

- **Inputs**: `Results/MC/mc_results.parquet`, `hist_flows_model_basis.csv`; `Parameters/MISO-Stock/stock_2024_total.csv`; `Parameters/UNEP-Materials/materials_region_DMC.csv`; `Inputs/Dict_Materials.xlsx`.
- **Outputs**: `Figures/Table1_Results.html`.

## Supporting figures (`Supporting-Figures/`)

Outputs go to `Figures/Supporting-Figures/` (+ SVG). S03-S09, S17 and S20-S26 are written by other scripts (see `Figures/Supporting-Figures/README.md`).

| Script | Figure | What it computes | Main inputs |
|---|---|---|---|
| `S01-S02 - CumulativeIntensityCurve.R` | S01 | 2024 step curves. Regions sorted by DMC per capita (x = cumulative population) and by DMC per GDP (x = cumulative GDP), each region stacked by material group. | UNEP DMC, population, GDP, `Dict_Materials.xlsx` |
| `S10 - SamplingBounds.R` | S10 | Range of every sampled variable in absolute units (2024 value × ratio bounds): population, GDP per capita, flow intensities by SSP × region, stock intensities, recovery rates, room shares, target years, ore grades, lifetimes. Open circle = 2024 value. | `flow_ratio_bounds.csv`, `stock_ratio_bounds.csv`, `ssp_drivers.csv`, `*_Assumptions.xlsx` |
| `S11 - FlowTimeSeries.R` | S11 | Envelopes (P5-P95, P25-P75, min-max, median) of global total inflow, primary, secondary and in-use stock. | `mc_results.parquet` |
| `S12 - TimeSeriesProjection.R` | S12 | Per material group: runs of primary consumption 1990-2060, joined to 2024 history, with 3-year smoothing for display. Highlights absolute decoupling (total CAGR < 0) and high growth (top 10% in 2060). Right: scatter of the two main input levers of that group. | `mc_results.parquet`, `mc_input_matrix.csv`, `mc_decoupling.parquet`, `intensity_world_bounds.csv`, UNEP DMC, stocks, `historical_secondary_flows.csv` |
| `S13-S14 - MaterialContribution.R` | S13, S14 | Material composition vs total consumption level, in 2050 (S13) and cumulative 2025-2060 (S14). Also material mix over time (`Figures/Simulation/05_*`). | `mc_results.parquet` |
| `S15 - RegionContribution.R` | S15 | Same as S13-S14 by region; S15 = regional shares over time. | `mc_results.parquet` |
| `S16 - Density.R` | S16 | Distributions of 2050 primary consumption and stock by region (violins) with 2024 reference points. Also global and per-material densities and a multi-page PDF of conditional densities per parameter (`Figures/Simulation/07_*`). | `mc_results.parquet`, `mc_input_matrix.csv`, UNEP DMC, `stock_2024_total.csv` |
| `S18 - RegionalAnalysis.R` | S18 | Population, GDP per capita and primary consumption per material group in 2060 by region: median bars, 5-95% intervals, and the 2025 median as a point. | `mc_results.parquet`, GDP, population, `ssp_drivers.csv` |
| `S19 - CumulativeExtraction.R` | S19 | Biomass annual consumption with land-equivalent axis (assumed yields); cumulative fossil fuels vs proved reserves (oil equivalent); cumulative metals and minerals vs 1990-2025 world extraction. Median + 5-95%. | `mc_results.parquet`, `materials_world_DE.csv` |
| `S27-S28 - RegionalTimeseries.R` | S27, S28 | S27: historical DMC by region, stacked by material group. S28: 10-year average DMC growth, `(DMC_t / DMC_t−10)^(1/10) − 1`, by region and by material. | UNEP DMC, GDP, population |

Raw data: S18 → `Figures/RawData/S18_region_{totals,point}.csv`; S19 → `Figures/RawData/S19{a,b,c,d}_*.csv`.
