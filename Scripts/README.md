# Scripts

All R code, organised by pipeline stage. `00-RunAll.R` runs every stage in order, each script in a fresh `Rscript` process, from the project root. Paths below are relative to the project root.

| Folder | Stage | Documentation |
|---|---|---|
| `00-*.R`, `00b-Theme.R`, `00-Functions/` | 0. Shared setup: libraries, theme, palettes, constants, DSM functions | this file |
| `01-PrepareData/` | 1. Aggregate raw data (materials, population, GDP, stocks, power capacity) to the 8 model regions | [01-PrepareData/README.md](01-PrepareData/README.md) |
| `02-HistoricalStock/` | 2. Historical end-use split, ore grade, stock-flow model 1970-2024, secondary flows | [02-HistoricalStock/README.md](02-HistoricalStock/README.md) |
| `03-SSP Trajectories/` | 3. SSP population/GDP drivers and ScenarioMIP intensity ratios | [03-SSP Trajectories/README.md](<03-SSP Trajectories/README.md>) |
| `04-Simulation/` | 4. Monte Carlo sampling, power sector, simulation, diagnostics, decoupling | [04-Simulation/README.md](04-Simulation/README.md) |
| `05-Figures/` | 5. Main figures, Table 1, supporting figures | [05-Figures/README.md](05-Figures/README.md) |
| `98-Exploratory/`, `99-Deprecated/`, `05-Figures/Deprecated/` | Not part of the pipeline (not tracked in git) | none |

## Conventions shared by all scripts

- Every script starts with `source("Scripts/00-Libraries.R")`, which also loads the theme, the common parameters and every file in `00-Functions/`.
- **Regions**: 8 model regions (East Asia, Europe & Russia, North America, Latin America, Sub-Saharan Africa, Middle East & North Africa, South Asia, Oceania). Country-to-region maps are sheets of `Inputs/Dict_Countries.xlsx`, one per data source.
- **Material groups**: biomass and fossil fuels are flow materials (Kaya identity, no stock). Metal ores (split into ferrous `Metal_Fe` and non-ferrous `Metal_NonFe`) and non-metallic minerals are stock materials (dynamic stock model).
- **End uses**: 4 super-categories (buildings, civil infrastructure, machinery, short-lived products) split into 8 sub-uses (residential, non-residential, roads, civil engineering, machinery & equipment, vehicles, durables, packaging). The simulation adds 4 power-sector end uses.
- **Units**: mass in Mt unless stated; GDP in constant 2015 USD (World Bank) or 2015 USD PPP (SSP drivers).
- **Years**: history 1970-2024, base year 2024, projection 2025 to `FORECAST_END` (2060).

## Stage 0: shared setup

### `00-RunAll.R`

Runs the whole analysis.

1. Holds the ordered list of 50 scripts (stages 1 to 6; stage 6 = supporting figures).
2. Reads an optional start index from the command line (`Rscript Scripts/00-RunAll.R 22` resumes at script 22).
3. Runs each script with `system2(Rscript, ...)`, prints the elapsed time, and stops at the first non-zero exit code with the command to resume.

- **Inputs**: none (calls the other scripts).
- **Outputs**: none directly.

### `00-Libraries.R`

1. Loads the packages (tidyverse, readxl, data.table, openxlsx, scales, sf, ggrepel, geomtextpath, glmnet, and others). Installation lines are commented out.
2. Sources `00b-Theme.R` and `00-CommonParameters.R`.
3. Sources every `.R` file in `00-Functions/`.
4. Defines two SVG helpers:
   - `clean_svg()` removes `textLength` and `lengthAdjust` attributes, so text is not stretched when the SVG is edited.
   - `group_svg_layers()` moves SVG elements into three Inkscape layers (Grid, Data, Labels), then calls `clean_svg()`.

### `00-CommonParameters.R`

Constants used across stages. No calculations.

- `FORECAST_END = 2060`: projection horizon for the simulation and all figures.
- `GRADE_ORE_FE_2024 = 0.4`, `GRADE_ORE_NONFE_2024 = 0.016`: 2024 ore-to-metal factors. The historical series ramps to these, and each Monte Carlo run starts from them.
- Colour palettes: `PALETTE_REGIONS`, `PALETTE_MATERIALS` (22 UNEP categories), `PALETTE_MATERIAL_GROUPS`, `PALETTE_ENDUSE`, `PALETTE_SUBENDUSE`, `SSP_COLORS`, `PALETTE_DECOUPLING`.
- `SSP_SAMPLED = SSP1, SSP2, SSP3, SSP5`. SSP4 is excluded because ScenarioMIP has a single run for it.

### `00b-Theme.R`

ggplot2 theme and text-size rules.

- `theme_pb(preset)` with presets `small`/`wide` (7 pt floor) and `largeFont` (8 pt floor). Titles are 1.4 times the floor, so the max/min font ratio stays below 1.5.
- `pb_annot_size(preset, pt)` converts a point size to `geom_text` mm units and never goes below the preset floor.
- `pb_set_geom_defaults(preset)` sets the default `geom_text`/`geom_label` size. The file calls it once with `largeFont`.

### `00-Functions/dsm_functions.R`

Dynamic stock model (DSM) functions.

- `weibull_survival()`, `get_survival()`: Weibull survival with the scale set from the mean lifetime, `lambda = mean_life / gamma(1 + 1/k)`.
- `run_forward_dsm_fast()`: the forward, stock-driven DSM used by the Monte Carlo (`04-Simulation/02-RunSimulations.R`). For one region × material × sub-use:
  1. Re-ages the 2024 cohorts onto the run's sampled lifetime curve with the cumulative exposure model. Each cohort gets the equivalent age `s* = lambda_new (a / lambda_hist)^(k_hist / k_new)`, so the surviving fraction is unchanged at the 2024 to 2025 seam.
  2. Each year: surviving stock from old and new cohorts; waste = decline of each cohort since the previous year.
  3. Production = max(0, target stock − surviving stock). Replacement = min(waste, production); new additions = production − replacement (an accounting split only).
  4. The full production enters the stock as a new age-0 cohort.
  5. Returns year, total stock, new additions, replacement, production and waste (vectors 2025-2060).
- `run_forward_dsm()`: tibble-based version of the same model. It is not called by any pipeline script.
- `allocate_eol(group, demand, waste, rate, room)`: end-of-life allocation, used for both metals and minerals. Within each group (region × year):
  - pool = Σ rate × waste
  - secondary = min(pool, Σ room), split across rows in proportion to room
  - primary = demand − secondary
  - surplus (recovered material that no demand can absorb) = pool − secondary
  - not recovered = (1 − rate) × waste

  Mass balance closes per group: waste = not recovered + secondary + surplus.
