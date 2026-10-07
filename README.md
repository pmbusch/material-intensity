# Global material consumption persists despite optimal efficiency and circularity

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbuschh@gmail.com.

# Organization

* **Inputs**: raw data inputs and assumption workbooks (see [Inputs/README.md](Inputs/README.md)).
* **Parameters**: intermediate datasets produced by the scripts and consumed downstream (see [Parameters/README.md](Parameters/README.md)).
* **Results**: Monte Carlo model outputs (`Results/MC/`).
* **Figures**: main-text figures (`Figures/Fig1` to `Fig5`, `Table1_Results.html`). Running the pipeline also writes the supporting figures S01-S21 to `Figures/Supporting-Figures/` and SVG versions of all figures to `Figures/SVG/` (not tracked in git for now).
* **Scripts**: all code, organised by pipeline stage. Each script starts with a description of its purpose.

Folders or scripts prefixed `98` (exploratory) or `99` (deprecated), and `old/` folders, are not part of the main analysis and are not tracked in git.

# Reproducing the analysis

From the project root:

```
Rscript Scripts/00-RunAll.R        # full pipeline, each script in a fresh R process
Rscript Scripts/00-RunAll.R 22     # resume from script #22 of the list
```

| Stage | Folder | What it does | Main outputs |
|---|---|---|---|
| 0 | `Scripts/00-*.R`, `00-Functions/` | Libraries, theme, palettes, shared constants, DSM functions | sourced by every script |
| 1 | `01-PrepareData/` | Aggregate UNEP material flows, UN population, World Bank GDP and MISO2 stocks to regions | `Parameters/UNEP-Materials`, `UN-Population`, `Worldbank-GDP`, `MISO-Stock` |
| 2 | `02-HistoricalStock/` | End-use split of DMC, ore-grade factor g, historical stock-flow model 1970-2024, secondary flows | `Parameters/Intermediate`, `Parameters/MISO-Stock` |
| 3 | `03-SSP Trajectories/` | SSP population/GDP drivers and ScenarioMIP intensity ratios 2060/2025 | `Parameters/IIASA-Trajectories` |
| 4 | `04-Simulation/` | Latin-hypercube sampling, power-sector inputs, Monte Carlo DSM runs, diagnostics, decoupling classification | `Parameters/Simulation`, `Results/MC` |
| 5 | `05-Figures/` | Main figures, Table 1 and supporting figures | `Figures/` |

Scripts must run in the order listed in `00-RunAll.R`; several read files written by earlier ones. The main dependencies:

* `04-Simulation/01-Sampling.R` → `01b-PowerSector.R`: 01b needs `r10_region_weights.csv` (with its `gdp` column) and `flow_ratio_bounds.csv`. It also reads stage 3 outputs (`ssp_long_clean.csv`, `metrics_levels_raw.csv`, `summary_ratio_2060_2025.csv`).
* `01b-PowerSector.R` → `02-RunSimulations.R`: 02 reads the `Parameters/Simulation/power_*.csv` files (capacity paths, fossil indices, material intensities, 2025 carve-out from civil engineering).
* `02-RunSimulations.R` → figures: Figure 5 reads `Results/MC/mc_power_run_link.csv` (per-run fossil index and 2060 warming) in addition to `mc_results.parquet`.

Power sector: generation capacity (13 technologies) and stationary batteries are modelled as explicit stocks driven by each run's coal/gas/oil draws. No new parameters are sampled. All fixed coefficients (lifetimes, battery hours and coverage, energy densities, material mappings) are in the "Power sector" section of `00-Parameters.R`.

All model constants and Monte Carlo settings live in `Scripts/04-Simulation/00-Parameters.R` (`N_RUNS`, seed, sampling bounds read from `Inputs/MC_Assumptions.xlsx`). Colour palettes and the projection horizon live in `Scripts/00-CommonParameters.R`.

# Figures

| Figure | Script (`Scripts/05-Figures/`) |
|---|---|
| Fig1 - Historical Timeseries | `Figure 1 - HistoricalTimeseries.R` |
| Fig2 - Assumptions | `Figure 2 - Assumptions.R` |
| Fig3 - Contours-GrowthRate | `Figure 3 - Contours-GrowthRate.R` |
| Fig4 - Climate | `Figure 4 - Climate - PrepareData.R`, then `Figure 4 - Climate.R` |
| Fig5 - Sensitivity | `Figure 5 - Sensitivity - PrepareData.R`, then `Figure 5 - Sensitivity.R` |
| Table 1 | `Table 1 - Results.R` |

Supporting figures S01-S21 are written by the scripts in `Scripts/05-Figures/Supporting-Figures/` and by the pipeline scripts in stages 2-3. The header of each script names the figures it writes.

# License
This project is covered under the **MIT License**
