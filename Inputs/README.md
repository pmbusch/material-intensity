# Inputs: data sources and setup

All raw inputs sit in this folder, in the sub-folders listed below. Files under ~50 MB are tracked in git. Larger files are not tracked: download them from the public link and save them under the exact path shown, then run `Rscript Scripts/00-RunAll.R` from the project root.

## Files needed by the pipeline

| Path (under `Inputs/`) | In git? | Source / download | Used by |
|---|---|---|---|
| `UNEP/mfa13_export.csv` | yes | UNEP IRP Global Material Flows Database, https://unep-irp.fineprint.global/mfa13 (export all countries, 1970-2024, 13-category detail, all flow indicators) | `01-PrepareData/01-Aggregate_UNEP.R` |
| `UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx` | yes | UN World Population Prospects 2024, https://population.un.org/wpp/downloads/files/wpp2024/excel/ | `01-PrepareData/02-Aggregate_UN.R` |
| `WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls` | yes | World Bank WDI, GDP (constant 2015 US$), https://data.worldbank.org/indicator/NY.GDP.MKTP.KD (Download > Excel) | `01-PrepareData/03-Aggregate_GDP.R`, `04-Simulation/01-Sampling.R` |
| `MISO/miso2_global_data_v1.csv` | yes | MISO2 model data, https://zenodo.org/records/12794253 | `01-PrepareData/05_Aggregate_MISO.R` |
| `MISO/SI_Wiedenhofer2024_globalStocks.xlsx` | yes | Supporting information of Wiedenhofer et al. (2024), https://onlinelibrary.wiley.com/doi/10.1111/jiec.13575 | `02-HistoricalStock/01b_UNEP_enduse_detail.R` |
| `IIASA_SSP/ssp_basic_drivers_release_3.2_full.xlsx` | **no (60 MB)** | IIASA SSP Scenario Database v3.2, https://data.ece.iiasa.ac.at/ssp (Downloads > basic drivers release 3.2) | `03-SSP Trajectories/00_preprocess_ssp_drivers.R` |
| `IIASA_SSP/2026-MIP-CMIP7/*.csv` (6 files) | yes | ScenarioMIP for CMIP7 IAM scenarios, IIASA ScenarioMIP Scenario Explorer, https://scenariomip.apps.ece.iiasa.ac.at/ | `03-SSP Trajectories/01_load_and_check.R`, `05_climate_figures.R`, `05-Figures/Figure 4 - Climate*.R` |
| `MC_Assumptions.xlsx` | yes | project assumptions: Monte Carlo sampling bounds (Parameters, Stock_Bounds, Lifetimes sheets) | `04-Simulation/*`, figures |
| `MatIntensity_Assumptions.xlsx` | yes | project assumptions: material-intensity bounds by material group | `04-Simulation/*`, figures |
| `Recycling_Assumptions.xlsx` | yes | project assumptions: end-of-life recycling and downcycling rates | `04-Simulation/*`, figures |
| `Dict_Countries.xlsx`, `Dict_Materials.xlsx` | yes | project dictionaries: country-to-region and material-category mappings | most scripts |

Not used by the main pipeline: `UNDP/hdr-data.xlsx` (exploratory HDI figure), `WorldBank/API_NY.GDP.PCAP.PP.KD*`, `API_SP.URB*`, `CLASS_*` (alternatives and classification), `IIASA_SSP/2018_Quantification/`, `Cheng2025_*`/`Wang2023_*` (literature reference), and the `Temp/`, `MaterialFlows/`, `oldData/` and `99-Deprecated/` folders.

## Citations

1. **Material flows.** Schandl, H. et al. (2024). Global material flows and resource productivity: The 2024 update. *Journal of Industrial Ecology*. https://doi.org/10.1111/jiec.13593. Indicators: DE (domestic extraction), DMC (domestic material consumption = DE + imports - exports; primary indicator), DMI, IMP, EXP, PTB. Excavated earthen materials are excluded from all analyses. Units: tonnes.
2. **Population.** United Nations, DESA, Population Division (2024). World Population Prospects 2024. Medium variant; historical 1970-2024. Units: thousands of persons.
3. **GDP.** World Bank (2024). World Development Indicators, NY.GDP.MKTP.KD, GDP in constant 2015 US$ (market exchange rates). Regional aggregates are built from World Bank regions with country corrections. Country classification: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
4. **Material stocks (MISO2).** Wiedenhofer, D. et al. (2024). *Journal of Industrial Ecology*. https://doi.org/10.1111/jiec.13575. Data: https://zenodo.org/records/12794253
5. **SSP drivers.** IIASA SSP Scenario Database, release 3.2 (population and GDP projections). https://data.ece.iiasa.ac.at/ssp
6. **ScenarioMIP (CMIP7).** IAM scenario data (energy, capacity, food, regional, climate variables) from the IIASA ScenarioMIP Scenario Explorer, https://scenariomip.apps.ece.iiasa.ac.at/
