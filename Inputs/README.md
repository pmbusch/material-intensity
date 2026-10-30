# Inputs: data sources and setup

All raw inputs sit in this folder, in the sub-folders listed below. Files under ~50 MB are tracked in git. Larger files are not tracked: download them from the public link and save them under the exact path shown, then run `Rscript Scripts/00-RunAll.R` from the project root.

## Files needed by the pipeline

| Path (under `Inputs/`) | In git? | Source / download | Used by |
|---|---|---|---|
| `UNEP/mfa13_export.csv` | yes | UNEP IRP Global Material Flows Database, https://unep-irp.fineprint.global/mfa13 (export all countries, 1970-2024, 13-category detail, all flow indicators) | `01-PrepareData/01-Aggregate_UNEP.R` |
| `UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx` | yes | UN World Population Prospects 2024, https://population.un.org/wpp/downloads/files/wpp2024/excel/ | `01-PrepareData/02-Aggregate_UN.R` |
| `WorldBank/API_NY.GDP.MKTP.PP.KD_DS2_en_excel_v2_2811.xls` | yes | World Bank WDI, GDP, PPP (constant 2021 international $), https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD (Download > Excel) | `01-PrepareData/03-Aggregate_GDP.R`, `04-Simulation/01-Sampling.R` |
| `WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls` | yes | World Bank WDI, GDP (constant 2015 US$), https://data.worldbank.org/indicator/NY.GDP.MKTP.KD (Download > Excel) | `01-PrepareData/03-Aggregate_GDP.R` (pre-1990 PPP backcast; MER column) |
| `MISO/miso2_global_data_v1.csv` | yes | MISO2 model data, https://zenodo.org/records/12794253 | `01-PrepareData/04-Aggregate_MISO.R` |
| `MISO/SI_Wiedenhofer2024_globalStocks.xlsx` | yes | Supporting information of Wiedenhofer et al. (2024), https://onlinelibrary.wiley.com/doi/10.1111/jiec.13575 | `02-HistoricalStock/02-UNEP_enduse_detail.R` |
| `IIASA_SSP/ssp_basic_drivers_release_3.2_full.xlsx` | **no (60 MB)** | IIASA SSP Scenario Database v3.2, https://data.ece.iiasa.ac.at/ssp (Downloads > basic drivers release 3.2) | `03-SSP Trajectories/01-preprocess_ssp_drivers.R` |
| `IIASA_SSP/2026-MIP-CMIP7/*.csv` (6 files) | yes | ScenarioMIP for CMIP7 IAM scenarios, IIASA ScenarioMIP Scenario Explorer, https://scenariomip.apps.ece.iiasa.ac.at/ | `03-SSP Trajectories/02-load_and_check.R`, `09-climate_figures.R`, `04-Simulation/02-PowerSector.R` (climate file), `05-Figures/Figure 4 - Climate*.R` |
| `MC_Assumptions.xlsx` | yes | project assumptions: Monte Carlo sampling bounds (Parameters, Stock_Bounds, Lifetimes sheets). Parameters include recycling rate and ore grade per metal (Fe, Cu, Al, other non-Fe) | `04-Simulation/*`, figures |
| `Recycling_Assumptions.xlsx` | yes | project assumptions: end-of-life recycling and downcycling rates (sheet `Recycling_EOL` columns read: `Recycling_rate_Fe`, `Copper EOL-RR`, `Aluminum EOL-RR`, `OtherNonFE EOL-RR`; sheet `Downcycling`) | `04-Simulation/*`, figures |
| `Dict_Countries.xlsx`, `Dict_Materials.xlsx` | yes | project dictionaries: country-to-region and material-category mappings | most scripts |
| `EIA/INT-Export-10-06-2026_16-08-02.csv` | yes | U.S. EIA International Energy Statistics, installed electricity capacity by country and technology (million kW), https://www.eia.gov/international/data/world | `01-PrepareData/05-Aggregate_EIA_Capacity.R` |
| `Wang2023_material_intensity_technology.xlsx` | yes | literature dataset: material intensity (t/GW) of power-generation technologies, sheet "Master - all intensity values" | `04-Simulation/02-PowerSector.R` |
| `Battery_intensity.xlsx` | yes | battery material intensity (kg/kWh) by cathode chemistry, sheet "Battery_Intensity" (LFP and NMC 811 used) | `04-Simulation/02-PowerSector.R` |
| `MaterialFlows/*bymaterialgroup.csv` | **no** | materialflows.net domestic extraction by country and Level 3 material, https://www.materialflows.net/visualisation-centre/data-visualisations/ (output `Parameters/MaterialFlows/DE_detail_region.csv` is tracked) | `01-PrepareData/06-Aggregate_MaterialFlows.R` |
| `SP/{IronOre,Copper,Bauxite}_*.xls` | **no (proprietary)** | S&P Capital IQ Pro, Mineral Intelligence deposit exports (reserves and resources, grades, development stage) | `01-PrepareData/08-IronOre_GradeCurve.R`, `09-NonFerrous_GradeCurve.R` |

Not used by the main pipeline: `UNDP/hdr-data.xlsx` (exploratory HDI figure), `WorldBank/API_NY.GDP.PCAP.PP.KD*`, `API_SP.URB*`, `CLASS_*` (alternatives and classification), `IIASA_SSP/2018_Quantification/`, `Cheng2025_*` (literature reference), and the `Temp/` and `oldData/` folders.

## Citations

1. **Material flows.** Schandl, H. et al. (2024). Global material flows and resource productivity: The 2024 update. *Journal of Industrial Ecology*. https://doi.org/10.1111/jiec.13593. Indicators: DE (domestic extraction), DMC (domestic material consumption = DE + imports - exports; primary indicator), DMI, IMP, EXP, PTB. Excavated earthen materials are excluded from all analyses. Units: tonnes.
2. **Population.** United Nations, DESA, Population Division (2024). World Population Prospects 2024. Medium variant; historical 1970-2024. Units: thousands of persons.
3. **GDP.** World Bank (2024). World Development Indicators, NY.GDP.MKTP.PP.KD, GDP, PPP (constant 2021 international $), rebased to 2017 international $ with the US GDP deflator (U.S. Bureau of Economic Analysis, FRED series A191RD3A086NBEA); 1970-1989 backcast with NY.GDP.MKTP.KD (constant 2015 US$, market exchange rates) real growth. Regional aggregates are built from World Bank regions with country corrections. Country classification: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
4. **Material stocks (MISO2).** Wiedenhofer, D. et al. (2024). *Journal of Industrial Ecology*. https://doi.org/10.1111/jiec.13575. Data: https://zenodo.org/records/12794253
5. **SSP drivers.** IIASA SSP Scenario Database, release 3.2 (population and GDP projections). https://data.ece.iiasa.ac.at/ssp
6. **ScenarioMIP (CMIP7).** IAM scenario data (energy, capacity, food, regional, climate variables) from the IIASA ScenarioMIP Scenario Explorer, https://scenariomip.apps.ece.iiasa.ac.at/
