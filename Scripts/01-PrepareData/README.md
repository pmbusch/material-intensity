# 01-PrepareData

Aggregates the raw country-level datasets to the 8 model regions, plus a world total and a fixed list of about 80 selected countries. Every later stage reads these outputs instead of the raw files. The scripts are independent of each other and run in the order below.

Country-to-region maps come from `Inputs/Dict_Countries.xlsx`, one sheet per source (`UNEP_Agg`, `UN_Agg` + `Countries`, `MISO_Agg`, `R10_Agg`).

## `01-Aggregate_UNEP.R`: material flows

1. Reads the UNEP IRP export and pivots the year columns to long format, converting t to Mt.
2. Drops supra-national aggregates (World, continents, income groups, ...).
3. Joins ISO3 and Region via `UNEP_Agg` and drops countries with no region.
4. Fixes historical political changes, per flow indicator:
   - Drops the aggregate after its successors appear: USSR (1992), Yugoslavia (1992), Serbia & Montenegro (2006), Czechoslovakia (1993), former Sudan (2012), former Ethiopia (1993), Netherlands Antilles (2012).
   - Merges North and South Yemen into YEM before 1990.
5. For each indicator (DE, DMC, DMI, EXP, IMP, PTB): checks for duplicate rows, then sums to region, world and selected countries.

- **Inputs**: `Inputs/UNEP/mfa13_export.csv`, `Inputs/Dict_Countries.xlsx` (`UNEP_Agg`).
- **Outputs** (`Parameters/UNEP-Materials/`): `materials_{region,world,country}_{DE,DMC,DMI,EXP,IMP,PTB}.csv` (columns: Region/ISO3, year, material_category, `<FC>_Mt`).

## `02-Aggregate_UN.R`: population

1. Builds the ISO3 → Region map by joining the `Countries` and `UN_Agg` sheets on the UN country name.
2. Reads the WPP 2024 "Estimates" and "Medium variant" sheets and converts thousands to persons.
3. Historical series = Estimates 1970-2023 + Medium variant 2024; forecast = Medium variant 2025-2100. The USSR, Yugoslavia, Czechoslovakia and Serbia & Montenegro aggregates are dropped after their split years.
4. Sums to region and world (only countries with a region) and saves the selected countries.

- **Inputs**: `Inputs/UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx`, `Inputs/Dict_Countries.xlsx`.
- **Outputs** (`Parameters/UN-Population/`): `population_{region,world,country}_{historical,forecast}.csv`.

## `03-Aggregate_GDP.R`: GDP

Uses the World Bank regional aggregates as the base and moves individual countries between regions to match the model regions.

1. Reads GDP (constant 2015 USD), 1970-2024.
2. Takes the WB aggregates EAS, ECS, LCN, MEA, NAC, SAS, SSF as the base regions.
3. For each moved country, subtracts its GDP from the source aggregate and adds it to the target region:
   - Central Asia (KAZ, KGZ, TJK, TKM, UZB): ECS → East Asia
   - Caucasus (ARM, AZE, GEO): ECS → Middle East & North Africa
   - MLT: MEA → Europe & Russia
   - PAK, AFG: MEA → South Asia
   - AUS, NZL and the Pacific islands: EAS → Oceania (new region)
4. Prints the regional sum against the WB World total as a check.
5. Saves region, world (sum of regions) and selected countries.

- **Inputs**: `Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls`.
- **Outputs** (`Parameters/Worldbank-GDP/`): `gdp_region.csv`, `gdp_world.csv`, `gdp_country.csv` (column `GDP_2015USD`).

## `05_Aggregate_MISO.R`: MISO2 stocks and flows

1. Reads the MISO2 global data (kt, 1900-2016) and pivots to long format.
2. Recodes sectors to end uses (buildings, civil_infrastructure, machinery, short_lived) and materials to 4 groups.
3. Joins Region via `MISO_Agg` and drops unmatched countries.
4. Splits the data into three series: stock (`S10_stock_enduse`), gross additions to stock (`F_9_10_GAS_enduse`) and end-of-life outflows (`F_10_11_supply_EoL_waste_enduse`).
5. Sums each series to Region × material × end use × year and converts kt to Mt.
6. Saves all 4 materials (`*_all.csv`, for reference) and the model scope (metal ores and non-metallic minerals).
7. Validation: stops if the 2016 global stock (all materials) is outside 1,000-1,100 Gt. The MISO2 reference value is 1,093 Gt.

- **Inputs**: `Inputs/MISO/miso2_global_data_v1.csv`, `Inputs/Dict_Countries.xlsx` (`MISO_Agg`).
- **Outputs** (`Parameters/MISO-Stock/`): `MISO_{stock,flows,outflows}_regional.csv` and `MISO_{stock,flows,outflows}_regional_all.csv`.

## `06-Aggregate_EIA_Capacity.R`: installed power capacity

1. Builds the ISO3 → region map from the `R10_Agg` and `UNEP_Agg` sheets, plus four EIA-only codes (West/East Germany, Kosovo, Netherlands Antilles).
2. Reads the EIA International capacity export (million kW = GW) and extracts ISO3 from the API code. Missing values (`NA`, `--`) are set to 0.
3. Prints the share of EIA world capacity covered by mapped countries.
4. Sums to region × year × EIA technology (nuclear, fossil, hydro, geothermal, solar, wind, biomass, tide_wave, pumped_storage).

- **Inputs**: `Inputs/EIA/INT-Export-10-06-2026_16-08-02.csv`, `Inputs/Dict_Countries.xlsx`.
- **Outputs**: `Parameters/EIA-Capacity/capacity_region_historical.csv` (region, year, eia_tech, gw). Used by Figure 2 for the historical power-sector stock.
