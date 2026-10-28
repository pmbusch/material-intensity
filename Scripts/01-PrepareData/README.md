# Documentation: 01-PrepareData

**Goal**: Read raw (original) input data and convert it to usable parameters for the model. All input data is in original form. Common actions are regional aggregation (8 global regions). Run the scripts in numbering order: later scripts read files written by earlier ones (`07` reads `06`; `08` and `09` read `07`).

Country-to-region maps come from `Inputs/Dict_Countries.xlsx`, one sheet per source (`UNEP_Agg`, `UN_Agg` + `Countries`, `MISO_Agg`, `MaterialFlows_Agg`, `R10_Agg`).

Scripts `06`, `08` and `09` need local data that is not in git (materialflows.net raw files; S&P Capital IQ, proprietary). Their outputs in `Parameters/MaterialFlows/` and `Parameters/SP/` are tracked, except the deposit-level S&P tables. `00-RunAll.R` therefore lists them commented out.

## `01-Aggregate_UNEP.R`: material flows

1. Reads the UNEP IRP material flow database export and **pivots** the year columns to long format, converting ton to Mt (million tons).
2. Drops supra-national aggregates (World, continents, income groups, ...).
3. Joins ISO3 and Region via `UNEP_Agg` and drops countries with no region.
4. Fixes historical political changes (country level):
   - Drops the aggregate after its successors appear: USSR (1992), Yugoslavia (1992), Serbia & Montenegro (2006), Czechoslovakia (1993), former Sudan (2012), former Ethiopia (1993), Netherlands Antilles (2012).
   - Merges North and South Yemen into YEM before 1990.
5. For each indicator (DE, DMC, DMI, EXP, IMP, PTB): checks for duplicate rows, then sums to region, world and selected countries.

- **Inputs**: `Inputs/UNEP/mfa13_export.csv`, `Inputs/Dict_Countries.xlsx` (`UNEP_Agg`). Available at https://unep-irp-resourcepanel.resource-use.global/mfa13/
- **Outputs** (`Parameters/UNEP-Materials/`): `materials_{region,world,country}_{DE,DMC,DMI,EXP,IMP,PTB}.csv` (columns: Region/ISO3, year, material_category, `<FC>_Mt`).

## `02-Aggregate_UN.R`: population

Aggregates population to global regions.

1. Builds the ISO3 → Region map by joining the `Countries` and `UN_Agg` sheets on the UN country name.
2. Reads the WPP 2024 "Estimates" and "Medium variant" sheets and converts thousands to persons.
3. Historical series = Estimates 1970-2023 + Medium variant 2024; forecast = Medium variant 2025-2100. The USSR, Yugoslavia, Czechoslovakia and Serbia & Montenegro aggregates are dropped after their split years.
4. Sums to region and world (only countries with a region) and saves the selected countries.

- **Inputs**: `Inputs/UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx`, `Inputs/Dict_Countries.xlsx`. Available at https://www.un-ilibrary.org/content/books/9789211065138.
- **Outputs** (`Parameters/UN-Population/`): `population_{region,world,country}_{historical,forecast}.csv`.

## `03-Aggregate_GDP.R`: GDP

Uses the World Bank regional aggregates as the base and moves individual countries between regions to match the model regions.

1. Reads **GDP, PPP (constant 2021 intl $)**, 1990-2024, and **GDP, MER (constant 2015 USD)**, 1970-2024. PPP before each code's first PPP year (1990 for all regional aggregates) is backcast as MER x that code's PPP/MER ratio in its first PPP year (identical real growth, continuous splice), then rebased to constant 2017 intl $ with the US GDP deflator (FRED A191RD3A086NBEA, 2021 = 110.186, 2017 = 100).
2. Takes the WB aggregates EAS, ECS, LCN, MEA, NAC, SAS, SSF as the base regions.
3. For each moved country, subtracts its GDP from the source aggregate and adds it to the target region:
   - Central Asia (KAZ, KGZ, TJK, TKM, UZB): ECS → East Asia
   - Caucasus (ARM, AZE, GEO): ECS → Middle East & North Africa
   - MLT: MEA → Europe & Russia
   - PAK, AFG: MEA → South Asia
   - AUS, NZL and the Pacific islands: EAS → Oceania (new region)
4. Prints the regional sum against the WB World total as a check.
5. Saves region, world (sum of regions) and selected countries.

- **Inputs**: `Inputs/WorldBank/API_NY.GDP.MKTP.PP.KD_DS2_en_excel_v2_2811.xls` (https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD) and `Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls` (https://data.worldbank.org/indicator/NY.GDP.MKTP.KD).
- **Outputs** (`Parameters/Worldbank-GDP/`): `gdp_region.csv`, `gdp_world.csv`, `gdp_country.csv` (columns `GDP_PPP2017USD`, main GDP basis; `GDP_MER2015USD`, kept to convert MER-based inputs).

## `04-Aggregate_MISO.R`: MISO2 stocks and flows

1. Reads the MISO2 global data (kt, 1900-2016) and pivots to long format.
2. Recodes sectors to end uses (buildings, civil_infrastructure, machinery, short_lived) and materials to the 4 categories: biomass, fossil fuels, metal and non-metallic minerals.
3. Joins Region via `MISO_Agg` and drops unmatched countries.
4. Splits the data into three series: stock (`S10_stock_enduse`), gross additions to stock (`F_9_10_GAS_enduse`) and end-of-life outflows (`F_10_11_supply_EoL_waste_enduse`).
5. Sums each series to Region × material × end use × year and converts kt to Mt.
6. Saves all 4 materials (`*_all.csv`, for reference) and the model scope (metal ores and non-metallic minerals).
7. Validation: stops if the 2016 global stock (all materials) is outside 1,000-1,100 Gt. The MISO2 reference value is 1,093 Gt.

- **Inputs**: `Inputs/MISO/miso2_global_data_v1.csv`, `Inputs/Dict_Countries.xlsx` (`MISO_Agg`). Available at: https://zenodo.org/records/12794253
- **Outputs** (`Parameters/MISO-Stock/`): `MISO_{stock,flows,outflows}_regional.csv` and `MISO_{stock,flows,outflows}_regional_all.csv`.

## `05-Aggregate_EIA_Capacity.R`: historical installed power capacity

1. Builds the ISO3 → region map from the `R10_Agg` and `UNEP_Agg` sheets, plus four EIA-only codes (West/East Germany, Kosovo, Netherlands Antilles).
2. Reads the EIA International capacity (1980 to 2024) export (million kW = GW) and extracts ISO3 from the API code. Missing values (`NA`, `--`) are set to 0.
3. Prints the share of EIA world capacity covered by mapped countries.
4. Sums to region × year × EIA technology (nuclear, fossil, hydro, geothermal, solar, wind, biomass, tide_wave, pumped_storage).

- **Inputs**: `Inputs/EIA/INT-Export-10-06-2026_16-08-02.csv`, `Inputs/Dict_Countries.xlsx`. Available at https://www.eia.gov/international/data/world/electricity/electricity-capacity.
- **Outputs**: `Parameters/EIA-Capacity/capacity_region_historical.csv` (region, year, eia_tech, gw). Used by Figure 2 for the historical power-sector stock.

## `06-Aggregate_MaterialFlows.R`: domestic extraction at full material detail

1. Lists the materialflows.net files (one per country × year) and keeps the latest download for each country-year. Drops North/South Yemen, which overlap the unified Yemen series.
2. Reads all files (a few minutes). Countries are matched to regions **by name**:
   - The country name in each file name (e.g. `RussianFederation`) is joined to the `country` column of sheet `MaterialFlows_Agg` (columns `country`, `ISO3`, `Region`).
   - The `ISO3` column is not used in the join.
   - `Region` holds the same 8 model regions as the other sheets.
   - Countries missing from the sheet are printed and dropped.
3. Prints the year span of former countries and their successors to check for double counting.
4. Keeps the Level 3 rows (e.g. Copper ores) with their Level 2 parent (e.g. Non-ferrous ores), and prints the Level 3 sum against the Level 2 totals.
5. Sums to Region × year × material and converts t to Mt.

- **Inputs**: `Inputs/MaterialFlows/*bymaterialgroup.csv` (not in git), `Inputs/Dict_Countries.xlsx` (`MaterialFlows_Agg`). Available at https://www.materialflows.net/visualisation-centre/data-visualisations/.
- **Outputs**: `Parameters/MaterialFlows/DE_detail_region.csv` (material_parent, material_detail, Region, year, DE_Mt). Figures S30 and S31 (`05-Figures/Supporting-Figures/`) plot it.

## `07-NonFerrous_Split.R`: non-ferrous ore DMC by sub-category

Splits UNEP non-ferrous ore DMC into Copper, Bauxite, Gold and Other non-ferrous (the remaining 11 ores). Copper, gold and bauxite account for 78% of non-ferrous ore mass in 2024.

1. **Data read**: materialflows.net domestic extraction (DE) of non-ferrous ores at Level 3 detail (`DE_detail_region.csv`, from `06`), and UNEP non-ferrous ore DMC by region.
   - **Key assumption**: each region's DE mix of non-ferrous ores also holds for its DMC.
   - No ore-level DMC exists, so traded ores and concentrates are assumed to have the domestic mix.
2. Shares = each sub-category's share of regional non-ferrous DE, per region × year. Global shares are the fallback for region-years with no DE (none in the current data).
3. Regional DE shares × regional UNEP non-ferrous DMC. Checks that shares sum to 1 and that the split sums back to UNEP.
4. Gold is modelled as a flow (fixed intensity per GDP), so a copy of the UNEP DMC file is saved with gold removed from "Non-ferrous ores". The 02-HistoricalStock scripts read this copy, so gold is not counted twice.
5. Used downstream:
   - The copper, bauxite and other shares split non-Fe metal into `Metal_Cu`, `Metal_Al` and `Metal_OtherNonFe` (`02-HistoricalStock/05`).
   - 2024 gold DMC is the anchor of the gold flow (`04-Simulation/03`).
   - 2024 copper and bauxite DMC set the demand of `09`.

- **Inputs**: `Parameters/MaterialFlows/DE_detail_region.csv`, `Parameters/UNEP-Materials/materials_region_DMC.csv`.
- **Outputs** (`Parameters/MaterialFlows/`): `nonfe_split_region.csv` (Region, year, nonfe_sub, de_share, DMC_Mt), `materials_region_DMC_exGold.csv` (same columns as `materials_region_DMC.csv`).

## `08-IronOre_GradeCurve.R`: iron ore grade curve

Cumulative ore vs ore grade for known iron ore deposits, to gauge how far grade could decline as future primary Fe demand depletes the best deposits.

1. Reads the S&P Capital IQ export (4 header rows; duplicated fields dropped). "Contained" is Fe metal, so ore = contained Fe / grade.
2. Per deposit, takes contained Fe and grade from the first available tier:
   1. R&R
   2. Reserves + Resources excl. Reserves
   3. M&I incl. Reserves + Inferred
   4. M&I incl. Reserves
   5. Reserves
3. Keeps deposits whose **primary commodity is iron ore** (all grades kept).
   - Tonnage is scaled to USGS world resources (>800 Gt ore containing >230 Gt Fe): every deposit's ore and Fe × 800 / 744 = 1.08 (`SCALE_TO_ORE_GT`).
   - Grades and order are unchanged. At S&P grades, 800 Gt of ore holds 294 Gt Fe, so both USGS bounds hold.
4. Sorts deposits by development stage, then by grade from high to low.
   - Stage (`DEV_STAGE`): producing or in construction first (operating, expansion, satellite, limited/residual production, commissioning, preproduction, construction started/planned), then all other stages.
   - `ACTV_STATUS` is kept in the output but not used.
5. **Future demand** (no model results are used, so there is no circularity):
   - Starts from 2024 world primary Fe = UNEP 2024 ferrous ore DMC × `GRADE_ORE_FE_2024`.
   - Five cases for 2025-2060: that level changing by −1, 0 (held constant), +1, +2 and +3 %/yr.
   - On the curve: dashed line = cumulative 2025-2060 Fe of the constant case; shaded = −1 to +3 %/yr.
6. **Option B (default): grade of the remaining resource.**
   - Mining takes the producing (or in-construction) deposits first, best grade first.
   - The grade in year t is the average grade of the producing-group ore still in the ground after cumulative extraction to t. Once that group is used up, it is the average of the remaining ore in all other stages. It is empty once all known resources are used up.
   - Relative grade = that grade / today's producing-group average (39.2% Fe).
   - It tracks the quality of the resource base the industry still has, rather than that of the single slice mined that year, so it changes smoothly.
7. **Option A (comparison): grade of the slice mined.** The grade in year t = Fe / ore of the slice mined over the 3 years t-2..t, relative to the 2025-2027 slice.
8. **Grade targets for the Monte Carlo**:
   - For each demand case, 2060 grade = `GRADE_ORE_FE_2024` × the option B relative grade in 2060. The table with all metals is written by `09`.
   - The min / central / max of `GRADE_ORE_FE` in `MC_Assumptions.xlsx` are chosen from these by hand; the model does not read the file.
   - The spread is a range of demand cases, not a statistical interval.

- **Inputs**: `Inputs/SP/IronOre_*.xls` (not in git), `Parameters/UNEP-Materials/materials_region_DMC.csv`. S&P Mineral Intelligence is proprietary data, so it cannot be shared publicly.
- **Outputs**:
  - `Parameters/SP/IronOre_deposits.csv` (not in git).
  - `Parameters/SP/IronOre_GradeIndex.csv` (growth_pct, year, fe_Gt; option A `grade_pct`, `grade_rel`; option B `b_grade_pct`, `b_rel`).
  - `Figures/SP/IronOre_GradeCurve.png`, `Figures/SP/IronOre_GradeIndex.png` (option B).

## `09-NonFerrous_GradeCurve.R`: copper and bauxite grade curves

The same analysis as `08-IronOre_GradeCurve.R` for copper (% Cu) and bauxite (% Al₂O₃), looped over the two materials. Same tier hierarchy, development-stage grouping, demand cases and options. Only deposits whose primary commodity is the material are used. Gold was dropped because S&P coverage is too low.

1. Tonnage is scaled to USGS totals (`scale_to_Gt`, `scale_basis` in `MATS`). Every deposit's ore and contained metal are multiplied by the same factor, so grades and order are unchanged and only the ore axis changes:
   - Bauxite (by ore): S&P reports tonnage for only 106 of 278 deposits (36 Gt ore), against USGS's 55-75 Gt; factor 65 / 36 = 1.8 (midpoint).
   - Copper (by contained Cu): S&P holds 3.0 Gt Cu, against USGS's 2015 assessment of 1.5 Gt identified + 3.5 Gt undiscovered = 5.0 Gt Cu; factor 1.64. This assumes undiscovered deposits share the known grade distribution and development-stage split.
2. **Demand** is gross ore: 2024 world ore DMC of the material (copper or bauxite, from `07`), with the same five cases as `08` (−1 to +3 %/yr).
3. **Option B (default)**, as in `08`: average grade of the producing group's remaining ore, relative to its average today (0.49% Cu, 41.9% Al₂O₃). Empty beyond known resources.
4. **Option A (comparison)**: demand is ore, so the curve is sliced by ore. Grade in year t = metal / ore of the slice mined over t-2..t, relative to the 2025-2027 slice.
5. **Grade targets**: `Parameters/SP/GradeTargets_2060.csv` collects iron (from `IronOre_GradeIndex.csv`), copper and bauxite. For each demand case:
   - 2060 grade = the model's 2024 grade (`GRADE_ORE_FE_2024`, `GRADE_ORE_CU_2024`, `GRADE_ORE_AL_2024`) × the option B relative grade in 2060.
   - For bauxite, the relative decline of % Al₂O₃ is applied to the Al grade.
   - The values are copied by hand into `MC_Assumptions.xlsx` (`GRADE_ORE_FE`, `GRADE_ORE_CU`, `GRADE_ORE_AL`).
6. Comparison figures: relative grade of iron, copper and bauxite on a shared y axis. One shows option B (line = constant demand, band = −1 to +3 %/yr); the other shows option B vs option A.

- **Inputs**: `Inputs/SP/{Copper,Bauxite}_*.xls` (not in git), `Parameters/MaterialFlows/nonfe_split_region.csv`, `Parameters/SP/IronOre_GradeIndex.csv` (run `08` first). S&P Mineral Intelligence is proprietary data, so it cannot be shared publicly.
- **Outputs**:
  - `Parameters/SP/{Copper,Bauxite}_deposits.csv` (tonnage scaled; not in git).
  - `Parameters/SP/NonFerrous_GradeIndex.csv` (material, growth_pct, year, option A and B grades).
  - `Parameters/SP/GradeTargets_2060.csv` (metal, growth_pct, rel_grade_2060, grade_2024, grade_2060).
  - `Figures/SP/{Copper,Bauxite}_GradeCurve.png`, `Figures/SP/Metals_GradeIndex.png`, `Figures/SP/Metals_GradeIndex_AvsB.png`.
