## =============================================================================
## 01-Merge_Data.R
## Process UNEP material flows (all flow codes), UN Population, and World Bank
## GDP into separate, join-ready panel files.
##
## Outputs (Parameters/):
##   materials_DE.csv            — Domestic Extraction          (ISO3, year, material_category, Analysis_group, DE_Mt)
##   materials_DMC.csv           — Domestic Material Consumption (…, DMC_Mt)
##   materials_DMI.csv           — Direct Material Input         (…, DMI_Mt)
##   materials_EXP.csv           — Exports                       (…, EXP_Mt)
##   materials_IMP.csv           — Imports                       (…, IMP_Mt)
##   materials_PTB.csv           — Physical Trade Balance        (…, PTB_Mt)
##   gdp.csv                     — (ISO3, year, GDP_PPP_2017USD)
##   population_historical.csv   — (ISO3, year, population) 1970–2024
##   population_forecast.csv     — (ISO3, year, population) 2025–2100 median variant
##
## Design: GDP and population are stored separately (one row per ISO3-year).
## Figure scripts left-join them only when needed, preventing the accidental
## "sum GDP over material rows" bug that arises from a single merged panel.
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Regional aggregate labels to exclude from country-level analysis
# fmt: skip
REGION_AGGREGATES <- c(
  "World", "Africa", "Asia", "Europe", "Americas", "Oceania",
  "Asia + Pacific", "Latin America + Caribbean", "North America",
  "West Asia", "EECCA", "High-and-upper-middle-income countries",
  "Low-and-Lower-middle-income countries", "Least developed countries",
  "Land-locked Developing Countries", "Geographic regions",
  "Sustainable Development Goal (SDG) regions",
  "UN development groups", "World Bank income groups"
)

FLOW_CODES <- c("DE", "DMC", "DMI", "EXP", "IMP", "PTB")


# Step 0: Explore raw files -------------------------------------------------------

cat("STEP 0: Explore raw input files\n")

## UNEP
cat("\n--- UNEP mfa13_export.csv ---\n")
unep_raw <- read_csv("Inputs/UNEP/mfa13_export.csv", show_col_types = FALSE)
cat("Shape:", nrow(unep_raw), "x", ncol(unep_raw), "\n")
cat("Flow codes in file:", paste(sort(unique(unep_raw[["Flow code"]])), collapse = ", "), "\n")
cat("Categories (", length(unique(unep_raw$Category)), "):\n")
print(sort(unique(unep_raw$Category)))
yr_cols_unep <- names(unep_raw)[grepl("^[0-9]{4}$", names(unep_raw))]
cat("Year range:", min(yr_cols_unep), "-", max(yr_cols_unep), "\n")
cat("Unique countries:", length(unique(unep_raw$Country)), "\n")

missing_flows <- setdiff(FLOW_CODES, unique(unep_raw[["Flow code"]]))
if (length(missing_flows) > 0) {
  cat("[WARNING] Flow codes not found in file:", paste(missing_flows, collapse = ", "), "\n")
}

## Dict
cat("\n--- Dict_Countries.xlsx ---\n")
dict <- read_excel("Inputs/Dict_Countries.xlsx")
cat("Shape:", nrow(dict), "x", ncol(dict), "\n")
cat("Columns:", paste(names(dict), collapse = ", "), "\n")
print(head(dict, 5))

## UN Population
cat("\n--- WPP2024 Population xlsx ---\n")
pop_path <- "Inputs/UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"
cat("Sheets:", paste(excel_sheets(pop_path), collapse = ", "), "\n")

## World Bank GDP
cat("\n--- World Bank GDP xls ---\n")
gdp_path <- "Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls"
cat("Sheets:", paste(excel_sheets(gdp_path), collapse = ", "), "\n")


# Step 1: Pivot UNEP to long and harmonise countries ---------------------------------------------

cat("\nSTEP 1: Pivot UNEP to long format (all flow codes) + ISO3 join\n")

unep_long <- unep_raw %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value_t") %>%
  mutate(
    year = as.integer(year),
    value_Mt = value_t / 1e6 # metric tonnes → megatonnes
  ) %>%
  rename(UNEP_name = Country, material_category = Category, flow_code = `Flow code`) %>%
  filter(!UNEP_name %in% REGION_AGGREGATES) %>%
  select(UNEP_name, year, material_category, flow_code, value_Mt)

cat("Rows after pivot + regional aggregate drop:", nrow(unep_long), "\n")
cat("Flow codes present:", paste(sort(unique(unep_long$flow_code)), collapse = ", "), "\n")

# Join ISO3 + Analysis_group from country concordance
unep_matched <- unep_long %>% left_join(dict, by = "UNEP_name")

unmatched <- unep_matched %>% filter(is.na(ISO3)) %>% distinct(UNEP_name) %>% pull(UNEP_name) %>% sort()
cat("\nUNEP country names not matched in Dict_Countries (", length(unmatched), "):\n")
print(unmatched)

unep_matched <- unep_matched %>%
  filter(!is.na(ISO3)) %>%
  select(ISO3, year, material_category, flow_code, value_Mt, Analysis_group)

cat("Rows after ISO3 join:", nrow(unep_matched), "\n")
cat("Unique ISO3:", length(unique(unep_matched$ISO3)), "\n")


# Step 2: Historical political-aggregate helper -------------------------------------------------------

cat("\nSTEP 2: Define historical political-aggregate helper\n")

apply_flow_aggregates <- function(df, dict) {
  # Applies all country-split / country-merge cases to a single-flow data frame.
  # df columns expected: ISO3, year, material_category, flow_code, value_Mt, Analysis_group

  apply_split <- function(df, aggregate_iso, split_year, successor_isos, label) {
    succ_before <- df %>% filter(ISO3 %in% successor_isos, year < split_year)
    if (nrow(succ_before) > 0) {
      cat("    [WARNING]", label, "— successors have data before split year:\n")
      print(succ_before %>% distinct(ISO3, year) %>% arrange(ISO3, year))
    }
    n_dropped <- nrow(df %>% filter(ISO3 == aggregate_iso, year >= split_year))
    cat("   ", label, "— dropped", n_dropped, "aggregate rows after split year\n")
    df %>% filter(!(ISO3 == aggregate_iso & year >= split_year))
  }

  # CASE 1: USSR → successors from 1992
  df <- apply_split(
    df,
    "SUN",
    1992,
    c("RUS", "UKR", "KAZ", "UZB", "BLR", "AZE", "GEO", "ARM", "TJK", "KGZ", "TKM", "MDA", "EST", "LVA", "LTU"),
    "CASE1_USSR"
  )

  # CASE 2a: Yugoslavia → successors from 1992
  df <- apply_split(df, "YUG", 1992, c("SRB", "HRV", "SVN", "BIH", "MKD", "MNE", "SCG"), "CASE2_Yugoslavia")

  # CASE 2b: Serbia & Montenegro → successors from 2006
  df <- apply_split(df, "SCG", 2006, c("SRB", "MNE"), "CASE2b_SCG")

  # CASE 3: Czechoslovakia → successors from 1993
  df <- apply_split(df, "CSK", 1993, c("CZE", "SVK"), "CASE3_Czechoslovakia")

  # CASE 4: Sudan Former → successors from 2012
  sudan_iso <- dict %>% filter(grepl("Sudan.*Former|Former.*Sudan", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
  if (length(sudan_iso) > 0) {
    df <- apply_split(df, sudan_iso[1], 2012, c("SDN", "SSD"), "CASE4_Sudan")
  } else {
    cat("    CASE4_Sudan: no entry found — skipping\n")
  }

  # CASE 5: Ethiopia Former → successors from 1993
  eth_iso <- dict %>% filter(grepl("Ethiopia.*Former|Former.*Ethiopia", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
  if (length(eth_iso) > 0) {
    df <- apply_split(df, eth_iso[1], 1993, c("ETH", "ERI"), "CASE5_Ethiopia")
  } else {
    cat("    CASE5_Ethiopia: no entry found — skipping\n")
  }

  # CASE 6: Yemen — sum North + South Yemen pre-1990 into unified YEM
  north_yem_iso <- dict %>% filter(grepl("North.*Yemen|Yemen.*North", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
  south_yem_iso <- dict %>%
    filter(grepl("South.*Yemen|Yemen.*South|Yemen.*People|Democratic.*Yemen", UNEP_name, ignore.case = TRUE)) %>%
    pull(ISO3)

  old_yem_isos <- c(north_yem_iso, south_yem_iso)
  if (length(old_yem_isos) > 0) {
    cat(
      "    CASE6_Yemen: N ISO3 =",
      paste(north_yem_iso, collapse = ","),
      "| S ISO3 =",
      paste(south_yem_iso, collapse = ","),
      "\n"
    )
    yem_group <- df %>% filter(ISO3 == "YEM") %>% pull(Analysis_group) %>% na.omit() %>% unique() %>% head(1)
    flow_val <- df %>% filter(ISO3 %in% old_yem_isos) %>% pull(flow_code) %>% unique() %>% head(1)

    yem_pre <- df %>%
      filter(ISO3 %in% old_yem_isos, year <= 1989) %>%
      group_by(year, material_category) %>%
      summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        ISO3 = "YEM",
        flow_code = flow_val,
        Analysis_group = if (length(yem_group) > 0) yem_group else NA_character_
      )

    n_dropped_yem <- df %>% filter(ISO3 %in% old_yem_isos) %>% nrow()
    df <- df %>% filter(!ISO3 %in% old_yem_isos)
    df <- bind_rows(df, yem_pre)
    cat("    CASE6_Yemen — aggregated", n_dropped_yem, "N+S Yemen rows into YEM pre-1990\n")
  } else {
    cat("    CASE6_Yemen: N/S Yemen ISO3 not found — skipping\n")
  }

  # CASE 7: Netherlands Antilles → successors from 2012
  df <- apply_split(df, "ANT", 2012, c("CUW", "SXM"), "CASE7_NethAntilles")

  df
}


# Step 3: Process each flow code and save -------------------------------------------

cat("\nSTEP 3: Process historical aggregates and save per-flow CSV files\n")

for (fc in FLOW_CODES) {
  cat("\n  ══ Flow:", fc, "══\n")

  df_flow <- unep_matched %>% filter(flow_code == fc)

  if (nrow(df_flow) == 0) {
    cat("  [SKIP] No data for flow code", fc, "— not present in mfa13_export.csv\n")
    next
  }
  cat("  Rows before aggregate processing:", nrow(df_flow), "\n")

  df_flow <- apply_flow_aggregates(df_flow, dict)

  # Duplicate guard
  dups <- df_flow %>% group_by(ISO3, year, material_category) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1)
  if (nrow(dups) > 0) {
    cat("  [ERROR] Duplicate (ISO3, year, material) rows found for", fc, "!\n")
    print(dups)
  } else {
    cat("  [OK] No duplicate (ISO3, year, material) rows\n")
  }

  # Rename value column to flow-specific name and write out
  out_col <- paste0(fc, "_Mt")
  df_out <- df_flow %>%
    select(ISO3, year, material_category, Analysis_group, value_Mt) %>%
    rename(!!out_col := value_Mt)

  out_path <- paste0("Parameters/materials_", fc, ".csv")
  write_csv(df_out, out_path)
  cat("  Saved:", out_path, "(", nrow(df_out), "rows,", length(unique(df_out$ISO3)), "countries )\n")
}


# Step 4: Population -------------------------------------------------------

cat("\nSTEP 4: UN Population data\n")

# Helper: read one WPP sheet, keep country-level rows, return (ISO3, year, population)
read_wpp_sheet <- function(path, sheet_name) {
  raw <- read_excel(path, sheet = sheet_name, skip = 16, col_types = c(rep("guess", 5), "text", rep("guess", 59)))

  type_col <- names(raw)[grepl("type|LocType", names(raw), ignore.case = TRUE)][1]
  pop_name_col <- names(raw)[grepl("region.*country|country.*area|location", names(raw), ignore.case = TRUE)][1]
  iso3_col <- names(raw)[grepl("ISO3|alpha.3|iso.*3", names(raw), ignore.case = TRUE)][1]
  year_col <- names(raw)[grepl("^year$|^Time$", names(raw), ignore.case = TRUE)][1]
  pop_col <- names(raw)[grepl("total.*pop|pop.*total", names(raw), ignore.case = TRUE)][1]

  # fmt: skip
  cat("  Sheet:", sheet_name, "| type:", type_col,"| Name:", pop_name_col, "| iso3:", iso3_col, "| year:", year_col, "| pop:", pop_col, "\n")

  raw %>%
    rename(
      loc_type = all_of(type_col),
      loc_name = all_of(pop_name_col),
      ISO3 = all_of(iso3_col),
      year = all_of(year_col),
      pop_thou = all_of(pop_col)
    ) %>%
    filter(grepl("country|area", loc_type, ignore.case = TRUE)) %>%
    mutate(
      year = as.integer(year),
      population = as.numeric(pop_thou) * 1e3 # thousands → persons
    ) %>%
    filter(!is.na(population)) %>%
    select(ISO3, loc_name, year, population)
}

pop_est <- read_wpp_sheet(pop_path, "Estimates")
pop_med <- read_wpp_sheet(pop_path, "Medium variant")

cat("Estimates year range:       ", min(pop_est$year), "-", max(pop_est$year), "\n")
cat("Medium variant year range:  ", min(pop_med$year), "-", max(pop_med$year), "\n")

## Historical: Estimates 1970–2023  +  Medium Variant 2024
pop_historical <- bind_rows(pop_est %>% filter(year >= 1970, year <= 2023), pop_med %>% filter(year == 2024)) %>%
  arrange(ISO3, year)

# Apply same country-split filters as materials
pop_historical <- pop_historical %>%
  filter(!(ISO3 == "SUN" & year >= 1992)) %>%
  filter(!(ISO3 == "YUG" & year >= 1992)) %>%
  filter(!(ISO3 == "CSK" & year >= 1993)) %>%
  filter(!(ISO3 == "SCG" & year >= 2006))

cat(
  "Historical population: ",
  nrow(pop_historical),
  "rows |",
  min(pop_historical$year),
  "-",
  max(pop_historical$year),
  "|",
  length(unique(pop_historical$ISO3)),
  "ISO3\n"
)

write_csv(pop_historical, "Parameters/population_historical.csv")
cat("Saved: Parameters/population_historical.csv\n")

## Forecast: Medium Variant 2025–2100
pop_forecast <- pop_med %>% filter(year >= 2025, year <= 2100) %>% arrange(ISO3, year)

cat(
  "Forecast population:   ",
  nrow(pop_forecast),
  "rows |",
  min(pop_forecast$year),
  "-",
  max(pop_forecast$year),
  "|",
  length(unique(pop_forecast$ISO3)),
  "ISO3\n"
)

write_csv(pop_forecast, "Parameters/population_forecast.csv")
cat("Saved: Parameters/population_forecast.csv\n")


# Step 5: GDP -------------------------------------------------------

cat("\nSTEP 5: World Bank GDP data\n")

gdp_raw <- suppressWarnings(read_excel(gdp_path, sheet = "Data", skip = 3))
cc_col <- names(gdp_raw)[grepl("country.code|countrycode", names(gdp_raw), ignore.case = TRUE)][1]
yr_cols_gdp <- names(gdp_raw)[grepl("^[0-9]{4}$", names(gdp_raw))]
cat("GDP year range:", min(yr_cols_gdp), "-", max(yr_cols_gdp), "\n")

valid_iso3 <- unique(dict$ISO3)

gdp <- gdp_raw %>%
  rename(ISO3 = all_of(cc_col)) %>%
  filter(ISO3 %in% valid_iso3) %>%
  pivot_longer(cols = all_of(yr_cols_gdp), names_to = "year", values_to = "GDP_PPP_2017USD") %>%
  mutate(year = as.integer(year), GDP_PPP_2017USD = suppressWarnings(as.numeric(GDP_PPP_2017USD))) %>%
  filter(year >= 1970, year <= 2024) %>%
  select(ISO3, year, GDP_PPP_2017USD)

cat("GDP: ", nrow(gdp), "rows |", length(unique(gdp$ISO3)), "ISO3\n")

gdp_missing <- gdp %>%
  filter(is.na(GDP_PPP_2017USD)) %>%
  group_by(ISO3) %>%
  summarise(missing_years = n(), .groups = "drop") %>%
  arrange(desc(missing_years))
cat("Top 10 ISO3 with most missing GDP years:\n")
print(head(gdp_missing, 10))

write_csv(gdp, "Parameters/gdp.csv")
cat("Saved: Parameters/gdp.csv\n")


# Step 6: Coverage summary (cross-check DMC × pop × GDP) -------------------------------------------------------

cat("\nSTEP 6: Coverage summary (DMC × population × GDP)\n")

dmc_base <- read_csv("Parameters/materials_DMC.csv", show_col_types = FALSE)

coverage <- dmc_base %>%
  distinct(ISO3, year) %>%
  left_join(pop_historical %>% select(ISO3, year, population), by = c("ISO3", "year")) %>%
  left_join(gdp %>% select(ISO3, year, GDP_PPP_2017USD), by = c("ISO3", "year")) %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(ISO3),
    pct_with_pop = mean(!is.na(population)) * 100,
    pct_with_gdp = mean(!is.na(GDP_PPP_2017USD)) * 100,
    .groups = "drop"
  )

good_cov <- coverage %>% filter(pct_with_pop > 90, pct_with_gdp > 90)
if (nrow(good_cov) > 0) {
  cat("Years with >90% pop and GDP coverage:", min(good_cov$year), "-", max(good_cov$year), "\n")
} else {
  cat("[WARNING] No years with >90% three-way coverage\n")
}

cat("ISO3 with no population data at all:\n")
print(
  dmc_base %>%
    distinct(ISO3, year) %>%
    left_join(pop_historical, by = c("ISO3", "year")) %>%
    filter(is.na(population)) %>%
    distinct(ISO3) %>%
    pull() %>%
    sort()
)

cat("ISO3 with no GDP data at all:\n")
print(
  dmc_base %>%
    distinct(ISO3, year) %>%
    left_join(gdp, by = c("ISO3", "year")) %>%
    filter(is.na(GDP_PPP_2017USD)) %>%
    distinct(ISO3) %>%
    pull() %>%
    sort()
)

cat("\nAll outputs written to Parameters/\n")

# EoF
