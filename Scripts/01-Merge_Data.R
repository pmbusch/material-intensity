## =============================================================================
## 01_Merge_Data.R
## Merge UNEP DMC, UN Population, and World Bank GDP data
## Output: Parameters/merged_DMC_pop_GDP.csv, Parameters/country_aggregate.csv
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Regional aggregates to drop from UNEP
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


# Step 0: Explore raw files -----------

cat("STEP 0: Explore raw input files\n")

## UNEP --- -----------
cat("\n--- UNEP mfa13_export.csv ---\n")
unep_raw <- read_csv("Inputs/UNEP/mfa13_export.csv", show_col_types = FALSE)
cat("Shape:", nrow(unep_raw), "x", ncol(unep_raw), "\n")
cat("Columns:", paste(names(unep_raw), collapse = ", "), "\n")
cat("First 3 rows (key cols):\n")
print(head(unep_raw[, 1:7], 3))
cat("Unique 'Flow name':", paste(unique(unep_raw[["Flow name"]]), collapse = " | "), "\n")
cat("Unique Categories (", length(unique(unep_raw$Category)), "):\n")
print(sort(unique(unep_raw$Category)))
yr_cols_unep <- names(unep_raw)[grepl("^[0-9]{4}$", names(unep_raw))]
cat("Year range:", min(yr_cols_unep), "-", max(yr_cols_unep), "\n")
cat("Unique countries:", length(unique(unep_raw$Country)), "\n")
cat(
  "Missing in key cols:",
  sum(is.na(unep_raw$Country)),
  "(country),",
  sum(is.na(unep_raw$Category)),
  "(category),",
  sum(is.na(unep_raw[["Flow name"]])),
  "(flow)\n"
)

## Dict --------------
cat("\n--- Dict_Countries.xlsx ---\n")
dict_raw <- read_excel("Inputs/Dict_Countries.xlsx")
cat("Shape:", nrow(dict_raw), "x", ncol(dict_raw), "\n")
cat("Columns:", paste(names(dict_raw), collapse = ", "), "\n")
print(head(dict_raw, 5))

## UN Population --------------
cat("\n--- WPP2024 Population xlsx ---\n")
pop_path <- "Inputs/UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"
cat("Sheets:", paste(excel_sheets(pop_path), collapse = ", "), "\n")

## World Bank GDP --------------
cat("\n--- World Bank GDP xls ---\n")
gdp_path <- "Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls"
cat("Sheets:", paste(excel_sheets(gdp_path), collapse = ", "), "\n")


# Steps 1–2: Filter UNEP + harmonise countries -----------

cat("\n STEPS 1-2: Filter UNEP + harmonise countries\n")

# Keep only DMC flow, melt to long
unep_long <- unep_raw %>%
  filter(`Flow name` == "Domestic Material Consumption") %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "DMC_t") %>%
  mutate(
    year = as.integer(year),
    DMC_Mt = DMC_t / 1e6 # metric tonnes → megatonnes
  ) %>%
  rename(UNEP_name = Country, material_category = Category) %>%
  select(UNEP_name, year, material_category, DMC_Mt)

cat("Rows after DMC filter + melt:", nrow(unep_long), "\n")

# Drop regional aggregates
unep_long <- unep_long %>% filter(!UNEP_name %in% REGION_AGGREGATES)

cat("Rows after dropping regional aggregates:", nrow(unep_long), "\n")

# Load concordance
dict <- read_excel("Inputs/Dict_Countries.xlsx")
cat("Dict columns found:", paste(names(dict), collapse = ", "), "\n")

# Exact match on UNEP_name
unep_matched <- unep_long %>% left_join(dict, by = "UNEP_name")

# Diagnose unmatched
unmatched_names <- unep_matched %>% filter(is.na(ISO3)) %>% distinct(UNEP_name) %>% pull(UNEP_name) %>% sort()

cat("\nUNEP country names NOT matched in Dict_Countries (", length(unmatched_names), "):\n")
print(unmatched_names)

# Drop unmatched rows (flag them but continue)
unep_matched <- unep_matched %>% filter(!is.na(ISO3))
cat("Rows after dropping unmatched countries:", nrow(unep_matched), "\n")
cat("Unique ISO3 codes retained:", length(unique(unep_matched$ISO3)), "\n")


## ── Step 3: Historical political aggregates -------------------------------

cat("\n STEP 3: Historical political aggregates\n")

dmc <- unep_matched

# Helper: count rows affected by each case
affected_counts <- list()

apply_split <- function(df, aggregate_iso, split_year, successor_isos, case_label) {
  # Drop aggregate rows for years >= split_year (keep successors)
  agg_rows_after <- df %>% filter(ISO3 == aggregate_iso, year >= split_year)
  succ_rows_before <- df %>% filter(ISO3 %in% successor_isos, year < split_year)

  if (nrow(succ_rows_before) > 0) {
    warning_countries <- succ_rows_before %>% distinct(ISO3, year) %>% arrange(ISO3, year)
    cat("[WARNING]", case_label, "- successor state(s) have DMC data before split year:\n")
    print(warning_countries)
  }

  rows_dropped <- nrow(agg_rows_after)
  df_out <- df %>% filter(!(ISO3 == aggregate_iso & year >= split_year))
  affected_counts[[case_label]] <<- rows_dropped
  cat(case_label, "- dropped", rows_dropped, "aggregate rows after split year\n")
  df_out
}

# CASE 1: USSR → split 1992
USSR_SUCCS <- c("RUS", "UKR", "KAZ", "UZB", "BLR", "AZE", "GEO", "ARM", "TJK", "KGZ", "TKM", "MDA", "EST", "LVA", "LTU")
dmc <- apply_split(dmc, "SUN", 1992, USSR_SUCCS, "CASE1_USSR")

# CASE 2: Yugoslavia → split 1992 (SCG handled below)
YUG_SUCCS <- c("SRB", "HRV", "SVN", "BIH", "MKD", "MNE", "SCG")
dmc <- apply_split(dmc, "YUG", 1992, YUG_SUCCS, "CASE2_Yugoslavia")

# Serbia and Montenegro (SCG) → split 2006
SCG_SUCCS <- c("SRB", "MNE")
dmc <- apply_split(dmc, "SCG", 2006, SCG_SUCCS, "CASE2b_SCG")

# CASE 3: Czechoslovakia → split 1993
CSK_SUCCS <- c("CZE", "SVK")
dmc <- apply_split(dmc, "CSK", 1993, CSK_SUCCS, "CASE3_Czechoslovakia")

# CASE 4: Sudan Former → split 2012
# Keep "Sudan Former" (whatever ISO3 it maps to) for <= 2010; SDN+SSD take over >= 2011
# Check what ISO3 UNEP uses for Sudan Former
sudan_former_iso <- dict %>% filter(grepl("Sudan.*Former|Former.*Sudan", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
if (length(sudan_former_iso) > 0) {
  dmc <- apply_split(dmc, sudan_former_iso[1], 2012, c("SDN", "SSD"), "CASE4_Sudan")
} else {
  cat("CASE4_Sudan: no 'Sudan Former' entry found in dict — skipping\n")
  affected_counts[["CASE4_Sudan"]] <- 0
}

# CASE 5: Ethiopia Former → split 1993
eth_former_iso <- dict %>%
  filter(grepl("Ethiopia.*Former|Former.*Ethiopia", UNEP_name, ignore.case = TRUE)) %>%
  pull(ISO3)
if (length(eth_former_iso) > 0) {
  dmc <- apply_split(dmc, eth_former_iso[1], 1993, c("ETH", "ERI"), "CASE5_Ethiopia")
} else {
  cat("CASE5_Ethiopia: no 'Ethiopia Former' entry found — skipping\n")
  affected_counts[["CASE5_Ethiopia"]] <- 0
}

# CASE 6: Yemen — sum North + South for years <= 1990, assign to YEM
north_yem_iso <- dict %>% filter(grepl("North.*Yemen|Yemen.*North", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
south_yem_iso <- dict %>%
  filter(grepl("South.*Yemen|Yemen.*South|Yemen.*People|Democratic.*Yemen", UNEP_name, ignore.case = TRUE)) %>%
  pull(ISO3)

if (length(north_yem_iso) > 0 | length(south_yem_iso) > 0) {
  cat(
    "CASE6_Yemen: North ISO3 =",
    paste(north_yem_iso, collapse = ","),
    "| South ISO3 =",
    paste(south_yem_iso, collapse = ","),
    "\n"
  )

  old_yem_isos <- c(north_yem_iso, south_yem_iso)
  yem_pre <- dmc %>%
    filter(ISO3 %in% old_yem_isos, year <= 1989) %>%
    group_by(year, material_category) %>%
    summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
    mutate(ISO3 = "YEM") %>%
    left_join(dict %>% filter(ISO3 == "YEM") %>% select(-UNEP_name), by = "ISO3")

  rows_dropped_yem <- dmc %>% filter(ISO3 %in% old_yem_isos) %>% nrow()
  dmc <- dmc %>% filter(!ISO3 %in% old_yem_isos) # remove N+S Yemen entirely
  dmc <- bind_rows(dmc, yem_pre) # add summed pre-1990 YEM
  affected_counts[["CASE6_Yemen"]] <- rows_dropped_yem
  cat("CASE6_Yemen - aggregated", rows_dropped_yem, "N+S Yemen rows into YEM pre-1990\n")
} else {
  cat("CASE6_Yemen: North/South Yemen ISO3 not found — skipping\n")
  affected_counts[["CASE6_Yemen"]] <- 0
}

# CASE 7: Netherlands Antilles → split 2012
ANT_SUCCS <- c("CUW", "SXM")
dmc <- apply_split(dmc, "ANT", 2012, ANT_SUCCS, "CASE7_NethAntilles")

# Assert no duplicates
dup_check <- dmc %>% group_by(ISO3, year, material_category) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1)

if (nrow(dup_check) > 0) {
  cat("\n[ERROR] Duplicate (ISO3, year, material) rows found!\n")
  print(dup_check)
} else {
  cat("\n[OK] No duplicate (ISO3, year, material) rows.\n")
}

cat("\nSummary of rows affected by historical aggregate cases:\n")
print(as.data.frame(t(as.data.frame(affected_counts))))
cat("Final UNEP DMC rows:", nrow(dmc), "\n")


## Step 4: Population data -------------------------------

cat("\n STEP 4: UN Population data\n")

pop_raw <- read_excel(pop_path, sheet = "Estimates", skip = 16)
cat("Population raw shape:", nrow(pop_raw), "x", ncol(pop_raw), "\n")
cat("Columns:", paste(names(pop_raw), collapse = ", "), "\n")
print(head(pop_raw, 3))

# Identify key columns flexibly
pop_type_col <- names(pop_raw)[grepl("type|LocType", names(pop_raw), ignore.case = TRUE)][1]
pop_name_col <- names(pop_raw)[grepl("region.*country|country.*area|location", names(pop_raw), ignore.case = TRUE)][1]
pop_iso3_col <- names(pop_raw)[grepl("ISO3|alpha.3|iso.*3", names(pop_raw), ignore.case = TRUE)][1]
pop_year_col <- names(pop_raw)[grepl("^year$|^Time$", names(pop_raw), ignore.case = TRUE)][1]
pop_tot_col <- names(pop_raw)[grepl("total.*pop|pop.*total", names(pop_raw), ignore.case = TRUE)][1]

cat("\nIdentified columns:\n")
cat("  Type:", pop_type_col, "\n")
cat("  Name:", pop_name_col, "\n")
cat("  ISO3:", pop_iso3_col, "\n")
cat("  Year:", pop_year_col, "\n")
cat("  Total pop:", pop_tot_col, "\n")

# Filter to Country/Area type and year range
pop_long <- pop_raw %>%
  rename(
    loc_type = all_of(pop_type_col),
    loc_name = all_of(pop_name_col),
    ISO3_pop = all_of(pop_iso3_col),
    year = all_of(pop_year_col),
    pop_thou = all_of(pop_tot_col)
  ) %>%
  filter(grepl("country|area", loc_type, ignore.case = TRUE)) %>%
  mutate(
    year = as.integer(year),
    population = as.numeric(pop_thou) * 1e3 # thousands → persons
  ) %>%
  filter(year >= 1970, year <= 2024) %>%
  select(ISO3_pop, loc_name, year, population)

cat("Population rows after filter:", nrow(pop_long), "\n")
cat("Year range:", min(pop_long$year), "-", max(pop_long$year), "\n")

# Harmonise to ISO3 via dict — join on UN_Pop_name or ISO3 directly
# First try ISO3 direct match if WPP provides ISO3
if (!all(is.na(pop_long$ISO3_pop))) {
  pop_iso <- pop_long %>% rename(ISO3 = ISO3_pop) %>% filter(!is.na(ISO3)) %>% select(ISO3, year, population)
  cat("Using ISO3 direct match from WPP. Unique ISO3:", length(unique(pop_iso$ISO3)), "\n")
} else {
  # Fall back to UN_Pop_name join
  pop_iso <- pop_long %>%
    left_join(dict %>% select(UN_Pop_name, ISO3) %>% distinct(), by = c("loc_name" = "UN_Pop_name")) %>%
    filter(!is.na(ISO3)) %>%
    select(ISO3, year, population)
  cat("Used UN_Pop_name join. Unique ISO3:", length(unique(pop_iso$ISO3)), "\n")
}

# Apply same historical aggregate logic for population
# USSR: keep SUN rows <= 1991, successor rows >= 1991
pop_iso <- pop_iso %>%
  filter(!(ISO3 == "SUN" & year >= 1992)) %>%
  filter(!(ISO3 == "YUG" & year >= 1992)) %>%
  filter(!(ISO3 == "CSK" & year >= 1993)) %>%
  filter(!(ISO3 == "SCG" & year >= 2006))

cat("Population rows after historical aggregate filter:", nrow(pop_iso), "\n")


# Step 5: GDP data -------------------------------

cat("\n STEP 5: World Bank GDP data\n")

gdp_raw <- suppressWarnings(read_excel(gdp_path, sheet = "Data", skip = 3))
cat("GDP raw shape:", nrow(gdp_raw), "x", ncol(gdp_raw), "\n")
cat("First 6 col names:", paste(names(gdp_raw)[1:6], collapse = ", "), "\n")
print(head(gdp_raw[, 1:6], 4))

# Country Code column (typically 2nd column)
cc_col <- names(gdp_raw)[grepl("country.code|countrycode", names(gdp_raw), ignore.case = TRUE)][1]
cn_col <- names(gdp_raw)[grepl("country.name|countryname", names(gdp_raw), ignore.case = TRUE)][1]
cat("Country code col:", cc_col, "| Country name col:", cn_col, "\n")

yr_cols_gdp <- names(gdp_raw)[grepl("^[0-9]{4}$", names(gdp_raw))]
cat("GDP year range:", min(yr_cols_gdp), "-", max(yr_cols_gdp), "\n")

# Get set of valid ISO3s from concordance (country-level only)
valid_iso3 <- unique(dict$ISO3)

gdp_long <- gdp_raw %>%
  rename(ISO3 = all_of(cc_col)) %>%
  filter(ISO3 %in% valid_iso3) %>% # keep only country-level rows
  pivot_longer(cols = all_of(yr_cols_gdp), names_to = "year", values_to = "GDP_PPP_2017USD") %>%
  mutate(year = as.integer(year), GDP_PPP_2017USD = suppressWarnings(as.numeric(GDP_PPP_2017USD))) %>%
  filter(year >= 1970, year <= 2024) %>%
  select(ISO3, year, GDP_PPP_2017USD)

cat("GDP long rows:", nrow(gdp_long), "\n")
cat("Unique ISO3:", length(unique(gdp_long$ISO3)), "\n")

# Coverage gaps
gdp_missing <- gdp_long %>%
  filter(is.na(GDP_PPP_2017USD)) %>%
  group_by(ISO3) %>%
  summarise(missing_years = n(), min_yr = min(year), max_yr = max(year), .groups = "drop") %>%
  arrange(desc(missing_years))
cat("\nTop 15 ISO3 with most missing GDP years:\n")
print(head(gdp_missing, 15))


#  Step 6: Merge all datasets -------------------------------

cat("\n STEP 6: Merge all datasets\n")

# Left join: start from UNEP DMC (already country-level, filtered)
merged <- dmc %>%
  left_join(pop_iso %>% distinct(ISO3, year, population), by = c("ISO3", "year")) %>%
  left_join(gdp_long %>% distinct(ISO3, year, GDP_PPP_2017USD), by = c("ISO3", "year"))

cat("Merged master rows:", nrow(merged), "\n")

# Merge diagnostics
no_pop <- merged %>% filter(is.na(population)) %>% distinct(ISO3, year) %>% nrow()
no_gdp <- merged %>% filter(is.na(GDP_PPP_2017USD)) %>% distinct(ISO3, year) %>% nrow()
cat("ISO3-year combos with DMC but no population:", no_pop, "\n")
cat("ISO3-year combos with DMC but no GDP:", no_gdp, "\n")

coverage <- merged %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(ISO3),
    pct_with_pop = mean(!is.na(population)) * 100,
    pct_with_gdp = mean(!is.na(GDP_PPP_2017USD)) * 100,
    .groups = "drop"
  )
good_coverage <- coverage %>% filter(pct_with_pop > 90, pct_with_gdp > 90)
cat("Year range with >90% three-way coverage:", min(good_coverage$year), "-", max(good_coverage$year), "\n")

# Countries with no pop data
cat("ISO3 with no population data at all:\n")
print(merged %>% filter(is.na(population)) %>% distinct(ISO3) %>% pull() %>% sort())

# Countries with no GDP data at all
cat("ISO3 with no GDP data at all:\n")
print(merged %>% filter(is.na(GDP_PPP_2017USD)) %>% distinct(ISO3) %>% pull() %>% sort())

# Save master (material-level)
write_csv(merged, "Parameters/panel_data.csv")
nrow(merged) / 1e6

# EoF
