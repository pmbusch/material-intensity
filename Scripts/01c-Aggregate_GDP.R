## =============================================================================
## 01c-Aggregate_GDP.R
## Builds regional GDP aggregates (constant 2017 USD PPP) from World Bank data.
##
## Approach:
##   Step 1 — Use WB regional aggregate codes as the base (EAS, ECS, LCN, MEA,
##             NAC, SAS, SSF, PSS).
##   Step 2 — Apply country-level corrections to align WB regions with our 8
##             regions (including Oceania). Country GDP is subtracted from its
##             WB source region and added to the correct target region.
##
## Corrections applied:
##   KAZ, KGZ, TJK, TKM, UZB  : ECS → East Asia
##   ARM, AZE, GEO             : ECS → Middle East & North Africa
##   MLT                       : MEA → Europe & Russia
##   PAK, AFG                  : MEA → South Asia
##     [Note: WB MEA includes AFG+PAK; our materials/population sheets
##      assign AFG to Middle East & North Africa and PAK to South Asia.
##      For GDP we move both to South Asia to avoid double-counting in MEA
##      and to align as closely as possible with the SAS aggregate.]
##   AUS, NZL + Pacific islands: EAS → Oceania (new region)
##
## Outputs (Parameters/):
##   gdp_region.csv   — (Region, year, GDP_PPP_2017USD)
##   gdp_country.csv  — (ISO3, year, GDP_PPP_2017USD)  [selected countries]
##   gdp_world.csv    — (year, GDP_PPP_2017USD)
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

gdp_path <- "Inputs/WorldBank/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_753.xls" # 2015 constant
# gdp_path <- "Inputs/WorldBank/API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_251.xls" # 2021 constant, PPP adjusted (purchasing power parity)

# Pacific island small states (PSS members, from WB composition sheet)
# fmt: skip
PSS_ISO3 <- c("FJI", "KIR", "MHL", "FSM", "NRU", "PLW", "WSM", "SLB", "TON", "TUV", "VUT")

# Country-level corrections: ISO3 → source WB code and target region
# fmt: skip
CORRECTIONS <- tibble(
  ISO3       = c("KAZ","KGZ","TJK","TKM","UZB",           # ECS → East Asia
                 "ARM","AZE","GEO",                         # ECS → MENA
                 "MLT",                                     # MEA → Europe & Russia
                 "PAK","AFG",                               # MEA → South Asia
                 "AUS","NZL",                               # EAS → Oceania
                 PSS_ISO3),                                 # EAS → Oceania
  from_wb    = c(rep("ECS", 5),
                 rep("ECS", 3),
                 "MEA",
                 rep("MEA", 2),
                 rep("EAS", 2),
                 rep("EAS", length(PSS_ISO3))),
  to_region  = c(rep("East Asia",                  5),
                 rep("Middle East & North Africa",  3),
                 "Europe & Russia",
                 rep("South Asia",                  2),
                 rep("Oceania", 2 + length(PSS_ISO3)))
)

# WB aggregate code → our region name (after corrections applied)
WB_TO_REGION <- c(
  EAS = "East Asia",
  ECS = "Europe & Russia",
  LCN = "Latin America",
  MEA = "Middle East & North Africa",
  NAC = "North America",
  SAS = "South Asia",
  SSF = "Sub-Saharan Africa"
  # PSS is handled via country-level corrections (added into Oceania)
)

# Selected countries to save individual-level data (ISO3 codes).
# fmt: skip
COUNTRY_ISO3 <- c(
  "AFG", "ALB", "DZA", "AGO", "ARG", "AUS", "AUT", "BGD", "BEL", "BOL",
  "BRA", "KHM", "CMR", "CAN", "CHL", "CHN", "COL", "CUB", "CZE", "DNK",
  "COD", "ECU", "EGY", "ETH", "FJI", "FIN", "FRA", "DEU", "GHA", "HKG",
  "HUN", "IND", "IDN", "IRN", "IRQ", "ISR", "ITA", "JPN", "KAZ", "KEN",
  "KWT", "LAO", "MYS", "MEX", "MNG", "MAR", "MOZ", "MMR", "NPL", "NLD",
  "NZL", "NGA", "NOR", "PAK", "PNG", "PER", "PHL", "POL", "PRT", "QAT",
  "ROU", "RUS", "SAU", "SGP", "ZAF", "KOR", "ESP", "LKA", "SWE", "CHE",
  "TZA", "THA", "TUR", "UKR", "ARE", "GBR", "USA", "UZB", "VEN", "VNM"
)


# Step 1: Load and pivot GDP data ─────────────────────────────────────────────

cat("STEP 1: Load World Bank GDP data\n")

gdp_raw <- suppressWarnings(read_excel(gdp_path, sheet = "Data", skip = 3))
cc_col <- names(gdp_raw)[grepl("country.code|countrycode", names(gdp_raw), ignore.case = TRUE)][1]
yr_cols <- names(gdp_raw)[grepl("^[0-9]{4}$", names(gdp_raw))]
cat("GDP year range:", min(yr_cols), "-", max(yr_cols), "\n")
cat("Total rows in file:", nrow(gdp_raw), "\n")

gdp_long <- gdp_raw %>%
  rename(ISO3 = all_of(cc_col)) %>%
  pivot_longer(cols = all_of(yr_cols), names_to = "year", values_to = "GDP_PPP_2017USD") %>%
  mutate(year = as.integer(year), GDP_PPP_2017USD = suppressWarnings(as.numeric(GDP_PPP_2017USD))) %>%
  filter(year >= 1970, year <= 2024) %>%
  select(ISO3, year, GDP_PPP_2017USD)


# Step 2: Extract WB regional aggregates and country corrections ──────────────

cat("\nSTEP 2: Extract WB regional aggregates and correction countries\n")

WB_BASE_CODES <- names(WB_TO_REGION) # EAS ECS LCN MEA NAC SAS SSF

# Base WB regional totals
gdp_wb_regions <- gdp_long %>% filter(ISO3 %in% WB_BASE_CODES) %>% rename(wb_code = ISO3)

cat("WB base regions present:", paste(sort(unique(gdp_wb_regions$wb_code)), collapse = ", "), "\n")

# Country-level GDP for all correction ISO3s (and general country saves)
gdp_countries <- gdp_long %>% filter(!ISO3 %in% c(WB_BASE_CODES, "PSS", "WLD")) # exclude aggregates

# GDP for correction countries only
correction_iso3 <- unique(CORRECTIONS$ISO3)
gdp_corr <- gdp_countries %>% filter(ISO3 %in% correction_iso3) %>% left_join(CORRECTIONS, by = "ISO3")

missing_corr <- setdiff(correction_iso3, unique(gdp_corr$ISO3))
if (length(missing_corr) > 0) {
  cat("[NOTE] Correction ISO3 not found in GDP data:", paste(missing_corr, collapse = ", "), "\n")
}


# Step 3: Build correction deltas ─────────────────────────────────────────────

cat("\nSTEP 3: Compute per-year correction deltas\n")

# Amount to subtract from each WB source region, per year
delta_subtract <- gdp_corr %>%
  group_by(from_wb, year) %>%
  summarise(delta = sum(GDP_PPP_2017USD, na.rm = TRUE), .groups = "drop")

# Amount to add to each target region, per year
delta_add <- gdp_corr %>%
  group_by(to_region, year) %>%
  summarise(delta = sum(GDP_PPP_2017USD, na.rm = TRUE), .groups = "drop")

cat("Correction countries with GDP data:", length(unique(gdp_corr$ISO3)), "\n")


# Step 4: Compute adjusted regional GDP ───────────────────────────────────────

cat("\nSTEP 4: Apply corrections to WB regional aggregates\n")

# For each WB base region: subtract the countries that move out
gdp_adjusted_base <- gdp_wb_regions %>%
  left_join(delta_subtract, by = c("wb_code" = "from_wb", "year")) %>%
  mutate(delta = replace_na(delta, 0), GDP_PPP_2017USD = GDP_PPP_2017USD - delta) %>%
  select(wb_code, year, GDP_PPP_2017USD) %>%
  mutate(Region = WB_TO_REGION[wb_code])

# Oceania = AUS + NZL + all PSS countries (all moved out of EAS)
gdp_oceania <- gdp_corr %>%
  filter(to_region == "Oceania") %>%
  group_by(year) %>%
  summarise(GDP_PPP_2017USD = sum(GDP_PPP_2017USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "Oceania")

# Countries moved INTO existing regions: add their GDP to those regions
gdp_additions <- delta_add %>%
  filter(to_region != "Oceania") %>% # Oceania is handled above as new region
  rename(Region = to_region, add_GDP = delta)

# Combine: base (already subtracted) + additions + Oceania
gdp_region_base <- gdp_adjusted_base %>%
  select(Region, year, GDP_PPP_2017USD) %>%
  left_join(gdp_additions, by = c("Region", "year")) %>%
  mutate(add_GDP = replace_na(add_GDP, 0), GDP_PPP_2017USD = GDP_PPP_2017USD + add_GDP) %>%
  select(Region, year, GDP_PPP_2017USD)

gdp_region <- bind_rows(gdp_region_base, gdp_oceania) %>% arrange(Region, year)

cat("Regions in output:", paste(sort(unique(gdp_region$Region)), collapse = ", "), "\n")

# Sanity check: compare world total against WB World aggregate
gdp_world_check <- gdp_long %>% filter(ISO3 == "WLD") %>% rename(WLD_GDP = GDP_PPP_2017USD)

gdp_our_world <- gdp_region %>%
  group_by(year) %>%
  summarise(our_GDP = sum(GDP_PPP_2017USD, na.rm = TRUE), .groups = "drop")

comparison <- gdp_our_world %>%
  left_join(gdp_world_check %>% select(year, WLD_GDP), by = "year") %>%
  mutate(pct_diff = (our_GDP - WLD_GDP) / WLD_GDP * 100) %>%
  filter(!is.na(WLD_GDP))

cat("\nSanity check — our total vs WB World aggregate (sample years):\n")
print(comparison %>% filter(year %in% c(1990, 2000, 2010, 2020, 2024)))


# Step 5: Save outputs ────────────────────────────────────────────────────────

cat("\nSTEP 5: Save GDP outputs\n")

# ── Regional ─────────────────────────────────────────────────────────────────
write_csv(gdp_region, "Parameters/gdp_region.csv")
cat("Saved: Parameters/gdp_region.csv (", nrow(gdp_region), "rows,", length(unique(gdp_region$Region)), "regions )\n")

# ── World aggregate ───────────────────────────────────────────────────────────
gdp_world <- gdp_region %>%
  group_by(year) %>%
  summarise(GDP_PPP_2017USD = sum(GDP_PPP_2017USD, na.rm = TRUE), .groups = "drop")

write_csv(gdp_world, "Parameters/gdp_world.csv")
cat("Saved: Parameters/gdp_world.csv (", nrow(gdp_world), "rows )\n")

# ── Selected country data ─────────────────────────────────────────────────────
gdp_country <- gdp_countries %>% filter(ISO3 %in% COUNTRY_ISO3) %>% select(ISO3, year, GDP_PPP_2017USD)

missing_ctry <- setdiff(COUNTRY_ISO3, unique(gdp_country$ISO3))
if (length(missing_ctry) > 0) {
  cat("[NOTE] Selected countries with no WB GDP data:", paste(missing_ctry, collapse = ", "), "\n")
}

write_csv(gdp_country, "Parameters/gdp_country.csv")
cat(
  "Saved: Parameters/gdp_country.csv (",
  nrow(gdp_country),
  "rows,",
  length(unique(gdp_country$ISO3)),
  "countries )\n"
)

cat("\nAll GDP outputs written to Parameters/\n")

# EoF
