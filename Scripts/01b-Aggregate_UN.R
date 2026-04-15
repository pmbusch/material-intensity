## =============================================================================
## 01b-Aggregate_UN.R
## Aggregates UN WPP population data to world regions using the UN_Agg sheet
## in Inputs/Dict_Countries.xlsx.
##
## Outputs (Parameters/):
##   population_region_historical.csv — (Region, year, population) 1970–2024
##   population_region_forecast.csv   — (Region, year, population) 2025–2100
##   population_country_historical.csv — (ISO3, year, population) [selected countries]
##   population_country_forecast.csv   — (ISO3, year, population) [selected countries]
##   population_world_historical.csv  — (year, population)
##   population_world_forecast.csv    — (year, population)
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

pop_path <- "Inputs/UN/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"

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


# Step 1: Build ISO3 → Region mapping from Countries + UN_Agg ─────────────────

cat("STEP 1: Build ISO3 → Region mapping\n")

# Countries sheet: has ISO3 and UN_Pop_name
dict_countries <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "Countries")

# UN_Agg sheet: has UN_Pop_name and Region
dict_un_agg <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "UN_Agg")

# Join to get ISO3 ↔ Region via the shared UN_Pop_name key
iso_region_map <- dict_countries %>%
  select(ISO3, UN_Pop_name) %>%
  inner_join(dict_un_agg %>% select(UN_Pop_name, Region), by = "UN_Pop_name") %>%
  filter(!is.na(ISO3), !is.na(Region))

cat("ISO3-Region pairs in mapping:", nrow(iso_region_map), "\n")
cat("Regions:", paste(sort(unique(iso_region_map$Region)), collapse = ", "), "\n")


# Step 2: Read WPP sheets ─────────────────────────────────────────────────────

cat("\nSTEP 2: Read WPP population sheets\n")

read_wpp_sheet <- function(path, sheet_name) {
  raw <- read_excel(path, sheet = sheet_name, skip = 16, col_types = c(rep("guess", 5), "text", rep("guess", 59)))

  type_col <- names(raw)[grepl("type|LocType", names(raw), ignore.case = TRUE)][1]
  pop_nm_col <- names(raw)[grepl("region.*country|country.*area|location", names(raw), ignore.case = TRUE)][1]
  iso3_col <- names(raw)[grepl("ISO3|alpha.3|iso.*3", names(raw), ignore.case = TRUE)][1]
  year_col <- names(raw)[grepl("^year$|^Time$", names(raw), ignore.case = TRUE)][1]
  pop_col <- names(raw)[grepl("total.*pop|pop.*total", names(raw), ignore.case = TRUE)][1]

  cat("  Sheet:", sheet_name, "| type:", type_col, "| iso3:", iso3_col, "| year:", year_col, "| pop:", pop_col, "\n")

  raw %>%
    rename(
      loc_type = all_of(type_col),
      loc_name = all_of(pop_nm_col),
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

cat("Estimates year range:      ", min(pop_est$year), "-", max(pop_est$year), "\n")
cat("Medium variant year range: ", min(pop_med$year), "-", max(pop_med$year), "\n")


# Step 3: Build historical (1970–2024) and forecast (2025–2100) series ────────

cat("\nSTEP 3: Build historical and forecast population series\n")

# Historical: Estimates 1970–2023 + Medium Variant 2024
pop_historical <- bind_rows(pop_est %>% filter(year >= 1970, year <= 2023), pop_med %>% filter(year == 2024)) %>%
  arrange(ISO3, year)

# Apply same historical political-split filters as materials
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

# Forecast: Medium Variant 2025–2100
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


# Step 4: Save outputs ────────────────────────────────────────────────────────

cat("\nSTEP 4: Aggregate and save population files\n")

save_pop_outputs <- function(pop_df, label) {
  pop_with_region <- pop_df %>% left_join(iso_region_map %>% select(ISO3, Region), by = "ISO3")

  # ── Regional aggregate ──────────────────────────────────────────────────────
  pop_region <- pop_with_region %>%
    filter(!is.na(Region)) %>%
    group_by(Region, year) %>%
    summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

  path <- paste0("Parameters/population_region_", label, ".csv")
  write_csv(pop_region, path)
  cat("  Saved:", path, "(", nrow(pop_region), "rows,", length(unique(pop_region$Region)), "regions )\n")

  # ── World aggregate ─────────────────────────────────────────────────────────
  # Sum only countries that have a region assignment (consistent with regional)
  pop_world <- pop_with_region %>%
    filter(!is.na(Region)) %>%
    group_by(year) %>%
    summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

  path <- paste0("Parameters/population_world_", label, ".csv")
  write_csv(pop_world, path)
  cat("  Saved:", path, "(", nrow(pop_world), "rows )\n")

  # ── Selected country data ───────────────────────────────────────────────────
  pop_country <- pop_df %>% filter(ISO3 %in% COUNTRY_ISO3) %>% select(ISO3, year, population)

  path <- paste0("Parameters/population_country_", label, ".csv")
  write_csv(pop_country, path)
  cat("  Saved:", path, "(", nrow(pop_country), "rows,", length(unique(pop_country$ISO3)), "countries )\n")

  # Coverage report: countries in COUNTRY_ISO3 not found in this dataset
  missing_ctry <- setdiff(COUNTRY_ISO3, unique(pop_df$ISO3))
  if (length(missing_ctry) > 0) {
    cat("  [NOTE] Selected countries with no UN population data:", paste(missing_ctry, collapse = ", "), "\n")
  }
}

save_pop_outputs(pop_historical, "historical")
save_pop_outputs(pop_forecast, "forecast")

cat("\nAll UN population outputs written to Parameters/\n")

# EoF
