## =============================================================================
## 01a-Aggregate_UNEP.R
## Aggregates UNEP material flow data to world regions using the UNEP_Agg sheet
## in Inputs/Dict_Countries.xlsx. Countries with Region == "NA" are dropped.
##
## Outputs (Parameters/):
##   materials_region_{FC}.csv   — (Region, year, material_category, {FC}_Mt)
##   materials_country_{FC}.csv  — (ISO3, year, material_category, {FC}_Mt)  [selected countries]
##   materials_world_{FC}.csv    — (year, material_category, {FC}_Mt)
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Regional aggregate labels to exclude from country-level data
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
  "TWN", "TZA", "THA", "TUR", "UKR", "ARE", "GBR", "USA", "UZB", "VEN",
  "VNM"
)


# Step 1: Load data -------------------------------------------------------

cat("STEP 1: Load UNEP data and dictionaries\n")

unep_raw <- read_csv("Inputs/UNEP/mfa13_export.csv", show_col_types = FALSE)
cat("UNEP raw:", nrow(unep_raw), "x", ncol(unep_raw), "\n")
cat("Flow codes in file:", paste(sort(unique(unep_raw[["Flow code"]])), collapse = ", "), "\n")

# UNEP_Agg: UNEP_name → ISO3, Region
dict_unep_agg <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "UNEP_Agg")
cat(
  "UNEP_Agg regions (excluding NA):",
  paste(sort(unique(dict_unep_agg$Region[dict_unep_agg$Region != "NA"])), collapse = ", "),
  "\n"
)


# Step 2: Pivot to long and drop supra-national aggregates ----------------

cat("\nSTEP 2: Pivot to long format\n")

unep_long <- unep_raw %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value_t") %>%
  mutate(
    year = as.integer(year),
    value_Mt = value_t / 1e6 # metric tonnes → megatonnes
  ) %>%
  rename(UNEP_name = Country, material_category = Category, flow_code = `Flow code`) %>%
  filter(!UNEP_name %in% REGION_AGGREGATES) %>%
  dplyr::select(UNEP_name, year, material_category, flow_code, value_Mt)

cat("Rows after pivot + supra-national drop:", nrow(unep_long), "\n")


# Step 3: Join ISO3 and Region, drop unassigned countries -----------------

cat("\nSTEP 3: Join ISO3 and Region from UNEP_Agg\n")

unep_matched <- unep_long %>% left_join(dict_unep_agg %>% dplyr::select(UNEP_name, ISO3, Region), by = "UNEP_name")

unmatched <- unep_matched %>% filter(is.na(ISO3)) %>% distinct(UNEP_name) %>% pull() %>% sort()
cat("UNEP names not matched in UNEP_Agg (", length(unmatched), "):", paste(unmatched, collapse = ", "), "\n")

# Drop countries without ISO3 or with Region == "NA" (no assignment)
unep_matched <- unep_matched %>% filter(!is.na(ISO3), !is.na(Region), Region != "NA")

cat("Rows after ISO3 + Region filter:", nrow(unep_matched), "\n")
cat("Unique ISO3:", length(unique(unep_matched$ISO3)), "\n")
cat("Regions:", paste(sort(unique(unep_matched$Region)), collapse = ", "), "\n")


# Step 4: Historical political-aggregate corrections ----------------------

cat("\nSTEP 4: Apply historical political-aggregate corrections\n")

apply_flow_aggregates <- function(df, dict) {
  # Applies country-split / country-merge cases to a single-flow data frame.
  # df columns: ISO3, year, material_category, flow_code, value_Mt, Region

  apply_split <- function(df, aggregate_iso, split_year, successor_isos, label) {
    succ_before <- df %>% filter(ISO3 %in% successor_isos, year < split_year)
    if (nrow(succ_before) > 0) {
      cat("    [WARNING]", label, "— successors have data before split year\n")
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
    cat("    CASE4_Sudan: not found in dict — skipping\n")
  }

  # CASE 5: Ethiopia Former → successors from 1993
  eth_iso <- dict %>% filter(grepl("Ethiopia.*Former|Former.*Ethiopia", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
  if (length(eth_iso) > 0) {
    df <- apply_split(df, eth_iso[1], 1993, c("ETH", "ERI"), "CASE5_Ethiopia")
  } else {
    cat("    CASE5_Ethiopia: not found in dict — skipping\n")
  }

  # CASE 6: Yemen — sum North + South Yemen pre-1990 into unified YEM
  north_yem_iso <- dict %>% filter(grepl("North.*Yemen|Yemen.*North", UNEP_name, ignore.case = TRUE)) %>% pull(ISO3)
  south_yem_iso <- dict %>%
    filter(grepl("South.*Yemen|Yemen.*South|Yemen.*People|Democratic.*Yemen", UNEP_name, ignore.case = TRUE)) %>%
    pull(ISO3)
  old_yem_isos <- c(north_yem_iso, south_yem_iso)

  if (length(old_yem_isos) > 0) {
    yem_region <- df %>% filter(ISO3 == "YEM") %>% pull(Region) %>% na.omit() %>% unique() %>% head(1)
    flow_val <- df %>% filter(ISO3 %in% old_yem_isos) %>% pull(flow_code) %>% unique() %>% head(1)
    yem_pre <- df %>%
      filter(ISO3 %in% old_yem_isos, year <= 1989) %>%
      group_by(year, material_category) %>%
      summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
      mutate(ISO3 = "YEM", flow_code = flow_val, Region = if (length(yem_region) > 0) yem_region else NA_character_)
    df <- df %>% filter(!ISO3 %in% old_yem_isos) %>% bind_rows(yem_pre)
    cat("    CASE6_Yemen — aggregated N+S Yemen into YEM pre-1990\n")
  } else {
    cat("    CASE6_Yemen: N/S Yemen not found — skipping\n")
  }

  # CASE 7: Netherlands Antilles → successors from 2012
  df <- apply_split(df, "ANT", 2012, c("CUW", "SXM"), "CASE7_NethAntilles")

  df
}


# Step 5: Process each flow code and save ---------------------------------

cat("\nSTEP 5: Process per-flow and save regional, country, and world CSV files\n")

for (fc in FLOW_CODES) {
  cat("\n  ══ Flow:", fc, "══\n")

  df_flow <- unep_matched %>% filter(flow_code == fc)

  if (nrow(df_flow) == 0) {
    cat("  [SKIP] No data for flow code", fc, "\n")
    next
  }
  cat("  Rows before aggregate processing:", nrow(df_flow), "\n")

  df_flow <- apply_flow_aggregates(df_flow, dict_unep_agg)

  # Duplicate guard
  dups <- df_flow %>% group_by(ISO3, year, material_category) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1)
  if (nrow(dups) > 0) {
    cat("  [ERROR] Duplicate (ISO3, year, material) rows for", fc, "!\n")
    print(dups)
  } else {
    cat("  [OK] No duplicate rows\n")
  }

  out_col <- paste0(fc, "_Mt")

  # ── Regional aggregate ──────────────────────────────────────────────────────
  df_region <- df_flow %>%
    group_by(Region, year, material_category) %>%
    summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
    rename(!!out_col := value_Mt)

  out_path <- paste0("Parameters/materials_region_", fc, ".csv")
  write_csv(df_region, out_path)
  cat("  Saved:", out_path, "(", nrow(df_region), "rows,", length(unique(df_region$Region)), "regions )\n")

  # ── Selected country data ───────────────────────────────────────────────────
  df_country <- df_flow %>%
    filter(ISO3 %in% COUNTRY_ISO3) %>%
    dplyr::select(ISO3, year, material_category, value_Mt) %>%
    rename(!!out_col := value_Mt)

  out_path <- paste0("Parameters/materials_country_", fc, ".csv")
  write_csv(df_country, out_path)
  cat("  Saved:", out_path, "(", nrow(df_country), "rows,", length(unique(df_country$ISO3)), "countries )\n")

  # ── World aggregate ─────────────────────────────────────────────────────────
  df_world <- df_flow %>%
    group_by(year, material_category) %>%
    summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
    rename(!!out_col := value_Mt)

  out_path <- paste0("Parameters/materials_world_", fc, ".csv")
  write_csv(df_world, out_path)
  cat("  Saved:", out_path, "(", nrow(df_world), "rows )\n")
}

cat("\nAll UNEP outputs written to Parameters/\n")

# EoF
