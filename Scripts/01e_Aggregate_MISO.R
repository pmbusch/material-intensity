## =============================================================================
## 01_Aggregate_MISO.R
## Parse and aggregate MISO2 global database to regional totals.
## Produces clean stock, inflow, and outflow files by region × material ×
## end_use × year, converted from kt to Mt.
##
## Input:
##   Inputs/MISO/miso2_global_data_v1.csv  — wide: country × sector × material,
##                                           year columns 1900–2016 (values in kt)
##   Inputs/Dict_Countries.xlsx            — sheet "MISO_Agg": Country → Region
##
## Outputs (Parameters/MISO/):
##   MISO_stock_regional.csv        — Metal ores + Non-metallic minerals only
##   MISO_flows_regional.csv        — Metal ores + Non-metallic minerals only
##   MISO_outflows_regional.csv     — Metal ores + Non-metallic minerals only
##   MISO_stock_regional_all.csv    — all 4 materials (reference)
##   MISO_flows_regional_all.csv    — all 4 materials (reference)
##   MISO_outflows_regional_all.csv — all 4 materials (reference)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# ── Lookup tables (defined before use) ────────────────────────────────────────

# MISO sector labels → snake_case end_use names (global convention)
SECTOR_MAP <- c(
  "Buildings" = "buildings",
  "Infrastructure" = "civil_infrastructure",
  "Machinery" = "machinery",
  "Short lived products" = "short_lived"
)

# MISO material labels → 4-group taxonomy (names match PALETTE_MATERIAL_GROUPS)
MATERIAL_MAP <- c(
  "Biomass" = "Biomass",
  "Fossil-fuels derived" = "Fossil fuels",
  "Metals" = "Metal ores",
  "Non-metallics" = "Non-metallic minerals"
)

# MISO `name` column values and their meaning
NAME_STOCK <- "S10_stock_enduse" # total stock
NAME_FLOWS <- "F_9_10_GAS_enduse" # Gross Addition to Stock
NAME_OUTFLOWS <- "F_10_11_supply_EoL_waste_enduse" # End-of-Life waste

# Only these two materials are in scope for the stock-flow model
IN_SCOPE_MATERIALS <- c("Metal ores", "Non-metallic minerals")


# Step 1: Load data -----------------------------------------------------------

cat("STEP 1: Load MISO2 data and region dictionary\n")

miso_raw <- read_csv("Inputs/MISO/miso2_global_data_v1.csv", show_col_types = FALSE)
cat("MISO raw:", nrow(miso_raw), "x", ncol(miso_raw), "\n")
cat("Column names:\n")
print(names(miso_raw))

dict_miso <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "MISO_Agg")
cat("\nRegions in MISO_Agg:", paste(sort(unique(dict_miso$Region[!is.na(dict_miso$Region)])), collapse = ", "), "\n")


# Step 2: Pivot to long and recode labels ------------------------------------

cat("\nSTEP 2: Pivot to long format and recode sector/material labels\n")

# Identify year columns by pattern (avoids hard-coding year range)
year_cols <- names(miso_raw)[grepl("^[0-9]{4}$", names(miso_raw))]
cat("Year range in MISO:", min(year_cols), "-", max(year_cols), "\n")

cat("Flow types (name column):", paste(sort(unique(miso_raw$name)), collapse = ", "), "\n")
cat("Sectors:  ", paste(sort(unique(miso_raw$sector)), collapse = ", "), "\n")
cat("Materials:", paste(sort(unique(miso_raw$material)), collapse = ", "), "\n")

miso_long <- miso_raw %>%
  pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "value_kt") %>%
  mutate(
    year = as.integer(year),
    end_use = SECTOR_MAP[sector], # recode sector to snake_case
    material = MATERIAL_MAP[material] # recode to 4-group taxonomy
  )
head(miso_long)

# Rows with unmapped sector or material would get NA — flag them
bad_sector <- miso_long %>% filter(is.na(end_use)) %>% distinct(sector) %>% pull()
bad_material <- miso_long %>% filter(is.na(material)) %>% distinct(material) %>% pull()
if (length(bad_sector) > 0) {
  cat("[WARNING] Unmapped sectors:", paste(bad_sector, collapse = ", "), "\n")
}
if (length(bad_material) > 0) {
  cat("[WARNING] Unmapped materials:", paste(bad_material, collapse = ", "), "\n")
}


# Step 3: Join Region, flag unmatched countries ------------------------------

cat("\nSTEP 3: Join Region from MISO_Agg\n")

# MISO `region` column contains country names matching Dict's `Country` column
miso_long <- miso_long %>% left_join(dict_miso %>% dplyr::select(Country, Region), by = c("region" = "Country"))

unmatched <- miso_long %>% filter(is.na(Region)) %>% distinct(region) %>% pull() %>% sort()
if (length(unmatched) > 0) {
  cat("[NOTE] Countries not matched in MISO_Agg (", length(unmatched), "):", paste(unmatched, collapse = ", "), "\n")
}

miso_long <- miso_long %>% filter(!is.na(Region))
cat("Rows after region filter:", nrow(miso_long), "\n")


# Step 4: Split by flow type -------------------------------------------------

cat("\nSTEP 4: Split by flow type\n")

miso_stock <- miso_long %>% filter(name == NAME_STOCK)
miso_flows <- miso_long %>% filter(name == NAME_FLOWS)
miso_outflows <- miso_long %>% filter(name == NAME_OUTFLOWS)

cat("  Stock rows:", nrow(miso_stock), "\n")
cat("  Flows rows:", nrow(miso_flows), "\n")
cat("  Outflows rows:", nrow(miso_outflows), "\n")


# Step 5: Aggregate by Region × material × end_use × year -------------------

cat("\nSTEP 5: Aggregate by Region, material, end_use, year\n")

aggregate_miso <- function(df) {
  df %>%
    group_by(Region, material, end_use, year) %>%
    summarise(value_kt = sum(value_kt, na.rm = TRUE), .groups = "drop") %>%
    mutate(value_Mt = value_kt / 1e3) %>% # kt → Mt
    dplyr::select(Region, material, end_use, year, value_Mt)
}

stock_agg <- aggregate_miso(miso_stock)
flows_agg <- aggregate_miso(miso_flows)
outflows_agg <- aggregate_miso(miso_outflows)

head(stock_agg)

# Step 6: Save all-material reference files and filter to in-scope -----------

cat("\nSTEP 6: Save reference files (all materials) and filter to in-scope\n")

write_csv(stock_agg, "Parameters/MISO/MISO_stock_regional_all.csv")
write_csv(flows_agg, "Parameters/MISO/MISO_flows_regional_all.csv")
write_csv(outflows_agg, "Parameters/MISO/MISO_outflows_regional_all.csv")
cat("  Saved all-material reference files (biomass + fossil_fuels included)\n")

stock_agg <- stock_agg %>% filter(material %in% IN_SCOPE_MATERIALS)
flows_agg <- flows_agg %>% filter(material %in% IN_SCOPE_MATERIALS)
outflows_agg <- outflows_agg %>% filter(material %in% IN_SCOPE_MATERIALS)

write_csv(stock_agg, "Parameters/MISO/MISO_stock_regional.csv")
write_csv(flows_agg, "Parameters/MISO/MISO_flows_regional.csv")
write_csv(outflows_agg, "Parameters/MISO/MISO_outflows_regional.csv")

cat("  Saved: Parameters/MISO/MISO_stock_regional.csv    (", nrow(stock_agg), "rows )\n")
cat("  Saved: Parameters/MISO/MISO_flows_regional.csv    (", nrow(flows_agg), "rows )\n")
cat("  Saved: Parameters/MISO/MISO_outflows_regional.csv (", nrow(outflows_agg), "rows )\n")


# Step 7: Validate global stock 2016 — must be 1093 Gt -------------------

cat("\nSTEP 7: Validate global in-use stock for 2016 (all 4 materials)\n")

# Use the all-material file so biomass and fossil_fuels are included
stock_all <- read_csv("Parameters/MISO/MISO_stock_regional_all.csv", show_col_types = FALSE)

global_2016_Mt <- stock_all %>%
  filter(year == 2016) %>%
  summarise(total_Mt = sum(value_Mt, na.rm = TRUE)) %>%
  pull(total_Mt)

global_2016_Gt <- global_2016_Mt / 1e3
cat(sprintf("  Global in-use stock 2016: %.1f Gt  (expected 1093 Gt; MISO2 ref: 1093 Gt)\n", global_2016_Gt))

if (global_2016_Gt < 1000 || global_2016_Gt > 1100) {
  stop(sprintf(
    "[VALIDATION FAILED] Global 2016 stock = %.1f Gt, expected 1000–1100 Gt. ",
    "Check unit conversion (MISO is in kt; dividing by 1000 gives Mt).",
    global_2016_Gt
  ))
}
cat("  [OK] Global 2016 stock is within the expected range.\n")


# Step 8: Summary — stock by material and region for 2016 -------------------

cat("\nSTEP 8: Summary — 2016 in-scope stock by material and Region\n")

summary_2016 <- stock_agg %>%
  filter(year == 2016) %>%
  group_by(material, Region) %>%
  summarise(total_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(total_Gt = round(total_Mt / 1e3, 3))

print(summary_2016 %>% arrange(material, Region), n = 50)

cat("\n── Sanity check ────────────────────────────────────────────────────────────\n")
cat("  Row counts:  stock =", nrow(stock_agg), "| flows =", nrow(flows_agg), "| outflows =", nrow(outflows_agg), "\n")
cat("  Year range:", min(stock_agg$year), "-", max(stock_agg$year), "\n")

total_by_mat <- stock_agg %>%
  filter(year == 2016) %>%
  group_by(material) %>%
  summarise(Gt = round(sum(value_Mt, na.rm = TRUE) / 1e3, 2), .groups = "drop")
cat("  Total in-scope stock 2016 by material:\n")
print(total_by_mat)

# EoF
