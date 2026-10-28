## 06-Aggregate_MaterialFlows.R
## Aggregates the materialflows.net domestic extraction (DE) files (one CSV per
## country × year, full material detail) to the 8 model regions.
## Source: https://visualisations.materialflows.net/mf-shiny/?#shiny-tab-sunburst
##
## Inputs:
##   Inputs/MaterialFlows/<date>_DomesticExtractionof<Country>in<Year>bymaterialgroup.csv
##   Inputs/Dict_Countries.xlsx — sheet "MaterialFlows_Agg": country → Region
##
## Output (Parameters/MaterialFlows/):
##   DE_detail_region.csv — (material_parent, material_detail, Region, year, DE_Mt)
##   material_parent = Level 2 (e.g. Non-ferrous ores), material_detail = Level 3 (e.g. Copper ores ...)

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Level 0 / Level 1 labels: anything that is not one of these (or a child of them) is Level 3 detail
LEVEL_0_1 <- c("Domestic Extraction", "Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")

# N/S Yemen files overlap the unified Yemen series (all years present) -> drop to avoid double counting
DROP_COUNTRIES <- c("NorthYemen", "SouthYemen")


# Step 1: List files and keep latest download per country-year -----------

cat("STEP 1: List files\n")

mf_files <- tibble(path = list.files("Inputs/MaterialFlows", pattern = "bymaterialgroup\\.csv$", full.names = TRUE)) %>%
  mutate(
    filename = basename(path),
    download_date = ymd(str_extract(filename, "^[0-9]{4}-[0-9]{2}-[0-9]{2}")),
    country = str_match(filename, "DomesticExtractionof(.+)in[0-9]{4}bymaterialgroup\\.csv$")[, 2],
    year = as.integer(str_match(filename, "in([0-9]{4})bymaterialgroup\\.csv$")[, 2])
  )
cat("Files found:", nrow(mf_files), "\n")

# Some country-years were downloaded more than once: keep the most recent download
mf_files <- mf_files %>%
  group_by(country, year) %>%
  filter(download_date == max(download_date)) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!country %in% DROP_COUNTRIES)
cat("Files after de-duplication:", nrow(mf_files), "| countries:", n_distinct(mf_files$country), "\n")
cat("Years:", paste(range(mf_files$year), collapse = "-"), "\n")


# Step 2: Read all files --------------------------------------------------

cat("\nSTEP 2: Read files (takes a few minutes)\n")

# Empty files (country did not exist that year) only have the header and drop out here
# read.csv, not fread: fread memory-maps each file and R aborts after ~8,000 files on Google Drive
mf_raw <- rbindlist(
  lapply(mf_files$path, read.csv, check.names = FALSE, colClasses = c("character", "character", "numeric"), encoding = "UTF-8"),
  idcol = "file_id"
) %>%
  as_tibble() %>%
  rename(amount_t = `Amount (t)`) %>%
  mutate(country = mf_files$country[file_id], year = mf_files$year[file_id]) %>%
  dplyr::select(country, year, Label, Parent, amount_t)
cat("Rows read:", nrow(mf_raw), "\n")


# Step 3: Join Region -----------------------------------------------------

cat("\nSTEP 3: Join Region from MaterialFlows_Agg\n")

dict_agg <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "MaterialFlows_Agg")

mf <- mf_raw %>% left_join(dict_agg %>% dplyr::select(country, Region), by = "country")

unmatched <- mf %>% filter(is.na(Region)) %>% distinct(country) %>% pull() %>% sort()
cat("Countries not matched in dict (dropped,", length(unmatched), "):", paste(unmatched, collapse = ", "), "\n")

mf <- mf %>% filter(!is.na(Region), Region != "NA")

# Former countries vs successors: report any year where both have data (double counting)
cat("Year overlap between former countries and successors:\n")
mf %>%
  distinct(country, year) %>%
  filter(country %in% c("USSR", "RussianFederation", "YugoslaviaSFR", "Serbia(andMontenegro)", "Serbia",
                        "Czechoslovakia", "CzechRepublic", "Sudan(Former)", "Sudan", "SouthSudan",
                        "Ethiopia(Former)", "Ethiopia", "Eritrea", "NetherlandsAntilles")) %>%
  group_by(country) %>%
  summarise(first = min(year), last = max(year), .groups = "drop") %>%
  print()


# Step 4: Keep Level 3 detail ---------------------------------------------

cat("\nSTEP 4: Keep Level 3 rows (detail) with their Level 2 parent\n")

# Level 2 rows have a Level 1 parent; Level 3 rows have a Level 2 parent
mf_l3 <- mf %>%
  filter(!Label %in% LEVEL_0_1, !Parent %in% LEVEL_0_1, !is.na(Parent), Parent != "") %>%
  rename(material_parent = Parent, material_detail = Label)

# Check: sum of Level 3 should match the Level 2 totals reported in the same files
l2_check <- mf %>%
  filter(Parent %in% LEVEL_0_1[-1]) %>%
  group_by(material_parent = Label) %>%
  summarise(L2_Mt = sum(amount_t, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  left_join(
    mf_l3 %>% group_by(material_parent) %>% summarise(L3_Mt = sum(amount_t, na.rm = TRUE) / 1e6, .groups = "drop"),
    by = "material_parent"
  ) %>%
  mutate(diff_pct = round((L3_Mt / L2_Mt - 1) * 100, 2))
print(l2_check, n = 50)

cat("Detail materials per parent:\n")
mf_l3 %>% distinct(material_parent, material_detail) %>% count(material_parent) %>% print(n = 50)


# Step 5: Aggregate to region and save ------------------------------------

cat("\nSTEP 5: Aggregate to Region × year × material and save\n")

de_region <- mf_l3 %>%
  group_by(material_parent, material_detail, Region, year) %>%
  summarise(DE_Mt = sum(amount_t, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  arrange(material_parent, material_detail, Region, year)

dir.create("Parameters/MaterialFlows", showWarnings = FALSE)
write_csv(de_region, "Parameters/MaterialFlows/DE_detail_region.csv")
cat("Saved: Parameters/MaterialFlows/DE_detail_region.csv (", nrow(de_region), "rows )\n")

# Global DE by Level 2 in the last year, as a quick summary
de_region %>%
  filter(year == max(year)) %>%
  group_by(material_parent) %>%
  summarise(DE_Gt = round(sum(DE_Mt) / 1e3, 2), .groups = "drop") %>%
  arrange(desc(DE_Gt)) %>%
  print(n = 50)

# EoF
