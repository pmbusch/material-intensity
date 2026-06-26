## =============================================================================
## 01d-Aggregate_MaterialFlows.R
## Aggregates UNEP domestic extraction (DE) material flow data to world and
## regional totals at three levels of material detail.
##
## Input:
##   Inputs/MaterialFlows_all.csv         — country × year × material (Level 1-3)
##   Inputs/Dict_Countries.xlsx           — sheet "MaterialFlows_Agg": country → Region
##
## Outputs (Parameters/MaterialFlows/):
##   materialFlows_region_L1_DE.csv  — (Region, year, material_l1, DE_Mt)
##   materialFlows_world_L1_DE.csv   — (year, material_l1, DE_Mt)
##   materialFlows_region_L2_DE.csv  — (Region, year, material_l1, material_l2, DE_Mt)
##   materialFlows_world_L2_DE.csv   — (year, material_l1, material_l2, DE_Mt)
##   materialFlows_region_L3_DE.csv  — (Region, year, material_l1, material_l2, material_l3, DE_Mt)
##   materialFlows_world_L3_DE.csv   — (year, material_l1, material_l2, material_l3, DE_Mt)
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# Step 1: Load data -----------------------------------------------------------

cat("STEP 1: Load data and dictionaries\n")

mf_raw <- read_csv("Inputs/MaterialFlows_all.csv", show_col_types = FALSE)
cat("MaterialFlows raw:", nrow(mf_raw), "x", ncol(mf_raw), "\n")
cat("Levels present:", paste(sort(unique(mf_raw$level_material)), collapse = ", "), "\n")
dim(mf_raw)

dict_agg <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "MaterialFlows_Agg")
cat(
  "Dict regions:",
  paste(sort(unique(dict_agg$Region[dict_agg$Region != "NA" & !is.na(dict_agg$Region)])), collapse = ", "),
  "\n"
)


# Step 2: Join Region and clean -----------------------------------------------

cat("\nSTEP 2: Join Region from MaterialFlows_Agg\n")

mf <- mf_raw %>%
  rename(amount_t = `Amount (t)`) %>%
  left_join(dict_agg %>% dplyr::select(country, Region), by = "country") %>%
  mutate(DE_Mt = amount_t / 1e6)

unmatched <- mf %>% filter(is.na(Region)) %>% distinct(country) %>% pull() %>% sort()
if (length(unmatched) > 0) {
  cat("[NOTE] Countries not matched in dict (", length(unmatched), "):", paste(unmatched, collapse = ", "), "\n")
}

mf <- mf %>% filter(!is.na(Region), Region != "NA")
cat("Rows after region filter:", nrow(mf), "\n")
cat("Unique countries:", length(unique(mf$country)), "\n")


# Step 3: Build Level 2 → Level 1 parent mapping ------------------------------

cat("\nSTEP 3: Build material hierarchy mapping\n")

# Level 2 labels have Level 1 as their Parent column
map_l2_to_l1 <- mf %>%
  filter(level_material == "Level 2") %>%
  distinct(Label, Parent) %>%
  rename(material_l2 = Label, material_l1 = Parent)

cat("Level 2 → Level 1 mappings:", nrow(map_l2_to_l1), "\n")
print(map_l2_to_l1)


# Helper: save regional + world pair -----------------------------------------

save_outputs <- function(df_region, df_world, level_tag) {
  region_path <- paste0("Parameters/MaterialFlows/materialFlows_region_", level_tag, "_DE.csv")
  world_path <- paste0("Parameters/MaterialFlows/materialFlows_world_", level_tag, "_DE.csv")

  write_csv(df_region, region_path)
  cat("  Saved:", region_path, "(", nrow(df_region), "rows,", length(unique(df_region$Region)), "regions )\n")

  write_csv(df_world, world_path)
  cat("  Saved:", world_path, "(", nrow(df_world), "rows )\n")
}


# Step 4: Level 1 -------------------------------------------------------------

cat("\nSTEP 4: Aggregate Level 1\n")

mf_l1 <- mf %>% filter(level_material == "Level 1") %>% rename(material_l1 = Label)

l1_region <- mf_l1 %>%
  group_by(Region, year, material_l1) %>%
  summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

l1_world <- mf_l1 %>% group_by(year, material_l1) %>% summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

save_outputs(l1_region, l1_world, "L1")


# Step 5: Level 2 -------------------------------------------------------------

cat("\nSTEP 5: Aggregate Level 2\n")

mf_l2 <- mf %>% filter(level_material == "Level 2") %>% rename(material_l2 = Label, material_l1 = Parent)

l2_region <- mf_l2 %>%
  group_by(Region, year, material_l1, material_l2) %>%
  summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

l2_world <- mf_l2 %>%
  group_by(year, material_l1, material_l2) %>%
  summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

save_outputs(l2_region, l2_world, "L2")


# Step 6: Level 3 -------------------------------------------------------------

cat("\nSTEP 6: Aggregate Level 3\n")

mf_l3 <- mf %>%
  filter(level_material == "Level 3") %>%
  rename(material_l3 = Label, material_l2 = Parent) %>%
  left_join(map_l2_to_l1, by = "material_l2")

unresolved_l3 <- mf_l3 %>% filter(is.na(material_l1)) %>% distinct(material_l2) %>% pull()
if (length(unresolved_l3) > 0) {
  cat("[WARNING] Level 3 parents not found in Level 2 mapping:", paste(unresolved_l3, collapse = ", "), "\n")
}

l3_region <- mf_l3 %>%
  group_by(Region, year, material_l1, material_l2, material_l3) %>%
  summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

l3_world <- mf_l3 %>%
  group_by(year, material_l1, material_l2, material_l3) %>%
  summarise(DE_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

save_outputs(l3_region, l3_world, "L3")


cat("\nAll MaterialFlows outputs written to Parameters/MaterialFlows/\n")

# EoF
