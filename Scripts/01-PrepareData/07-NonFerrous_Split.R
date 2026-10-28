## 07-NonFerrous_Split.R
## Splits UNEP non-ferrous ore DMC into 4 sub-categories (Copper, Bauxite, Gold,
## Other non-ferrous) using regional shares of domestic extraction (DE) by year
## from materialflows.net (06-Aggregate_MaterialFlows.R). Regional DE shares are
## applied to regional DMC: trade in concentrates (DMC != DE) is assumed to have
## the same mix as domestic extraction.
##
## Gold is modelled as a flow material (fixed intensity per GDP), so it is also
## removed from the non-ferrous ore that feeds the stock model, to avoid double
## counting.
##
## Inputs:
##   Parameters/MaterialFlows/DE_detail_region.csv
##   Parameters/UNEP-Materials/materials_region_DMC.csv
##
## Outputs (Parameters/MaterialFlows/):
##   nonfe_split_region.csv        — (Region, year, nonfe_sub, de_share, DMC_Mt)
##   materials_region_DMC_exGold.csv — UNEP DMC with gold removed from "Non-ferrous ores"
##                                     (same columns as materials_region_DMC.csv)

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Level 3 non-ferrous label -> sub-category (everything else -> Other non-ferrous)
NONFE_SUB <- c(
  "Copper ores concentrates and compounds" = "Copper",
  "Bauxite and other aluminium ores concentrates and compounds" = "Bauxite",
  "Gold ores concentrates and compounds" = "Gold"
)
NONFE_SUB_LEVELS <- c("Copper", "Bauxite", "Gold", "Other non-ferrous")


# Step 1: DE shares by region × year --------------------------------------

cat("STEP 1: Non-ferrous DE shares by sub-category\n")

de_sub <- read_csv("Parameters/MaterialFlows/DE_detail_region.csv", show_col_types = FALSE) %>%
  filter(material_parent == "Non-ferrous ores") %>%
  mutate(nonfe_sub = coalesce(unname(NONFE_SUB[material_detail]), "Other non-ferrous")) %>%
  group_by(Region, year, nonfe_sub) %>%
  summarise(DE_Mt = sum(DE_Mt), .groups = "drop") %>%
  complete(Region, year, nonfe_sub = NONFE_SUB_LEVELS, fill = list(DE_Mt = 0))

# Global shares, used where a region has no non-ferrous DE in a year
share_world <- de_sub %>%
  group_by(year, nonfe_sub) %>%
  summarise(DE_Mt = sum(DE_Mt), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share_world = DE_Mt / sum(DE_Mt)) %>%
  ungroup() %>%
  dplyr::select(year, nonfe_sub, share_world)

shares <- de_sub %>%
  group_by(Region, year) %>%
  mutate(de_share = if (sum(DE_Mt) > 0) DE_Mt / sum(DE_Mt) else NA_real_) %>%
  ungroup() %>%
  left_join(share_world, by = c("year", "nonfe_sub")) %>%
  mutate(de_share = coalesce(de_share, share_world)) %>%
  dplyr::select(Region, year, nonfe_sub, de_share)

cat("Region-years using global shares (no regional DE):",
    de_sub %>% group_by(Region, year) %>% summarise(t = sum(DE_Mt), .groups = "drop") %>% filter(t <= 0) %>% nrow(), "\n")


# Step 2: Apply shares to UNEP non-ferrous DMC ----------------------------

cat("\nSTEP 2: Apply regional DE shares to regional non-ferrous DMC\n")

dmc <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)

dmc_nonfe <- dmc %>% filter(material_category == "Non-ferrous ores") %>% dplyr::select(Region, year, nonfe_DMC_Mt = DMC_Mt)

nonfe_split <- dmc_nonfe %>%
  inner_join(shares, by = c("Region", "year"), relationship = "one-to-many") %>%
  mutate(DMC_Mt = nonfe_DMC_Mt * de_share, nonfe_sub = factor(nonfe_sub, levels = NONFE_SUB_LEVELS)) %>%
  arrange(Region, year, nonfe_sub) %>%
  dplyr::select(Region, year, nonfe_sub, de_share, DMC_Mt)

# Checks: years covered, shares sum to 1, split sums back to UNEP
cat("UNEP non-ferrous years:", paste(range(dmc_nonfe$year), collapse = "-"),
    "| split years:", paste(range(nonfe_split$year), collapse = "-"), "\n")
missing_years <- anti_join(dmc_nonfe, nonfe_split, by = c("Region", "year"))
if (nrow(missing_years) > 0) {
  cat("[WARNING] Region-years in UNEP DMC without DE shares (dropped):\n")
  print(missing_years)
}
check <- nonfe_split %>%
  group_by(Region, year) %>%
  summarise(share_sum = sum(de_share), split_Mt = sum(DMC_Mt), .groups = "drop") %>%
  inner_join(dmc_nonfe, by = c("Region", "year"))
cat("Max |share sum - 1|:", signif(max(abs(check$share_sum - 1)), 3),
    "| max |split - UNEP| (Mt):", signif(max(abs(check$split_Mt - check$nonfe_DMC_Mt)), 3), "\n")
cat("Negative non-ferrous DMC region-years:", sum(dmc_nonfe$nonfe_DMC_Mt < 0), "\n")

cat("\nWorld non-ferrous DMC by sub-category (Mt):\n")
nonfe_split %>%
  filter(year %in% c(1970, 2000, 2016, max(year))) %>%
  group_by(year, nonfe_sub) %>%
  summarise(DMC_Mt = round(sum(DMC_Mt)), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = round(DMC_Mt / sum(DMC_Mt) * 100, 1)) %>%
  print(n = 20)

cat("\nRegional gold share of non-ferrous DMC,", max(nonfe_split$year), "(%):\n")
nonfe_split %>%
  filter(year == max(year), nonfe_sub == "Gold") %>%
  transmute(Region, gold_Mt = round(DMC_Mt), gold_share = round(de_share * 100, 1)) %>%
  print()


# Step 3: Save split and gold-free DMC ------------------------------------

cat("\nSTEP 3: Save\n")

write_csv(nonfe_split, "Parameters/MaterialFlows/nonfe_split_region.csv")
cat("Saved: Parameters/MaterialFlows/nonfe_split_region.csv (", nrow(nonfe_split), "rows )\n")

# Same file as materials_region_DMC.csv, with gold ore taken out of "Non-ferrous ores"
gold_dmc <- nonfe_split %>% filter(nonfe_sub == "Gold") %>% dplyr::select(Region, year, gold_Mt = DMC_Mt)

dmc_ex_gold <- dmc %>%
  left_join(gold_dmc, by = c("Region", "year")) %>%
  mutate(DMC_Mt = if_else(material_category == "Non-ferrous ores", DMC_Mt - coalesce(gold_Mt, 0), DMC_Mt)) %>%
  dplyr::select(-gold_Mt)

write_csv(dmc_ex_gold, "Parameters/MaterialFlows/materials_region_DMC_exGold.csv")
cat("Saved: Parameters/MaterialFlows/materials_region_DMC_exGold.csv (", nrow(dmc_ex_gold), "rows )\n")
cat("World gold removed from non-ferrous DMC,", max(gold_dmc$year), ":",
    round(sum(gold_dmc$gold_Mt[gold_dmc$year == max(gold_dmc$year)])), "Mt\n")

# EoF
