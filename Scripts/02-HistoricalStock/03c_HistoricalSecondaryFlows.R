## =============================================================================
## 03c_HistoricalSecondaryFlows.R
## Historical secondary (recycled) material flow reconstruction, 1970-2024,
## for Metal ores and Non-metallic minerals.
##
## No historical secondary-flow time series exists anywhere upstream -- only a
## single 2024 anchor for metals (Parameters/Intermediate/nonprimary_share_2024.csv,
## from Script 03 Step 5b). This script builds a full 1970-2024 series for both
## metal ores and non-metallic minerals using:
##   implied recycling rate(Region, super_category, year)
##     = clamp( [MISO_inflow - UNEP_inflow] / MISO_waste , 0, 1 )
##   secondary_Mt = MISO_waste x implied recycling rate
## i.e. the MISO/UNEP inflow scope-factor wedge (Script 02b's A, where
## MISO_inflow = UNEP_inflow x A) is treated as an empirical estimate of
## non-primary supply, then re-expressed as a RATE relative to real historical
## waste (MISO's own end-of-life outflow series) so the reconstructed secondary
## flow can never exceed the waste actually available that year.
##
## Non-metallic minerals have no scope factor A computed anywhere upstream
## (Script 02b explicitly skips them, noting the scope gap is "negligible" for
## stock calibration); this script computes an analogous A_nonmet inline. That
## prior "negligible" finding means the non-metallic secondary series here may
## come out small/noisy -- printed diagnostics flag this rather than assume it.
##
## Input:
##   Parameters/Intermediate/miso_unep_scope_factor_A.csv   -- metal A (Script 02b)
##   Parameters/Intermediate/UNEP_flows_subenduse.csv       -- Script 01b
##   Parameters/Intermediate/nonprimary_share_2024.csv      -- Script 03 (cross-check anchor)
##   Parameters/materials_region_DMC.csv                    -- raw UNEP DMC (primary)
##   Parameters/MISO/MISO_flows_regional.csv                -- MISO inflows (non-metallic A)
##   Parameters/MISO/MISO_outflows_regional.csv             -- MISO waste/EoL flows
##   Parameters/MISO/metal_grade_ore.csv                    -- ore -> metal grade g
##
## Output:
##   Parameters/Intermediate/historical_secondary_flows.csv
##     columns: Region, material_group, year, primary_Mt, secondary_Mt,
##       waste_Mt, recovered_Mt
##     waste_Mt/recovered_Mt (added for Figure 5's contour version, panels
##     c/d's historical recycling/downcycling anchor) are the pre-ore-grade-
##     conversion mass pair recovered_Mt/waste_Mt already computed in Step 5
##     below (secondary_Mt_carrier/waste_Mt) -- i.e. real recovered-outflow
##     mass over real total-outflow mass, not a literature rate average.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# -- Constants (mirrors Script 02b's bridge-year convention) ------------------
HIST_START <- 1970L
HIST_END <- 2024L
MISO_LAST_YEAR <- 2016L
A_CLIP_MIN <- 0.5
A_CLIP_MAX <- 4.0
A_SMOOTH_WINDOW <- 5L

METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

cat("=== Historical secondary flow reconstruction (metal ores + non-metallic minerals) ===\n\n")


# Step 1: Metal scope factor (Script 02b output) ------------------------------

cat("STEP 1: Load metal A factor + raw inflow components\n")

metal_A <- read_csv("Parameters/Intermediate/miso_unep_scope_factor_A.csv", show_col_types = FALSE) %>%
  mutate(material_group = "metal_ores")

cat("  Metal A rows:", nrow(metal_A), "| years:", paste(range(metal_A$year), collapse = "-"), "\n")


# Step 2: Non-metallic minerals scope factor (analogous, computed here) ------
# Simpler than metals: no Fe/NonFe split, no ore-grade conversion -- UNEP DMC
# carrier mass IS the reported mass for non-metallic minerals.

cat("\nSTEP 2: Non-metallic minerals scope factor A_nonmet\n")

unep_sub <- read.csv("Parameters/Intermediate/UNEP_flows_subenduse.csv")
miso_flows <- read_csv("Parameters/MISO/MISO_flows_regional.csv", show_col_types = FALSE)

unep_nonmet_super <- unep_sub %>%
  filter(material == "Non-metallic minerals", year >= HIST_START, year <= MISO_LAST_YEAR) %>%
  group_by(Region, super_category, year) %>%
  summarise(unep_inflow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop")

miso_nonmet_super <- miso_flows %>%
  filter(material == "Non-metallic minerals", year >= HIST_START, year <= MISO_LAST_YEAR) %>%
  rename(super_category = end_use) %>%
  group_by(Region, super_category, year) %>%
  summarise(miso_inflow_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop")

nonmet_A_raw <- unep_nonmet_super %>%
  left_join(miso_nonmet_super, by = c("Region", "super_category", "year")) %>%
  mutate(
    A = case_when(
      is.na(miso_inflow_Mt) ~ 1.0,
      unep_inflow_Mt <= 1e-6 ~ 1.0,
      TRUE ~ miso_inflow_Mt / unep_inflow_Mt
    )
  ) %>%
  arrange(Region, super_category, year)

# Centered rolling mean via data.table::frollmean (already a project
# dependency) -- avoids re-declaring Script 02b's roll_mean_partial(). Edge
# years without a full window fall back to the unsmoothed A.
nonmet_A_smooth <- nonmet_A_raw %>%
  group_by(Region, super_category) %>%
  mutate(
    A_smooth = data.table::frollmean(A, n = A_SMOOTH_WINDOW, align = "center", na.rm = TRUE),
    A_smooth = dplyr::coalesce(A_smooth, A),
    A_clipped = pmin(A_CLIP_MAX, pmax(A_CLIP_MIN, A_smooth))
  ) %>%
  ungroup()

# hold at MISO_LAST_YEAR value for the bridge years 2017-2024, same convention as Script 02b
nonmet_A_hold <- nonmet_A_smooth %>%
  filter(year == MISO_LAST_YEAR) %>%
  dplyr::select(Region, super_category, A_clipped, unep_inflow_Mt, miso_inflow_Mt) %>%
  tidyr::crossing(year = seq(MISO_LAST_YEAR + 1L, HIST_END))

nonmet_A <- bind_rows(
  nonmet_A_smooth %>% dplyr::select(Region, super_category, year, A_clipped, unep_inflow_Mt, miso_inflow_Mt),
  nonmet_A_hold
) %>%
  rename(A = A_clipped) %>%
  mutate(material_group = "nonmetallic_minerals") %>%
  arrange(Region, super_category, year)

cat("  Non-metallic A rows:", nrow(nonmet_A), "| median A:", round(median(nonmet_A$A), 2), "\n")


# Step 3: Combine scope factors, compute the MISO-scope wedge ----------------

cat("\nSTEP 3: MISO-scope wedge (non-primary inflow) = unep_inflow_Mt x (A - 1)\n")

scope_combined <- bind_rows(
  metal_A %>% dplyr::select(Region, super_category, year, material_group, A, unep_inflow_Mt, miso_inflow_Mt),
  nonmet_A %>% dplyr::select(Region, super_category, year, material_group, A, unep_inflow_Mt, miso_inflow_Mt)
) %>%
  mutate(nonprimary_inflow_Mt = unep_inflow_Mt * (A - 1))

# Diagnostic: A < 1 (net-exporting Region x super_category x year -- e.g.
# East Asia for metals, per Script 02b's docstring) makes this wedge negative.
# Physically, recycling can't be negative -- flagged here, not silently
# clamped away in Step 5, so it's visible before deciding how to handle it.
n_negative <- scope_combined %>% filter(nonprimary_inflow_Mt < 0) %>% nrow()
cat(
  "  Region x super_category x year groups with NEGATIVE nonprimary_inflow_Mt (A < 1):",
  n_negative, "/", nrow(scope_combined), "\n"
)
if (n_negative > 0) {
  cat("  [FLAG] Negative-wedge groups by material_group x Region:\n")
  print(
    scope_combined %>%
      filter(nonprimary_inflow_Mt < 0) %>%
      count(material_group, Region) %>%
      arrange(material_group, desc(n)),
    n = 30
  )
}


# Step 4: Real historical waste (EoL) flows, MISO -----------------------------
# MISO_outflows_regional.csv covers 1900-2016; hold flat 2017-2024 (bridge
# years), matching Script 02b/03's existing bridge-year convention -- no
# growth signal exists for waste beyond MISO's own coverage.

cat("\nSTEP 4: Historical waste flows (MISO outflows)\n")

miso_waste_raw <- read_csv("Parameters/MISO/MISO_outflows_regional.csv", show_col_types = FALSE) %>%
  filter(material %in% c("Metal ores", "Non-metallic minerals"), year >= HIST_START, year <= MISO_LAST_YEAR) %>%
  mutate(
    material_group = recode(material, "Metal ores" = "metal_ores", "Non-metallic minerals" = "nonmetallic_minerals")
  ) %>%
  rename(super_category = end_use, waste_Mt = value_Mt) %>%
  dplyr::select(Region, material_group, super_category, year, waste_Mt)

waste_hold <- miso_waste_raw %>%
  filter(year == MISO_LAST_YEAR) %>%
  dplyr::select(-year) %>%
  tidyr::crossing(year = seq(MISO_LAST_YEAR + 1L, HIST_END))

miso_waste <- bind_rows(miso_waste_raw, waste_hold) %>% arrange(Region, material_group, super_category, year)

cat("  Waste rows:", nrow(miso_waste), "| years:", paste(range(miso_waste$year), collapse = "-"), "\n")


# Step 5: Implied recycling rate + secondary flow (metal mass / raw mass) -----

cat("\nSTEP 5: Implied recycling rate = wedge / waste (clamped to [0, 1])\n")

secondary_super <- scope_combined %>%
  inner_join(miso_waste, by = c("Region", "material_group", "super_category", "year")) %>%
  mutate(
    raw_rate = if_else(is.na(waste_Mt) | waste_Mt <= 0, NA_real_, nonprimary_inflow_Mt / waste_Mt),
    recycling_rate = pmin(1, pmax(0, replace_na(raw_rate, 0))),
    secondary_Mt_carrier = waste_Mt * recycling_rate # metal mass for metal_ores, raw mass for non-metallic
  )

n_clamped <- sum(!is.na(secondary_super$raw_rate) & (secondary_super$raw_rate < 0 | secondary_super$raw_rate > 1))
n_total <- sum(!is.na(secondary_super$raw_rate))
cat("  Region x super_category x year groups where rate was clamped:", n_clamped, "/", n_total, "\n")


# Step 6: Convert metal secondary flow to ore-equivalent Mt -------------------
# secondary_Mt_carrier for metal_ores is metal mass (Script 02b's UNEP inflow
# is post Fe/NonFe-split + grade conversion); divide by the same year's
# blended ore grade to express it on the same ore-equivalent basis as
# primary_Mt (raw DMC ore) and as mc_results.parquet's secondary_supply_Mt.

cat("\nSTEP 6: Convert metal secondary flow to ore-equivalent Mt\n")

unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

fe_share_rt <- unep_raw %>%
  filter(material_category %in% METAL_CATS) %>%
  group_by(Region, year, material_category) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = material_category, values_from = DMC_Mt, values_fill = 0) %>%
  rename(fe_Mt = `Ferrous ores`, nonfe_Mt = `Non-ferrous ores`) %>%
  mutate(
    fe_share = case_when(
      fe_Mt < 0 & nonfe_Mt >= 0 ~ 0,
      nonfe_Mt < 0 & fe_Mt >= 0 ~ 1,
      fe_Mt + nonfe_Mt > 0 ~ fe_Mt / (fe_Mt + nonfe_Mt),
      TRUE ~ 0.9
    )
  ) %>%
  dplyr::select(Region, year, fe_share)

grade_raw <- read_csv("Parameters/MISO/metal_grade_ore.csv", show_col_types = FALSE)
grade_wide <- grade_raw %>%
  pivot_wider(names_from = group, values_from = g) %>%
  rename(g_Fe = Ferrous, g_NonFe = `Non-ferrous`)

grade_early <- grade_wide %>% filter(year == min(year)) %>% dplyr::select(-year)
grade_late <- grade_wide %>% filter(year == max(year)) %>% dplyr::select(-year)
if (min(grade_wide$year) > HIST_START) {
  grade_wide <- bind_rows(expand_grid(grade_early, year = seq(HIST_START, min(grade_wide$year) - 1)), grade_wide)
}
if (max(grade_wide$year) < HIST_END) {
  grade_wide <- bind_rows(grade_wide, expand_grid(grade_late, year = seq(max(grade_wide$year) + 1, HIST_END)))
}

grade_blend <- fe_share_rt %>%
  left_join(grade_wide, by = "year") %>%
  mutate(grade_blend = fe_share * g_Fe + (1 - fe_share) * g_NonFe) %>%
  dplyr::select(Region, year, grade_blend)

secondary_super <- secondary_super %>%
  left_join(grade_blend, by = c("Region", "year")) %>%
  mutate(
    secondary_Mt = if_else(material_group == "metal_ores", secondary_Mt_carrier / grade_blend, secondary_Mt_carrier)
  )


# Step 7: Aggregate to Region x material_group x year -------------------------
# waste_Mt/recovered_Mt are also carried through here (pre-ore-grade-
# conversion mass, i.e. Step 5's own units) so the recovered/total-outflow
# ratio survives aggregation intact -- Figure 5's contour version uses this
# ratio directly as its historical recycling/downcycling rate.

cat("\nSTEP 7: Aggregate secondary flow to Region x material_group x year\n")

secondary_hist <- secondary_super %>%
  group_by(Region, material_group, year) %>%
  summarise(
    secondary_Mt = sum(secondary_Mt, na.rm = TRUE),
    waste_Mt = sum(waste_Mt, na.rm = TRUE),
    recovered_Mt = sum(secondary_Mt_carrier, na.rm = TRUE),
    .groups = "drop"
  )


# Step 8: Historical primary flow (raw UNEP DMC extraction) ------------------

cat("\nSTEP 8: Historical primary flow (raw UNEP DMC)\n")

primary_hist <- unep_raw %>%
  mutate(
    material_group = case_when(
      material_category %in% METAL_CATS ~ "metal_ores",
      material_category %in% NONMET_CATS ~ "nonmetallic_minerals",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(material_group), year >= HIST_START, year <= HIST_END) %>%
  group_by(Region, material_group, year) %>%
  summarise(primary_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")


# Step 9: Combine + save -------------------------------------------------------

cat("\nSTEP 9: Combine and save\n")

historical_secondary_flows <- primary_hist %>%
  full_join(secondary_hist, by = c("Region", "material_group", "year")) %>%
  mutate(
    secondary_Mt = replace_na(secondary_Mt, 0),
    waste_Mt = replace_na(waste_Mt, 0),
    recovered_Mt = replace_na(recovered_Mt, 0)
  ) %>%
  arrange(material_group, Region, year)

write_csv(historical_secondary_flows, "Parameters/Intermediate/historical_secondary_flows.csv")
cat("  Saved: Parameters/Intermediate/historical_secondary_flows.csv (", nrow(historical_secondary_flows), "rows )\n")


# Step 10: Sanity checks -------------------------------------------------------

cat("\nSTEP 10: Sanity checks\n")

# (1) Cross-check global 2024 metal non-primary inflow against the existing
#     single-year anchor (Script 03's nonprimary_share_2024.csv), both in
#     metal-mass terms (pre ore-conversion) so no grade transform is needed.
anchor_2024 <- read_csv("Parameters/Intermediate/nonprimary_share_2024.csv", show_col_types = FALSE) %>%
  summarise(anchor_nonprimary_Mt = sum(inflow_Mt - primary_metal_Mt, na.rm = TRUE))

recon_2024_metalmass <- secondary_super %>%
  filter(material_group == "metal_ores", year == 2024) %>%
  summarise(recon_nonprimary_Mt = sum(secondary_Mt_carrier, na.rm = TRUE))

cat(sprintf(
  "[CHECK 1] Global 2024 metal non-primary inflow (Mt, metal mass): anchor (Script 03) %.0f | reconstructed (this script) %.0f\n",
  anchor_2024$anchor_nonprimary_Mt,
  recon_2024_metalmass$recon_nonprimary_Mt
))

# (2) Non-metallic minerals secondary share magnitude (Script 02b's docstring
#     called this scope gap "negligible" for stock calibration purposes)
nonmet_share_2024 <- historical_secondary_flows %>%
  filter(material_group == "nonmetallic_minerals", year == 2024) %>%
  summarise(primary_Mt = sum(primary_Mt), secondary_Mt = sum(secondary_Mt)) %>%
  mutate(share = secondary_Mt / (primary_Mt + secondary_Mt))

cat(sprintf(
  "[CHECK 2] Non-metallic minerals global 2024 secondary share: %.1f%% (prior finding: negligible)\n",
  100 * nonmet_share_2024$share
))

cat("\n=== Historical secondary flow reconstruction complete ===\n")

# EoF
