## =============================================================================
## 02_OreGrade_Factor_g.R
## Estimates the ore-to-metal physical conversion factor g (grade × recovery)
## for ferrous and non-ferrous metals, globally and by region, by stripping
## recycling out of MISO metal flows.
##
## Equation (per year, per group):
##   g_grp(t) = [share_grp · M(t) − rr_grp · Out(t−1)] / O_grp_ore(t)
##
##   share_Fe = PHI_FE, share_nF = 1 − PHI_FE
##   M(t)        = MISO total metal inflow (Metal ores, all end-uses)
##   Out(t−1)    = MISO total metal outflow, lagged 1 year
##   O_grp_ore   = UNEP DMC: "Ferrous ores" → Fe; "Non-ferrous ores" → non-Fe
##                 "Products mainly from metals nec." is excluded (not gross ore)
##
## Inputs:
##   Parameters/materials_region_DMC.csv           — UNEP DMC Region × year
##   Parameters/MISO/MISO_flows_regional.csv        — MISO metal inflows
##   Parameters/MISO/MISO_outflows_regional.csv     — MISO metal outflows
##
## Figures (Figures/MISO/):
##   ore_grade_g_global.png       — global g over time (Fig 1)
##   ore_grade_g_sensitivity.png  — recycling rate sensitivity, global (Fig 2)
##   ore_grade_g_regional.png     — regional g over time (Fig 3)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Parameters --------
PHI_FE <- 0.9 # ferrous share of MISO total metal mass (φ)
RR_FE <- 0.30 # ferrous recycling rate
RR_NF <- 0.10 # non-ferrous recycling rate
RR_FE_SENS <- c(0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.6) # recycling rate sensitivity sweep (Fig 2)

CAT_FE <- "Ferrous ores"
CAT_NONFE <- "Non-ferrous ores"
FIG_DIR <- "Figures/MISO"

GROUP_COLORS <- c("Ferrous" = "#B71C1C", "Non-ferrous" = "#C77B00")


# Step 1: Load data --------
cat("STEP 1: Load data\n")

miso_inflow <- read_csv("Parameters/MISO/MISO_flows_regional.csv", show_col_types = FALSE)
miso_outflow <- read_csv("Parameters/MISO/MISO_outflows_regional.csv", show_col_types = FALSE)
unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

cat("  Inflow  rows:", nrow(miso_inflow), "| years:", min(miso_inflow$year), "-", max(miso_inflow$year), "\n")
cat("  Outflow rows:", nrow(miso_outflow), "| years:", min(miso_outflow$year), "-", max(miso_outflow$year), "\n")
cat("  UNEP    rows:", nrow(unep_raw), "| years:", min(unep_raw$year), "-", max(unep_raw$year), "\n")


# Step 2: Aggregate MISO total metal flows (Metal ores) → Region × year --------
cat("\nSTEP 2: Aggregate MISO metal inflows and outflows\n")

miso_M <- miso_inflow %>%
  filter(material == "Metal ores") %>%
  group_by(Region, year) %>%
  summarise(M_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop")

miso_Out <- miso_outflow %>%
  filter(material == "Metal ores") %>%
  group_by(Region, year) %>%
  summarise(Out_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop")

cat("  MISO M   rows:", nrow(miso_M), "| regions:", n_distinct(miso_M$Region), "\n")
cat("  MISO Out rows:", nrow(miso_Out), "| regions:", n_distinct(miso_Out$Region), "\n")


# Step 3: Extract UNEP ore DMC (Ferrous and Non-ferrous only) --------
# "Products mainly from metals nec." is excluded — not a gross ore mass category.
cat("\nSTEP 3: Extract UNEP ore DMC\n")

dmc_ore <- unep_raw %>%
  filter(material_category %in% c(CAT_FE, CAT_NONFE)) %>%
  mutate(ore_type = if_else(material_category == CAT_FE, "ore_Fe_Mt", "ore_nF_Mt")) %>%
  group_by(Region, year, ore_type) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ore_type, values_from = DMC_Mt, values_fill = NA_real_)

cat("  DMC ore rows:", nrow(dmc_ore), "| years:", min(dmc_ore$year), "-", max(dmc_ore$year), "\n")
cat("  Regions in DMC ore:", paste(sort(unique(dmc_ore$Region)), collapse = ", "), "\n")


# Step 4: Determine overlap year window (last 20 years of MISO–UNEP overlap) --------
yr_overlap_min <- max(min(miso_M$year), min(dmc_ore$year))
yr_overlap_max <- min(max(miso_M$year), max(dmc_ore$year))
WINDOW_YEARS <- seq(max(yr_overlap_min, yr_overlap_max - 19), yr_overlap_max)
WINDOW_YEARS <- seq(yr_overlap_min, yr_overlap_max)
cat("\n  Analysis window:", min(WINDOW_YEARS), "-", max(WINDOW_YEARS), "(", length(WINDOW_YEARS), "years )\n")


# Step 5: Global aggregation --------
cat("\nSTEP 5: Global aggregation\n")

M_global <- miso_M %>% group_by(year) %>% summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop")

Out_global <- miso_Out %>% group_by(year) %>% summarise(Out_Mt = sum(Out_Mt, na.rm = TRUE), .groups = "drop")

ore_global <- dmc_ore %>%
  group_by(year) %>%
  summarise(ore_Fe_Mt = sum(ore_Fe_Mt, na.rm = TRUE), ore_nF_Mt = sum(ore_nF_Mt, na.rm = TRUE), .groups = "drop")

# Compute Out lag on full data first, then filter — so year t gets Out(t-1) correctly
global_joined <- M_global %>%
  inner_join(Out_global, by = "year") %>%
  inner_join(ore_global, by = "year") %>%
  arrange(year) %>%
  mutate(Out_lag1 = dplyr::lag(Out_Mt, 1)) %>% # NA only at very first year of series
  filter(year %in% WINDOW_YEARS)

cat("  Global joined rows:", nrow(global_joined), "\n")
cat("  Missing Out_lag1:", sum(is.na(global_joined$Out_lag1)), "year(s)\n")


# Step 6: Compute g_Fe and g_nF (global, baseline recycling rates) --------
cat("\nSTEP 6: Compute g (global, baseline RR_FE =", RR_FE, ", RR_NF =", RR_NF, ")\n")

global_g <- global_joined %>%
  mutate(
    g_Fe = (PHI_FE * M_Mt - RR_FE * Out_lag1) / ore_Fe_Mt,
    g_nF = ((1 - PHI_FE) * M_Mt - RR_NF * Out_lag1) / ore_nF_Mt
  )

# Diagnostic: print summary
cat(
  "  g_Fe  range (non-NA):",
  round(min(global_g$g_Fe, na.rm = TRUE), 4),
  "–",
  round(max(global_g$g_Fe, na.rm = TRUE), 4),
  "\n"
)
cat(
  "  g_nF  range (non-NA):",
  round(min(global_g$g_nF, na.rm = TRUE), 4),
  "–",
  round(max(global_g$g_nF, na.rm = TRUE), 4),
  "\n"
)

# Flags: g outside [0, 1] per group
flag_g_Fe <- global_g %>% filter(!is.na(g_Fe), g_Fe < 0 | g_Fe > 1)
flag_g_nF <- global_g %>% filter(!is.na(g_nF), g_nF < 0 | g_nF > 1)

if (nrow(flag_g_Fe) > 0) {
  warning(paste("[FLAG] g_Fe outside [0,1] in", nrow(flag_g_Fe), "global year(s)."))
  cat("[FLAG] g_Fe outside [0,1] —  g>1: impossible (bad φ or rr); g<0: secondary exceeds inflow.\n")
  print(flag_g_Fe %>% select(year, M_Mt, Out_lag1, ore_Fe_Mt, g_Fe))
}
if (nrow(flag_g_nF) > 0) {
  warning(paste("[FLAG] g_nF outside [0,1] in", nrow(flag_g_nF), "global year(s)."))
  cat("[FLAG] g_nF outside [0,1] —  g>1: impossible (bad φ or rr); g<0: secondary exceeds inflow.\n")
  print(flag_g_nF %>% select(year, M_Mt, Out_lag1, ore_nF_Mt, g_nF))
}
if (nrow(flag_g_Fe) == 0 && nrow(flag_g_nF) == 0) {
  cat("  [OK] All global g values are within [0, 1].\n")
}


# Step 7: Figure 1 — global g over time --------
cat("\nSTEP 7: Figure 1 — global g over time\n")

global_g_long <- global_g %>%
  select(year, g_Fe, g_nF) %>%
  pivot_longer(c(g_Fe, g_nF), names_to = "group", values_to = "g") %>%
  mutate(group = recode(group, g_Fe = "Ferrous", g_nF = "Non-ferrous"))

ggplot(global_g_long, aes(year, g, colour = group)) +
  # geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
  # geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.5) +
  facet_wrap(~group, scales = "free", ncol = 2) +
  scale_colour_manual(values = GROUP_COLORS, guide = "none") +
  scale_x_continuous(breaks = seq(min(WINDOW_YEARS), max(WINDOW_YEARS), by = 5)) +
  labs(
    title = "Ore-to-metal conversion factor g (global)",
    subtitle = sprintf(
      "g(t) = [share·M(t) − rr·Out(t−1)] / Ore(t)   |   φ_Fe = %.1f,  RR_Fe = %.2f,  RR_nFe = %.2f",
      PHI_FE,
      RR_FE,
      RR_NF
    ),
    x = NULL,
    y = "g  (metal mass per unit ore mass)"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_pb_large() +
  theme(panel.spacing = unit(0.8, "lines"))

# fmt: skip
ggsave(file.path(FIG_DIR, "ore_grade_g_global.png"), ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("  Saved: ore_grade_g_global.png\n")


# Step 8: Figure 2 — recycling rate sensitivity (global) --------
cat("\nSTEP 8: Figure 2 — recycling sensitivity\n")

# Cross-join: apply each rr value to both Fe and non-Fe simultaneously
sens_df <- crossing(global_joined, tibble(rr_val = RR_FE_SENS)) %>%
  mutate(
    g_Fe = (PHI_FE * M_Mt - rr_val * Out_lag1) / ore_Fe_Mt,
    g_nF = ((1 - PHI_FE) * M_Mt - rr_val * Out_lag1) / ore_nF_Mt,
    rr_label = factor(sprintf("rr = %.2f", rr_val), levels = sprintf("rr = %.2f", RR_FE_SENS))
  ) %>%
  select(year, rr_val, rr_label, g_Fe, g_nF) %>%
  pivot_longer(c(g_Fe, g_nF), names_to = "group", values_to = "g") %>%
  mutate(group = recode(group, g_Fe = "Ferrous", g_nF = "Non-ferrous"))

ggplot(sens_df, aes(year, g, colour = rr_label)) +
  # geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
  # geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.65) +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1, name = "Recycling rate") +
  scale_x_continuous(breaks = seq(min(WINDOW_YEARS), max(WINDOW_YEARS), by = 5)) +
  labs(
    title = "Sensitivity of g to recycling rate assumption (global)",
    subtitle = sprintf("φ_Fe = %.1f  |  recycling rate applied to both Fe and non-Fe simultaneously", PHI_FE),
    x = NULL,
    y = "g  (metal mass per unit ore mass)"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_pb_large() +
  theme(legend.position = "right", panel.spacing = unit(0.8, "lines"))

# fmt: skip
ggsave(file.path(FIG_DIR, "ore_grade_g_sensitivity.png"), ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
cat("  Saved: ore_grade_g_sensitivity.png\n")


# Step 9: Regional analysis --------
cat("\nSTEP 9: Regional analysis\n")

# Compute Out lag on full regional series, then filter to window
region_joined <- miso_M %>%
  inner_join(miso_Out, by = c("Region", "year")) %>%
  inner_join(dmc_ore, by = c("Region", "year")) %>%
  group_by(Region) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(Out_lag1 = dplyr::lag(Out_Mt, 1)) %>% # NA only at each region's first year
  ungroup() %>%
  filter(year %in% WINDOW_YEARS) %>%
  mutate(
    g_Fe = (PHI_FE * M_Mt - RR_FE * Out_lag1) / ore_Fe_Mt,
    g_nF = ((1 - PHI_FE) * M_Mt - RR_NF * Out_lag1) / ore_nF_Mt
  )

cat("  Regional joined rows:", nrow(region_joined), "\n")
cat("  Regions:", paste(sort(unique(region_joined$Region)), collapse = ", "), "\n")

# Flags: g outside [0, 1] per Region × group × year
# Known issue: extraction-heavy regions (e.g. East Asia, South Asia) will flag
# because UNEP ore is extraction-side but MISO is use-side — expected mismatch for exporters.
region_g_flags <- region_joined %>%
  select(Region, year, g_Fe, g_nF) %>%
  pivot_longer(c(g_Fe, g_nF), names_to = "group", values_to = "g") %>%
  mutate(group = recode(group, g_Fe = "Ferrous", g_nF = "Non-ferrous")) %>%
  filter(!is.na(g), g < 0 | g > 1)

if (nrow(region_g_flags) > 0) {
  warning(paste("[FLAG] Regional g outside [0,1] in", nrow(region_g_flags), "Region×group×year cells."))
  cat("[FLAG] Regional g outside [0,1] —", nrow(region_g_flags), "cells.\n")
  cat("  NOTE: Ore-exporting regions (East Asia, South Asia) expected to show g>1.\n")
  cat("        UNEP ore = extraction-side; MISO = use-side. Net exporters: ore > use → g inflated.\n")
  print(region_g_flags %>% count(Region, group, name = "n_flagged_years") %>% arrange(desc(n_flagged_years)))
} else {
  cat("  [OK] All regional g values within [0, 1].\n")
}


# Step 10: Figure 3 — regional g over time --------
cat("\nSTEP 10: Figure 3 — regional g over time\n")

region_g_long <- region_joined %>%
  select(Region, year, g_Fe, g_nF) %>%
  pivot_longer(c(g_Fe, g_nF), names_to = "group", values_to = "g") %>%
  mutate(
    group = recode(group, g_Fe = "Ferrous", g_nF = "Non-ferrous"),
    Region = factor(Region, levels = names(PALETTE_REGIONS)[names(PALETTE_REGIONS) %in% unique(region_joined$Region)])
  )

ggplot(region_g_long, aes(year, g, colour = Region)) +
  # geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.35) +
  # geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.35) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  facet_wrap(Region ~ group, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(min(WINDOW_YEARS), max(WINDOW_YEARS), by = 5)) +
  labs(
    title = "Ore-to-metal conversion factor g by region",
    subtitle = sprintf(
      "φ_Fe = %.1f,  RR_Fe = %.2f,  RR_nFe = %.2f   |   dashed lines: g = 0, g = 1",
      PHI_FE,
      RR_FE,
      RR_NF
    ),
    x = NULL,
    y = "g  (metal mass per unit ore mass)"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_pb_large() +
  theme(
    panel.spacing = unit(0.3, "lines"),
    strip.text.y = element_text(size = 5.5, angle = 0, hjust = 0),
    strip.text.x = element_text(size = 6.5),
    axis.text = element_text(size = 5)
  )

# fmt: skip
ggsave(file.path(FIG_DIR, "ore_grade_g_regional.png"), ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 3)
cat("  Saved: ore_grade_g_regional.png\n")


cat(sprintf(
  "\nDone.  Parameters: φ_Fe = %.2f | RR_Fe = %.2f | RR_nFe = %.2f | window: %d–%d\n",
  PHI_FE,
  RR_FE,
  RR_NF,
  min(WINDOW_YEARS),
  max(WINDOW_YEARS)
))
cat("  ⚠ RR values need verification against worldsteel / IAI / UNEP IRP sources.\n")
cat("  ⚠ g > 1 in extraction-heavy regions is expected (UNEP = extraction-side, MISO = use-side).\n")

# Last 10 years average (2007-2016)
global_joined %>%
  filter(year >= 2007) |>
  # filter(year > 1970) |> # for lag
  mutate(dummy = "X") |>
  group_by(dummy) |>
  reframe(M_Mt = sum(M_Mt), Out_lag1 = sum(Out_lag1), ore_Fe_Mt = sum(ore_Fe_Mt), ore_nF_Mt = sum(ore_nF_Mt)) |>
  mutate(
    g_Fe = (PHI_FE * M_Mt - RR_FE * Out_lag1) / ore_Fe_Mt,
    g_nF = ((1 - PHI_FE) * M_Mt - RR_NF * Out_lag1) / ore_nF_Mt
  )

# 0 recycling assumption
global_joined %>%
  # filter(year > 2011) |>
  mutate(dummy = "X") |>
  group_by(dummy) |>
  reframe(M_Mt = sum(M_Mt), ore_Fe_Mt = sum(ore_Fe_Mt), ore_nF_Mt = sum(ore_nF_Mt)) |>
  mutate(g_Fe = (PHI_FE * M_Mt) / ore_Fe_Mt, g_nF = ((1 - PHI_FE) * M_Mt) / ore_nF_Mt)

# SAVE RESULTS -----------------

# fill 1970 with 1971
hist_save <- global_g_long %>% group_by(group) %>% arrange(year) %>% fill(g, .direction = "up")
write.csv(hist_save, "Parameters/MISO/metal_grade_ore.csv", row.names = F)

# values last 10 years
# grade Fe: 38.1%
# grade Non Fe: 1.64%

# EoF
