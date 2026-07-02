## =============================================================================
## 02c_MISO_scope_factor.R
## Empirical MISO/UNEP scope-correction factor A for metal inflows.
##
## UNEP DMC counts ore extracted + traded raw materials. Regional stock is
## built from ALL metal entering use: primary + recycled scrap + metal embodied
## in imported goods. MISO inflows measure exactly that. A corrects UNEP metal
## inflows empirically (no assumed recycling or trade series):
##
##   A(Region, super_category, year) = MISO_inflow / UNEP_metal_inflow
##
## computed 1970-2016 (MISO coverage), 5-yr rolling mean, clipped to
## [A_CLIP_MIN, A_CLIP_MAX], held at 2016 value for 2017-2024.
## A jointly captures recycling and embodied trade; NOT decomposed (documented
## assumption). Applied downstream (script 03) to Metal_Fe and Metal_NonFe
## only; non-metallic minerals need no correction (negligible recycling /
## embodied trade; DSM already fits MISO stocks).
##
## Verified basis (2016, region totals, MISO/UNEP metal inflow):
##   North America 2.9 | Europe & Russia 2.5 | MENA 2.1 | East Asia 0.85
##   -- matches lambda_cal>2 pattern for mature economies and East Asia's
##   embodied-export surplus.
##
## Input:
##   Parameters/Intermediate/UNEP_flows_subenduse.csv   -- from script 01b
##   Parameters/materials_region_DMC.csv                -- fe_share
##   Parameters/MISO/metal_grade_ore.csv                -- ore -> metal (g)
##   Parameters/MISO/MISO_flows_regional.csv            -- MISO inflows
##
## Output:
##   Parameters/Intermediate/miso_unep_scope_factor_A.csv
##   Figures/Stocks/scope_factor_A.png
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# TODO: move to config module per project rule (no hardcoded params in logic)
A_CLIP_MIN <- 0.5
A_CLIP_MAX <- 4.0
A_SMOOTH_WINDOW <- 5L
MISO_LAST_YEAR <- 2016L
A_EXTEND_TO <- 2024L

cat("=== Scope factor A: MISO/UNEP metal inflow ratio ===\n\n")

# -- Step 1: UNEP metal inflows in METAL MASS at super_category level ---------
# (duplicates script 03 Step 1 Fe/NonFe split + g conversion; keep in sync)

unep_sub <- read.csv("Parameters/Intermediate/UNEP_flows_subenduse.csv")
unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)

fe_share_rt <- unep_raw %>%
  filter(material_category %in% c("Ferrous ores", "Non-ferrous ores")) %>%
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
  )

grade_raw <- read_csv("Parameters/MISO/metal_grade_ore.csv", show_col_types = FALSE)
grade_wide <- grade_raw %>%
  pivot_wider(names_from = group, values_from = g) %>%
  rename(g_Fe = Ferrous, g_NonFe = `Non-ferrous`)

# LOCF extension of g to cover 1970..MISO_LAST_YEAR (same logic as script 03)
yr_min <- 1970L
yr_max <- MISO_LAST_YEAR
grade_early <- grade_wide %>% filter(year == min(year)) %>% dplyr::select(-year)
grade_late <- grade_wide %>% filter(year == max(year)) %>% dplyr::select(-year)
if (min(grade_wide$year) > yr_min) {
  grade_wide <- bind_rows(expand_grid(grade_early, year = seq(yr_min, min(grade_wide$year) - 1)), grade_wide)
}
if (max(grade_wide$year) < yr_max) {
  grade_wide <- bind_rows(grade_wide, expand_grid(grade_late, year = seq(max(grade_wide$year) + 1, yr_max)))
}

# UNEP metal inflow (Fe + NonFe combined, metal mass) per Region x super x year.
# Combined because MISO has no Fe/NonFe detail; A is applied identically to both.
unep_metal_super <- unep_sub %>%
  filter(material == "Metal ores", year >= 1970, year <= MISO_LAST_YEAR) %>%
  left_join(fe_share_rt %>% dplyr::select(Region, year, fe_share), by = c("Region", "year")) %>%
  mutate(fe_share = replace_na(fe_share, 0.9)) %>%
  left_join(grade_wide, by = "year") %>%
  mutate(metal_Mt = flow_Mt * (fe_share * g_Fe + (1 - fe_share) * g_NonFe)) %>%
  group_by(Region, super_category, year) %>%
  summarise(unep_inflow_Mt = sum(metal_Mt, na.rm = TRUE), .groups = "drop")

cat("STEP 1: UNEP metal inflow (metal mass):", nrow(unep_metal_super), "rows\n")

# -- Step 2: MISO metal inflows ------------------------------------------------
# NOTE: schema assumed as MISO_stock_regional.csv (end_use labels = super_category
# labels; value column value_Mt). Adjust renames if the flows file differs.

miso_flows <- read_csv("Parameters/MISO/MISO_flows_regional.csv", show_col_types = FALSE)

miso_inflow_super <- miso_flows %>%
  filter(material == "Metal ores", year >= 1970, year <= MISO_LAST_YEAR) %>%
  rename(super_category = end_use) %>%
  group_by(Region, super_category, year) %>%
  summarise(miso_inflow_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop")

cat("STEP 2: MISO metal inflow:", nrow(miso_inflow_super), "rows\n")

# -- Step 3: A = MISO / UNEP, smooth, clip, extend ----------------------------

A_raw <- unep_metal_super %>%
  left_join(miso_inflow_super, by = c("Region", "super_category", "year")) %>%
  mutate(
    A = case_when(
      is.na(miso_inflow_Mt) ~ 1.0, # no MISO data -> no correction
      unep_inflow_Mt <= 1e-6 ~ 1.0, # degenerate denominator
      TRUE ~ miso_inflow_Mt / unep_inflow_Mt
    )
  )

# centered rolling mean, partial windows at edges
roll_mean_partial <- function(x, w) {
  n <- length(x)
  half <- w %/% 2L
  vapply(seq_len(n), function(i) mean(x[max(1L, i - half):min(n, i + half)], na.rm = TRUE), numeric(1))
}

A_smooth <- A_raw %>%
  arrange(Region, super_category, year) %>%
  group_by(Region, super_category) %>%
  mutate(A_smooth = roll_mean_partial(A, A_SMOOTH_WINDOW)) %>%
  ungroup() %>%
  mutate(
    A_clipped = pmin(A_CLIP_MAX, pmax(A_CLIP_MIN, A_smooth)),
    flag_clipped = A_smooth < A_CLIP_MIN | A_smooth > A_CLIP_MAX
  )

# hold at MISO_LAST_YEAR value for the bridge years
A_hold <- A_smooth %>%
  filter(year == MISO_LAST_YEAR) %>%
  dplyr::select(Region, super_category, A_clipped) %>%
  tidyr::crossing(year = seq(MISO_LAST_YEAR + 1L, A_EXTEND_TO))

A_factor <- bind_rows(A_smooth %>% dplyr::select(Region, super_category, year, A_clipped), A_hold) %>%
  rename(A = A_clipped) %>%
  arrange(Region, super_category, year)

write_csv(A_factor, "Parameters/Intermediate/miso_unep_scope_factor_A.csv")
cat("STEP 3: Saved Parameters/Intermediate/miso_unep_scope_factor_A.csv (", nrow(A_factor), "rows )\n")

# -- Step 4: Check-ups ---------------------------------------------------------

cat("\nSTEP 4: Check-ups\n")

# (1) clipping: many clipped group-years means A is doing violence somewhere
n_clip <- sum(A_smooth$flag_clipped)
cat("[CHECK 1] group-years clipped to [", A_CLIP_MIN, ",", A_CLIP_MAX, "]:", n_clip, "\n")
if (n_clip > 0) {
  print(A_smooth %>% filter(flag_clipped) %>% count(Region, super_category) %>% arrange(desc(n)), n = 20)
}

# (2) 2016 snapshot by region -- compare against verified region totals:
#     NA ~2.9, Europe ~2.5, MENA ~2.1, East Asia ~0.85 (inflow-weighted)
cat("[CHECK 2] A at", MISO_LAST_YEAR, "by Region (inflow-weighted across super_categories):\n")
print(
  A_smooth %>%
    filter(year == MISO_LAST_YEAR) %>%
    group_by(Region) %>%
    summarise(A_2016 = round(weighted.mean(A_clipped, w = pmax(unep_inflow_Mt, 1e-9)), 2)) %>%
    arrange(desc(A_2016))
)

# (3) implied global consistency: sum(UNEP x A) should ~= sum(MISO) at 2016
glob <- A_raw %>%
  filter(year == MISO_LAST_YEAR) %>%
  summarise(
    unep = sum(unep_inflow_Mt, na.rm = TRUE),
    miso = sum(miso_inflow_Mt, na.rm = TRUE),
    unep_x_A = sum(unep_inflow_Mt * A, na.rm = TRUE)
  )
cat(sprintf(
  "[CHECK 3] Global 2016 metal inflow (Mt): UNEP %.0f | MISO %.0f | UNEP x A %.0f (should ~= MISO)\n",
  glob$unep,
  glob$miso,
  glob$unep_x_A
))

# (4) SI figure: A(t) by region x super_category
p_A <- A_factor %>%
  ggplot(aes(x = year, y = A, colour = Region)) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "grey60") +
  geom_vline(xintercept = MISO_LAST_YEAR, linetype = "dotted", linewidth = 0.3, colour = "grey45") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~super_category) +
  scale_colour_manual(values = PALETTE_REGIONS, na.value = "#999999") +
  labs(
    x = "Year",
    y = "A = MISO inflow / UNEP metal inflow",
    title = "Metal inflow scope factor (recycling + embodied trade, undecomposed)"
  ) +
  theme_pb_large()
p_A
# fmt: skip
ggsave("Figures/Stocks/scope_factor_A.png", p_A, units = "cm", dpi = 600, width = 8.7*2, height = 8.7*1.5)
cat("  Saved: Figures/Stocks/scope_factor_A.png\n")

cat("\n=== Scope factor A complete ===\n")
# EoF
