## =============================================================================
## 02_UNEP_enduse_shares.R
## Disaggregate UNEP domestic material consumption (DMC) flows by end-use,
## using MISO2 flow shares as the disaggregation key.
##
## Input:
##   Parameters/materials_region_DMC.csv       — UNEP DMC by Region × material_category × year
##   Parameters/MISO/MISO_flows_regional.csv   — MISO2 inflows by Region × material × end_use × year
##
## Output:
##   Parameters/Intermediate/UNEP_flows_enduse.csv
##     columns: Region, material, end_use, year, flow_Mt
##
## Figures (Figures/MISO/):
##   enduse_shares_world.png   — world total, 100% stacked area, facet by material
##   enduse_shares_region.png  — by region, facet_grid(Region ~ material)
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00-CommonParameters.R", encoding = "UTF-8")

# ── UNEP material_category → 4-group taxonomy (explicit mapping) ──────────────
# Names match PALETTE_MATERIAL_GROUPS keys: "Biomass", "Fossil fuels",
# "Metal ores", "Non-metallic minerals".
# "Mixed" and "Waste" are assigned to "Non-metallic minerals" — they often
# represent construction or demolition residuals in UNEP reporting.
UNEP_MATERIAL_MAP <- tribble(
  ~material_category                                              , ~material               ,
  "Wood"                                                          , "Biomass"               ,
  "Crops"                                                         , "Biomass"               ,
  "Crop Residues"                                                 , "Biomass"               ,
  "Grazed biomass and fodder crops"                               , "Biomass"               ,
  "Wild catch and harvest"                                        , "Biomass"               ,
  "Non-wild animal products"                                      , "Biomass"               ,
  "Products mainly from biomass nec."                             , "Biomass"               ,
  "Coal"                                                          , "Fossil fuels"          ,
  "Natural Gas"                                                   , "Fossil fuels"          ,
  "Petroleum"                                                     , "Fossil fuels"          ,
  "Oil shale and tar sands"                                       , "Fossil fuels"          ,
  "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel" , "Fossil fuels"          ,
  "Other products mainly from fossil fuels e.g. plastics"         , "Fossil fuels"          ,
  "Ferrous ores"                                                  , "Metal ores"            ,
  "Non-ferrous ores"                                              , "Metal ores"            ,
  "Products mainly from metals nec."                              , "Metal ores"            ,
  "Non-metallic minerals - construction dominant"                 , "Non-metallic minerals" ,
  "Non-metallic minerals - industrial or agricultural dominant"   , "Non-metallic minerals" ,
  "Products mainly from non-metallic minerals"                    , "Non-metallic minerals" ,
  "Excavated earthen materials (including soil) nec"              , "Non-metallic minerals" ,
  "Mixed / complex products nec."                                 , "Mixed"                 ,
  "Waste for final treatment and disposal"                        , "Waste"
)

IN_SCOPE_MATERIALS <- c("Metal ores", "Non-metallic minerals")

# Tolerance for share-sum validation
SHARE_TOLERANCE <- 0.001


# Step 1: Load UNEP DMC flows ------------------------------------------------

cat("STEP 1: Load UNEP DMC flows\n")

unep_raw <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
cat("UNEP raw:", nrow(unep_raw), "x", ncol(unep_raw), "\n")
cat("Column names:\n")
print(names(unep_raw))
cat("Year range:", min(unep_raw$year), "-", max(unep_raw$year), "\n")
cat("Regions:", paste(sort(unique(unep_raw$Region)), collapse = ", "), "\n")
cat("Material categories:", paste(sort(unique(unep_raw$material_category)), collapse = "\n  "), "\n")


# Step 2: Load MISO2 inflows -------------------------------------------------

cat("\nSTEP 2: Load MISO2 inflows\n")

miso_flows <- read_csv("Parameters/MISO/MISO_flows_regional.csv", show_col_types = FALSE)
cat("MISO flows:", nrow(miso_flows), "x", ncol(miso_flows), "\n")
cat("Column names:", paste(names(miso_flows), collapse = ", "), "\n")


# Step 3: Map UNEP categories to 4-group taxonomy and filter -----------------

cat("\nSTEP 3: Map UNEP material_category to 4-group taxonomy\n")

# Warn about any categories in the data that are not in our map
unmapped_cats <- setdiff(unique(unep_raw$material_category), UNEP_MATERIAL_MAP$material_category)
if (length(unmapped_cats) > 0) {
  cat("[WARNING] UNEP categories not in mapping (will be dropped):", paste(unmapped_cats, collapse = ", "), "\n")
}

unep <- unep_raw %>%
  left_join(UNEP_MATERIAL_MAP, by = "material_category") %>%
  filter(!is.na(material)) %>% # drop unmapped categories
  filter(material %in% IN_SCOPE_MATERIALS) %>% # scope: metals + non_metallics only
  group_by(Region, material, year) %>%
  summarise(flow_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

cat("UNEP after grouping to 4-group taxonomy:", nrow(unep), "rows\n")
cat("Year range:", min(unep$year), "-", max(unep$year), "\n")
unique(unep$material)

# Step 4: Compute end-use shares from MISO2 inflows --------------------------

cat("\nSTEP 4: Compute end-use shares from MISO2 inflows\n")

# Share = inflow(end_use) / total_inflow(all end_uses), per Region×material×year
miso_total <- miso_flows %>%
  group_by(Region, material, year) %>%
  summarise(total_flow_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop")

shares_miso <- miso_flows %>%
  left_join(miso_total, by = c("Region", "material", "year")) %>%
  mutate(share = if_else(total_flow_Mt > 0, value_Mt / total_flow_Mt, 0)) %>%
  dplyr::select(Region, material, end_use, year, share)
head(shares_miso)

# Validate: shares must sum to 1.0 ± tolerance per Region×material×year
share_sums <- shares_miso %>% group_by(Region, material, year) %>% summarise(sum_share = sum(share), .groups = "drop")

bad_shares <- share_sums %>% filter(abs(sum_share - 1) > SHARE_TOLERANCE)
if (nrow(bad_shares) > 0) {
  cat("[WARNING] Share sums deviate from 1.0 (first 10 offending rows):\n")
  print(head(bad_shares, 10))
  stop("[VALIDATION FAILED] End-use shares do not sum to 1.0 within tolerance.")
}
cat("  [OK] All end-use shares sum to 1.0 ± ", SHARE_TOLERANCE, "\n")


# Step 5: Extend shares to 2017–2024 via LOCF --------------------------------

cat("\nSTEP 5: Extend shares beyond 2016 (last-observation-carried-forward)\n")

miso_max_year <- max(miso_flows$year) # 2016
unep_max_year <- max(unep$year)

post_miso_years <- seq(miso_max_year + 1, unep_max_year)

if (length(post_miso_years) > 0) {
  warning(paste(
    "MISO2 flows only cover up to",
    miso_max_year,
    "—",
    "end-use shares for years",
    paste(range(post_miso_years), collapse = "–"),
    "are extrapolated from",
    miso_max_year,
    "shares (LOCF)."
  ))

  shares_2016 <- shares_miso %>% filter(year == miso_max_year) %>% dplyr::select(-year)

  # Repeat 2016 shares for all post-MISO years
  shares_extended <- expand_grid(shares_2016, year = post_miso_years) %>% dplyr::select(Region, material, end_use, year, share) # restore column order

  shares_all <- bind_rows(shares_miso, shares_extended) %>% arrange(Region, material, end_use, year)
} else {
  shares_all <- shares_miso
}

cat("  Share table covers years:", min(shares_all$year), "-", max(shares_all$year), "\n")


# Step 6: Apply shares to UNEP flows -----------------------------------------

cat("\nSTEP 6: Apply end-use shares to UNEP flows\n")

unep |> filter(year == 2016) |> group_by(material) |> reframe(x = sum(flow_Mt))

unep_enduse <- unep %>%
  left_join(shares_all, by = c("Region", "material", "year"), relationship = "many-to-many") %>%
  mutate(flow_Mt = flow_Mt * share) %>%
  filter(!is.na(end_use)) %>%
  dplyr::select(Region, material, end_use, year, flow_Mt)

cat("UNEP by end-use:", nrow(unep_enduse), "rows\n")

# Sanity: summing end-uses should recover original UNEP totals
reconstructed_total <- unep_enduse %>%
  group_by(Region, material, year) %>%
  summarise(flow_Mt_check = sum(flow_Mt, na.rm = TRUE), .groups = "drop")

roundtrip_check <- unep %>%
  left_join(reconstructed_total, by = c("Region", "material", "year")) %>%
  mutate(diff = abs(flow_Mt - flow_Mt_check)) %>%
  summarise(max_diff = max(diff, na.rm = TRUE)) %>%
  pull(max_diff)

cat(sprintf("  Round-trip check (max abs diff): %.6f Mt\n", roundtrip_check))


# Step 7: Divergence check 1970–2016 (UNEP vs MISO2 totals) -----------------

cat("\nSTEP 7: Divergence check — UNEP vs MISO2 total flows (1970–2016)\n")

overlap_years <- seq(max(1970, min(unep$year)), miso_max_year)

unep_overlap <- unep %>%
  filter(year %in% overlap_years) %>%
  group_by(Region, material) %>%
  summarise(unep_avg_Mt = mean(flow_Mt, na.rm = TRUE), .groups = "drop")

miso_overlap <- miso_total %>%
  filter(year %in% overlap_years) %>%
  group_by(Region, material) %>%
  summarise(miso_avg_Mt = mean(total_flow_Mt, na.rm = TRUE), .groups = "drop")

divergence <- unep_overlap %>%
  left_join(miso_overlap, by = c("Region", "material")) %>%
  mutate(
    ratio = if_else(miso_avg_Mt > 0, unep_avg_Mt / miso_avg_Mt, NA_real_),
    abs_diff = abs(unep_avg_Mt - miso_avg_Mt),
    flag = !is.na(ratio) & (ratio < 0.7 | ratio > 1.4)
  )

flagged <- divergence %>% filter(flag)
if (nrow(flagged) > 0) {
  cat("[NOTE] Divergence > ±30% for the following Region × material pairs", "(avg 1970–2016):\n")
  print(flagged %>% dplyr::select(Region, material, ratio, unep_avg_Mt, miso_avg_Mt, abs_diff))
} else {
  cat("  [OK] All Region × material ratios are within ±30% (1970–2016 average).\n")
}


# Step 8: Save output --------------------------------------------------------

cat("\nSTEP 8: Save UNEP flows by end-use\n")

write_csv(unep_enduse, "Parameters/Intermediate/UNEP_flows_enduse.csv")
cat("  Saved: Parameters/Intermediate/UNEP_flows_enduse.csv (", nrow(unep_enduse), "rows )\n")


# Step 9: Summary ------------------------------------------------------------

cat("\nSTEP 9: Summary — total flow by material for selected years\n")

summary_flow <- unep_enduse %>%
  filter(year %in% c(1970, 1990, 2010, 2016)) %>%
  group_by(material, year) %>%
  summarise(total_Mt = round(sum(flow_Mt, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = total_Mt)

print(summary_flow)

cat("\n── Sanity check ────────────────────────────────────────────────────────────\n")
cat(
  "  Rows:",
  nrow(unep_enduse),
  "| Year range:",
  min(unep_enduse$year),
  "-",
  max(unep_enduse$year),
  "| Materials:",
  paste(sort(unique(unep_enduse$material)), collapse = ", "),
  "\n"
)


# Step 10: Figures — 100% stacked area by end-use, 1970–2016 -----------------

cat("\nSTEP 10: Figures — end-use share time series\n")


# ── End-use palette and ordered display labels ────────────────────────────────

ENDUSE_LABELS <- c(
  "buildings" = "Buildings",
  "civil_infrastructure" = "Civil infrastructure",
  "machinery" = "Machinery",
  "short_lived" = "Short-lived products"
)

# Shared data: 1970–2016, end_use recoded to display label (ordered factor)
flows_16 <- unep_enduse %>%
  filter(year >= 1970, year <= 2016) %>%
  mutate(end_use = factor(ENDUSE_LABELS[end_use], levels = names(PALETTE_ENDUSE)))

unep_enduse |> filter(year == 2016) |> group_by(material) |> reframe(x = sum(flow_Mt))

# ── Fig 10a: World average — facet by material ────────────────────────────────

world_shares <- flows_16 %>%
  group_by(material, year, end_use) %>%
  summarise(flow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") %>%
  group_by(material, year) %>%
  mutate(share = flow_Mt / sum(flow_Mt, na.rm = TRUE)) %>%
  ungroup()

# Build label positions — use a single year snapshot for placement
label_data <- world_shares %>%
  filter(material == "Metal ores") %>%
  # Pick a label year; mid-point often works well
  filter(year == 2000) %>%
  # For stacked areas, compute cumulative midpoint per year so labels sit inside each band
  arrange(year, desc(end_use)) %>%
  mutate(cumsum = cumsum(share), midpoint = cumsum - share / 2)

p_world <- world_shares %>%
  ggplot(aes(x = year, y = share, fill = end_use)) +
  geom_area(colour = "white", linewidth = 0.15, alpha = 0.95) +
  geom_text(
    data    = label_data,
    aes(x = 2000, y = midpoint, label = end_use),
    inherit.aes = FALSE,
    size        = 3,
    fontface    = "bold",
    hjust       = 0.5,
    show.legend = FALSE
  ) +
  facet_wrap(~material, ncol = 2) +
  scale_fill_manual(values = PALETTE_ENDUSE, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(expand = F, clip = "off") +
  labs(
    title = "End-use shares of material consumption — world average, 1970–2016",
    subtitle = "UNEP DMC flows disaggregated by MISO2 end-use shares",
    x = NULL,
    y = "Share of total flow"
  ) +
  theme_pb_large() +
  theme(legend.position = "none", panel.spacing = unit(0.8, "lines"))
p_world

# fmt: skip
ggsave("Figures/MISO/enduse_shares_world.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

# ── Fig 10b: By region — facet_grid(Region ~ material) ───────────────────────

# Preserve PALETTE_REGIONS order for consistent row layout
region_order <- names(PALETTE_REGIONS)[names(PALETTE_REGIONS) %in% unique(flows_16$Region)]

region_shares <- flows_16 %>%
  mutate(Region = factor(Region, levels = region_order)) %>%
  group_by(Region, material, year, end_use) %>%
  summarise(flow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") %>%
  group_by(Region, material, year) %>%
  mutate(share = flow_Mt / sum(flow_Mt, na.rm = TRUE)) %>%
  ungroup()

label_data_region <- region_shares %>%
  filter(material == "Metal ores", year == 2000) %>%
  arrange(Region, desc(end_use)) %>%
  group_by(Region) %>%
  mutate(cumsum = cumsum(share), midpoint = cumsum - share / 2) %>%
  ungroup()

p_region <- region_shares %>%
  ggplot(aes(x = year, y = share, fill = end_use)) +
  geom_area(position = "stack", colour = "white", linewidth = 0.1, alpha = 0.95) +
  geom_text(
    data        = label_data_region,
    aes(x = 2000, y = midpoint, label = end_use),
    inherit.aes = FALSE,
    size        = 2,
    fontface    = "bold",
    hjust       = 0.5,
    show.legend = FALSE
  ) +
  # facet_grid(Region ~ material) +
  facet_wrap(Region ~ material) +
  scale_fill_manual(values = PALETTE_ENDUSE, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2016, by = 20)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, 1)) +
  labs(
    title = "End-use shares of material consumption by region, 1970–2016",
    subtitle = "UNEP DMC flows disaggregated by MISO2 end-use shares",
    x = NULL,
    y = "Share of total flow"
  ) +
  theme_pb() +
  theme(
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    panel.spacing = unit(0.4, "lines"),
    legend.position = "none"
  )
p_region
# fmt: skip
ggsave("Figures/MISO/enduse_shares_region.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*3)

# EoF
