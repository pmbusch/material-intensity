## =============================================================================
## 01b_UNEP_enduse_detail.R
## Further disaggregate UNEP super-category flows (buildings, civil_infrastructure,
## machinery, short_lived) into 8 sub-end-uses using inflow shares derived
## from Wiedenhofer2024 global material stocks (SI figs 3a, 3c).
##
## Method: stock-driven Müller-type inversion of Wiedenhofer2024 global stocks
## to recover gross inflows per sub-use. Inflow shares applied to UNEP flows.
## Inflow shares (not stock shares) are used to avoid over-weighting long-lived
## sub-uses, since S = I·τ inflates stock-shares by the lifetime ratio.
##
## [ASSUMPTION] CV = 0.3 (lognormal σ_real / M); MISO gives only mean lifetimes.
## [ASSUMPTION] σ in log-space is constant across sub-uses (function of CV only,
##   not τ). This is a simplification — not a MISO result — flagged in output.
## [ASSUMPTION] Wiedenhofer2024 global inflow shares applied uniformly to all regions.
##
## Input:
##   Parameters/Intermediate/UNEP_flows_enduse.csv  — 4 super-categories (from 01)
##   Inputs/MISO/SI_Wiedenhofer2024_globalStocks.xlsx
##     sheet data_from_fig_3a_in_manuscript  : stocks 1900–2016, buildings + civil
##     sheet data_from_fig_3c_in_manuscript  : stocks 1900–2016, machinery + short_lived
##
## Output:
##   Parameters/Intermediate/UNEP_flows_subenduse.parquet
##     columns: year, Region, material, super_category, sub_use,
##              inflow_share, flow_Mt
##   Figures/MISO/:
##     subenduse_shares_[category].png  — time-varying inflow share per super-cat
##   Console: inversion vs ΔS vs S·τ⁻¹ diagnostic table; CV sensitivity table
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00-CommonParameters.R", encoding = "UTF-8")

# ── Constants ──────────────────────────────────────────────────────────────────

CV_PRIMARY <- 0.3 # [ASSUMPTION] CV = σ_real / M
CV_SENS <- c(0.2, 0.3, 0.5) # sensitivity sweep
STABLE_START <- 1990 # stable-period for mean share summary
STABLE_END <- 2010
SHARE_TOL <- 0.001
FIG_DIR <- "Figures/MISO"

# Sub-use mean lifetimes (MISO arithmetic mean, yr) --------
MEAN_LIFE <- c(
  residential = 80,
  non_residential = 50,
  roads = 33,
  civil_engineering = 120,
  machinery_group = 25, # Machinery 25 + Electrical equip. 25 + Other transport 28
  vehicles_group = 14, # Motor vehicles 14 + Computers 11
  durables = 12, # Furniture 14 + Printed matter 10 + Products nec 10
  packaging = 0.5 # Food packaging
)

# Super-category each sub-use belongs to --------
SUPER_CAT <- c(
  residential = "buildings",
  non_residential = "buildings",
  roads = "civil_infrastructure",
  civil_engineering = "civil_infrastructure",
  machinery_group = "machinery",
  vehicles_group = "machinery",
  durables = "short_lived",
  packaging = "short_lived"
)

# Display labels and palette --------
SUBENDUSE_LABELS <- c(
  residential = "Residential",
  non_residential = "Non-residential",
  roads = "Roads",
  civil_engineering = "Civil engineering",
  machinery_group = "Machinery & equipment",
  vehicles_group = "Vehicles",
  durables = "Durables",
  packaging = "Packaging"
)

# Step 1: Load UNEP super-category flows ---------------------------------------

cat("STEP 1: Load UNEP super-category flows\n")

unep_super <- read_csv("Parameters/Intermediate/UNEP_flows_enduse.csv", show_col_types = FALSE)
cat("  UNEP super-cat rows:", nrow(unep_super), "| years:", min(unep_super$year), "-", max(unep_super$year), "\n")
cat("  End-uses:", paste(sort(unique(unep_super$end_use)), collapse = ", "), "\n")
cat("  Materials:", paste(sort(unique(unep_super$material)), collapse = ", "), "\n")

# Rename to super_category for clarity
unep_super <- unep_super %>% rename(super_category = end_use)


# Step 3: Build tidy MISO sub-use stock table ---------------------------
# Sheet 3a: A4:DN9  — row 4 is header (years 1900–2016), rows 5–9 are sectors
# Sheet 3c: A4:DN13 — row 4 is header (years 1900–2016), rows 5–13 are sectors
# First column in each sheet = sector label (renamed to "sector" below)

cat("\nSTEP 3: Read MISO stocks and build tidy sub-use table\n")

df_3a_raw <- readxl::read_excel(
  "Inputs/MISO/SI_Wiedenhofer2024_globalStocks.xlsx",
  sheet = "data_from_fig_3a_in_manuscript",
  range = "A4:DN9"
)
cat("  Sheet 3a sectors found:", paste(df_3a_raw$sector, collapse = " | "), "\n")

df_3c_raw <- readxl::read_excel(
  "Inputs/MISO/SI_Wiedenhofer2024_globalStocks.xlsx",
  sheet = "data_from_fig_3c_in_manuscript",
  range = "A4:DN13"
)
cat("  Sheet 3c sectors found:", paste(df_3c_raw$sector, collapse = " | "), "\n")

# Map sectors → sub_use; unrecognized sectors get NA and are dropped
stock_3a <- df_3a_raw %>%
  pivot_longer(-sector, names_to = "year", values_to = "stock_Gt") %>%
  mutate(
    year = as.integer(year),
    sub_use = case_when(
      sector == "Residential buildings" ~ "residential",
      sector == "Non-residential buildings" ~ "non_residential",
      sector == "Roads" ~ "roads",
      sector == "Civil engineering" ~ "civil_engineering",
      TRUE ~ NA_character_ # "Machinery / short lived products" — covered by 3c
    )
  ) %>%
  filter(!is.na(sub_use)) %>%
  group_by(sub_use, year) %>%
  summarise(stock_Gt = sum(stock_Gt, na.rm = TRUE), .groups = "drop")

stock_3c <- df_3c_raw %>%
  pivot_longer(-sector, names_to = "year", values_to = "stock_Gt") %>%
  mutate(
    year = as.integer(year),
    sub_use = case_when(
      sector %in% c("Machinery and equipment", "Electrical equipment", "Other transport") ~ "machinery_group",
      sector %in% c("Motor vehicles", "Computers") ~ "vehicles_group",
      sector %in% c("Furniture", "Printed matter", "Products nec") ~ "durables",
      sector == "Food packaging" ~ "packaging",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(sub_use)) %>%
  group_by(sub_use, year) %>%
  summarise(stock_Gt = sum(stock_Gt, na.rm = TRUE), .groups = "drop")

stock_tidy <- bind_rows(stock_3a, stock_3c) %>%
  mutate(super_category = SUPER_CAT[sub_use], mean_life = MEAN_LIFE[sub_use]) %>%
  arrange(super_category, sub_use, year)

cat("  Stock tidy rows:", nrow(stock_tidy), "| year range:", min(stock_tidy$year), "-", max(stock_tidy$year), "\n")
cat("  Sub-uses:", paste(sort(unique(stock_tidy$sub_use)), collapse = ", "), "\n")

# Diagnostic: stock at 2016
stock_2016_diag <- stock_tidy %>%
  filter(year == 2016) %>%
  arrange(super_category, sub_use) %>%
  dplyr::select(super_category, sub_use, mean_life, stock_Gt)
cat("\n  Stock at 2016 (Gt):\n")
print(stock_2016_diag, n = 10)


# Step 4: Müller-type inversion — recover gross inflows from stocks ------------
# For each sub-use and each CV value in CV_SENS:
#   I(t) = S(t) - Σ_{k<t} I(k)·ℓ(t-k)   [forward recursion, I≥0]
#   ℓ(a) = 1 - plnorm(a; μ, σ)   lognormal survival at age a
#   σ² = ln(1 + CV²)   μ = ln(τ) - σ²/2   [ASSUMPTION: σ constant across sub-uses for given CV]

cat("\nSTEP 4: Müller-type inversion — recover inflows from stocks\n")
cat("  [NOTE] σ in log-space is constant across sub-uses for a given CV (simplification, not MISO result)\n")

shares_cv_all <- purrr::map_dfr(CV_SENS, function(cv) {
  sigma2_ls <- log(1 + cv^2)
  sigma_ls <- sqrt(sigma2_ls)

  inflows_this_cv <- stock_tidy %>%
    group_by(sub_use, super_category, mean_life) %>%
    arrange(year) %>%
    nest() %>%
    mutate(
      inflow_tbl = purrr::map2(data, mean_life, function(d, tau) {
        S <- d$stock_Gt
        n <- length(S)
        mu_ls <- log(tau) - sigma2_ls / 2
        # ell[k] = lognormal survival at age (k-1); ell[1]=1 (age 0)
        ell <- c(1, plnorm(seq_len(n - 1), meanlog = mu_ls, sdlog = sigma_ls, lower.tail = FALSE))
        I <- numeric(n)
        I[1] <- max(0, S[1])
        for (t in seq(2, n)) {
          I[t] <- max(0, S[t] - sum(I[seq_len(t - 1)] * ell[t:2]))
        }
        tibble(year = d$year, inflow_Gt = I)
      })
    ) %>%
    dplyr::select(-data) %>%
    unnest(inflow_tbl) %>%
    ungroup() %>%
    mutate(cv = cv)

  # Inflow shares within each super-category
  inflows_this_cv %>%
    group_by(super_category, year, cv) %>%
    mutate(denom = sum(inflow_Gt), share_inversion = if_else(denom > 0, inflow_Gt / denom, 0)) %>%
    dplyr::select(-denom) %>%
    ungroup()
})

cat("  Inversion done for CV ∈ {", paste(CV_SENS, collapse = ", "), "}\n")
cat("  Rows:", nrow(shares_cv_all), "\n")

# Primary CV result
shares_primary <- shares_cv_all %>% filter(cv == CV_PRIMARY)


# Step 5: Diagnostic brackets — inversion vs ΔS vs S·τ⁻¹ ----------------------

cat("\nSTEP 5: Diagnostic brackets — compare three share methods\n")

brackets <- stock_tidy %>%
  arrange(super_category, sub_use, year) %>%
  group_by(sub_use, super_category, mean_life) %>%
  mutate(delta_S = pmax(0, stock_Gt - lag(stock_Gt, default = 0))) %>%
  ungroup() %>%
  mutate(ss_flow = stock_Gt / mean_life) %>% # steady-state S·τ⁻¹
  left_join(shares_primary %>% dplyr::select(sub_use, year, inflow_Gt, share_inversion), by = c("sub_use", "year")) %>%
  group_by(super_category, year) %>%
  mutate(
    denom_delta = sum(delta_S),
    denom_ss = sum(ss_flow),
    share_delta_S = if_else(denom_delta > 0, delta_S / denom_delta, 0),
    share_ss = if_else(denom_ss > 0, ss_flow / denom_ss, 0)
  ) %>%
  dplyr::select(-denom_delta, -denom_ss) %>%
  ungroup()

# Stable-period mean per sub-use
bracket_mean <- brackets %>%
  filter(year >= STABLE_START, year <= STABLE_END) %>%
  group_by(super_category, sub_use, mean_life) %>%
  summarise(
    share_inversion = mean(share_inversion, na.rm = TRUE),
    share_delta_S = mean(share_delta_S, na.rm = TRUE),
    share_ss = mean(share_ss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(super_category, sub_use)

{
  cat(sprintf("\n  Stable-period mean shares (%d–%d), CV = %.1f:\n", STABLE_START, STABLE_END, CV_PRIMARY))
  cat(sprintf(
    "  %-20s %-22s %-6s %-12s %-12s %-12s\n",
    "super_category",
    "sub_use",
    "tau",
    "inversion",
    "delta_S",
    "S/tau"
  ))
  for (i in seq_len(nrow(bracket_mean))) {
    r <- bracket_mean[i, ]
    cat(sprintf(
      "  %-20s %-22s %4.0f   %10.3f   %10.3f   %10.3f\n",
      r$super_category,
      r$sub_use,
      r$mean_life,
      r$share_inversion,
      r$share_delta_S,
      r$share_ss
    ))
  }
}

# Step 6: CV sensitivity table -------------------------------------------------

cat("\nSTEP 6: CV sensitivity — stable-period mean shares per sub-use\n")
cat("  [NOTE] Wide-gap splits (civil 25/120, short_lived 12/0.5) are CV-sensitive;\n")
cat("         narrow-gap splits (buildings 80/50, machinery 25/14) are CV-invariant.\n\n")

cv_sens_table <- shares_cv_all %>%
  filter(year >= STABLE_START, year <= STABLE_END) %>%
  group_by(cv, super_category, sub_use) %>%
  summarise(mean_share = mean(share_inversion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cv, values_from = mean_share, names_prefix = "CV=") %>%
  arrange(super_category, sub_use) %>%
  mutate(spread = `CV=0.5` - `CV=0.2`)
{
  cat("  CV sensitivity (stable-period mean shares):\n")
  cat(sprintf(
    "  %-20s %-22s %-8s %-8s %-8s %-8s\n",
    "super_category",
    "sub_use",
    "CV=0.2",
    "CV=0.3",
    "CV=0.5",
    "spread"
  ))
  for (i in seq_len(nrow(cv_sens_table))) {
    r <- cv_sens_table[i, ]
    cat(sprintf(
      "  %-20s %-22s %6.3f   %6.3f   %6.3f   %6.3f\n",
      r$super_category,
      r$sub_use,
      r$`CV=0.2`,
      r$`CV=0.3`,
      r$`CV=0.5`,
      r$spread
    ))
  }
}

# Step 7: Apply shares to UNEP flows -------------------------------------------

cat("\nSTEP 7: Apply sub-use inflow shares to UNEP super-category flows\n")

# Time-varying shares from primary CV (1900–2016)
# Extend to post-2016 UNEP years via LOCF
miso_max_year <- max(shares_primary$year)
unep_max_year <- max(unep_super$year)
post_years <- seq(miso_max_year + 1, unep_max_year)

if (length(post_years) > 0) {
  warning(paste0(
    "Wiedenhofer stocks cover up to ",
    miso_max_year,
    "; ",
    "inflow shares for ",
    min(post_years),
    "–",
    max(post_years),
    " are LOCF-extrapolated from ",
    miso_max_year,
    "."
  ))
  shares_2016_locf <- shares_primary %>%
    filter(year == miso_max_year) %>%
    dplyr::select(sub_use, super_category, mean_life, share_inversion)

  shares_extended <- expand_grid(shares_2016_locf, year = post_years)
  shares_all_years <- bind_rows(
    shares_primary %>% dplyr::select(sub_use, super_category, mean_life, year, share_inversion),
    shares_extended
  )
} else {
  shares_all_years <- shares_primary %>% dplyr::select(sub_use, super_category, mean_life, year, share_inversion)
}

# Validate shares sum to 1 within super-category × year
share_check <- shares_all_years %>%
  filter(year %in% unique(unep_super$year)) %>%
  group_by(super_category, year) %>%
  summarise(total = sum(share_inversion), .groups = "drop") %>%
  filter(abs(total - 1) > SHARE_TOL)
if (nrow(share_check) > 0) {
  cat("[WARNING] Share sums deviate from 1 in", nrow(share_check), "super_category×year cells.\n")
  print(head(share_check, 10))
}

# Normalize to enforce exact mass balance (handles small float errors from max(0,...))
shares_all_years <- shares_all_years %>%
  group_by(super_category, year) %>%
  mutate(share_inversion = share_inversion / sum(share_inversion)) %>%
  ungroup()

# Join shares to UNEP flows and multiply
unep_sub <- unep_super %>%
  left_join(shares_all_years, by = c("super_category", "year"), relationship = "many-to-many") %>%
  mutate(flow_Mt = flow_Mt * share_inversion) %>%
  filter(!is.na(sub_use)) %>%
  dplyr::select(year, Region, material, super_category, sub_use, mean_life, inflow_share = share_inversion, flow_Mt)

cat("  UNEP sub-use rows:", nrow(unep_sub), "\n")

# Mass-balance check: sub-use flows should sum back to super-category totals
roundtrip <- unep_sub %>%
  group_by(year, Region, material, super_category) %>%
  summarise(recon_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(unep_super, by = c("year", "Region", "material", "super_category")) %>%
  summarise(max_diff = max(abs(recon_Mt - flow_Mt), na.rm = TRUE)) %>%
  pull(max_diff)
cat(sprintf("  Mass-balance round-trip (max abs diff): %.6f Mt\n", roundtrip))


# Step 8: Save parquet ---------------------------------------------------------

cat("\nSTEP 8: Save output parquet\n")

nrow(unep_sub)
write.csv(unep_sub, "Parameters/Intermediate/UNEP_flows_subenduse.csv", row.names = F)
cat("  Rows:", nrow(unep_sub), "| columns:", paste(names(unep_sub), collapse = ", "), "\n")

# Summary table
cat("\n  Total flow by material × year (sample years):\n")
unep_sub %>%
  filter(year %in% c(1970, 1990, 2010, 2016)) %>%
  group_by(material, super_category, year) %>%
  summarise(flow_Mt = round(sum(flow_Mt), 1), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = flow_Mt) %>%
  print(n = 20)


# Step 9: Figures — inflow share time series per super-category ----------------

cat("\nSTEP 9: Save figures to", FIG_DIR, "\n")

share_plot_data <- shares_cv_all %>%
  filter(cv == CV_PRIMARY, year >= 1970) %>%
  mutate(
    sub_use_label = factor(SUBENDUSE_LABELS[sub_use], levels = SUBENDUSE_LABELS),
    super_label = recode(
      super_category,
      buildings = "Buildings",
      civil_infrastructure = "Civil infrastructure",
      machinery = "Machinery",
      short_lived = "Short-lived products"
    )
  )

# One figure per super-category --------
for (sc in unique(share_plot_data$super_category)) {
  sc_data <- share_plot_data %>% filter(super_category == sc)
  sc_label <- unique(sc_data$super_label)
  sub_uses <- unique(sc_data$sub_use)

  # Label positions: rightmost year, midpoint of stacked area
  label_yr <- max(sc_data$year)
  label_pos <- sc_data %>%
    filter(year == label_yr) %>%
    arrange(desc(sub_use_label)) %>%
    mutate(cum = cumsum(share_inversion), mid = cum - share_inversion / 2)

  ggplot(sc_data, aes(x = year, y = share_inversion, fill = sub_use_label)) +
    geom_area(colour = "black", linewidth = 0.15, alpha = 0.92) +
    geom_text(
      data        = label_pos,
      aes(x = label_yr, y = mid, label = sub_use_label),
      inherit.aes = FALSE,
      size = 2.7, fontface = "bold", hjust = 1.05, show.legend = FALSE
    ) +
    scale_fill_manual(values = PALETTE_SUBENDUSE[SUBENDUSE_LABELS[sub_uses]], name = NULL) +
    scale_x_continuous(breaks = seq(1970, 2010, by = 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    labs(
      title = paste0("Sub-use inflow shares — ", sc_label),
      subtitle = sprintf("Müller inversion of Wiedenhofer2024 global stocks  |  CV = %.1f [ASSUMPTION]", CV_PRIMARY),
      x = NULL,
      y = "Share of super-category inflow"
    ) +
    theme_pb_large() +
    theme(legend.position = "none")

  fname <- file.path(FIG_DIR, paste0("01b-subenduse_shares_", sc, ".png"))
  # fmt: skip
  ggsave(fname, ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
  cat("  Saved:", fname, "\n")
}

# CV sensitivity overlay — one panel per super-category --------
cv_plot_data <- shares_cv_all %>%
  filter(year >= 1970) %>%
  mutate(
    cv_label = paste0("CV=", cv),
    sub_use_label = SUBENDUSE_LABELS[sub_use],
    super_label = recode(
      super_category,
      buildings = "Buildings",
      civil_infrastructure = "Civil infrastructure",
      machinery = "Machinery",
      short_lived = "Short-lived products"
    )
  )

ggplot(cv_plot_data, aes(x = year, y = share_inversion, colour = sub_use_label, linetype = cv_label)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  facet_wrap(~super_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = PALETTE_SUBENDUSE[unname(SUBENDUSE_LABELS)], name = NULL) +
  scale_linetype_manual(
    values = c("CV=0.2" = "dotted", "CV=0.3" = "solid", "CV=0.5" = "dashed"),
    name = "CV assumption"
  ) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "CV sensitivity of sub-use inflow shares",
    subtitle = "Narrow-gap splits (buildings, machinery) are nearly CV-invariant; wide-gap splits (civil, short-lived) are CV-sensitive",
    x = NULL,
    y = "Inflow share"
  ) +
  theme_pb_large() +
  theme(legend.position = "right", panel.spacing = unit(0.8, "lines"))

# fmt: skip
ggsave(file.path(FIG_DIR, "01b-subenduse_shares_cv_sensitivity.png"), ggplot2::last_plot(),
       units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
cat("  Saved:", file.path(FIG_DIR, "subenduse_shares_cv_sensitivity.png"), "\n")

# EoF
