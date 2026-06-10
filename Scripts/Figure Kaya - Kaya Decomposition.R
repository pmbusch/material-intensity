## =============================================================================
## Figure Kaya - Kaya Identity Decomposition.R
## Decomposes global material consumption (DMC) into Kaya drivers:
##   Mat = Population × (GDP / Population) × (Mat / GDP)
## All four drivers expressed as % change relative to 1970 (base = 0 %).
##
## Panels / figures produced:
##   Ka  — Global, all materials (single panel)
##   Kb  — Facet by region (9 panels)
##   Kc  — Global + facet by material group (7 panels)
##   Kd  — Global + facet by material category (22 panels)
##   Ke1 — PDF: facet by region, one page per material group  (6 pages)
##   Ke2 — PDF: facet by region, one page per material category (~22 pages)
##
## Reads from Parameters/ and Inputs/; saves to Figures/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

## Load data -------------------------------------------------------------------

df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) %>% rename(Analysis_group = Region)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)

# Material group lookup (22 categories → 6 groups)
dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")
cat("Material categories:", paste(sort(unique(df$material_category)), collapse = " | "), "\n")


## Kaya colour / linewidth / inline-label position palettes -------------------

PALETTE_KAYA <- c(
  "Population" = "#004488",
  "GDP / capita" = "#228B22",
  "Mat / GDP" = "#CC3311",
  "Materials (DMC)" = "#222222"
)
# All lines are solid; Materials (DMC) slightly thicker as the summary metric
LINEWIDTH_KAYA <- c("Population" = 0.55, "GDP / capita" = 0.55, "Mat / GDP" = 0.55, "Materials (DMC)" = 1.0)
# hjust controls where along the path geomtextpath places the inline label
# (0 = line start, 1 = line end). Stagger to prevent overlaps.
HJUST_KAYA <- c("Population" = 0.22, "GDP / capita" = 0.42, "Mat / GDP" = 0.62, "Materials (DMC)" = 0.80)
DRIVER_LEVELS <- c("Population", "GDP / capita", "Mat / GDP", "Materials (DMC)")

## Shared event lines ----------------------------------------------------------

event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40")
)

## Helper: compute Kaya indices ------------------------------------------------
# Input:  data frame with columns: year, total_DMC (tonnes), total_pop, total_GDP
# Output: long data frame with columns: year, driver (factor), index (% change vs 1970)
#         Returns an empty tibble if 1970 base data are missing or zero.

compute_kaya_idx <- function(agg_df) {
  empty_out <- tibble(year = integer(), driver = factor(character(), levels = DRIVER_LEVELS), index = numeric())

  base_yr <- agg_df %>% filter(year == 1970)
  if (nrow(base_yr) == 0) {
    return(empty_out)
  }

  base_pop <- base_yr$total_pop[1]
  base_gdp <- base_yr$total_GDP[1]
  base_dmc <- base_yr$total_DMC[1]

  if (any(is.na(c(base_pop, base_gdp, base_dmc)))) {
    return(empty_out)
  }
  if (base_pop == 0 || base_gdp == 0 || base_dmc == 0) {
    return(empty_out)
  }

  agg_df %>%
    mutate(
      idx_pop = (total_pop / base_pop - 1) * 100,
      idx_gdp_pc = ((total_GDP / total_pop) / (base_gdp / base_pop) - 1) * 100,
      idx_mat_gdp = ((total_DMC / total_GDP) / (base_dmc / base_gdp) - 1) * 100,
      idx_mat = (total_DMC / base_dmc - 1) * 100
    ) %>%
    pivot_longer(cols = starts_with("idx_"), names_to = "driver", values_to = "index") %>%
    mutate(
      driver = factor(
        recode(
          driver,
          "idx_pop" = "Population",
          "idx_gdp_pc" = "GDP / capita",
          "idx_mat_gdp" = "Mat / GDP",
          "idx_mat" = "Materials (DMC)"
        ),
        levels = DRIVER_LEVELS
      )
    ) %>%
    dplyr::select(year, driver, index)
}

## Helper: Kaya base plot ------------------------------------------------------
# Uses geomtextpath for inline driver labels; adds end-of-line % change annotations.
# No colour legend — colour + inline text distinguish the four lines.

plot_kaya <- function(kaya_long, title = NULL, subtitle = NULL) {
  # Attach per-driver inline-label position (hjust along the line path)
  kaya_long <- kaya_long %>% mutate(hjust_val = HJUST_KAYA[as.character(driver)])

  # End-of-line annotation: % change value at the last available year
  max_yr <- max(kaya_long$year, na.rm = TRUE)
  last_vals <- kaya_long %>%
    filter(year == max_yr, !is.na(index)) %>%
    mutate(label_end = paste0(ifelse(index >= 0, "+", ""), round(index, 0), "%"))

  ggplot(kaya_long, aes(x = year, y = index, colour = driver, label = driver, hjust = hjust_val)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_textline(aes(linewidth = driver), size = 2, vjust = -0.3, fontface = "bold") +
    geom_text(
      data = last_vals,
      aes(x = year, y = index, label = label_end, colour = driver),
      hjust = 0, size = 1.8, fontface = "bold", nudge_x = 0.8,
      inherit.aes = FALSE
    ) +
    event_lines +
    scale_colour_manual(values = PALETTE_KAYA, guide = "none") +
    scale_linewidth_manual(values = LINEWIDTH_KAYA, guide = "none") +
    scale_x_continuous(
      breaks = seq(1970, 2024, 10),
      expand = expansion(mult = c(0.01, 0.14)) # right padding for end labels
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    coord_cartesian(clip = "off") +
    theme_pb_large() +
    labs(title = title, subtitle = subtitle, x = "Year", y = "Change since 1970 (%)") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

## Build regional base (all materials) -----------------------------------------
# Data already at regional level — join pop and GDP directly by region-year.

region_base <- df %>%
  rename(analysis_group = Analysis_group) %>%
  filter(!is.na(analysis_group), !is.na(DMC_Mt)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Mt_total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_pop %>% rename(analysis_group = Region), by = c("year", "analysis_group")) %>%
  left_join(df_gdp %>% rename(analysis_group = Region), by = c("year", "analysis_group")) %>%
  filter(!is.na(population), !is.na(GDP_2015USD))

# Global pop + GDP by year (reused in material-level global panels)
global_pop_gdp <- region_base %>%
  group_by(year) %>%
  summarise(total_pop = sum(population, na.rm = TRUE), total_GDP = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop")

# Regional pop + GDP by year (reused in material-level regional panels)
region_pop_gdp <- region_base %>% dplyr::select(year, analysis_group, total_pop = population, total_GDP = GDP_2015USD)

# Material ordering: descending total DMC (largest material first)
mat_order <- df %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(material_category)


## Figure Ka: Global Kaya, all materials ----------------------------------------

cat("── Figure Ka ──\n")

world_agg <- region_base %>%
  group_by(year) %>%
  summarise(
    total_DMC = sum(DMC_Mt_total * 1e6, na.rm = TRUE), # Mt → tonnes
    total_pop = sum(population, na.rm = TRUE),
    total_GDP = sum(GDP_2015USD, na.rm = TRUE),
    .groups = "drop"
  )

world_kaya <- compute_kaya_idx(world_agg)

plot_kaya(world_kaya, title = "Global Material Consumption", subtitle = "")

# fmt: skip
ggsave("Figures/FigKa_global_kaya.png", last_plot(), units = 'cm', dpi = 600,width = 8.7 * 2, height = 8.7 * 1.4)
ggsave("Figures/SVG/FigKa_global_kaya.svg", last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7 * 1.4)
clean_svg("Figures/SVG/FigKa_global_kaya.svg")


## Figure Kb: Kaya faceted by region -------------------------------------------

cat("── Figure Kb ──\n")

region_agg <- region_base %>%
  mutate(total_DMC = DMC_Mt_total * 1e6, total_pop = population, total_GDP = GDP_2015USD) %>%
  dplyr::select(year, analysis_group, total_DMC, total_pop, total_GDP)

region_kaya <- region_agg %>% group_by(analysis_group) %>% group_modify(~ compute_kaya_idx(.x)) %>% ungroup()

plot_kaya(region_kaya, title = "Kaya Decomposition by Region", subtitle = "All materials  ·  % change since 1970") +
  facet_wrap(~analysis_group, nrow = 3, scales = "free_y")

# fmt: skip
ggsave("Figures/FigKb_region_kaya.png", last_plot(), units = 'cm', dpi = 600, width = 8.7 * 3, height = 8.7 * 3.2)
ggsave("Figures/SVG/FigKb_region_kaya.svg", last_plot(), units = 'cm', width = 8.7 * 3, height = 8.7 * 3.2)
clean_svg("Figures/SVG/FigKb_region_kaya.svg")


## Figure Kc: Kaya global + faceted by material group --------------------------

cat("── Figure Kc ──\n")

# Sum DMC by material group — restricted to valid_iso_year for consistency
dmc_by_grp <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Material_group)) %>%
  group_by(year, Material_group) %>%
  summarise(total_DMC = sum(DMC_Mt * 1e6, na.rm = TRUE), .groups = "drop") %>%
  left_join(global_pop_gdp, by = "year")

grp_kaya <- dmc_by_grp %>% group_by(Material_group) %>% group_modify(~ compute_kaya_idx(.x)) %>% ungroup()


grp_kaya <- grp_kaya |> filter(!(Material_group %in% c("Mixed", "Waste")))

plot_kaya(
  grp_kaya,
  title = "Kaya Decomposition by Material Group",
  subtitle = "% change since 1970  ·  population and GDP from consistent country set"
) +
  facet_wrap(~Material_group, nrow = 2, scales = "free_y")

# fmt: skip
ggsave("Figures/FigKc_materialgroup_kaya.png", last_plot(), units = 'cm', dpi = 600,width = 8.7 * 2, height = 8.7 * 2.3)
ggsave("Figures/SVG/FigKc_materialgroup_kaya.svg", last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7 * 2.3)
clean_svg("Figures/SVG/FigKc_materialgroup_kaya.svg")


## Figure Kd: Kaya global + faceted by material category -----------------------

cat("── Figure Kd ──\n")

dmc_by_mat <- df %>%
  group_by(year, material_category) %>%
  summarise(total_DMC = sum(DMC_Mt * 1e6, na.rm = TRUE), .groups = "drop") %>%
  left_join(global_pop_gdp, by = "year")

mat_kaya <- dmc_by_mat %>% group_by(material_category) %>% group_modify(~ compute_kaya_idx(.x)) %>% ungroup()

# Combine global panel with material-category panels
mat_kaya_all <- bind_rows(world_kaya %>% mutate(material_category = "World (all materials)"), mat_kaya) %>%
  mutate(
    material_category = str_wrap(material_category, width = 32),
    material_category = factor(material_category, levels = str_wrap(c("World (all materials)", mat_order), width = 32))
  )

plot_kaya(
  mat_kaya_all,
  title = "Kaya Decomposition by Material Category",
  subtitle = "% change since 1970  ·  population and GDP from consistent country set"
) +
  facet_wrap(~material_category, nrow = 4, scales = "free_y")

# fmt: skip
ggsave("Figures/FigKd_material_kaya.png", last_plot(), units = 'cm', dpi = 600,width = 8.7 * 6, height = 8.7 * 4.2)
ggsave("Figures/SVG/FigKd_material_kaya.svg", last_plot(), units = 'cm', width = 8.7 * 6, height = 8.7 * 4.2)
clean_svg("Figures/SVG/FigKd_material_kaya.svg")


## Figure Ke: PDFs — Kaya by region for each material group / category ---------

cat("── Figure Ke ──\n")

# -- Part 1: one page per material group (6 pages) ----------------------------

pdf_path_e1 <- "Figures/FigKe1_kaya_region_by_materialgroup.pdf"
pdf(
  pdf_path_e1,
  width = 8.7 * 3 / 2.54, # cm → inches
  height = 8.7 * 3 / 2.54
)

for (grp in names(PALETTE_MATERIAL_GROUPS)) {
  cat("  Material group:", grp, "\n")

  dmc_grp_region <- df %>%
    rename(analysis_group = Analysis_group) %>%
    left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
    filter(Material_group == grp, !is.na(analysis_group)) %>%
    group_by(year, analysis_group) %>%
    summarise(total_DMC = sum(DMC_Mt * 1e6, na.rm = TRUE), .groups = "drop") %>%
    left_join(region_pop_gdp, by = c("year", "analysis_group"))

  region_kaya_grp <- dmc_grp_region %>%
    filter(!is.na(total_pop), !is.na(total_GDP)) %>%
    group_by(analysis_group) %>%
    group_modify(~ compute_kaya_idx(.x)) %>%
    ungroup()

  if (nrow(region_kaya_grp) == 0) {
    cat("    [SKIP] no data for", grp, "\n")
    next
  }

  p <- plot_kaya(
    region_kaya_grp,
    title = paste("Kaya Decomposition —", grp),
    subtitle = "By region  ·  % change since 1970"
  ) +
    facet_wrap(~analysis_group, nrow = 3, scales = "free_y")

  print(p)
}

dev.off()
cat("  Saved:", pdf_path_e1, "\n")


# -- Part 2: one page per material category (~22 pages) -----------------------

pdf_path_e2 <- "Figures/FigKe2_kaya_region_by_material.pdf"
pdf(pdf_path_e2, width = 8.7 * 3 / 2.54, height = 8.7 * 3 / 2.54)

for (mat in mat_order) {
  cat("  Material:", mat, "\n")

  dmc_mat_region <- df %>%
    rename(analysis_group = Analysis_group) %>%
    filter(material_category == mat, !is.na(analysis_group)) %>%
    group_by(year, analysis_group) %>%
    summarise(total_DMC = sum(DMC_Mt * 1e6, na.rm = TRUE), .groups = "drop") %>%
    left_join(region_pop_gdp, by = c("year", "analysis_group"))

  region_kaya_mat <- dmc_mat_region %>%
    filter(!is.na(total_pop), !is.na(total_GDP)) %>%
    group_by(analysis_group) %>%
    group_modify(~ compute_kaya_idx(.x)) %>%
    ungroup()

  if (nrow(region_kaya_mat) == 0) {
    cat("    [SKIP] no data for", mat, "\n")
    next
  }

  p <- plot_kaya(region_kaya_mat, title = str_wrap(mat, width = 60), subtitle = "By region  ·  % change since 1970") +
    facet_wrap(~analysis_group, nrow = 3, scales = "free_y")

  print(p)
}

dev.off()
cat("  Saved:", pdf_path_e2, "\n")

cat("\nAll Kaya figures written to Figures/\n")

# EoF
