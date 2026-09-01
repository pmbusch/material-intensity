## =============================================================================
## Contour.R
## Phase-space trajectories: Mat/GDP (x) vs GDP/Person (y) by region, 1970–2023
## Each path traces one region every 5 years; iso-lines show constant Mat/capita.
## X axis: linear (Mat/GDP).  Y axis: log (GDP/capita).
## Iso-lines y = k/x appear as hyperbolas on this semi-log layout.
## Three figures:
##   A — all materials summed
##   B — faceted by material group  (6 groups, same classification as Fig 1C)
##   C — faceted by all 22 material categories
## Reads from Parameters/ and Inputs/; saves figures to Figures/Other/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

HIST_END <- 2024L
CENTRAL_SSP <- "SSP2"

## Load data -------------------------------------------------------------------
# Pre-aggregated regional files produced by 01a/01b/01c scripts.
df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) %>% rename(Analysis_group = Region)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)

df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")

# Projection forecast files (Results/)
ssp_drivers <- read_csv("Parameters/IIASA/ssp_drivers.csv", show_col_types = FALSE)
biomass_fc <- read_csv("Results/biomass_forecast.csv", show_col_types = FALSE)
fossil_fc <- read_csv("Results/fossil_fuel_forecast.csv", show_col_types = FALSE)
metal_fc <- read_csv("Results/metal_ores_forecast.csv", show_col_types = FALSE)
nonmet_flow_fc <- read_csv("Results/nonmetallic_minerals_forecast.csv", show_col_types = FALSE)

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS          : 9 analysis groups (regions)
# PALETTE_MATERIALS        : 21 material categories
# PALETTE_MATERIAL_GROUPS  : 6 material groups

## Material group dictionary ---------------------------------------------------

dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group)

grp_mat_order <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Material_group)) %>%
  group_by(Material_group) %>%
  summarise(total = sum(DMC_Mt), .groups = "drop") %>%
  arrange(total)

years_5yr <- seq(1970, 2024, by = 1)

## Helper: build iso-line dataframe -------------------------------------------
# Mat/cap = Mat/GDP × GDP/cap  →  y = iso_level_kg / x  (hyperbola)
# Log-spaced x gives smooth curves (denser at small x where the hyperbola bends sharply).
# y_min / y_max clip the output so iso-lines don't expand the facet axis limits.

make_isolines <- function(iso_levels_t, x_min, x_max, y_min = -Inf, y_max = Inf, n = 200) {
  x_min <- max(x_min, 1e-10, na.rm = TRUE)
  x_max <- max(x_max, x_min * 2, na.rm = TRUE)
  x_seq <- 10^seq(log10(x_min), log10(x_max), length.out = n)
  expand.grid(iso_t = iso_levels_t, x = x_seq) %>%
    mutate(
      y = (iso_t * 1e3) / x, # t/cap → kg/cap;  y = level_kg / x
      iso_label = paste0(iso_t, " t/cap")
    ) %>%
    filter(y >= y_min, y <= y_max)
}

## Helper: auto-pick iso-line levels that bracket a mat/cap range --------------
# Selects up to n_max "nice" log-spaced values (1/2/5 series) spanning from
# just below the data minimum to just above the data maximum.

NICE_LEVELS_T <- sort(c(outer(c(1, 2, 3, 4, 5), 10^(-3:3)))) # 0.001 … 5000 t/cap

pick_iso_levels <- function(mat_pc_vals_kg, n_max = 5) {
  min_t <- min(mat_pc_vals_kg, na.rm = TRUE) / 1000 # kg → t
  max_t <- max(mat_pc_vals_kg, na.rm = TRUE) / 1000
  # Keep levels that span from below the min to above the max
  in_range <- NICE_LEVELS_T[NICE_LEVELS_T >= min_t * 0.4 & NICE_LEVELS_T <= max_t * 2.5]
  if (length(in_range) == 0) {
    return(c(min_t, max_t))
  }
  if (length(in_range) > n_max) {
    idx <- round(seq(1, length(in_range), length.out = n_max))
    in_range <- in_range[idx]
  }
  in_range
}

# LOWESS smoothing  ----------
# Idea: avoid showing annual variability while preserving end points
# frac: 0.181818, 10-11 years window
smooth_lowess <- function(df, group_cols = NULL, frac = 0.18181818, year_col = "year") {
  split_df <- if (is.null(group_cols)) list(df) else split(df, df[group_cols], drop = TRUE)

  out <- lapply(split_df, function(d) {
    x <- d[[year_col]]

    for (col in c("mat_gdp", "gdp_pc")) {
      y <- d[[col]]

      d[[paste0(col, "_raw")]] <- y

      ok <- is.finite(x) & is.finite(y)

      # need enough points
      if (sum(ok) < 5) {
        d[[col]] <- y
        next
      }

      fit <- tryCatch(loess(y[ok] ~ x[ok], span = frac, na.action = na.exclude), error = function(e) NULL)

      if (is.null(fit)) {
        d[[col]] <- y
      } else {
        pred <- tryCatch(predict(fit, newdata = x), error = function(e) rep(NA_real_, length(x)))
        d[[col]] <- pred
      }
    }
    d
  })

  do.call(rbind, out)
}


## FIGURE A: All materials summed ----------------------------------------------

cat("\n── Figure Contour A: all materials ──\n")

# Data already at regional level — join GDP and population directly by region-year
region_all <- df %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_gdp, by = c("Analysis_group" = "Region", "year")) %>%
  left_join(df_pop, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(GDP_2015USD), !is.na(population)) %>%
  mutate(GDP = GDP_2015USD, pop = population, mat_gdp = DMC_kg / GDP_2015USD, gdp_pc = GDP_2015USD / population) %>%
  filter(year %in% years_5yr, !is.na(mat_gdp), !is.na(gdp_pc)) %>%
  arrange(Analysis_group, year)

region_all <- smooth_lowess(region_all, group_cols = c("Analysis_group"))


# Region-year econ totals reused as denominator in Figures B and C
region_econ <- region_all %>% dplyr::select(year, Analysis_group, GDP, pop)

# World average: sum across all regions
world_all <- region_all %>%
  group_by(year) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

world_all <- smooth_lowess(world_all)

## Material category constants (reused across projection figures) ---------------

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum")
METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

## Build projection data for new combined figures --------------------------------

gdp_2024_region <- df_gdp |>
  filter(year == HIST_END) |>
  rename(region = Region) |>
  dplyr::select(region, gdp_2024 = GDP_2015USD)

pop_2024_region <- df_pop |>
  filter(year == HIST_END) |>
  rename(region = Region) |>
  dplyr::select(region, pop_2024 = population)

# World GDP/pop projections (SSP2, HIST_END-FORECAST_END)
world_gdp_proj <- ssp_drivers |>
  filter(variable == "GDP|PPP", scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
  left_join(gdp_2024_region, by = "region") |>
  mutate(gdp = gdp_2024 * index) |>
  group_by(year) |>
  summarise(world_gdp = sum(gdp, na.rm = TRUE), .groups = "drop")

world_pop_proj <- ssp_drivers |>
  filter(variable == "Population", scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
  left_join(pop_2024_region, by = "region") |>
  mutate(pop = pop_2024 * index) |>
  group_by(year) |>
  summarise(world_pop = sum(pop, na.rm = TRUE), .groups = "drop")

world_gdpcap_proj <- world_gdp_proj |> left_join(world_pop_proj, by = "year") |> mutate(gdp_pc = world_gdp / world_pop)

# Regional GDP and pop: 2024 (historical) + 2025-FORECAST_END (SSP2 indexed)
gdp_region_proj <- bind_rows(
  df_gdp |>
    filter(year == HIST_END) |>
    rename(region = Region, gdp_proj = GDP_2015USD) |>
    dplyr::select(region, year, gdp_proj),
  ssp_drivers |>
    filter(variable == "GDP|PPP", scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    left_join(gdp_2024_region, by = "region") |>
    mutate(gdp_proj = gdp_2024 * index) |>
    dplyr::select(region, year, gdp_proj)
)

pop_region_proj <- bind_rows(
  df_pop |>
    filter(year == HIST_END) |>
    rename(region = Region, pop_proj = population) |>
    dplyr::select(region, year, pop_proj),
  ssp_drivers |>
    filter(variable == "Population", scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    left_join(pop_2024_region, by = "region") |>
    mutate(pop_proj = pop_2024 * index) |>
    dplyr::select(region, year, pop_proj)
)

# Historical metal/nonmet DMC at 2024 per region (bridge to connect historical path)
hist_metal_nonmet_2024 <- df |>
  filter(year == HIST_END, !is.na(Analysis_group), material_category %in% c(METAL_CATS, NONMET_CATS)) |>
  group_by(region = Analysis_group, year) |>
  summarise(M_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

# Total material by region (2024-FORECAST_END, SSP2)
# At 2024: bio/fossil from forecast, metal/nonmet from historical DMC
# At 2025+: all four from forecast
total_mat_region_proj <- bind_rows(
  biomass_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    group_by(region, year) |>
    summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop"),
  fossil_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    group_by(region, year) |>
    summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop"),
  hist_metal_nonmet_2024,
  metal_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(region, year) |>
    summarise(M_Mt = sum(production_Mt, na.rm = TRUE), .groups = "drop"),
  nonmet_flow_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(region, year) |>
    summarise(M_Mt = sum(production_Mt, na.rm = TRUE), .groups = "drop")
) |>
  group_by(region, year) |>
  summarise(M_Mt = sum(M_Mt, na.rm = TRUE), .groups = "drop")

region_all_proj <- total_mat_region_proj |>
  left_join(gdp_region_proj, by = c("region", "year")) |>
  left_join(pop_region_proj, by = c("region", "year")) |>
  filter(!is.na(gdp_proj), !is.na(pop_proj)) |>
  rename(Analysis_group = region) |>
  mutate(DMC_kg = M_Mt * 1e9, mat_gdp = DMC_kg / gdp_proj, gdp_pc = gdp_proj / pop_proj)

# World average projection (2024-FORECAST_END)
world_all_proj <- region_all_proj |>
  group_by(year) |>
  summarise(
    DMC_kg = sum(DMC_kg, na.rm = TRUE),
    world_gdp = sum(gdp_proj, na.rm = TRUE),
    world_pop = sum(pop_proj, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mat_gdp = DMC_kg / world_gdp, gdp_pc = world_gdp / world_pop)

iso_a <- make_isolines(
  iso_levels_t = c(0.5, 1, 2, 5, 10, 20, 30, 40),
  x_min = min(region_all$mat_gdp),
  x_max = max(region_all$mat_gdp),
  y_min = min(region_all$gdp_pc),
  y_max = max(region_all$gdp_pc)
)

ggplot(region_all, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_a,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.25,
    size = 2.8,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last"),
    linewidth = 0.6
  ) +
  # World average — black line on top
  geom_path(
    data = world_all,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_all, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 2.4, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
  data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
  shape = 16, size = 2, colour = "white", alpha = 1
) +
  geom_point(
      data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
      aes(alpha = year),
      shape = 16, size = 2
    ) +
  # Direct region labels at 2024 endpoint
  geom_text_repel(
    data = filter(region_all, year == 2024),
    aes(label = Analysis_group),
    size = 2.2,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.3,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_all, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 2.4,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.3,
    segment.colour = "grey40"
  ) +
  # Year labels for world avg at start (1970) and end (2024)
  geom_text(
    data = filter(world_all, year %in% c(1970, 2024)),
    aes(label = year,vjust=c(1.5, 0),hjust=c(-0.2, 1.5)),
    fontface="bold",
    colour = "black", size = 2.5,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(breaks = c(0.5, 2, 4, 6)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.2, colour = "grey50") +
  coord_cartesian(clip = "off", expand = F, xlim = c(0.3, 7.4), ylim = c(NA, 100e3)) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/Other/FigContour_A_all.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave("Figures/Other/SVG/FigContour_A_all.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2, height = 8.7 * 2)
clean_svg("Figures/Other/SVG/FigContour_A_all.svg")


## FIGURE B: Faceted by material group -----------------------------------------

cat("\n── Figure Contour B: by material group ──\n")

# DMC numerator only — restricted to the consistent country base so the
# denominator (region_econ) is shared with region_all.
region_grp <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  filter(!is.na(Analysis_group), !is.na(Material_group)) %>%
  group_by(year, Analysis_group, Material_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(region_econ, by = c("year", "Analysis_group")) %>%
  filter(!is.na(GDP), year %in% years_5yr) %>%
  filter(!(Material_group %in% c("Mixed", "Waste"))) |>
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop) %>%
  mutate(Material_group = factor(Material_group, levels = grp_mat_order$Material_group)) %>%
  arrange(Analysis_group, Material_group, year)

region_grp <- smooth_lowess(region_grp, group_cols = c("Analysis_group", "Material_group"))

# World average per material group
world_grp <- region_grp %>%
  group_by(year, Material_group) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

world_grp <- smooth_lowess(world_grp, group_cols = c("Material_group"))

# Per-group iso-lines: auto-pick levels that bracket actual mat/cap range,
# and clip x/y to the data range so the facet axes aren't expanded by the lines.
iso_b <- region_grp %>%
  group_by(Material_group) %>%
  summarise(
    x_min = min(mat_gdp),
    x_max = max(mat_gdp),
    y_min = min(gdp_pc),
    y_max = max(gdp_pc),
    iso_levels = list(pick_iso_levels(mat_gdp * gdp_pc)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(iso_df = list(make_isolines(iso_levels, x_min, x_max, y_min = y_min, y_max = y_max))) %>%
  dplyr::select(Material_group, iso_df) %>%
  unnest(iso_df)

ggplot(region_grp, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_b,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = 2.2,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed", ends = "last"),
    linewidth = 0.5
  ) +
  # World average per group — black line
  geom_path(
    data = world_grp,
    aes(x = mat_gdp, y = gdp_pc, group = Material_group),
    colour = "black",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_grp, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 2, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
    data = filter(region_grp, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.8, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_grp, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year),
    shape = 16, size = 1.8
  ) +
  # Direct labels at 2024 endpoint
  geom_text_repel(
    data = filter(region_grp, year == 2024),
    aes(label = Analysis_group),
    size = 1.8,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.25,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_grp, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 1.9,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.25,
    segment.colour = "grey40"
  ) +
  # Year labels for world avg at start (1970) and end (2024)
  geom_text(
    data = filter(world_grp, year %in% c(1970, 2024)),
    aes(x = mat_gdp, y = gdp_pc, label = year,
        vjust = ifelse(year == 1970, 1.5, 0),
        hjust = ifelse(year == 1970, -0.2, 1.5)),
    fontface = "bold",
    colour = "black", size = 2,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  facet_wrap(~Material_group, nrow = 2, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.18, colour = "grey50") +
  coord_cartesian(clip = "off", expand = F, ylim = c(NA, 100e3)) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

# fmt: skip
ggsave("Figures/Other/FigContour_B_material_group.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 3, height = 8.7 * 2)
ggsave(
  "Figures/Other/SVG/FigContour_B_material_group.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 3,
  height = 8.7 * 2
)


## World-average projection per material group (used by Figure D and Fig3) ------

world_grp_proj <- bind_rows(
  # Biomass and fossil: forecast starts at 2024 (continuous)
  biomass_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(M_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(Material_group = "Biomass"),
  fossil_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(M_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(Material_group = "Fossil fuels"),
  # Metal ores: 2024 bridge from historical DMC, then forecast production 2025+
  df |>
    filter(year == HIST_END, !is.na(Analysis_group), material_category %in% METAL_CATS) |>
    summarise(DMC_kg = sum(DMC_Mt, na.rm = TRUE) * 1e9) |>
    mutate(Material_group = "Metal ores", year = HIST_END),
  metal_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(production_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(Material_group = "Metal ores"),
  # Non-metallic minerals: same approach
  df |>
    filter(year == HIST_END, !is.na(Analysis_group), material_category %in% NONMET_CATS) |>
    summarise(DMC_kg = sum(DMC_Mt, na.rm = TRUE) * 1e9) |>
    mutate(Material_group = "Non-metallic minerals", year = HIST_END),
  nonmet_flow_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(production_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(Material_group = "Non-metallic minerals")
) |>
  left_join(world_gdpcap_proj |> dplyr::select(year, world_gdp, gdp_pc), by = "year") |>
  mutate(mat_gdp = DMC_kg / world_gdp)

# Projection starts at 2024 — no separate bridge needed
world_grp_proj_full <- world_grp_proj |>
  dplyr::select(Material_group, year, mat_gdp, gdp_pc) |>
  arrange(Material_group, year)


## FIGURE C: Faceted by all 22 material categories -----------------------------

cat("\n── Figure Contour C: 22 material categories ──\n")


# DMC numerator only — same consistent country base and region_econ denominator
# as region_all, so sum of all 22 materials' mat/cap equals the total.
region_mat <- df %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group, material_category) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(region_econ, by = c("year", "Analysis_group")) %>%
  filter(!is.na(GDP), year %in% years_5yr) %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

# Facet order: same as Fig 1F (descending total DMC)
mat_order_c <- region_mat %>%
  group_by(material_category) %>%
  summarise(total = sum(DMC_kg), .groups = "drop") %>%
  arrange(desc(total))

region_mat <- region_mat %>%
  mutate(
    mat_label = str_wrap(material_category, width = 30),
    mat_label = factor(mat_label, levels = str_wrap(mat_order_c$material_category, width = 30))
  ) %>%
  arrange(Analysis_group, material_category, year)

region_mat <- smooth_lowess(region_mat, group_cols = c("Analysis_group", "material_category"))

# World average per material category
world_mat <- region_mat %>%
  group_by(year, material_category, mat_label) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)

world_mat <- smooth_lowess(world_mat, group_cols = c("material_category"))

# Per-material iso-lines: auto-pick levels that bracket actual mat/cap range,
# and clip x/y to the data range so facet axes aren't expanded by the lines.
iso_c <- region_mat %>%
  group_by(mat_label) %>%
  summarise(
    x_min = min(mat_gdp),
    x_max = max(mat_gdp),
    y_min = min(gdp_pc),
    y_max = max(gdp_pc),
    iso_levels = list(pick_iso_levels(mat_gdp * gdp_pc)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(iso_df = list(make_isolines(iso_levels, x_min, x_max, y_min = y_min, y_max = y_max))) %>%
  dplyr::select(mat_label, iso_df) %>%
  unnest(iso_df)

ggplot(region_mat, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_c,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.18,
    size = 1.8,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.08, "cm"), type = "closed", ends = "last"),
    linewidth = 0.38
  ) +
  # World average per material — black line
  geom_path(
    data = world_mat,
    aes(x = mat_gdp, y = gdp_pc, group = material_category),
    colour = "black",
    linewidth = 0.7,
    arrow = arrow(length = unit(0.08, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # World average decade points
  geom_point(
    data = filter(world_mat, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.4, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(world_mat, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 1.4, inherit.aes = FALSE
  ) +
  # Decade points with alpha gradient: light (1970) → dark (2020)
  geom_point(
    data = filter(region_mat, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.3, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_mat, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year),
    shape = 16, size = 1.3
  ) +
  # Direct labels at 2024 endpoint (skip to avoid clutter in 22-panel plot)
  geom_text_repel(
    data = filter(region_mat, year == 2024),
    aes(label = Analysis_group),
    size = 1.5,
    show.legend = FALSE,
    max.overlaps = 5,
    seed = 42,
    segment.size = 0.2,
    segment.colour = "grey60",
    min.segment.length = 0.3
  ) +
  facet_wrap(~mat_label, nrow = 4, scales = "free") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.15, colour = "grey50") +
  theme_pb_large() +
  coord_cartesian(clip = "off", expand = F, ylim = c(NA, 100e3)) +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5))

# fmt: skip
ggsave("Figures/Other/FigContour_C_22_materials.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 4, height = 8.7 * 3)
ggsave(
  "Figures/Other/SVG/FigContour_C_22_materials.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 4,
  height = 8.7 * 3
)

## FIGURE D: Material groups as color (world average, all materials, + SSP2 proj) ----

cat("\n── Figure Contour D: material groups colored ──\n")

# Historical world average per material group (already smoothed in Figure B section)
world_grp_hist <- world_grp |> mutate(Material_group = as.character(Material_group))

# Combined bounds for iso-lines
all_mg_x <- c(world_grp_hist$mat_gdp, world_grp_proj_full$mat_gdp)
all_mg_y <- c(world_grp_hist$gdp_pc, world_grp_proj_full$gdp_pc)

iso_d <- make_isolines(
  iso_levels_t = pick_iso_levels(all_mg_x * all_mg_y, n_max = 6),
  x_min = min(all_mg_x, na.rm = TRUE),
  x_max = max(all_mg_x, na.rm = TRUE),
  y_min = min(all_mg_y, na.rm = TRUE),
  y_max = max(all_mg_y, na.rm = TRUE)
)

ggplot() +
  geom_textline(
    data = iso_d,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.25,
    size = 2.8,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  # Historical solid paths per material group
  geom_path(
    data = world_grp_hist,
    aes(x = mat_gdp, y = gdp_pc, colour = Material_group, group = Material_group),
    linewidth = 1.0,
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")
  ) +
  # SSP2 projection dashed paths
  geom_path(
    data = world_grp_proj_full,
    aes(x = mat_gdp, y = gdp_pc, colour = Material_group, group = Material_group),
    linewidth = 0.8,
    linetype = "dashed",
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")
  ) +
  # Decade points on historical paths
  geom_point(
    data = filter(world_grp_hist, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, colour = Material_group, alpha = year),
    shape = 16, size = 2.2
  ) +
  # Direct material group labels at 2024 endpoint
  geom_text_repel(
    data = filter(world_grp_hist, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = Material_group, colour = Material_group),
    size = 2.2,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.3,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  # Year annotations: 1970 and 2024 (historical), FORECAST_END (projection)
  geom_text(
    data = filter(world_grp_hist, year == 1970),
    aes(x = mat_gdp, y = gdp_pc, colour = Material_group, label = year),
    vjust = 1.5, hjust = -0.2, fontface = "bold", size = 2.2, show.legend = FALSE
  ) +
  geom_text(
    data = filter(world_grp_proj_full, year == FORECAST_END),
    aes(x = mat_gdp, y = gdp_pc, colour = Material_group, label = year),
    vjust = -0.6, hjust = 0.5, fontface = "bold", size = 2.2, show.legend = FALSE
  ) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.2, colour = "grey50") +
  coord_cartesian(clip = "off", expand = FALSE, ylim = c(NA, 100e3)) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/Other/FigContour_D_material_groups.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave(
  "Figures/Other/SVG/FigContour_D_material_groups.svg",
  ggplot2::last_plot(),
  units = 'cm',
  width = 8.7 * 2,
  height = 8.7 * 2
)

## FIGURE Fig3: Region-colored (panel a) + 9 materials (panel b), side by side -

cat("\n── Figure Fig3: combined projection panels ──\n")

library(patchwork)

FUEL_LABEL <- c("coal" = "Coal", "gas" = "Natural Gas", "oil" = "Petroleum")

## 9-material historical (from world_mat computed in Fig C section) ------------

world_9mat_hist <- world_mat |>
  filter(material_category %in% c(BIOMASS_CATS, FOSSIL_CATS, METAL_CATS, NONMET_CATS)) |>
  mutate(
    mat9 = case_when(
      material_category %in% METAL_CATS ~ "Metal ores",
      material_category %in% NONMET_CATS ~ "Non-metallic minerals",
      material_category == "Grazed biomass and fodder crops" ~ "Grazed biomass",
      TRUE ~ material_category
    )
  ) |>
  group_by(year, mat9) |>
  summarise(
    DMC_kg = sum(DMC_kg, na.rm = TRUE),
    GDP = mean(GDP, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop) |>
  filter(year <= HIST_END)

## 9-material projection (SSP2 2024-FORECAST_END) ------------------------------
# Bio/fossil: forecast starts at 2024. Metal/nonmet: 2024 historical bridge + 2025+ forecast.

world_9mat_proj <- bind_rows(
  biomass_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    group_by(mat9 = category, year) |>
    summarise(DMC_kg = sum(M_Mt, na.rm = TRUE) * 1e9, .groups = "drop"),
  fossil_fc |>
    filter(scenario == CENTRAL_SSP, year >= HIST_END, year <= FORECAST_END) |>
    mutate(mat9 = FUEL_LABEL[fuel]) |>
    group_by(mat9, year) |>
    summarise(DMC_kg = sum(M_Mt, na.rm = TRUE) * 1e9, .groups = "drop"),
  world_9mat_hist |> filter(year == HIST_END, mat9 == "Metal ores") |> dplyr::select(mat9, year, DMC_kg),
  metal_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(production_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(mat9 = "Metal ores"),
  world_9mat_hist |> filter(year == HIST_END, mat9 == "Non-metallic minerals") |> dplyr::select(mat9, year, DMC_kg),
  nonmet_flow_fc |>
    filter(scenario == CENTRAL_SSP, year > HIST_END, year <= FORECAST_END) |>
    group_by(year) |>
    summarise(DMC_kg = sum(production_Mt, na.rm = TRUE) * 1e9, .groups = "drop") |>
    mutate(mat9 = "Non-metallic minerals")
) |>
  left_join(world_gdpcap_proj |> dplyr::select(year, world_gdp, gdp_pc), by = "year") |>
  filter(!is.na(world_gdp)) |>
  mutate(mat_gdp = DMC_kg / world_gdp, mat9 = ifelse(mat9 == "Grazed biomass and fodder crops", "Grazed biomass", mat9))

## Palette matching Figure 1 - TimeSeries.R PALETTE_MATERIALS ------------------

pal_9mat <- c(
  "Coal" = "#3E2723",
  "Natural Gas" = "#6D4C41",
  "Petroleum" = "#A1887F",
  "Crops" = "#1B5E20",
  "Crop Residues" = "#388E3C",
  "Wood" = "#558B2F",
  "Grazed biomass" = "#8BC34A",
  "Metal ores" = unname(PALETTE_MATERIAL_GROUPS["Metal ores"]),
  "Non-metallic minerals" = unname(PALETTE_MATERIAL_GROUPS["Non-metallic minerals"])
)

## Shared Y limit and panel-tag helper -----------------------------------------

# Shared Y limits: explicit min across all Fig3 data so both panels use identical Y axis
y_min_fig3 <- min(
  c(region_all$gdp_pc, region_all_proj$gdp_pc, world_9mat_hist$gdp_pc, world_9mat_proj$gdp_pc),
  na.rm = TRUE
)
Y_LIM_FIG3 <- c(y_min_fig3, 100e3)

panel_tag_contour <- function(ltr) {
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = ltr,
    hjust = 1.5,
    vjust = 1,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8 + 1.5,
    colour = "black"
  )
}

## Per-panel X limits with ~5% padding (scale-level expand, coord expand=F kept)
x_lim_a <- range(c(region_all$mat_gdp, region_all_proj$mat_gdp), na.rm = TRUE) * c(0.95, 1.05)
x_lim_b <- range(c(world_9mat_hist$mat_gdp, world_9mat_proj$mat_gdp), na.rm = TRUE) * c(0.95, 1.05)

## Panel a — regions colored ---------------------------------------------------

iso_fig3a <- make_isolines(
  iso_levels_t = c(0.5, 1, 2, 5, 10, 20, 30, 40),
  x_min = min(c(region_all$mat_gdp, region_all_proj$mat_gdp), na.rm = TRUE),
  x_max = max(c(region_all$mat_gdp, region_all_proj$mat_gdp), na.rm = TRUE),
  y_min = min(c(region_all$gdp_pc, region_all_proj$gdp_pc), na.rm = TRUE),
  y_max = 100e3
)

pFig3a <- ggplot(region_all, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_fig3a,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.25,
    size = 4.3,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last"),
    linewidth = 0.6
  ) +
  geom_path(
    data = world_all,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  # SSP2 regional projection (dashed, inherits colour)
  geom_path(
    data = region_all_proj,
    aes(group = Analysis_group),
    linewidth = 0.6,
    linetype = "dashed",
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")
  ) +
  # SSP2 world average projection (dashed)
  geom_path(
    data = world_all_proj,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 1.1,
    linetype = "dashed",
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(world_all, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 2.4, inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 2, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year), shape = 16, size = 2
  ) +
  geom_text_repel(
    data = filter(region_all, year == 2024),
    aes(label = Analysis_group),
    size = 3.7,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.3,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text_repel(
    data = filter(world_all, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 3.9,
    show.legend = FALSE,
    inherit.aes = FALSE,
    segment.size = 0.3,
    segment.colour = "grey40"
  ) +
  # Year labels for world avg: 1970, 2024, FORECAST_END
  geom_text(
    data = filter(world_all, year == 1970),
    aes(label = year), vjust = 1.5, hjust = -0.2,
    fontface = "bold", colour = "black", size = 4.0, show.legend = FALSE
  ) +
  geom_text(
    data = filter(world_all_proj, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = year),
    vjust = 0, hjust = 1.5,
    fontface = "bold", colour = "black", size = 4.0, show.legend = FALSE, inherit.aes = FALSE
  ) +
  geom_text(
    data = filter(world_all_proj, year == FORECAST_END),
    aes(x = mat_gdp, y = gdp_pc, label = year),
    vjust = -0.6, hjust = 0.5,
    fontface = "bold", colour = "black", size = 4.0, show.legend = FALSE, inherit.aes = FALSE
  ) +
  panel_tag_contour("a") +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.2, colour = "grey50") +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_a, ylim = Y_LIM_FIG3) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 10.5))


## Panel b — 9 materials colored -----------------------------------------------

iso_9mat <- make_isolines(
  iso_levels_t = pick_iso_levels(
    c(world_9mat_hist$mat_gdp * world_9mat_hist$gdp_pc, world_9mat_proj$mat_gdp * world_9mat_proj$gdp_pc),
    n_max = 7
  ),
  x_min = min(c(world_9mat_hist$mat_gdp, world_9mat_proj$mat_gdp), na.rm = TRUE),
  x_max = max(c(world_9mat_hist$mat_gdp, world_9mat_proj$mat_gdp), na.rm = TRUE),
  y_min = Y_LIM_FIG3[1],
  y_max = Y_LIM_FIG3[2]
)

# Year labels on Non-metallic minerals line: 1970, 2024, FORECAST_END
lbl_nonmet_1970 <- filter(world_9mat_hist, year == 1970, mat9 == "Non-metallic minerals")
lbl_nonmet_2024 <- filter(world_9mat_proj, year == HIST_END, mat9 == "Non-metallic minerals")
lbl_nonmet_end <- filter(world_9mat_proj, year == FORECAST_END, mat9 == "Non-metallic minerals")

p9mat <- ggplot() +
  geom_textline(
    data = iso_9mat,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = 4.0,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    data = world_9mat_hist,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, group = mat9),
    linewidth = 0.9,
    arrow = arrow(length = unit(0.11, "cm"), type = "closed", ends = "last")
  ) +
  geom_path(
    data = world_9mat_proj,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, group = mat9),
    linewidth = 0.7,
    linetype = "dashed",
    arrow = arrow(length = unit(0.11, "cm"), type = "closed", ends = "last")
  ) +
  geom_point(
    data = filter(world_9mat_hist, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, alpha = year),
    shape = 16, size = 2.0
  ) +
  geom_text_repel(
    data = filter(world_9mat_hist, year == 2024),
    aes(x = mat_gdp, y = gdp_pc, label = mat9, colour = mat9),
    size = 4.0,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.25,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  # Year labels on Non-metallic minerals line
  geom_text(
    data = lbl_nonmet_1970,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, label = year),
    vjust = 1.5, hjust = -0.2, fontface = "bold", size = 4.2, show.legend = FALSE
  ) +
  geom_text(
    data = lbl_nonmet_2024,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, label = year),
    vjust = 0, hjust = 1.5, fontface = "bold", size = 4.2, show.legend = FALSE
  ) +
  geom_text(
    data = lbl_nonmet_end,
    aes(x = mat_gdp, y = gdp_pc, colour = mat9, label = year),
    vjust = -0.6, hjust = 0.5, fontface = "bold", size = 4.2, show.legend = FALSE
  ) +
  panel_tag_contour("b") +
  scale_colour_manual(values = pal_9mat, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = 0.2, colour = "grey50") +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_b, ylim = Y_LIM_FIG3) +
  theme_pb_large() +
  labs(x = "Material Intensity (kg per 2015 USD)", y = "GDP per Capita (2015 USD)") +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 10.5))


## Assemble Fig3
fig3 <- (pFig3a | p9mat) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("Figures/Other/FigContour_F_MaterialIntensityGDP.png", fig3, units = "cm", dpi = 600, width = 8.7 * 4, height = 8.7 * 2)
ggsave("Figures/Other/SVG/FigContour_F_MaterialIntensityGDP.svg", fig3, units = "cm", width = 8.7 * 4, height = 8.7 * 2)
clean_svg("Figures/Other/SVG/FigContour_F_MaterialIntensityGDP.svg")
cat("  Saved: Figures/Other/FigContour_F_MaterialIntensityGDP.png\n")

# fmt: skip
ggsave("Figures/Other/FigContour_E_9mat.png", p9mat, units = "cm", dpi = 600, width = 8.7 * 2.5, height = 8.7 * 2.5)
# fmt: skip
ggsave("Figures/Other/SVG/FigContour_E_9mat.svg", p9mat, units = "cm", width = 8.7 * 2.5, height = 8.7 * 2.5)
clean_svg("Figures/Other/SVG/FigContour_E_9mat.svg")
cat("  Saved: Figures/Other/FigContour_E_9mat.png\n")


## Save figure data ----------------------------------------------------------
dir.create("Results/Data-Figures/", showWarnings = FALSE, recursive = TRUE)

bind_rows(
  region_all |> mutate(type = "historical") |> dplyr::select(year, Analysis_group, type, mat_gdp, gdp_pc),
  region_all_proj |> mutate(type = "SSP2") |> dplyr::select(year, Analysis_group, type, mat_gdp, gdp_pc),
  world_all |>
    mutate(type = "historical", Analysis_group = "World") |>
    dplyr::select(year, Analysis_group, type, mat_gdp, gdp_pc),
  world_all_proj |>
    mutate(type = "SSP2", Analysis_group = "World") |>
    dplyr::select(year, Analysis_group, type, mat_gdp, gdp_pc)
) |>
  write_csv("Results/Data-Figures/fig3a.csv")

bind_rows(
  world_9mat_hist |> mutate(type = "historical") |> dplyr::select(year, mat9, type, mat_gdp, gdp_pc),
  world_9mat_proj |> mutate(type = "SSP2") |> dplyr::select(year, mat9, type, mat_gdp, gdp_pc)
) |>
  write_csv("Results/Data-Figures/fig3b.csv")

cat("  Saved: Results/Data-Figures/fig3a-b.csv\n")

# EoF
