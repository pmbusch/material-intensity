## =============================================================================
## Figure 1 - HistoricalTimeseries.R  -> Figures/Fig1 - Historical Timeseries.png
## 2x2 figure: DMC by region (a) and material (b) as stacked areas, plus
## material-intensity phase-space contours by region (c) and by
## region-group x material-group (d).
## Reads from Parameters/; saves the figure to Figures/ and per-panel data to
## Figures/RawData/.
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Materials (DMC) — already aggregated to Region by 01-Aggregate_UNEP.R
# Columns: Region, year, material_category, DMC_Mt
df <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE)
df <- df |> filter(abs(DMC_Mt) > 0.1)
cat("Rows:", nrow(df), "\n")

TXT_AB <- pb_annot_size("small", 7.2) # in-area labels, panels a & b (>= tag size / 1.6)
TXT_GROUP_B <- pb_annot_size("largeFont", 8) # material-group labels right of panel b

## Colour palettes — defined in 00-CommonParameters.R -------------------------
# PALETTE_REGIONS          : 8 regions
# PALETTE_MATERIALS        : 21 material categories
# PALETTE_MATERIAL_GROUPS  : 6 material groups

# Panel b: Global DMC by material category -------------------------------------

global_mat <- df %>%
  filter(DMC_Mt > 0) |>
  filter(!str_detect(material_category, "Waste for ")) |>
  mutate(
    material_group = case_when(
      material_category %in%
        c(
          "Coal",
          "Natural Gas",
          "Petroleum",
          "Oil shale and tar sands",
          "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel",
          "Other products mainly from fossil fuels e.g. plastics"
        ) ~ "Fossil fuels",
      material_category %in%
        c(
          "Crops",
          "Crop Residues",
          "Wood",
          "Grazed biomass and fodder crops",
          "Non-wild animal products",
          "Wild catch and harvest",
          "Products mainly from biomass nec.",
          "Mixed / complex products nec."
        ) ~ "Biomass",
      material_category %in% c("Ferrous ores", "Non-ferrous ores", "Products mainly from metals nec.") ~ "Metal ores",
      material_category %in%
        c(
          "Non-metallic minerals - construction dominant",
          "Non-metallic minerals - industrial or agricultural dominant",
          "Products mainly from non-metallic minerals"
        ) ~ "Non-metallic minerals",

      TRUE ~ NA_character_ # flags anything unclassified
    )
  ) |>
  mutate(
    material_category_plot = case_when(
      material_category == "Coal" ~ "Coal",
      material_category == "Natural Gas" ~ "Natural Gas",
      material_category == "Petroleum" ~ "Petroleum",
      material_category == "Crops" ~ "Crops",
      material_category == "Crop Residues" ~ "Crop Residues",
      material_category == "Wood" ~ "Wood",
      material_category == "Grazed biomass and fodder crops" ~ "Grazed biomass",
      material_category == "Ferrous ores" ~ "Ferrous ores",
      material_category == "Non-ferrous ores" ~ "Non-ferrous ores",
      material_category == "Non-metallic minerals - construction dominant" ~ "Construction minerals",
      material_category == "Non-metallic minerals - industrial or agricultural dominant" ~ "Industrial minerals",
      material_group == "Fossil fuels" ~ "Other - Fossil fuels",
      material_group == "Biomass" ~ "Other - Biomass",
      material_group == "Metal ores" ~ "Other - Metal ores",
      material_group == "Non-metallic minerals" ~ "Other - Non-metallic minerals",
      TRUE ~ "Other"
    )
  ) |>
  group_by(year, material_group, material_category_plot) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(
    material_group = factor(
      material_group,
      levels = rev(c("Non-metallic minerals", "Biomass", "Fossil fuels", "Metal ores"))
    )
  )

PALETTE_MATERIALS <- c(
  # Fossil fuels — brown family
  "Coal" = "#3E2723",
  "Natural Gas" = "#6D4C41",
  "Petroleum" = "#A1887F",
  "Other - Fossil fuels" = "#D7CCC8",
  # Biomass — green family
  "Crops" = "#1B5E20",
  "Crop Residues" = "#388E3C",
  "Wood" = "#558B2F",
  "Grazed biomass" = "#8BC34A",
  "Other - Biomass" = "#DCEDC8",
  # Metal ores — red family
  "Ferrous ores" = "#B71C1C",
  "Non-ferrous ores" = "#E57373",
  "Other - Metal ores" = "#FFCDD2",
  # Non-metallic minerals — grey-blue family
  "Construction minerals" = "#455A64",
  "Industrial minerals" = "#90A4AE",
  "Other - Non-metallic minerals" = "#CFD8DC"
)

mat_totals_1a <- global_mat %>%
  group_by(material_group, material_category_plot) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(material_group, total)

global_mat <- global_mat %>%
  mutate(material_category_plot = factor(material_category_plot, levels = mat_totals_1a$material_category_plot))

# Each label sits at its own year (staggered so thin neighbouring bands don't
# stack their labels), rotated to the band's local slope: band mid-line slope
# over +-3 yr, scaled by the square panel's x/y ranges (54 yr / y max)
LABEL_YEAR_1B <- c(
  "Ferrous ores" = 1993, "Non-ferrous ores" = 1979, "Natural Gas" = 2005, "Petroleum" = 1994,
  "Coal" = 2013, "Wood" = 1977, "Crop Residues" = 1989, "Grazed biomass" = 1999, "Crops" = 2008,
  "Industrial minerals" = 2009, "Construction minerals" = 1988
)
stack_1a <- global_mat %>%
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup()
y_max_1b <- max(stack_1a$top)
labels_1a <- stack_1a %>%
  mutate(label_year = coalesce(unname(LABEL_YEAR_1B[as.character(material_category_plot)]), 1995)) %>%
  group_by(material_category_plot) %>%
  mutate(slope = (mid_y[year == first(label_year) + 3] - mid_y[year == first(label_year) - 3]) / 6) %>%
  ungroup() %>%
  filter(year == label_year) %>%
  mutate(
    angle = atan(slope * (2024 - 1970) / y_max_1b) * 180 / pi,
    material_category_label = if_else(DMC_Gt > 0.5, material_category_plot, NA_character_)
  )

group_boundaries <- global_mat %>%
  arrange(year, desc(material_category_plot)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt)) %>%
  group_by(year, material_group) %>%
  summarise(boundary_y = max(top), .groups = "drop") %>%
  filter(material_group != last(levels(global_mat$material_category_plot))) # drop top

group_labels <- group_boundaries %>%
  arrange(year, desc(material_group)) %>%
  filter(year == max(year)) %>%
  mutate(bot = lag(boundary_y, default = 0), mid_y = (boundary_y + bot) / 2) |>
  mutate(
    material_group_label = case_when(
      material_group == "Fossil fuels" ~ "Fossil\nfuels",
      material_group == "Metal ores" ~ "Metal\nores",
      TRUE ~ material_group
    )
  )

p_material <- ggplot(global_mat, aes(x = year, y = DMC_Gt, fill = material_category_plot)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_text(
    data = labels_1a |>
      filter(
        !material_category_plot %in%
          c("Coal", "Crops", "Industrial minerals", "Construction minerals", "Ferrous ores", "Other - Fossil fuels")
      ),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = "black",
    hjust = 0.5,
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |>
      filter(material_category_plot %in% c("Coal", "Crops")),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = "white",
    hjust = 0.5,
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Construction minerals"),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = "white",
    angle = 0,
    hjust = 0.5,
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Industrial minerals"),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = PALETTE_MATERIALS["Industrial minerals"],
    hjust = 0.5,
    size = TXT_AB,
    nudge_y = -4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Ferrous ores"),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = PALETTE_MATERIALS["Ferrous ores"],
    hjust = 0.5,
    size = TXT_AB,
    nudge_y = 4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1a |> filter(material_category_plot == "Other - Fossil fuels"),
    aes(x = year, y = mid_y, label = material_category_label, angle = angle),
    colour = PALETTE_MATERIALS["Other - Fossil fuels"],
    hjust = 0.5,
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = 2015,
    y = 90,
    label = "Other - Fossil fuels",
    colour = PALETTE_MATERIALS["Other - Fossil fuels"],
    angle = 30,
    hjust = 0.5,
    size = TXT_AB
  ) +
  geom_line(
    data = group_boundaries,
    aes(x = year, y = boundary_y, group = material_group),
    colour = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
  data = group_labels,
  aes(x = 2025, y = mid_y, label = material_group_label,color=material_group),
  inherit.aes = FALSE, lineheight = 0.8,
  fontface = "bold", size = TXT_GROUP_B,
  angle = 90, hjust = 0.5,
  clip = "off",vjust=0.9
) +
  # fmt: skip
  annotate("text",x = -Inf, y = Inf,label = "b",hjust = -1, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_MATERIALS, name = NULL) +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off", xlim = c(1970, 2024)) +
  theme_pb_large() +
  labs(title = "Material", x = "Year", y = "Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    aspect.ratio = 1,
    plot.margin = margin(t = 2, r = 22, b = 0, l = 2, unit = "pt") # r: material-group labels
  )
p_material


# Panel a: Global DMC by region -------------------------------------------------

# Data is already at region level — just rename for plot aesthetics
global_grp <- df %>%
  filter(DMC_Mt > 0) |>
  filter(!str_detect(material_category, "Waste for ")) |>
  rename(analysis_group = Region) %>%
  filter(!is.na(analysis_group)) %>%
  group_by(year, analysis_group) %>%
  summarise(DMC_Gt = sum(DMC_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

grp_totals_1b <- global_grp %>%
  group_by(analysis_group) %>%
  summarise(total = sum(DMC_Gt), .groups = "drop") %>%
  arrange(total)

global_grp <- global_grp %>% mutate(analysis_group = factor(analysis_group, levels = grp_totals_1b$analysis_group))

# Staggered label years + local-slope angles, as in panel b (Oceania and Middle
# East & North Africa sit just above the stack, nudged up)
LABEL_YEAR_1A <- c(
  "Middle East & North Africa" = 1983, "Oceania" = 2003, "Sub-Saharan Africa" = 1995,
  "South Asia" = 2010, "Latin America" = 1985, "North America" = 1994, "Europe & Russia" = 1990
)
stack_1b <- global_grp %>%
  arrange(year, desc(analysis_group)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup()
y_max_1a <- max(stack_1b$top)
labels_1b <- stack_1b %>%
  mutate(label_year = coalesce(unname(LABEL_YEAR_1A[as.character(analysis_group)]), 1995)) %>%
  group_by(analysis_group) %>%
  mutate(slope = (mid_y[year == first(label_year) + 3] - mid_y[year == first(label_year) - 3]) / 6) %>%
  ungroup() %>%
  filter(year == label_year) %>%
  mutate(
    angle = atan(slope * (2024 - 1970) / y_max_1a) * 180 / pi,
    label = if_else(DMC_Gt > 0.5, analysis_group, NA_character_),
    # Middle East & North Africa: horizontal, in the white space above the stack (+ nudge_y)
    angle = if_else(analysis_group == "Middle East & North Africa", 0, angle),
    mid_y = if_else(analysis_group == "Middle East & North Africa", 51, mid_y)
  )

# Thicker black boundary between each region (regions have no further sub-grouping,
# so every region gets the separator treatment analogous to the material-group
# boundaries in panel b)
region_boundaries <- global_grp %>%
  arrange(year, desc(analysis_group)) %>%
  group_by(year) %>%
  mutate(top = cumsum(DMC_Gt)) %>%
  group_by(year, analysis_group) %>%
  summarise(boundary_y = max(top), .groups = "drop")

p_region <- ggplot(global_grp, aes(x = year, y = DMC_Gt, fill = analysis_group)) +
  geom_area(colour = "black", linewidth = 0.05, alpha = 0.9) +
  geom_line(
    data = region_boundaries,
    aes(x = year, y = boundary_y, group = analysis_group),
    colour = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(!analysis_group %in% c("Oceania", "North America", "East Asia", "Middle East & North Africa")),
    aes(x = year, y = mid_y, label = label, angle = angle),
    colour = "black",
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "East Asia"),
    aes(x = year, y = mid_y, label = label, angle = angle),
    colour = "black",
    angle = 0,
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "North America"),
    aes(x = year, y = mid_y, label = label, angle = angle),
    colour = "white",
    size = TXT_AB,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "Oceania"),
    aes(x = year, y = mid_y, label = label, angle = angle),
    colour = PALETTE_REGIONS["Oceania"],
    size = TXT_AB,
    nudge_y = 3.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = labels_1b |> filter(analysis_group == "Middle East & North Africa"),
    aes(x = year, y = mid_y, label = label, angle = angle),
    colour = PALETTE_REGIONS["Middle East & North Africa"],
    size = TXT_AB,
    nudge_y = 3.5,
    inherit.aes = FALSE
  ) +
  # fmt: skip
  annotate("text",x = -Inf, y = Inf,label = "a",hjust = -1, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_colour_manual(values = PALETTE_REGIONS, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Region", x = "Year", y = "Material Consumption (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    aspect.ratio = 1,
    plot.margin = margin(t = 2, r = 4, b = 0, l = 2, unit = "pt")
  )
p_region


# Panels c & d: phase-space contours (Mat/GDP vs GDP/capita), 1970-2024 ------
# Historical only (no SSP projection). Column alignment: c (regions) sits
# under a (regions), d (region groups x material groups) sits under b (material).

HIST_END <- 2024L

df_gdp <- read_csv("Parameters/Worldbank-GDP/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/UN-Population/population_region_historical.csv", show_col_types = FALSE)

## Helper: build iso-line dataframe -------------------------------------------
# Mat/cap = Mat/GDP × GDP/cap  →  y = iso_level_kg / x  (hyperbola)
make_isolines <- function(iso_levels_t, x_min, x_max, y_min = -Inf, y_max = Inf, n = 200) {
  x_min <- max(x_min, 1e-10, na.rm = TRUE)
  x_max <- max(x_max, x_min * 2, na.rm = TRUE)
  x_seq <- 10^seq(log10(x_min), log10(x_max), length.out = n)
  expand.grid(iso_t = iso_levels_t, x = x_seq) %>%
    mutate(y = (iso_t * 1e3) / x, iso_label = paste0(iso_t, " t/cap")) %>%
    filter(y >= y_min, y <= y_max)
}

## Helper: auto-pick iso-line levels that bracket a mat/cap range --------------
NICE_LEVELS_T <- sort(c(outer(c(1, 2, 3, 4, 5), 10^(-3:3))))

pick_iso_levels <- function(mat_pc_vals_kg, n_max = 5) {
  min_t <- min(mat_pc_vals_kg, na.rm = TRUE) / 1000
  max_t <- max(mat_pc_vals_kg, na.rm = TRUE) / 1000
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

## LOWESS smoothing: avoid annual noise while preserving endpoints ------------
smooth_lowess <- function(df, group_cols = NULL, frac = 0.18181818, year_col = "year") {
  split_df <- if (is.null(group_cols)) list(df) else split(df, df[group_cols], drop = TRUE)
  out <- lapply(split_df, function(d) {
    x <- d[[year_col]]
    for (col in c("mat_gdp", "gdp_pc")) {
      y <- d[[col]]
      d[[paste0(col, "_raw")]] <- y
      ok <- is.finite(x) & is.finite(y)
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

## Panel c data: regions, all materials summed --------------------------------

region_all <- df %>%
  rename(Analysis_group = Region) %>%
  filter(!is.na(Analysis_group)) %>%
  group_by(year, Analysis_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_gdp, by = c("Analysis_group" = "Region", "year")) %>%
  left_join(df_pop, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(GDP_PPP2017USD), !is.na(population)) %>%
  mutate(GDP = GDP_PPP2017USD, pop = population, mat_gdp = DMC_kg / GDP_PPP2017USD, gdp_pc = GDP_PPP2017USD / population) %>%
  filter(year >= 1970, year <= HIST_END, !is.na(mat_gdp), !is.na(gdp_pc)) %>%
  arrange(Analysis_group, year)

region_all <- smooth_lowess(region_all, group_cols = c("Analysis_group"))

region_econ <- region_all %>% dplyr::select(year, Analysis_group, GDP, pop)

world_all <- region_all %>%
  group_by(year) %>%
  summarise(DMC_kg = sum(DMC_kg), GDP = sum(GDP), pop = sum(pop), .groups = "drop") %>%
  mutate(mat_gdp = DMC_kg / GDP, gdp_pc = GDP / pop)
world_all <- smooth_lowess(world_all)

## Panel d data: 3 region groups x 4 material groups ---------------------------
# Computed here (ahead of both panels' plots) so panels c & d can share one
# common Y range (GDP per capita) below.

BIOMASS_CATS <- c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wood")
FOSSIL_CATS <- c("Coal", "Natural Gas", "Petroleum")
METAL_CATS <- c("Ferrous ores", "Non-ferrous ores")
NONMET_CATS <- c(
  "Non-metallic minerals - construction dominant",
  "Non-metallic minerals - industrial or agricultural dominant"
)

# 2 region groups (Europe/N.America/Oceania vs rest of the world) — colour in
# panel d is carried by material (see below), region group is carried by linewidth.
REGION_GROUP3 <- c(
  "Europe & Russia" = "Advanced Economies",
  "North America" = "Advanced Economies",
  "Oceania" = "Advanced Economies",
  "East Asia" = "Rest of World",
  "South Asia" = "Rest of World",
  "Latin America" = "Rest of World",
  "Sub-Saharan Africa" = "Rest of World",
  "Middle East & North Africa" = "Rest of World"
)
REGION_GROUP3_LEVELS <- c("Advanced Economies", "Rest of World")
REGION_GROUP3_LINEWIDTHS <- c("Advanced Economies" = 1.1, "Rest of World" = 0.4)

group_econ3 <- region_econ %>%
  mutate(region_group3 = REGION_GROUP3[Analysis_group]) %>%
  group_by(year, region_group3) %>%
  summarise(GDP = sum(GDP), pop = sum(pop), .groups = "drop")

world_4mat_hist <- df %>%
  filter(material_category %in% c(BIOMASS_CATS, FOSSIL_CATS, METAL_CATS, NONMET_CATS)) %>%
  mutate(
    mat4 = case_when(
      material_category %in% BIOMASS_CATS ~ "Biomass",
      material_category %in% FOSSIL_CATS ~ "Fossil fuels",
      material_category %in% METAL_CATS ~ "Metal ores",
      material_category %in% NONMET_CATS ~ "Non-metallic minerals"
    ),
    region_group3 = REGION_GROUP3[Region]
  ) %>%
  group_by(year, region_group3, mat4) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(group_econ3, by = c("year", "region_group3")) %>%
  filter(!is.na(GDP), year >= 1970, year <= HIST_END) %>%
  mutate(
    mat_gdp = DMC_kg / GDP,
    gdp_pc = GDP / pop,
    region_group3 = factor(region_group3, levels = REGION_GROUP3_LEVELS)
  )

world_4mat_hist <- smooth_lowess(world_4mat_hist, group_cols = c("region_group3", "mat4"))
# Colour comes directly from PALETTE_MATERIAL_GROUPS (keys already match mat4)

## Shared axis bounds for panels c & d -----------------------------------------
# Panel c/d windows and the shared Y range are fixed per the project's figure
# design (rather than data-driven), so the two panels line up exactly.
x_lim_c <- c(0, 2.5)
x_lim_d <- c(0, 1)
TXT_CD <- pb_annot_size("largeFont", 8) # in-plot text, panels c & d
LOGTICK_LW <- 0.4 # y-axis log tick marks, panels c & d
LOGTICK_COL <- "grey20"
y_lim_cd <- c(600, 1e5)

# Iso-lines computed across the FULL displayed panel (not just the tight data
# range) so the dashed hyperbolas span edge-to-edge instead of stopping short.
iso_c <- make_isolines(
  iso_levels_t = c(1, 5, 10, 20, 30, 40),
  x_min = x_lim_c[1],
  x_max = x_lim_c[2],
  y_min = y_lim_cd[1],
  y_max = y_lim_cd[2]
)

iso_d <- make_isolines(
  # Drop levels that clutter panel d without adding readable separation
  # signif(): NICE_LEVELS_T holds e.g. 3 * 0.1 = 0.30000000000000004, which setdiff() would not match
  iso_levels_t = setdiff(
    signif(pick_iso_levels(world_4mat_hist$mat_gdp * world_4mat_hist$gdp_pc, n_max = 12), 6),
    c(0.2, 0.3, 0.4, 3, 4)
  ),
  x_min = x_lim_d[1],
  x_max = x_lim_d[2],
  y_min = y_lim_cd[1],
  y_max = y_lim_cd[2]
)

pFig1c <- ggplot(region_all, aes(x = mat_gdp, y = gdp_pc, colour = Analysis_group)) +
  geom_textline(
    data = iso_c,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.22,
    size = TXT_CD,
    hjust = 0.93, # right end: clear of the region labels
    inherit.aes = FALSE
  ) +
  geom_path(
    aes(group = Analysis_group),
    arrow = arrow(length = unit(0.16, "cm"), type = "closed", ends = "last"),
    linewidth = 0.45
  ) +
  geom_path(
    data = world_all,
    aes(x = mat_gdp, y = gdp_pc, group = 1),
    colour = "black",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "last"),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(world_all, year %in% seq(1970, 2010, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, alpha = year),
    colour = "black", shape = 16, size = 1.9, inherit.aes = FALSE
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    shape = 16, size = 1.6, colour = "white", alpha = 1
  ) +
  geom_point(
    data = filter(region_all, year %in% seq(1970, 2020, by = 10)),
    aes(alpha = year), shape = 16, size = 1.4
  ) +
  # Region and world labels repelled together, so they also avoid each other
  geom_text_repel(
    data = bind_rows(
      filter(region_all, year == HIST_END) |> mutate(label = Analysis_group, face = "plain"),
      filter(world_all, year == HIST_END) |> mutate(Analysis_group = "World avg", label = "World avg, 2024", face = "bold")
    ),
    aes(label = label, fontface = face),
    size = TXT_CD,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    box.padding = 0.35,
    xlim = c(NA, 1.85), # keep labels left of the iso-line labels
    force = 3,
    segment.size = 0.25,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text(
    data = filter(world_all, year == 1970),
    aes(label = year), vjust = 1.5, hjust = -0.2,
    fontface = "bold", colour = "black", size = TXT_CD, show.legend = FALSE
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "c",hjust = 1.5, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_colour_manual(values = c(PALETTE_REGIONS, "World avg" = "black"), name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  # ticks every 0.25, labels every 0.5
  scale_x_continuous(breaks = seq(0, 2.5, 0.25), labels = function(x) ifelse(abs(x / 0.5 - round(x / 0.5)) < 1e-9, sprintf("%g", x), "")) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = LOGTICK_LW, colour = LOGTICK_COL) +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_c, ylim = y_lim_cd) +
  theme_pb_large() +
  labs(title = " ", x = "Material Intensity (kg/$)", y = "GDP per Capita  ($/p)") +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    plot.margin = margin(t = -12, r = 4, b = 2, l = 2, unit = "pt") # t < 0: closer to panels a/b
  )
pFig1c

## Panel d plot: region groups x material groups --------------------------------
# (data computed above, alongside panel c, so the shared Y range could be built)

# Year labels anchored on the Non-metallic minerals
# (solid, least cluttered)
lbl_anchor_1970 <- filter(world_4mat_hist, year == 1970, mat4 == "Non-metallic minerals", region_group3 == "Rest of World")
lbl_anchor_end <- filter(world_4mat_hist, year == HIST_END, mat4 == "Non-metallic minerals", region_group3 == "Rest of World")

pFig1d <- ggplot() +
  geom_textline(
    data = iso_d,
    aes(x = x, y = y, group = iso_label, label = iso_label),
    colour = "grey60",
    linetype = "dashed",
    linewidth = 0.2,
    size = TXT_CD,
    hjust = 0.82,
    inherit.aes = FALSE
  ) +
  geom_path(
    data = world_4mat_hist,
    aes(x = mat_gdp, y = gdp_pc, colour = mat4, linewidth = region_group3, group = interaction(mat4, region_group3)),
    arrow = arrow(length = unit(0.16, "cm"), type = "closed", ends = "last")
  ) +
  geom_point(
    data = filter(world_4mat_hist, year %in% seq(1970, 2020, by = 10)),
    aes(x = mat_gdp, y = gdp_pc, colour = mat4, alpha = year, group = interaction(mat4, region_group3)),
    shape = 16, size = 1.4
  ) +
  # Material labelled once per colour, on the solid (Advanced Economies) line
  geom_text_repel(
    data = filter(world_4mat_hist, year == HIST_END, region_group3 == "Advanced Economies"),
    aes(x = mat_gdp, y = gdp_pc, label = mat4, colour = mat4),
    size = TXT_CD,
    show.legend = FALSE,
    max.overlaps = Inf,
    seed = 42,
    segment.size = 0.2,
    segment.colour = "grey60",
    min.segment.length = 0.2
  ) +
  geom_text(
    data = lbl_anchor_1970,
    aes(x = mat_gdp, y = gdp_pc, colour = mat4, label = year),
    vjust = 1.5, hjust = -0.2, fontface = "bold", size = TXT_CD, show.legend = FALSE
  ) +
  geom_text(
    data = lbl_anchor_end,
    aes(x = mat_gdp, y = gdp_pc, colour = mat4, label = year),
    vjust = 0, hjust = 1.5, fontface = "bold", size = TXT_CD, show.legend = FALSE
  ) +
  # Region groups labelled directly beside their (thick / thin) line bundles
  annotate("text", x = 0.40, y = 20e3, label = "Advanced economies", hjust = 0, fontface = "bold", size = TXT_CD) +
  annotate("text", x = 0.16, y = 2.2e3, label = "Rest of world", hjust = 0, size = TXT_CD) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "d",hjust = 1.5, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_colour_manual(values = PALETTE_MATERIAL_GROUPS, name = NULL, guide = "none") +
  scale_linewidth_manual(values = REGION_GROUP3_LINEWIDTHS, name = NULL) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  # ticks every 0.1, labels every 0.2 (one decimal)
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    labels = function(x) ifelse(abs(x / 0.2 - round(x / 0.2)) < 1e-9, sprintf("%g", round(x, 1)), "")
  ) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  annotation_logticks(sides = "l", linewidth = LOGTICK_LW, colour = LOGTICK_COL) +
  coord_cartesian(clip = "off", expand = FALSE, xlim = x_lim_d, ylim = y_lim_cd) +
  theme_pb_large() +
  labs(title = " ", x = "Material Intensity (kg/$)", y = "GDP per Capita ($/p)") +
  theme(
    legend.position = "none",
    legend.justification = c(0, 0),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "lines"),
    legend.background = element_rect(fill = alpha("white", 0.75), colour = NA),
    aspect.ratio = 1,
    plot.margin = margin(t = -12, r = 4, b = 2, l = 2, unit = "pt") # t < 0: closer to panels a/b
  )
pFig1d


# Merge: final 2x2 figure (a=regions, b=material, c=regions contour, d=material contour)

# patchwork aligns the four (square) boxes exactly; free(type = "label") lets
# a/b's y-axis titles sit next to their own short tick labels instead of being
# pushed out to the width of c/d's "$100,000" labels.
fig1 <- patchwork::wrap_plots(
  patchwork::free(p_region, type = "label", side = "l"),
  patchwork::free(p_material, type = "label", side = "l"),
  pFig1c, pFig1d,
  ncol = 2
)
ggsave("Figures/Fig1 - Historical Timeseries.png", fig1, units = 'cm', dpi = 600, width = 17.4, height = 16.4)
ggsave("Figures/SVG/Fig1 - Historical Timeseries.svg", fig1, units = 'cm', width = 17.4, height = 16.4)
group_svg_layers("Figures/SVG/Fig1 - Historical Timeseries.svg")


## Save figure data, one file per panel ----------------------------------------

dir.create("Figures/RawData/", showWarnings = FALSE, recursive = TRUE)

write_csv(global_grp |> dplyr::select(year, region = analysis_group, DMC_Gt), "Figures/RawData/fig1a.csv")
write_csv(
  global_mat |> dplyr::select(year, material_group, material = material_category_plot, DMC_Gt),
  "Figures/RawData/fig1b.csv"
)
write_csv(
  region_all |> dplyr::select(year, region = Analysis_group, GDP, pop, mat_gdp, gdp_pc),
  "Figures/RawData/fig1c.csv"
)
write_csv(
  world_4mat_hist |>
    dplyr::select(year, region_group = region_group3, material_group = mat4, GDP, pop, mat_gdp, gdp_pc),
  "Figures/RawData/fig1d.csv"
)
cat("  Saved: Figures/RawData/fig1a.csv, fig1b.csv, fig1c.csv, fig1d.csv\n")

# EoF
