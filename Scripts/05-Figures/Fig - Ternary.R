## =============================================================================
## Fig - Ternary.R
## Ternary trajectories: share of material consumption by broad group
## (Biomass / Minerals & Metal Ores / Fossil fuels) for each region, 1970–2024.
## Manual Cartesian projection, standard equilateral triangle:
##   Fossil fuels          → top          (0.5, H)
##   Biomass               → bottom-left  (0,   0)
##   Minerals & Metal Ores → bottom-right (1,   0)
## Point size at decade marks = total Mat/GDP (kg/USD).
## Colour = region (PALETTE_REGIONS); alpha gradient = time (light → dark).
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

H <- sqrt(3) / 2 # equilateral triangle height

## Ternary → Cartesian projection ----------------------------------------------
## Standard upright triangle: x = s + 0.5*e,  y = H * e
tern_to_xy <- function(b, s, e) tibble(x_cart = s + 0.5 * e, y_cart = H * e)

## Load data -------------------------------------------------------------------
df <- read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE) %>% rename(Analysis_group = Region)
df <- df |> filter(abs(DMC_Mt) > 0.1)
df_gdp <- read_csv("Parameters/gdp_region.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_region_historical.csv", show_col_types = FALSE)

dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") %>%
  dplyr::select(Material_22, Material_group)

years_all <- seq(1970, 2024, by = 1)

## Map to 3 ternary categories -------------------------------------------------
# Material_group values match Dict_Materials.xlsx → Categories → Material_group
# (updated to PALETTE_MATERIAL_GROUPS names: "Fossil fuels", "Metal ores",
# "Non-metallic minerals"). Both metals and minerals map to one ternary vertex.
ternary_map <- tibble(
  Material_group = c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals"),
  ternary_group = c("Biomass", "Fossil fuels", "Minerals & Metal Ores", "Minerals & Metal Ores")
)

## Aggregate DMC by region × year × ternary group -----------------------------
region_tern_raw <- df %>%
  left_join(dict_mat, by = c("material_category" = "Material_22")) %>%
  inner_join(ternary_map, by = "Material_group") %>%
  filter(!is.na(Analysis_group), year %in% years_all) %>%
  group_by(year, Analysis_group, ternary_group) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

## LOWESS on raw volumes (smooth first, shares computed after) -----------------
smooth_vol <- function(df, group_cols, frac = 0.18181818) {
  split_df <- split(df, df[group_cols], drop = TRUE)
  out <- lapply(split_df, function(d) {
    d <- arrange(d, year)
    x <- d$year
    y <- d$DMC_Mt
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 5) {
      return(d)
    }
    fit <- tryCatch(loess(y[ok] ~ x[ok], span = frac), error = function(e) NULL)
    if (!is.null(fit)) {
      pred <- tryCatch(predict(fit, newdata = x), error = function(e) rep(NA_real_, length(x)))
      d$DMC_Mt <- pmax(pred, 0)
    }
    d
  })
  do.call(rbind, out)
}

region_tern <- smooth_vol(region_tern_raw, group_cols = c("Analysis_group", "ternary_group"))

## Shares + Cartesian projection -----------------------------------------------
make_wide <- function(df) {
  df %>%
    pivot_wider(names_from = ternary_group, values_from = DMC_Mt) %>%
    mutate(
      total_3grp = Biomass + `Minerals & Metal Ores` + `Fossil fuels`,
      share_biomass = Biomass / total_3grp,
      share_stock = `Minerals & Metal Ores` / total_3grp,
      share_energy = `Fossil fuels` / total_3grp
    ) %>%
    filter(!is.na(share_biomass), !is.na(share_stock), !is.na(share_energy)) %>%
    mutate(x_cart = share_stock + 0.5 * share_energy, y_cart = H * share_energy)
}

region_wide <- make_wide(region_tern) %>% arrange(Analysis_group, year)

## World average ---------------------------------------------------------------
world_tern_raw <- region_tern_raw %>%
  group_by(year, ternary_group) %>%
  summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

world_tern <- smooth_vol(world_tern_raw, group_cols = "ternary_group")
world_wide <- make_wide(world_tern) %>% arrange(year)

## Total Mat/GDP (all materials) for decade-point sizing -----------------------
region_matgdp <- df %>%
  filter(!is.na(Analysis_group), year %in% years_all) %>%
  group_by(year, Analysis_group) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_gdp, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(GDP_2015USD)) %>%
  mutate(mat_gdp = DMC_kg / GDP_2015USD) %>%
  left_join(df_pop, by = c("Analysis_group" = "Region", "year")) %>%
  filter(!is.na(population)) %>%
  mutate(mat_pop = DMC_kg / population / 1e3) %>%
  dplyr::select(year, Analysis_group, mat_gdp, mat_pop)

region_wide <- region_wide %>% left_join(region_matgdp, by = c("year", "Analysis_group"))

world_matgdp <- df %>%
  filter(!is.na(Analysis_group), year %in% years_all) %>%
  group_by(year) %>%
  summarise(DMC_kg = sum(DMC_Mt * 1e9, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    df_gdp %>% group_by(year) %>% summarise(GDP = sum(GDP_2015USD, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) %>%
  filter(!is.na(GDP)) %>%
  mutate(mat_gdp = DMC_kg / GDP) |>
  left_join(
    df_pop %>% group_by(year) %>% summarise(pop = sum(population, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) %>%
  filter(!is.na(pop)) %>%
  mutate(mat_pop = DMC_kg / pop / 1e3)

world_wide <- world_wide %>% left_join(world_matgdp %>% dplyr::select(year, mat_gdp, mat_pop), by = "year")

decade_pts <- region_wide %>% filter(year %in% seq(1970, 2020, by = 10))
decade_pts_world <- world_wide %>% filter(year %in% seq(1970, 2020, by = 10))
end_pts <- region_wide %>% filter(year == 2024)

## Triangle border and grid geometry -------------------------------------------
tri_border <- tibble(x = c(0, 1, 0.5, 0), y = c(0, 0, H, 0))

grid_vals <- seq(0.1, 0.9, by = 0.1)

# Constant-biomass: from (b=v,s=1-v,e=0) → (b=v,s=0,e=1-v)
grid_biomass <- map_dfr(grid_vals, function(v) {
  bind_rows(tern_to_xy(v, 1 - v, 0), tern_to_xy(v, 0, 1 - v)) %>% mutate(grp = paste0("b", v))
})

# Constant-stock: from (b=1-v,s=v,e=0) → (b=0,s=v,e=1-v)
grid_stock <- map_dfr(grid_vals, function(v) {
  bind_rows(tern_to_xy(1 - v, v, 0), tern_to_xy(0, v, 1 - v)) %>% mutate(grp = paste0("s", v))
})

# Constant-energy (horizontal): from (b=1-v,s=0,e=v) → (b=0,s=1-v,e=v)
grid_energy <- map_dfr(grid_vals, function(v) {
  bind_rows(tern_to_xy(1 - v, 0, v), tern_to_xy(0, 1 - v, v)) %>% mutate(grp = paste0("e", v))
})

grid_all <- bind_rows(grid_biomass, grid_stock, grid_energy)

## Tick marks (ternary-style: each edge's ticks parallel to its grid family) ---
# Outward normals (used only for axis arrows below):
en_nx <- -sqrt(3) / 2
en_ny <- 0.5
bi_nx <- 0
bi_ny <- -1
st_nx <- sqrt(3) / 2
st_ny <- 0.5

# Tick directions (parallel to ternary grid lines, outward from triangle):
#   Energy (left edge)  → horizontal left  (-1,  0)
#   Biomass (bot edge)  → down-right 60°   (+0.5, -H)
#   Stock (right edge)  → up-right 60°     (+0.5, +H)
t_en_x <- -1
t_en_y <- 0
t_bi_x <- 0.5
t_bi_y <- -H
t_st_x <- 0.5
t_st_y <- H

tick_len <- 0.022
lbl_gap <- 0.016
tick_vals <- seq(0.1, 0.9, by = 0.1)
lbl_vals <- seq(0.1, 0.9, by = 0.1)

# Energy ticks: left edge (s=0), base = (0.5v, H*v)
energy_ticks <- map_dfr(tick_vals, function(v) {
  tibble(x = 0.5 * v, y = H * v, xend = 0.5 * v + t_en_x * tick_len, yend = H * v + t_en_y * tick_len)
})
energy_lbl <- map_dfr(lbl_vals, function(v) {
  tibble(
    x_lbl = 0.5 * v + t_en_x * (tick_len + lbl_gap),
    y_lbl = H * v + t_en_y * (tick_len + lbl_gap),
    label = paste0(round(v * 100), "%")
  )
})

# Biomass ticks: bottom edge (e=0), base = (1-v, 0)
biomass_ticks <- map_dfr(tick_vals, function(v) {
  tibble(x = 1 - v, y = 0, xend = (1 - v) + t_bi_x * tick_len, yend = t_bi_y * tick_len)
})
biomass_lbl <- map_dfr(lbl_vals, function(v) {
  tibble(
    x_lbl = 1 - v + t_bi_x * (tick_len + lbl_gap),
    y_lbl = t_bi_y * (tick_len + lbl_gap),
    label = paste0(round(v * 100), "%")
  )
})

# Stock ticks: right edge (b=0), base = (0.5+0.5v, H*(1-v))
stock_ticks <- map_dfr(tick_vals, function(v) {
  tibble(
    x = 0.5 + 0.5 * v,
    y = H * (1 - v),
    xend = 0.5 + 0.5 * v + t_st_x * tick_len,
    yend = H * (1 - v) + t_st_y * tick_len
  )
})
stock_lbl <- map_dfr(lbl_vals, function(v) {
  tibble(
    x_lbl = 0.5 + 0.5 * v + t_st_x * (tick_len + lbl_gap),
    y_lbl = H * (1 - v) + t_st_y * (tick_len + lbl_gap),
    label = paste0(round(v * 100), "%")
  )
})

## Axis arrows and labels (outside triangle, grey) ----------------------------
ax_off <- 0.09 # arrow offset from edge
ax_t0 <- 0.12 # arrow start (fraction along edge)
ax_t1 <- 0.88 # arrow end
lbl_xoff <- 0.025 # extra offset for label beyond arrow (tune this to move labels closer/farther from arrow)

axis_arrows <- tibble(
  # Energy: left edge, direction from (0,0) → (0.5,H), increasing energy
  x = c(0.5 * ax_t0 + en_nx * ax_off, 0.5 + 0.5 * ax_t0 + st_nx * ax_off, 1 - ax_t0),
  y = c(H * ax_t0 + en_ny * ax_off, H * (1 - ax_t0) + st_ny * ax_off, bi_ny * ax_off),
  xend = c(0.5 * ax_t1 + en_nx * ax_off, 0.5 + 0.5 * ax_t1 + st_nx * ax_off, 1 - ax_t1),
  yend = c(H * ax_t1 + en_ny * ax_off, H * (1 - ax_t1) + st_ny * ax_off, bi_ny * ax_off)
)

axis_lbl <- tibble(
  x = c(0.25 + en_nx * (ax_off + lbl_xoff), 0.75 + st_nx * (ax_off + lbl_xoff), 0.5),
  y = c(H * 0.5 + en_ny * (ax_off + lbl_xoff), H * 0.5 + st_ny * (ax_off + lbl_xoff), bi_ny * (ax_off + lbl_xoff)),
  label = c("Fossil fuels", "Minerals & Metal Ores", "Biomass"),
  angle = c(60, -60, 0),
  hjust = c(0.5, 0.5, 0.5),
  vjust = c(0.5, 0.5, 0.5)
)

## Corner labels (bold, close to each vertex) ----------------------------------
corner_lbl <- tibble(
  x = c(0, 1, 0.5),
  y = c(0, 0, H),
  label = c("Biomass", "Minerals &\nMetal Ores", "Fossil fuels"),
  hjust = c(1.1, -0.1, 0.5),
  vjust = c(1.4, 1.4, -0.5)
)

## Plot ------------------------------------------------------------------------
cat("\n── Figure Ternary: material share trajectories ──\n")

ggplot() +
  # Triangle border
  geom_path(data = tri_border, aes(x = x, y = y), colour = "black", linewidth = 0.5) +
  # Gridlines every 10%
  geom_line(
    data = grid_all,
    aes(x = x_cart, y = y_cart, group = grp),
    colour = "grey82",
    linewidth = 0.22,
    linetype = "dashed"
  ) +
  # Tick marks (angled, perpendicular to each edge)
  geom_segment(data = energy_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  geom_segment(data = biomass_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  geom_segment(data = stock_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  # % tick labels
  geom_text(data = energy_lbl,  aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=1, vjust=0.5, colour="grey40") +
  geom_text(data = biomass_lbl, aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=0.5, vjust=1, colour="grey40") +
  geom_text(data = stock_lbl,   aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=0, vjust=0, colour="grey40") +
  # Axis arrows with direction
  geom_segment(
    data = axis_arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "grey55",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed", ends = "last")
  ) +
  # Axis name labels (above arrow body)
  geom_text(data = axis_lbl,
            aes(x=x, y=y, label=label, angle=angle, hjust=hjust, vjust=vjust),
            size=3, fontface="plain", colour="grey30") +
  # Corner labels (bold, close to vertices)
  geom_text(data = corner_lbl,
            aes(x=x, y=y, label=label, hjust=hjust, vjust=vjust),
            size=3, fontface="bold", colour="black") +
  # World average — black line with arrow
  # Region trajectories with arrow at 2024
  geom_path(
    data = region_wide,
    aes(x = x_cart, y = y_cart, colour = Analysis_group, group = Analysis_group),
    linewidth = 0.6,
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")
  ) +
  # White under-layer for region decade points
  geom_point(data = decade_pts, aes(x = x_cart, y = y_cart, size = mat_gdp),
           shape = 16, colour = "white", alpha = 1) +
  # Region decade points: size = Mat/GDP; alpha = time gradient (light 1970 → dark 2020)
  geom_point(data = decade_pts,
           aes(x = x_cart, y = y_cart, colour = Analysis_group, alpha = year, size = mat_gdp),
           shape = 16) +
  # Direct region labels at 2024 endpoint
  geom_text(
    data = end_pts,
    aes(x = x_cart, y = y_cart, label = Analysis_group, colour = Analysis_group),
    size = 2.2,
    show.legend = FALSE,
    nudge_x = c( 0.10, -0.25,  0.05, 0.07, 0.00,  0.00,  -0.05,  -0.05)/2,
    nudge_y = c(0.00, 0.2, -0.03, 0.05,  0.11, -0.05, -0.05, -0.05)/2
  ) +
  geom_path(
    data = world_wide,
    aes(x = x_cart, y = y_cart),
    colour = "black",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last")
  ) +
  # World average decade points (white under-layer + alpha gradient + size)
  geom_point(data = decade_pts_world, aes(x = x_cart, y = y_cart, size = mat_gdp),
             shape = 16, colour = "white", alpha = 1) +
  geom_point(data = decade_pts_world, aes(x = x_cart, y = y_cart, alpha = year, size = mat_gdp),
             colour = "black", shape = 16) +
  # World average year labels at 1970 and 2024
  geom_text(data = filter(world_wide, year == 1970),
            aes(x = x_cart, y = y_cart, label = year),
            colour = "black", fontface = "bold", size = 2.5, vjust = -0.5, hjust = 1) +
  geom_text(data = filter(world_wide, year == 2024),
            aes(x = x_cart, y = y_cart, label = year),
            colour = "black", fontface = "bold", size = 2.5, vjust = 0.8, hjust = -0.3) +
  # World avg repel label at 2024 endpoint
  geom_text_repel(
    data = filter(world_wide, year == 2024),
    aes(x = x_cart, y = y_cart, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 2.4,
    nudge_y = -0.03,
    nudge_x = 0.03,
    segment.size = 0.3,
    segment.colour = "grey40"
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.6, 1), guide = "none") +
  scale_size_continuous(name = "Mat/GDP\n(kg/USD)", range = c(0.4, 3), breaks = c(1, 3, 5)) +
  coord_fixed(ratio = 1, xlim = c(-0.22, 1.22), ylim = c(-0.22, H + 0.1), clip = "off") +
  theme_pb_large() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.5, 0.6)
  ) +
  guides(colour = "none", size = guide_legend(order = 1, nrow = 1))

# fmt: skip
ggsave("Figures/FigTernary.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2.2, height = 8.7 * 2)
ggsave("Figures/SVG/FigTernary.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2.2, height = 8.7 * 2)
clean_svg("Figures/SVG/FigTernary.svg")

## Size Mat/cap ----------------
ggplot() +
  # Triangle border
  geom_path(data = tri_border, aes(x = x, y = y), colour = "black", linewidth = 0.5) +
  # Gridlines every 10%
  geom_line(
    data = grid_all,
    aes(x = x_cart, y = y_cart, group = grp),
    colour = "grey82",
    linewidth = 0.22,
    linetype = "dashed"
  ) +
  # Tick marks (angled, perpendicular to each edge)
  geom_segment(data = energy_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  geom_segment(data = biomass_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  geom_segment(data = stock_ticks, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey40", linewidth = 0.35) +
  # % tick labels
  geom_text(data = energy_lbl,  aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=1, vjust=0.5, colour="grey40") +
  geom_text(data = biomass_lbl, aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=0.5, vjust=1, colour="grey40") +
  geom_text(data = stock_lbl,   aes(x=x_lbl, y=y_lbl, label=label),
            size=2.1, hjust=0, vjust=0, colour="grey40") +
  # Axis arrows with direction
  geom_segment(
    data = axis_arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "grey55",
    linewidth = 0.9,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed", ends = "last")
  ) +
  # Axis name labels (above arrow body)
  geom_text(data = axis_lbl,
            aes(x=x, y=y, label=label, angle=angle, hjust=hjust, vjust=vjust),
            size=3, fontface="plain", colour="grey30") +
  # Corner labels (bold, close to vertices)
  geom_text(data = corner_lbl,
            aes(x=x, y=y, label=label, hjust=hjust, vjust=vjust),
            size=3, fontface="bold", colour="black") +
  # World average — black line with arrow
  # Region trajectories with arrow at 2024
  geom_path(
    data = region_wide,
    aes(x = x_cart, y = y_cart, colour = Analysis_group, group = Analysis_group),
    linewidth = 0.6,
    arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")
  ) +
  # White under-layer for region decade points
  geom_point(data = decade_pts, aes(x = x_cart, y = y_cart, size = mat_pop),
           shape = 16, colour = "white", alpha = 1) +
  # Region decade points: size = Mat/GDP; alpha = time gradient (light 1970 → dark 2020)
  geom_point(data = decade_pts,
           aes(x = x_cart, y = y_cart, colour = Analysis_group, alpha = year, size = mat_pop),
           shape = 16) +
  # Direct region labels at 2024 endpoint
  geom_text(
    data = end_pts,
    aes(x = x_cart, y = y_cart, label = Analysis_group, colour = Analysis_group),
    size = 2.2,
    show.legend = FALSE,
    nudge_x = c( 0.10, -0.25,  0.05, 0.07, 0.00,  0.00,  -0.05,  -0.05)/2,
    nudge_y = c(0.00, 0.2, -0.03, 0.05,  0.11, -0.05, -0.05, -0.05)/2
  ) +
  geom_path(
    data = world_wide,
    aes(x = x_cart, y = y_cart),
    colour = "black",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.13, "cm"), type = "closed", ends = "last")
  ) +
  # World average decade points (white under-layer + alpha gradient + size)
  geom_point(data = decade_pts_world, aes(x = x_cart, y = y_cart, size = mat_pop),
             shape = 16, colour = "white", alpha = 1) +
  geom_point(data = decade_pts_world, aes(x = x_cart, y = y_cart, alpha = year, size = mat_pop),
             colour = "black", shape = 16) +
  # World average year labels at 1970 and 2024
  geom_text(data = filter(world_wide, year == 1970),
            aes(x = x_cart, y = y_cart, label = year),
            colour = "black", fontface = "bold", size = 2.5, vjust = -0.5, hjust = 1) +
  geom_text(data = filter(world_wide, year == 2024),
            aes(x = x_cart, y = y_cart, label = year),
            colour = "black", fontface = "bold", size = 2.5, vjust = 0.8, hjust = -0.3) +
  # World avg repel label at 2024 endpoint
  geom_text_repel(
    data = filter(world_wide, year == 2024),
    aes(x = x_cart, y = y_cart, label = "World avg"),
    colour = "black",
    fontface = "bold",
    size = 2.4,
    nudge_y = -0.03,
    nudge_x = 0.03,
    segment.size = 0.3,
    segment.colour = "grey40"
  ) +
  scale_colour_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_alpha_continuous(range = c(0.6, 1), guide = "none") +
  scale_size_continuous(name = "Mat/Pop\n(t/cap)", range = c(0.4, 3), breaks = c(5, 10, 20, 35)) +
  coord_fixed(ratio = 1, xlim = c(-0.22, 1.22), ylim = c(-0.22, H + 0.1), clip = "off") +
  theme_pb_large() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.5, 0.6)
  ) +
  guides(colour = "none", size = guide_legend(order = 1, nrow = 1))

# fmt: skip
ggsave("Figures/FigTernary_pop.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2.2, height = 8.7 * 2)
ggsave("Figures/SVG/FigTernary_pop.svg", ggplot2::last_plot(), units = 'cm', width = 8.7 * 2.2, height = 8.7 * 2)
clean_svg("Figures/SVG/FigTernary_pop.svg")

# EoF
