## =============================================================================
## MaterialFlows_TimeSeries.R
## Stacked area charts of domestic extraction (DE) at Level 3 material detail.
## Reads from Parameters/MaterialFlows/; saves figures to Figures/Other/MaterialFlows/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# Data -------------------------------------------------------------------------

df_world <- read_csv("Parameters/MaterialFlows/materialFlows_world_L3_DE.csv", show_col_types = FALSE)
df_region <- read_csv("Parameters/MaterialFlows/materialFlows_region_L3_DE.csv", show_col_types = FALSE)

cat("World rows:", nrow(df_world), "\n")
cat("Year range:", min(df_world$year), "-", max(df_world$year), "\n")
cat("L1 categories:", paste(sort(unique(df_world$material_l1)), collapse = " | "), "\n")
cat("L2 categories:", length(unique(df_world$material_l2)), "\n")
cat("L3 categories:", length(unique(df_world$material_l3)), "\n")

# Share in 2024 by material -------
df_world |> 
  filter(year==2024) |> 
  group_by(material_l2) |> 
  mutate(share_mat=DE_Mt/sum(DE_Mt)) |> 
  dplyr::select(material_l2,material_l3,DE_Mt,share_mat)



# Colour palette for 61 L3 categories, shaded within each L1 family -----------
# Each L1 group gets a hue range; within the group materials are ordered
# large → small and shaded dark → light.

l3_info <- df_world %>%
  group_by(material_l1, material_l2, material_l3) %>%
  summarise(total_Gt = sum(DE_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

L1_RANGES <- list(
  "Biomass" = c("#1A4D0A", "#B8E994"),
  "Fossil fuels" = c("#1C0A00", "#C4A882"),
  "Metal ores" = c("#4A0000", "#F0A0A0"),
  "Non-metallic minerals" = c("#1C2B35", "#CFD8DC")
)

build_palette_l3 <- function(info_df, ranges) {
  pal <- character(0)
  for (l1 in names(ranges)) {
    items <- info_df %>% filter(material_l1 == l1) %>% arrange(desc(total_Gt)) %>% pull(material_l3)
    n <- length(items)
    if (n == 0) {
      next
    }
    shades <- colorRampPalette(ranges[[l1]])(n)
    names(shades) <- items
    pal <- c(pal, shades)
  }
  pal
}

PALETTE_L3 <- build_palette_l3(l3_info, L1_RANGES)


# Shared elements --------------------------------------------------------------

LABEL_YEAR <- 2008

event_lines <- list(
  geom_vline(xintercept = 2008, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", linewidth = 0.2),
  annotate("text", x = 2008, y = Inf, label = "GFC", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40"),
  annotate("text", x = 2020, y = Inf, label = "COVID", vjust = 1.5, hjust = 1.1, size = 2, colour = "grey40")
)


# Figure 1A: World DE by L3, sorted big to small ------------------------------

cat("── Figure 1A ──\n")

world_l3 <- df_world %>%
  group_by(year, material_l1, material_l2, material_l3) %>%
  summarise(DE_Gt = sum(DE_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

# Factor levels: small → large so the largest is on top of the stack
l3_totals_2a <- world_l3 %>%
  group_by(material_l3) %>%
  summarise(total = sum(DE_Gt), .groups = "drop") %>%
  arrange(total)

world_l3 <- world_l3 %>% mutate(material_l3 = factor(material_l3, levels = l3_totals_2a$material_l3))

labels_2a <- world_l3 %>%
  filter(year == LABEL_YEAR) %>%
  arrange(desc(material_l3)) %>%
  mutate(top = cumsum(DE_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  mutate(label = if_else(DE_Gt > 0.1, as.character(material_l3), ""))

ggplot(world_l3, aes(x = year, y = DE_Gt, fill = material_l3)) +
  geom_area(colour = "black", linewidth = 0.03, alpha = 0.9) +
  geom_text(
    data = labels_2a,
    aes(x = 2008, y = mid_y, label = label),
    hjust = 0, size = 1.5, inherit.aes = FALSE
  ) +
  event_lines +
  scale_fill_manual(values = PALETTE_L3, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material (Level 3)", x = "Year", y = "Domestic Extraction (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 100, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Other/MaterialFlows/Fig1A_world_DE_L3_sorted_by_size.png",     last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave(
  "Figures/Other/MaterialFlows/SVG/Fig1A_world_DE_L3_sorted_by_size.svg",
  last_plot(),
  units = "cm",
  width = 8.7 * 2,
  height = 8.7
)


# Figure 1B: World DE by L3, sorted by L1 group then size within group --------

cat("── Figure 1B ──\n")

# L1 groups ordered small → large (so largest L1 block is on top of stack)
l1_totals_2b <- l3_info %>%
  group_by(material_l1) %>%
  summarise(l1_total = sum(total_Gt), .groups = "drop") %>%
  arrange(l1_total)

# Within each L1 group, L3 ordered small → large (largest within group on top)
l3_order_2b <- l3_info %>%
  left_join(l1_totals_2b, by = "material_l1") %>%
  arrange(l1_total, total_Gt) %>%
  pull(material_l3)

world_l3_2b <- world_l3 %>% mutate(material_l3 = factor(material_l3, levels = l3_order_2b))

labels_2b <- world_l3_2b %>%
  filter(year == LABEL_YEAR) %>%
  arrange(desc(material_l3)) %>%
  mutate(top = cumsum(DE_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  mutate(label = if_else(DE_Gt > 0.1, as.character(material_l3), ""))

ggplot(world_l3_2b, aes(x = year, y = DE_Gt, fill = material_l3)) +
  geom_area(colour = "black", linewidth = 0.03, alpha = 0.9) +
  geom_text(
    data = labels_2b,
    aes(x = 2008, y = mid_y, label = label),
    hjust = 0, size = 1.5, inherit.aes = FALSE
  ) +
  event_lines +
  scale_fill_manual(values = PALETTE_L3, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2024, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Material (L3 grouped by L1)", x = "Year", y = "Domestic Extraction (Gt)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 100, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Other/MaterialFlows/Fig1B_world_DE_L3_sorted_by_L1group.png",     last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave(
  "Figures/Other/MaterialFlows/SVG/Fig1B_world_DE_L3_sorted_by_L1group.svg",
  last_plot(),
  units = "cm",
  width = 8.7 * 2,
  height = 8.7
)


# Figure 2C: DE by region, facet by L3, split into one figure per L1 group ----

cat("── Figure 2C ──\n")

region_order <- df_region %>%
  group_by(Region) %>%
  summarise(total = sum(DE_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(total) %>%
  pull(Region)

df_region_plot <- df_region %>% mutate(DE_Gt = DE_Mt / 1e3, Region = factor(Region, levels = region_order))

for (l1 in sort(unique(df_region_plot$material_l1))) {
  cat("  L1:", l1, "\n")

  df_sub <- df_region_plot %>% filter(material_l1 == l1)
  n_l3 <- length(unique(df_sub$material_l3))
  n_col <- min(5, ceiling(sqrt(n_l3)))
  n_row <- ceiling(n_l3 / n_col)
  l1_tag <- gsub("[^A-Za-z0-9]", "_", l1)

  # sort large to small
  mat_levels_l3 <- df_sub |>
    group_by(material_l3) |>
    reframe(DE_Gt = sum(DE_Gt)) |>
    arrange(desc(DE_Gt)) |>
    pull(material_l3)

  df_sub <- df_sub |> mutate(material_l3 = factor(material_l3, levels = mat_levels_l3))

  p <- ggplot(df_sub, aes(x = year, y = DE_Gt, fill = Region)) +
    geom_area(colour = "black", linewidth = 0.03, alpha = 0.9) +
    facet_wrap(
      ~material_l3,
      ncol = n_col,
      scales = "free_y",
      labeller = labeller(material_l3 = function(x) stringr::str_wrap(x, width = 20))
    ) +
    event_lines +
    scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
    scale_x_continuous(breaks = seq(1970, 2020, 20), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_pb_large() +
    labs(title = paste0("DE by Region — ", l1), x = "Year", y = "Domestic Extraction (Gt)") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.nrow = 2)

  fig_w <- n_col * 4.5
  fig_h <- n_row * 4 + 3

  out_base <- paste0("Fig1C_DE_region_facet_L3_", l1_tag)
  # fmt: skip
  ggsave(paste0("Figures/Other/MaterialFlows/",     out_base, ".png"), p, units = "cm", dpi = 600, width = fig_w, height = fig_h)
  ggsave(paste0("Figures/Other/MaterialFlows/SVG/", out_base, ".svg"), p, units = "cm", width = fig_w, height = fig_h)
  clean_svg(paste0("Figures/Other/MaterialFlows/SVG/", out_base, ".svg"))
  cat("  Saved:", out_base, "\n")
}


# Figure 2D: World DE by L3, facet by L2 -------------------------------------

cat("── Figure 2D ──\n")

world_l3_2d <- df_world %>% mutate(DE_Gt = DE_Mt / 1e3, material_l3 = factor(material_l3, levels = l3_order_2b))

labels_2d <- world_l3_2d %>%
  filter(year == LABEL_YEAR) %>%
  group_by(material_l2) %>%
  arrange(desc(material_l3)) %>%
  mutate(top = cumsum(DE_Gt), bot = lag(top, default = 0), mid_y = (top + bot) / 2) %>%
  ungroup() %>%
  mutate(label = if_else(DE_Gt > 0.02, as.character(material_l3), ""))

# to plot single and stacked things
world_l3_2d <- world_l3_2d %>% group_by(material_l2) %>% mutate(n_cat = n_distinct(material_l3)) %>% ungroup()

ggplot(world_l3_2d, aes(x = year, y = DE_Gt, fill = material_l3)) +
  geom_area(data=filter(world_l3_2d,n_cat!=1),colour = "black", linewidth = 0.03, alpha = 0.9,aes(group=material_l3)) +
  geom_area(data=filter(world_l3_2d,n_cat==1),colour = "black", linewidth = 0.03, alpha = 0.9,aes(group=1)) +
  # geom_col(col="black",linewidth=0.05,alpha=0.9,position = "stack") +
  geom_text(
    data = labels_2d,
    aes(x = 2008, y = mid_y, label = label),
    hjust = 0, size = 1.2, inherit.aes = FALSE
  ) +
  facet_wrap(~material_l2, ncol = 4, scales = "free_y") +
  event_lines +
  scale_fill_manual(values = PALETTE_L3, name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2020, 20), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Domestic Extraction (L3) by L2 group", x = "Year", y = "Domestic Extraction (Gt)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 30, b = 5, l = 5, unit = "pt")
  )

# fmt: skip
ggsave("Figures/Other/MaterialFlows/Fig1D_world_DE_L3_facet_L2.png",     last_plot(), units = "cm", dpi = 600, width = 8.7 * 4, height = 8.7 * 4)
ggsave(
  "Figures/Other/MaterialFlows/SVG/Fig1D_world_DE_L3_facet_L2.svg",
  last_plot(),
  units = "cm",
  width = 8.7 * 4,
  height = 8.7 * 4
)


cat("\nAll Figure outputs written to Figures/\n")

# EoF
