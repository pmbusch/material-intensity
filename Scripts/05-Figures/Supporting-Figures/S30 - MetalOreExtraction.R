## S30 - MetalOreExtraction.R (supporting figure S30)
## Metal ore domestic extraction (gross ore) by detail material (copper ores,
## gold ores, ...), 1970-latest year, world and by region. Stacked areas sorted
## from largest (bottom) to smallest (top), plus a stacked bar at the end with
## each material's share in the latest year.
##
## Input:   Parameters/MaterialFlows/DE_detail_region.csv (06-Aggregate_MaterialFlows.R)
## Outputs: Figures/Supporting-Figures/S30_MetalOres_DE.png (+ SVG): world panel on
##          top, regional panels below (shared y axis across regions)
##          Figures/Supporting-Figures/RawData/S30_MetalOres_DE.csv

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
pb_set_geom_defaults("small")
library(patchwork)

LABEL_MIN_SHARE <- 0.02 # label bar segments with at least this share of the latest year (world)
LABEL_MIN_SHARE_REGION <- 0.03 # same, regional panels (less vertical room)
BAR_GAP <- 5 # years between last data year and the centre of the end bar
BAR_W <- 4 # end bar width (years)
LABEL_SPACE <- 28 # extra x range (years) right of the bar for direct labels


# Data --------------------------------------------------------------------

de <- read_csv("Parameters/MaterialFlows/DE_detail_region.csv", show_col_types = FALSE) %>%
  filter(material_parent %in% c("Ferrous ores", "Non-ferrous ores")) %>%
  mutate(metal = coalesce(unname(LABEL_METAL_ORES[material_detail]), material_detail))

cat("Detail labels without short name:", paste(setdiff(unique(de$material_detail), names(LABEL_METAL_ORES)), collapse = ", "), "\n")

LAST_YEAR <- max(de$year)
BAR_X <- LAST_YEAR + BAR_GAP

de_plot <- bind_rows(
  de %>% group_by(Region = "World", metal, year) %>% summarise(DE_Mt = sum(DE_Mt), .groups = "drop"),
  de %>% group_by(Region, metal, year) %>% summarise(DE_Mt = sum(DE_Mt), .groups = "drop")
) %>%
  complete(Region, metal, year, fill = list(DE_Mt = 0)) %>%
  mutate(DE_Gt = DE_Mt / 1e3)

# Stack order by global extraction in the latest year: largest at the bottom
# (ggplot stacks the first factor level on top, so levels go smallest -> largest)
order_metal <- de_plot %>% filter(Region == "World", year == LAST_YEAR) %>% arrange(DE_Gt) %>% pull(metal)
order_region <- de_plot %>%
  filter(Region != "World", year == LAST_YEAR) %>%
  group_by(Region) %>%
  summarise(DE_Gt = sum(DE_Gt)) %>%
  arrange(desc(DE_Gt)) %>%
  pull(Region)

de_plot <- de_plot %>% mutate(metal = factor(metal, levels = order_metal), Region = factor(Region, levels = c("World", order_region)))


# End bar and labels ------------------------------------------------------

# Cumulate from the bottom of the stack (last factor level) to get segment midpoints
de_bar <- de_plot %>%
  filter(year == LAST_YEAR) %>%
  arrange(Region, desc(metal)) %>%
  group_by(Region) %>%
  mutate(
    ymax = cumsum(DE_Gt),
    ymid = ymax - DE_Gt / 2,
    share = DE_Gt / sum(DE_Gt),
    label = paste0(metal, " ", round(share * 100), "%")
  ) %>%
  ungroup()

de_bar_total <- de_bar %>% group_by(Region) %>% summarise(total = sum(DE_Gt), .groups = "drop")

print(de_bar %>% filter(Region == "World") %>% dplyr::select(metal, DE_Gt, share), n = 30)

dir.create("Figures/Supporting-Figures/RawData", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/SVG/Supporting-Figures", showWarnings = FALSE, recursive = TRUE)


# Panel a: World ----------------------------------------------------------

p_world <- ggplot(filter(de_plot, Region == "World"), aes(year, DE_Gt, fill = metal)) +
  geom_area(colour = "black", linewidth = 0.1) +
  geom_col(data = filter(de_bar, Region == "World"), aes(x = BAR_X), width = BAR_W, colour = "black", linewidth = 0.1) +
  geom_text(
    data = filter(de_bar_total, Region == "World"),
    aes(x = BAR_X, y = total, label = LAST_YEAR),
    inherit.aes = FALSE, vjust = -0.4
  ) +
  geom_text_repel(
    data = filter(de_bar, Region == "World", share >= LABEL_MIN_SHARE),
    aes(x = BAR_X + BAR_W / 2, y = ymid, label = label),
    inherit.aes = FALSE,
    size = pb_annot_size("small"), colour = "#222222", hjust = 0, direction = "y", nudge_x = 1.5,
    xlim = c(BAR_X + BAR_W / 2 + 1, NA), box.padding = 0.1, min.segment.length = 0, max.overlaps = Inf,
    segment.size = 0.2, segment.colour = "grey50"
  ) +
  scale_fill_manual(values = PALETTE_METAL_ORES, na.value = "#999999", guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2020, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(xlim = c(1970, BAR_X + LABEL_SPACE), clip = "off") +
  labs(title = "Global metal ore extraction", subtitle = "Domestic extraction, Gt of gross ore", x = NULL, y = NULL) +
  theme_pb_wide()


# Panel b: Regions ---------------------------------------------------------

# Same y-axis across regions for fair comparison
p_region <- ggplot(filter(de_plot, Region != "World"), aes(year, DE_Gt, fill = metal)) +
  geom_area(colour = "black", linewidth = 0.1) +
  geom_col(data = filter(de_bar, Region != "World"), aes(x = BAR_X), width = BAR_W, colour = "black", linewidth = 0.1) +
  geom_text_repel(
    data = filter(de_bar, Region != "World", share >= LABEL_MIN_SHARE_REGION),
    aes(x = BAR_X + BAR_W / 2, y = ymid, label = label),
    inherit.aes = FALSE,
    size = pb_annot_size("wide"), colour = "#222222", hjust = 0, direction = "y", nudge_x = 1.5,
    xlim = c(BAR_X + BAR_W / 2 + 1, NA), box.padding = 0.15, min.segment.length = 0, max.overlaps = Inf,
    max.iter = 20000, segment.size = 0.2, segment.colour = "grey50", seed = 1
  ) +
  facet_wrap(~Region, ncol = 2) +
  scale_fill_manual(values = PALETTE_METAL_ORES, na.value = "#999999", guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2020, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(xlim = c(1970, BAR_X + LABEL_SPACE), clip = "off") +
  labs(
    title = "Metal ore extraction by region",
    subtitle = paste0("Domestic extraction, Gt of gross ore; end bar shows ", LAST_YEAR, " shares"),
    x = NULL, y = NULL
  ) +
  theme_pb_wide()


# Save (S30) ---------------------------------------------------------------

wrap_plots(p_world, p_region, ncol = 1, heights = c(1, 2.6)) + plot_annotation(tag_levels = "a")
ggsave("Figures/Supporting-Figures/S30_MetalOres_DE.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 27)
ggsave("Figures/SVG/Supporting-Figures/S30_MetalOres_DE.svg", ggplot2::last_plot(), units = "cm", width = 17, height = 27)
clean_svg("Figures/SVG/Supporting-Figures/S30_MetalOres_DE.svg")
write_csv(de_plot, "Figures/Supporting-Figures/RawData/S30_MetalOres_DE.csv")

# EoF
