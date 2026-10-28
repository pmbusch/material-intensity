## S31 - MaterialGroupExtraction.R (supporting figure S31)
## Same figures as S30 - MetalOreExtraction.R for the other three material groups
## (biomass, fossil fuels, non-metallic minerals): domestic extraction by Level 3
## material, world and by region, stacked areas sorted largest (bottom) to
## smallest (top), plus a stacked bar with latest-year shares.
##
## Input:   Parameters/MaterialFlows/DE_detail_region.csv (06-Aggregate_MaterialFlows.R)
## Outputs: Figures/Supporting-Figures/S31_OtherMaterials_DE_World.png (+ SVG): world panels
##          of the three groups (supporting figure S31)
##          Figures/Supporting-Figures/RawData/S31_OtherMaterials_DE_World.csv
##          Figures/MaterialFlows/{Biomass,FossilFuels,NonMetallicMinerals}_DE_Region.png
##          (regional versions, exploratory)

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
pb_set_geom_defaults("small")
library(patchwork)

LABEL_MIN_SHARE <- 0.02 # label bar segments with at least this share of the latest year (world)
LABEL_MIN_SHARE_REGION <- 0.03 # same, regional panels (less vertical room)
BAR_GAP <- 5 # years between last data year and the centre of the end bar
BAR_W <- 4 # end bar width (years)
LABEL_SPACE <- 34 # extra x range (years) right of the bar for direct labels (longer names than metals)

# Level 2 parent -> material group, and file name per group
# fmt: skip
GROUP_PARENTS <- list(
  "Biomass"               = c("Crops", "Crop Residues", "Grazed biomass and fodder crops", "Wild catch and harvest", "Wood"),
  "Fossil fuels"          = c("Coal", "Natural Gas", "Petroleum", "Oil shale and tar sands"),
  "Non-metallic minerals" = c("Non-metallic minerals - construction dominant", "Non-metallic minerals - industrial or agricultural dominant")
)
GROUP_FILE <- c("Biomass" = "Biomass", "Fossil fuels" = "FossilFuels", "Non-metallic minerals" = "NonMetallicMinerals")

# Level 3 label -> short display name
# fmt: skip
LABEL_DETAIL <- c(
  # Biomass
  "Grazed biomass"                                         = "Grazed biomass",
  "Straw"                                                  = "Straw",
  "Other crop residues (sugar and fodder beet leaves etc)" = "Other crop residues",
  "Wheat"                                                  = "Wheat",
  "Rice"                                                   = "Rice",
  "Cereals n.e.c."                                         = "Other cereals",
  "Sugar crops"                                            = "Sugar crops",
  "Vegetables"                                             = "Vegetables",
  "Fruits"                                                 = "Fruits",
  "Roots and tubers"                                       = "Roots & tubers",
  "Oil bearing crops"                                      = "Oil crops",
  "Pulses"                                                 = "Pulses",
  "Fibres"                                                 = "Fibres",
  "Nuts"                                                   = "Nuts",
  "Tobacco"                                                = "Tobacco",
  "Spice - beverage - pharmaceutical crops"                = "Spice & beverage crops",
  "Other crops n.e.c"                                      = "Other crops",
  "Timber (Industrial roundwood)"                          = "Timber",
  "Wood fuel and other extraction"                         = "Wood fuel",
  "Wild fish catch"                                        = "Wild fish",
  "All other aquatic animals"                              = "Other aquatic animals",
  "Aquatic plants"                                         = "Aquatic plants",
  # Fossil fuels
  "Coking Coal"                                            = "Coking coal",
  "Other Bituminous Coal"                                  = "Bituminous coal",
  "Other Sub-Bituminous Coal"                              = "Sub-bituminous coal",
  "Lignite (brown coal)"                                   = "Lignite",
  "Anthracite"                                             = "Anthracite",
  "Peat"                                                   = "Peat",
  "Natural gas"                                            = "Natural gas",
  "Crude oil"                                              = "Crude oil",
  "Natural gas liquids"                                    = "Natural gas liquids",
  "Oil shale and tar sands"                                = "Oil shale & tar sands",
  # Non-metallic minerals
  "Sand gravel and crushed rock for construction"          = "Sand & gravel",
  "Limestone"                                              = "Limestone",
  "Structural clays"                                       = "Structural clays",
  "Gypsum"                                                 = "Gypsum",
  "Dolomite"                                               = "Dolomite",
  "Chalk"                                                  = "Chalk",
  "Ornamental or building stone"                           = "Building stone",
  "Industrial sand and gravel"                             = "Industrial sand",
  "Specialty clays"                                        = "Specialty clays",
  "Salt"                                                   = "Salt",
  "Fertilizer minerals n.e.c."                             = "Fertilizer minerals",
  "Chemical minerals n.e.c."                               = "Chemical minerals",
  "Industrial minerals n.e.c"                              = "Industrial minerals",
  "Other non-metallic minerals n.e.c."                     = "Other minerals"
)

# Colours evoke each material (grass, straw, grain, fruit red, timber brown, sea blue;
# coal blacks, lignite brown, gas blue-grey; sand, limestone, terracotta clay...)
# fmt: skip
PALETTE_DETAIL <- c(
  # Biomass
  "Grazed biomass"         = "#558B2F",
  "Straw"                  = "#D8C27A",
  "Other crop residues"    = "#A89060",
  "Wheat"                  = "#B8892B",
  "Rice"                   = "#EDE3C3",
  "Other cereals"          = "#E9C25A",
  "Sugar crops"            = "#9CCB86",
  "Vegetables"             = "#2F6B35",
  "Fruits"                 = "#B5473F",
  "Roots & tubers"         = "#8D6E4F",
  "Oil crops"              = "#A7A23A",
  "Pulses"                 = "#9C5B3B",
  "Fibres"                 = "#E8E4DA",
  "Nuts"                   = "#6D4C2F",
  "Tobacco"                = "#7A6A3A",
  "Spice & beverage crops" = "#4E3426",
  "Other crops"            = "#BDB5A6",
  "Timber"                 = "#5D4037",
  "Wood fuel"              = "#A1795B",
  "Wild fish"              = "#2B6A9E",
  "Other aquatic animals"  = "#6FA3CC",
  "Aquatic plants"         = "#2E8B7A",
  # Fossil fuels
  "Coking coal"            = "#1C1C1C",
  "Bituminous coal"        = "#454545",
  "Sub-bituminous coal"    = "#6E6E6E",
  "Lignite"                = "#7B5E3B",
  "Anthracite"             = "#2B2F38",
  "Peat"                   = "#8C7B5A",
  "Natural gas"            = "#78909C",
  "Crude oil"              = "#4A3528",
  "Natural gas liquids"    = "#A1887F",
  "Oil shale & tar sands"  = "#C2A878",
  # Non-metallic minerals
  "Sand & gravel"               = "#D9C08C",
  "Limestone"                   = "#ECE6D3",
  "Structural clays"            = "#B5651D",
  "Gypsum"                      = "#F5F3EE",
  "Dolomite"                    = "#BFAE9A",
  "Chalk"                       = "#DCDCD2",
  "Building stone"              = "#7D7F86",
  "Industrial sand"             = "#C7A86B",
  "Specialty clays"             = "#D58A5E",
  "Salt"                        = "#C6D9E6",
  "Fertilizer minerals"         = "#7DA27A",
  "Chemical minerals"           = "#8E79A8",
  "Industrial minerals"         = "#5F7A8A",
  "Other minerals"              = "#9E9E9E"
)


# Data --------------------------------------------------------------------

de_all <- read_csv("Parameters/MaterialFlows/DE_detail_region.csv", show_col_types = FALSE) %>%
  mutate(material_detail = str_trim(material_detail), detail = coalesce(unname(LABEL_DETAIL[material_detail]), material_detail))

cat("Detail labels without short name:", paste(setdiff(unique(de_all$material_detail), c(names(LABEL_DETAIL), names(LABEL_METAL_ORES))), collapse = ", "), "\n")

LAST_YEAR <- max(de_all$year)
BAR_X <- LAST_YEAR + BAR_GAP

dir.create("Figures/MaterialFlows", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/Supporting-Figures/RawData", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/SVG/Supporting-Figures", showWarnings = FALSE, recursive = TRUE)
p_world_list <- list()
s31_data <- list()


# Figures per material group ----------------------------------------------

for (grp in names(GROUP_PARENTS)) {
  cat("\n── ", grp, " ──\n")

  de <- de_all %>% filter(material_parent %in% GROUP_PARENTS[[grp]])

  de_plot <- bind_rows(
    de %>% group_by(Region = "World", detail, year) %>% summarise(DE_Mt = sum(DE_Mt), .groups = "drop"),
    de %>% group_by(Region, detail, year) %>% summarise(DE_Mt = sum(DE_Mt), .groups = "drop")
  ) %>%
    complete(Region, detail, year, fill = list(DE_Mt = 0)) %>%
    mutate(DE_Gt = DE_Mt / 1e3)

  # Stack order by global extraction in the latest year: largest at the bottom
  # (ggplot stacks the first factor level on top, so levels go smallest -> largest)
  order_detail <- de_plot %>% filter(Region == "World", year == LAST_YEAR) %>% arrange(DE_Gt) %>% pull(detail)
  order_region <- de_plot %>%
    filter(Region != "World", year == LAST_YEAR) %>%
    group_by(Region) %>%
    summarise(DE_Gt = sum(DE_Gt)) %>%
    arrange(desc(DE_Gt)) %>%
    pull(Region)

  de_plot <- de_plot %>%
    mutate(detail = factor(detail, levels = order_detail), Region = factor(Region, levels = c("World", order_region)))

  # Cumulate from the bottom of the stack (last factor level) to get segment midpoints
  de_bar <- de_plot %>%
    filter(year == LAST_YEAR) %>%
    arrange(Region, desc(detail)) %>%
    group_by(Region) %>%
    mutate(
      ymax = cumsum(DE_Gt),
      ymid = ymax - DE_Gt / 2,
      share = DE_Gt / sum(DE_Gt),
      label = paste0(detail, " ", round(share * 100), "%")
    ) %>%
    ungroup()

  de_bar_total <- de_bar %>% group_by(Region) %>% summarise(total = sum(DE_Gt), .groups = "drop")

  print(de_bar %>% filter(Region == "World") %>% dplyr::select(detail, DE_Gt, share) %>% arrange(desc(share)), n = 30)

  # World (one panel of S31)
  s31_data[[grp]] <- de_plot %>% filter(Region == "World") %>% mutate(material_group = grp)
  p_world_list[[grp]] <- ggplot(filter(de_plot, Region == "World"), aes(year, DE_Gt, fill = detail)) +
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
      size = pb_annot_size("wide"), colour = "#222222", hjust = 0, direction = "y", nudge_x = 1.5,
      xlim = c(BAR_X + BAR_W / 2 + 1, NA), box.padding = 0.1, min.segment.length = 0, max.overlaps = Inf,
      segment.size = 0.2, segment.colour = "grey50", seed = 1
    ) +
    scale_fill_manual(values = PALETTE_DETAIL, na.value = "#999999", guide = "none") +
    scale_x_continuous(breaks = seq(1970, 2020, 10), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    coord_cartesian(xlim = c(1970, BAR_X + LABEL_SPACE), clip = "off") +
    labs(title = paste("Global", tolower(grp), "extraction"), subtitle = "Domestic extraction, Gt", x = NULL, y = NULL) +
    theme_pb_wide()

  # Regions, same y-axis across regions for fair comparison
  ggplot(filter(de_plot, Region != "World"), aes(year, DE_Gt, fill = detail)) +
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
    scale_fill_manual(values = PALETTE_DETAIL, na.value = "#999999", guide = "none") +
    scale_x_continuous(breaks = seq(1970, 2020, 10), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    coord_cartesian(xlim = c(1970, BAR_X + LABEL_SPACE), clip = "off") +
    labs(
      title = paste(grp, "extraction by region"),
      subtitle = paste0("Domestic extraction, Gt; end bar shows ", LAST_YEAR, " shares"),
      x = NULL, y = NULL
    ) +
    theme_pb_wide()

  ggsave(
    paste0("Figures/MaterialFlows/", GROUP_FILE[grp], "_DE_Region.png"), ggplot2::last_plot(),
    units = "cm", dpi = 600, width = 17, height = 20
  )
}


# Save (S31): world panels of the three groups ----------------------------

wrap_plots(p_world_list, ncol = 1) + plot_annotation(tag_levels = "a")
ggsave("Figures/Supporting-Figures/S31_OtherMaterials_DE_World.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 24)
ggsave("Figures/SVG/Supporting-Figures/S31_OtherMaterials_DE_World.svg", ggplot2::last_plot(), units = "cm", width = 17, height = 24)
clean_svg("Figures/SVG/Supporting-Figures/S31_OtherMaterials_DE_World.svg")
write_csv(bind_rows(s31_data), "Figures/Supporting-Figures/RawData/S31_OtherMaterials_DE_World.csv")

# EoF
