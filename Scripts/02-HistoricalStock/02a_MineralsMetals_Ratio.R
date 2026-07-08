## =============================================================================
## 02a_MineralsMetals_Ratio.R
## Ratio of Non-metallic minerals to Metal ores from MISO inflows,
## by end-use category — global and regional.
##
## Figures (Figures/MISO/):
##   minerals_metals_ratio_global.png  — global, lines by end-use
##   minerals_metals_ratio_region.png  — faceted by region
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00-CommonParameters.R", encoding = "UTF-8")

FIG_DIR <- "Figures/MISO"

# ── Load data ─────────────────────────────────────────────────────────────────
miso_inflow <- read_csv("Parameters/MISO/MISO_flows_regional.csv", show_col_types = FALSE)

# Recode end_use to match PALETTE_ENDUSE keys
miso_inflow <- miso_inflow %>%
  mutate(
    end_use = recode(
      end_use,
      "buildings" = "Buildings",
      "civil_infrastructure" = "Civil infrastructure",
      "machinery" = "Machinery",
      "short_lived" = "Short-lived products"
    )
  )


# ── Figure 1: Global ratio by end-use ────────────────────────────────────────
ratio_global <- miso_inflow %>%
  group_by(material, end_use, year) %>%
  summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = material, values_from = value_Mt) %>%
  mutate(ratio = `Non-metallic minerals` / `Metal ores`)

ggplot(ratio_global, aes(year, ratio, colour = end_use)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = PALETTE_ENDUSE, name = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Non-metallic minerals / Metal ores ratio",
    subtitle = "Global inflows (MISO), by end use",
    x = NULL,
    y = "Ratio (t minerals per t metals)"
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_pb_large() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), colour = NA),
    legend.key.size = unit(0.4, "cm")
  )

# fmt: skip
ggsave(file.path(FIG_DIR, "02a-minerals_metals_ratio_global.png"),  ggplot2::last_plot(),  width = 8.7,  height = 8.7,  units = "cm",  dpi = 300)
cat("Saved: minerals_metals_ratio_global.png\n")


# ── Figure 2: Regional ratio faceted by region ────────────────────────────────
ratio_region <- miso_inflow %>%
  group_by(Region, material, end_use, year) %>%
  summarise(value_Mt = sum(value_Mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = material, values_from = value_Mt) %>%
  mutate(ratio = `Non-metallic minerals` / `Metal ores`)

ggplot(ratio_region, aes(year, ratio, colour = end_use)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~Region, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = PALETTE_ENDUSE, name = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Non-metallic minerals / Metal ores ratio by region",
    subtitle = "MISO inflows, by end use",
    x = NULL,
    y = "Ratio (t minerals per t metals)"
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_pb_large() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7),
    panel.spacing = unit(0.3, "cm")
  )

# fmt: skip
ggsave(file.path(FIG_DIR, "02a-minerals_metals_ratio_region.png"),ggplot2::last_plot(),width = 17,height = 14,units = "cm",dpi = 300)
cat("Saved: minerals_metals_ratio_region.png\n")
