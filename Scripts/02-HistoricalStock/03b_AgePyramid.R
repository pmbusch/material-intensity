# 03b_AgePyramid.R
# Age stock pyramid plots

# LOAD DATA ----------
# From 03_UNEP_stock_flow_model.R

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00-CommonParameters.R", encoding = "UTF-8")


age_profile_list <- read_csv("Parameters/stock_2024_age_profile.csv")


# -- Plot 1: 2024 age distribution by end-use and material --------------------

gap <- 7 # in Gt, adjust to taste

region_order <- age_profile_list %>%
  filter(material == "Non-metallic minerals") %>%
  group_by(Region) %>%
  summarise(total = sum(surviving_stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Region)

pyramid_data <- age_profile_list %>%
  mutate(Region = factor(Region, levels = region_order)) |>
  filter(
    material %in% "Non-metallic minerals",
    sub_use %in% c("residential", "non_residential", "roads", "civil_engineering")
  ) %>%
  mutate(end_use = if_else(sub_use %in% c("residential", "non_residential"), "Buildings", "Civil infrastructure")) %>%
  mutate(
    cohort_bin = cut(
      cohort_year,
      breaks = c(-Inf, seq(1970, 2025, by = 5)),
      labels = c("55+", paste0(2024 - seq(1974, 2024, by = 5), "-", 2024 - seq(1970, 2020, by = 5))),
      right = TRUE
    ),
    cohort_bin = fct_rev(cohort_bin),
  ) |>
  group_by(end_use, cohort_bin, Region) %>%
  summarise(stock_Gt = sum(surviving_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  # Compute cumulative stack positions per side
  group_by(end_use, cohort_bin) %>%
  arrange(Region) %>%
  mutate(cum_stock = cumsum(stock_Gt), cum_stock_lag = lag(cum_stock, default = 0)) %>%
  ungroup() %>%
  mutate(
    xmin = if_else(end_use == "Buildings", -(cum_stock) - gap, cum_stock_lag + gap),
    xmax = if_else(end_use == "Buildings", -(cum_stock_lag) - gap, cum_stock + gap)
  )

x_lim <- pyramid_data %>%
  group_by(end_use, cohort_bin) %>%
  summarise(stack_total = sum(stock_Gt), .groups = "drop") %>%
  pull(stack_total) %>%
  max() *
  1.05 +
  gap

age_ticks_df <- pyramid_data %>% distinct(cohort_bin)

total_stock <- pyramid_data |> group_by(end_use) |> reframe(stock_Gt = sum(stock_Gt)) |> ungroup()

age_dist <- pyramid_data |>
  dplyr::select(Region, end_use, cohort_bin, stock_Gt) |>
  group_by(Region, end_use) |>
  mutate(age_share = stock_Gt / sum(stock_Gt) * 200 + sign(stock_Gt) * 100) |> # scale and shift for plot reasons
  ungroup() |>
  mutate(age_share = age_share * if_else(end_use == "Buildings", -1, 1)) |>
  mutate(key = paste0(Region, end_use)) |>
  arrange(cohort_bin)


p_pyramid <- pyramid_data %>%
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = as.numeric(cohort_bin) - 0.4,
      ymax = as.numeric(cohort_bin) + 0.4,
      fill = Region
    ),
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = age_ticks_df,
    aes(x = 0, y = cohort_bin, label = cohort_bin),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20"
  ) +
  # geom_path(data = age_dist, aes(y = cohort_bin, x = age_share, col = Region, group = key)) +
  # fmt: skip
  annotate("text", x = -(x_lim / 2), y = Inf, label = paste0("Buildings\n",round(filter(total_stock,end_use=="Buildings")$stock_Gt)," Gt"),          hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x =  (x_lim / 2), y = Inf, label = paste0("Civil infrastructure\n",round(filter(total_stock,end_use=="Civil infrastructure")$stock_Gt)," Gt"), hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x = 0,            y = Inf, label = "Age",                 hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  scale_x_continuous(
    limits = c(-x_lim, x_lim),
    breaks = c(-(gap + c(100, 50, 0)), gap + c(0, 50, 100)),
    labels = c(100, 50, 0, 0, 50, 100)
  ) +
  scale_y_discrete() +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_color_manual(values = PALETTE_REGIONS, name = NULL) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "2024 Non-metallic minerals Stock (Gt)", y = NULL) +
  # manual X axis
  annotate("segment", x = gap, xend = x_lim, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  # fmt: skip
  annotate("segment", x = -gap, xend = -x_lim, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  theme_pb_large() +
  theme(
    legend.position = c(0.2, 0.8),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.ticks.y = element_blank(),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "mm")
  ) +
  guides(fill = guide_legend(ncol = 1))

p_pyramid

# fmt: skip
ggsave("Figures/Stocks/age_profile_2024.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
# fmt: skip
ggsave("Figures/SVG/age_profile_2024.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
clean_svg("Figures/SVG/age_profile_2024.svg")

# -- Plot 2: 2024 age distribution faceted by region (Buildings vs Civil infrastructure) --------------------------------

gap <- 7 # in Gt, adjust to taste

region_order <- age_profile_list %>%
  filter(material == "Non-metallic minerals") %>%
  group_by(Region) %>%
  summarise(total = sum(surviving_stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Region)

pyramid_data_facet_bldg <- age_profile_list %>%
  mutate(Region = factor(Region, levels = region_order)) |>
  filter(
    material %in% "Non-metallic minerals",
    sub_use %in% c("residential", "non_residential", "roads", "civil_engineering")
  ) %>%
  mutate(end_use = if_else(sub_use %in% c("residential", "non_residential"), "Buildings", "Civil infrastructure")) %>%
  mutate(
    cohort_bin = cut(
      cohort_year,
      breaks = c(-Inf, seq(1970, 2025, by = 5)),
      labels = c("55+", paste0(2024 - seq(1974, 2024, by = 5), "-", 2024 - seq(1970, 2020, by = 5))),
      right = TRUE
    ),
    cohort_bin = fct_rev(cohort_bin),
  ) |>
  group_by(Region, end_use, cohort_bin) %>%
  summarise(stock_Gt = sum(surviving_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  mutate(
    xmin = if_else(end_use == "Buildings", -stock_Gt - gap, gap),
    xmax = if_else(end_use == "Buildings", -gap, stock_Gt + gap)
  )

x_lim_bldg <- max(pyramid_data_facet_bldg$stock_Gt, na.rm = TRUE) * 1.05 + gap

age_ticks_df_bldg <- pyramid_data_facet_bldg %>% distinct(cohort_bin)

# auto-pick symmetric break step
.brk_step_b <- dplyr::case_when(
  x_lim_bldg - gap > 200 ~ 100,
  x_lim_bldg - gap > 100 ~ 50,
  x_lim_bldg - gap > 50 ~ 25,
  x_lim_bldg - gap > 20 ~ 10,
  TRUE ~ 5
)
.brk_pos_b <- seq(0, floor((x_lim_bldg - gap) / .brk_step_b) * .brk_step_b, by = .brk_step_b)

p_pyramid_facet_bldg <- pyramid_data_facet_bldg %>%
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = as.numeric(cohort_bin) - 0.4,
      ymax = as.numeric(cohort_bin) + 0.4,
      fill = end_use
    ),
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = age_ticks_df_bldg,
    aes(x = 0, y = cohort_bin, label = cohort_bin),
    inherit.aes = FALSE,
    size = 2,
    colour = "grey20"
  ) +
  scale_x_continuous(labels = function(x) abs(round(x, 1)), n.breaks = 5) +
  scale_y_discrete() +
  scale_fill_manual(values = c("Machinery" = "#59A14F", "Short Lived Products" = "#E15759"), name = NULL) +
  facet_wrap(~Region, ncol = 4, scales = "free_x") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "2024 Non-metallic minerals Stock (Gt)", y = NULL) +
  theme_pb_large() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.ticks.y = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm")
  )

p_pyramid_facet_bldg


# -- Plot 3: 2024 age distribution faceted by region (Machinery vs Short Lived Products) -------------------------

gap <- 0.15 # in Gt — smaller than the buildings/civil version since stocks are smaller; tune

region_order_msl <- age_profile_list %>%
  filter(super_category %in% c("machinery", "short_lived")) %>%
  group_by(Region) %>%
  summarise(total = sum(surviving_stock_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Region)

pyramid_data_msl <- age_profile_list %>%
  mutate(Region = factor(Region, levels = region_order_msl)) |>
  filter(super_category %in% c("machinery", "short_lived")) %>%
  mutate(end_use = dplyr::recode(super_category, machinery = "Machinery", short_lived = "Short Lived Products")) %>%
  mutate(
    cohort_bin = cut(
      cohort_year,
      breaks = c(-Inf, seq(1970, 2025, by = 5)),
      labels = c("55+", paste0(2024 - seq(1974, 2024, by = 5), "-", 2024 - seq(1970, 2020, by = 5))),
      right = TRUE
    ),
    cohort_bin = fct_rev(cohort_bin),
  ) |>
  group_by(end_use, cohort_bin, Region) %>%
  summarise(stock_Gt = sum(surviving_stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  # Compute cumulative stack positions per side
  group_by(end_use, cohort_bin) %>%
  arrange(Region) %>%
  mutate(cum_stock = cumsum(stock_Gt), cum_stock_lag = lag(cum_stock, default = 0)) %>%
  ungroup() %>%
  mutate(
    xmin = if_else(end_use == "Machinery", -(cum_stock) - gap, cum_stock_lag + gap),
    xmax = if_else(end_use == "Machinery", -(cum_stock_lag) - gap, cum_stock + gap)
  )

x_lim_msl <- pyramid_data_msl %>%
  group_by(end_use, cohort_bin) %>%
  summarise(stack_total = sum(stock_Gt), .groups = "drop") %>%
  pull(stack_total) %>%
  max() *
  1.05 +
  gap

age_ticks_df_msl <- pyramid_data_msl %>% distinct(cohort_bin)

total_stock_msl <- pyramid_data_msl |> group_by(end_use) |> reframe(stock_Gt = sum(stock_Gt)) |> ungroup()

# auto-pick symmetric break step
.brk_step_m <- dplyr::case_when(
  x_lim_msl - gap > 200 ~ 100,
  x_lim_msl - gap > 100 ~ 50,
  x_lim_msl - gap > 50 ~ 25,
  x_lim_msl - gap > 20 ~ 10,
  x_lim_msl - gap > 5 ~ 2,
  TRUE ~ 1
)
.brk_pos_m <- seq(0, floor((x_lim_msl - gap) / .brk_step_m) * .brk_step_m, by = .brk_step_m)

p_pyramid_msl <- pyramid_data_msl %>%
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = as.numeric(cohort_bin) - 0.4,
      ymax = as.numeric(cohort_bin) + 0.4,
      fill = Region
    ),
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    data = age_ticks_df_msl,
    aes(x = 0, y = cohort_bin, label = cohort_bin),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20"
  ) +
  # fmt: skip
  annotate("text", x = -(x_lim_msl / 2), y = Inf, label = paste0("Machinery\n",round(filter(total_stock_msl,end_use=="Machinery")$stock_Gt)," Gt"),                       hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x =  (x_lim_msl / 2), y = Inf, label = paste0("Short Lived Products\n",round(filter(total_stock_msl,end_use=="Short Lived Products")$stock_Gt)," Gt"), hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  # fmt: skip
  annotate("text", x = 0,                y = Inf, label = "Age",                                                                                                          hjust = 0.5, vjust = -0.5, fontface = "bold", size = 3, colour = "grey20") +
  scale_x_continuous(
    limits = c(-x_lim_msl, x_lim_msl),
    breaks = c(-(gap + rev(.brk_pos_m)), gap + .brk_pos_m),
    labels = c(rev(.brk_pos_m), .brk_pos_m)
  ) +
  scale_y_discrete() +
  scale_fill_manual(values = PALETTE_REGIONS, name = NULL) +
  scale_color_manual(values = PALETTE_REGIONS, name = NULL) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "2024 Stock (Gt)", y = NULL) +
  # manual X axis
  annotate("segment", x = gap, xend = x_lim_msl, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  # fmt: skip
  annotate("segment", x = -gap, xend = -x_lim_msl, y = -Inf, yend = -Inf, color = "black", linewidth = 0.4) +
  theme_pb_large() +
  theme(
    legend.position = c(0.2, 0.8),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.ticks.y = element_blank(),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "mm")
  ) +
  guides(fill = guide_legend(ncol = 1))

p_pyramid_msl

# fmt: skip
ggsave("Figures/Stocks/age_profile_2024_msl.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
# fmt: skip
ggsave("Figures/SVG/age_profile_2024_msl.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)
clean_svg("Figures/SVG/age_profile_2024_msl.svg")
