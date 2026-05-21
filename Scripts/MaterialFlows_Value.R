## =============================================================================
## MaterialFlows_Value.R
## Global material flows 2024: mass vs. monetary value at L3 detail.
## Four plots: dumbbell, cumulative curves, quadrant scatter, rank scatter.
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Data -------------------------------------------------------------------------

df_world <- read_csv("Parameters/MaterialFlows/materialFlows_world_L3_DE.csv", show_col_types = FALSE)

# Column name in price sheet is "$/t" — adjust if differs
prices <- readxl::read_excel("Inputs/Price_Materials.xlsx", sheet = "Mat_Level3") |>
  select(material_l3, label_name, price_usd_t = `$/t`) |>
  mutate(price_usd_t = as.numeric(price_usd_t))


df <- df_world |>
  filter(year == 2024) |>
  left_join(prices, by = "material_l3") |>
  mutate(
    # Mt * $/t × 1e6 (t/Mt) ÷ 1e9 = billion USD
    value_bn = DE_Mt * price_usd_t * 1e6 / 1e9
  ) |>
  mutate(material_l3 = label_name) # plot name

total_mass <- sum(df$DE_Mt, na.rm = TRUE)
total_value <- sum(df$value_bn, na.rm = TRUE)

df <- df |>
  mutate(
    share_mass = DE_Mt / total_mass * 100,
    share_value = value_bn / total_value * 100,
    rank_mass = rank(-DE_Mt, ties.method = "first"),
    rank_value = rank(-value_bn, ties.method = "first")
  )

cat("Materials with missing price:", sum(is.na(df$price_usd_t)), "\n")
cat("Total mass (Gt):", round(total_mass / 1e3, 1), "\n")
cat("Total value (trillion USD):", round(total_value / 1e3, 1), "\n")


# L2 colour palette (same hue families as Figure 1b) --------------------------

l2_info <- df |> group_by(material_l1, material_l2) |> summarise(total_Mt = sum(DE_Mt, na.rm = TRUE), .groups = "drop")

L1_RANGES <- list(
  "Biomass" = c("#1A4D0A", "#B8E994"),
  "Fossil fuels" = c("#1C0A00", "#C4A882"),
  "Metal ores" = c("#4A0000", "#F0A0A0"),
  "Non-metallic minerals" = c("#1C2B35", "#CFD8DC")
)

build_palette_l2 <- function(info_df, ranges) {
  pal <- character(0)
  for (l1 in names(ranges)) {
    items <- info_df |> filter(material_l1 == l1) |> arrange(desc(total_Mt)) |> pull(material_l2)
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

PALETTE_L2 <- build_palette_l2(l2_info, L1_RANGES)

dir.create("Figures/MaterialFlows", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/MaterialFlows/SVG", showWarnings = FALSE, recursive = TRUE)


# Figure A: Dumbbell — mass share vs value share per L3 material ---------------

cat("── Figure A: Dumbbell ──\n")

df_dumb <- df |>
  filter(!is.na(share_value)) |>
  arrange(share_mass) |>
  mutate(material_l3 = factor(material_l3, levels = material_l3))

p_dumb <- ggplot(df_dumb) +
  geom_segment(
    aes(x = share_mass, xend = share_value, y = material_l3, yend = material_l3),
    colour = "grey70",
    linewidth = 0.5
  ) +
  geom_point(
    aes(x = share_mass, y = material_l3, fill = material_l2),
    shape = 21, size = 2, colour = "black", stroke = 0.25
  ) +
  geom_point(
    aes(x = share_value, y = material_l3),
    shape = 23, size = 2, fill = "#E63946", colour = "black", stroke = 0.25
  ) +
  scale_fill_manual(values = PALETTE_L2, guide = "none") +
  scale_x_continuous(labels = scales::label_percent(scale = 1), expand = expansion(mult = c(0.02, 0.08))) +
  annotate(
    "text",
    x = Inf,
    y = 1,
    label = "● Mass (by L2 colour)     ◆ Value",
    hjust = 1,
    vjust = -0.5,
    size = 2.8,
    colour = "grey30"
  ) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Share of global mass vs. value — 2024", x = "Share of global total (%)", y = NULL) +
  theme(axis.text.y = element_text(size = 6.5), plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"))

p_dumb
# fmt: skip
ggsave("Figures/MaterialFlows/FigVal_A_dumbbell.png",       p_dumb, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 3)
ggsave("Figures/MaterialFlows/SVG/FigVal_A_dumbbell.svg", p_dumb, units = "cm", width = 8.7 * 2, height = 8.7 * 3)


# Figure B: Cumulative share curves — mass and value (side by side) -----------

cat("── Figure B: Cumulative curves ──\n")

N_LABEL <- 25 # label this many top materials on each curve

make_cumdf <- function(data, share_col, title) {
  data |>
    filter(!is.na({{ share_col }})) |>
    arrange(desc({{ share_col }})) |>
    mutate(
      n = row_number(),
      cum_share = cumsum({{ share_col }}),
      prev_cum = lag(cum_share, default = 0),
      label_mat = if_else(n <= N_LABEL, as.character(material_l3), NA_character_)
    ) |>
    mutate(panel = title)
}

df_cum_mass <- make_cumdf(df, share_mass, "By mass")
df_cum_value <- make_cumdf(df, share_value, "By value")

make_cump <- function(df_in, title_str, panel_label) {
  # Add n=0 origin row for geom_step
  df_step <- bind_rows(tibble(n = 0, cum_share = 0), df_in |> select(n, cum_share))

  ggplot(df_in) +
    # Filled incremental band for each material (from prev_cum to cum_share)
    geom_rect(
      aes(xmin = n - 1, xmax = n, ymin = prev_cum, ymax = cum_share, fill = material_l2),
      alpha = 0.5,
      colour = NA
    ) +
    # Staircase outline: step rises at left edge of each material's band
    geom_step(data = df_step, aes(x = n, y = cum_share), direction = "vh", colour = "grey25", linewidth = 0.5) +
    # Label above horizontal step, at midpoint of each step
    geom_text(
      data = filter(df_in, !is.na(label_mat)) |> 
        mutate(label_mat=str_remove_all(label_mat," for construction| concentrates and compounds")),
      aes(x = n, y = cum_share, label = label_mat, colour = material_l2),
      nudge_y=1,
      size = 2.5, vjust = -0.35, angle = 90, hjust = 0, fontface = "plain"
    ) +
    # Panel label inside top-left
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = panel_label,
      hjust = -0.4,
      vjust = 1.6,
      fontface = "bold",
      size = 3.5,
      colour = "black"
    ) +
    scale_fill_manual(values = PALETTE_L2, guide = "none") +
    scale_colour_manual(values = PALETTE_L2, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, 30)) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 115),
      expand = c(0, 0) # extra headroom for vertical labels
    ) +
    coord_cartesian(expand = F, clip = "off") +
    theme_pb_large() +
    labs(title = title_str, x = "Materials (ranked by share)", y = "Cumulative share (%)") +
    theme(plot.title = element_text(hjust = 0.5))
}

p_cum_mass <- make_cump(df_cum_mass, "By mass", "a")
p_cum_value <- make_cump(df_cum_value, "By value", "b")

library(cowplot)
p_cum <- plot_grid(p_cum_mass, p_cum_value, ncol = 2)
p_cum
# fmt: skip
ggsave("Figures/MaterialFlows/FigVal_B_cumulative.png",     p_cum, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/MaterialFlows/SVG/FigVal_B_cumulative.svg", p_cum, units = "cm", width = 8.7 * 2, height = 8.7)


# Figure C: Quadrant scatter — share mass (x) vs share value (y) --------------

cat("── Figure C: Quadrant scatter ──\n")

# Equal-share benchmark: if all materials were identical, each gets 1/n %
equal_share <- 100 / nrow(df)

df_quad <- df |> filter(!is.na(share_value)) |> mutate(above_diag = share_value > share_mass, label_mat = material_l3)

quad_labels <- tibble(
  x = c(Inf, -Inf, Inf, -Inf),
  y = c(Inf, Inf, -Inf, -Inf),
  label = c("High mass\nHigh value", "Low mass\nHigh value", "High mass\nLow value", "Low mass\nLow value"),
  hjust = c(1.05, -0.05, 1.05, -0.05),
  vjust = c(1.4, 1.4, -0.4, -0.4)
)

p_quad <- ggplot(df_quad, aes(x = share_mass, y = share_value)) +
  geom_vline(xintercept = equal_share, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = equal_share, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = "grey40", linewidth = 0.3) +
  geom_point(aes(fill = material_l2), shape = 21, size = 2, colour = "black", stroke = 0.25, alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = label_mat, colour = material_l2),
    size = 2.5,
    max.overlaps = 40,
    segment.colour = "grey60",
    segment.size = 0.3,
    box.padding = 0.2
  ) +
  geom_text(
    data = quad_labels,
    aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
    size = 2.5, colour = "grey45", fontface = "italic", inherit.aes = FALSE
  ) +
  scale_fill_manual(values = PALETTE_L2, guide = "none") +
  scale_colour_manual(values = PALETTE_L2, guide = "none") +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(title = "Mass vs. Value share — 2024", x = "Share of global mass (%)", y = "Share of global value (%)")

p_quad
# fmt: skip
ggsave("Figures/MaterialFlows/FigVal_C_quadrant.png",     p_quad, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave("Figures/MaterialFlows/SVG/FigVal_C_quadrant.svg", p_quad, units = "cm", width = 8.7 * 2, height = 8.7 * 2)


# Figure D: Rank scatter — mass rank (x) vs value rank (y) --------------------

cat("── Figure D: Rank scatter ──\n")

# Label top-10 by mass, top-10 by value, and outliers far from diagonal
df_rank <- df |>
  filter(!is.na(rank_value)) |>
  mutate(
    rank_diff = abs(rank_value - rank_mass),
    label_mat = if_else(
      rank_mass <= 10 | rank_value <= 10 | rank_diff >= quantile(rank_diff, 0.80),
      as.character(material_l3),
      NA_character_
    )
  )

p_rank <- ggplot(df_rank, aes(x = rank_mass, y = rank_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_point(aes(fill = material_l2), shape = 21, size = 2, colour = "black", stroke = 0.25, alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = label_mat, colour = material_l2),
    size = 2.5,
    max.overlaps = 35,
    segment.colour = "grey60",
    segment.size = 0.3,
    box.padding = 0.25,
    na.rm = TRUE
  ) +
  annotate(
    "text",
    x = 1,
    y = Inf,
    label = "High value-density\n(ranked higher by value)",
    hjust = 0,
    vjust = 1.4,
    size = 2.5,
    colour = "grey40",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 1,
    label = "Low value-density\n(ranked higher by mass)",
    hjust = 1,
    vjust = -0.3,
    size = 2.5,
    colour = "grey40",
    fontface = "italic"
  ) +
  scale_fill_manual(values = PALETTE_L2, guide = "none") +
  scale_colour_manual(values = PALETTE_L2, guide = "none") +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  labs(
    title = "Mass rank vs. Value rank — 2024",
    x = "Mass rank (1 = most massive)",
    y = "Value rank (1 = most valuable)"
  )

p_rank
# fmt: skip
ggsave("Figures/MaterialFlows/FigVal_D_rank_scatter.png",     p_rank, units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)
ggsave("Figures/MaterialFlows/SVG/FigVal_D_rank_scatter.svg", p_rank, units = "cm", width = 8.7 * 2, height = 8.7 * 2)

cat("\nAll figures saved to Figures/MaterialFlows/\n")

# EoF
