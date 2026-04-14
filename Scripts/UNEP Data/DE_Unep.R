# Exploration of Domestic Extraction by UNEP
# PBH Feb 2026

# LOAD DATA --------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

df <- read.csv("Inputs/MaterialFlows_all.csv") |> rename(tons = Amount..t.)
head(df)

unique(df$Parent) # 8 materials
table(df$level_material)

df <- df |> filter(level_material == "Level 3") # most detailed account
unique(df$Label) # 61 materials

# Most completed year
df |> group_by(year, country) |> tally() |> group_by(year) |> tally() |> arrange(desc(n)) |> head(10) # 2021


# FIGURES ---------

## BY COUNTRY ---------

df_country <- df %>% group_by(country, year) %>% summarise(total_extraction = sum(tons, na.rm = TRUE))

# order by total
country_order <- df_country |> filter(year == 2019) |> arrange((total_extraction)) |> pull(country)
df_country <- df_country |> mutate(country = factor(country, levels = country_order))

# label important countries
df_country_lab <- df_country %>%
  group_by(year) |>
  mutate(perc_country = total_extraction / sum(total_extraction) * 100) %>%
  filter(perc_country > 2) |>
  filter(year == 2019)

color_countries <- c(
  "Indonesia" = "#1B9E77",
  "Canada" = "#D73027",
  "Australia" = "#E66101",
  "RussianFederation" = "#4575B4",
  "Brazil" = "#009E73",
  "India" = "#FF9933",
  "UnitedStatesofAmerica" = "#B22234",
  "China" = "#DE2910"
)

ggplot(df_country, aes(year, total_extraction, fill = country)) +
  geom_area(col="black",linewidth=0.1) +
  geom_text(data = df_country_lab, aes(label = country), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = color_countries, na.value = "grey80") +
  theme_pb_wide() +
  labs(x = "", y = "", title = "Total Domestic Material Extraction by Country (UNEP Data), in tons") +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/UNEP_DE_Country.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

## BY MATERIAL ---------

df_material <- df %>% group_by(Parent, year) %>% summarise(total_extraction = sum(tons, na.rm = TRUE))
unique(df_material$Parent)

# order by total
material_order <- df_material |> filter(year == 2019) |> arrange((total_extraction)) |> pull(Parent)
df_material <- df_material |> mutate(Parent = factor(Parent, levels = material_order))

cols_material <- c(
  "Coal" = "#4D4D4D",
  "Crop Residues" = "#C49A6C",
  "Crops" = "#4CAF50",
  "Ferrous ores" = "#7F0000",
  "Grazed biomass and fodder crops" = "#7FBF7B",
  "Natural Gas" = "#1F78B4",
  "Non-ferrous ores" = "#B8860B",
  "Non-metallic minerals - construction dominant" = "#8D6E63",
  "Non-metallic minerals - industrial or agricultural dominant" = "#BCAAA4",
  "Oil shale and tar sands" = "#5D4037",
  "Petroleum" = "#000000",
  "Wild catch and harvest" = "#0096C7",
  "Wood" = "#A6761D"
)

ggplot(df_material, aes(year, total_extraction, fill = Parent)) +
  geom_area(col="black",linewidth=0.1) +
  geom_text(data = filter(df_material,year==2019), aes(label = Parent), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = cols_material, na.value = "grey80") +
  labs(x = "", y = "", title = "Total Domestic Material Extraction by Material (UNEP Data), in tons") +
  theme_pb_wide() +
  theme(legend.position = "none")

ggsave("Figures/UNEP_DE_Material.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)


## BY MATERIAL DETAIL ---------

df_material2 <- df %>% group_by(Label, year) %>% summarise(total_extraction = sum(tons, na.rm = TRUE))
unique(df_material2$Label)

# order by total
material_order <- df_material2 |> filter(year == 2019) |> arrange((total_extraction)) |> pull(Label)
df_material2 <- df_material2 |> mutate(Label = factor(Label, levels = material_order))


df_material2_lab <- df_material2 %>%
  group_by(year) |>
  mutate(perc_material = total_extraction / sum(total_extraction) * 100) %>%
  filter(perc_material > 2) |>
  filter(year == 2019)
unique(df_material2_lab$Label)


cols_material2 <- c(
  "Copper ores concentrates and compounds" = "#C87533",
  "Crude oil" = "#1A1A1A",
  "Grazed biomass" = "#66A61E",
  "Iron ores concentrates and compounds" = "#8B0000",
  "Limestone" = "#BFB8A5",
  "Natural gas" = "#2C7FB8",
  "Other Bituminous Coal" = "#525252",
  "Other crop residues (sugar and fodder beet leaves etc)" = "#C7A76C",
  "Sand gravel and crushed rock for construction" = "#A1887F",
  "Straw" = "#E6C65B",
  "Structural clays" = "#A0522D",
  "Sugar crops" = "#F1C232"
)

ggplot(df_material2, aes(year, total_extraction, fill = Label)) +
  geom_area(col="black",linewidth=0.1) +
  geom_text(data = df_material2_lab, aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = cols_material2, na.value = "grey80") +
  labs(x = "", y = "", title = "Total Domestic Material Extraction by Material (UNEP Data), in tons") +
  theme_pb_wide() +
  theme(legend.position = "none")

ggsave(
  "Figures/UNEP_DE_Material_detail.png",
  ggplot2::last_plot(),
  units = 'cm',
  dpi = 600,
  width = 8.7 * 2,
  height = 8.7
)


# Cumulative Curve by Mass  ---------------------
## Year 2021 for now, has 211 countries downloaded

data_fig <- df |>
  filter(year == 2021) |>
  group_by(Label) |>
  reframe(tons = sum(tons)) |>
  ungroup() |>
  arrange(desc(tons)) |>
  mutate(cum_tons = cumsum(tons), cum_perc = cum_tons / sum(tons) * 100)


# add point to start at zero and labels at middle
data_fig <- data_fig |>
  mutate(x = row_number()) |>
  bind_rows(tibble(Label = NA, tons = NA, cum_tons = 0, cum_perc = 0, x = 0)) |>
  arrange(x) |>
  mutate(y_mid = (lag(cum_tons) + cum_tons) / 2)


elements_not_included <- data_fig |> filter(cum_perc > 95) |> pull(Label)
lab <- paste0(
  "Elements below P95 (n:34):\n",
  paste(
    sapply(split(elements_not_included, ceiling(seq_along(elements_not_included) / 5)), paste, collapse = "; "),
    collapse = "\n"
  )
)

ggplot(data_fig, aes(x = x)) +
  geom_line(aes(y = cum_tons), linewidth = 0.8, col = "darkgrey") +
  geom_point(aes(y = cum_tons), size = 1) +
  geom_text(
    data = filter(data_fig, cum_perc <= 95),
    aes(y = y_mid, label = Label),
    hjust = -0.1, size = 6 * 5 / 14 * 0.8,
    nudge_x = 0.2 * 1:28
  ) +
  annotate("text", x = 60, y = 2e10, label = lab, hjust = 1, vjust = 1, size = 4 * 5 / 14 * 0.8) +
  scale_x_continuous(name = "Number of elements", expand = c(0, 0)) +
  scale_y_continuous(
    name = "Cumulative tons",
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / max(data_fig$cum_tons), name = "Cumulative share", labels = scales::percent)
  ) +
  labs(title = "Cumulative curve of Domestic Material Extraction by Material (UNEP Data, 2021)") +
  theme_pb_wide()

# fmt: skip
ggsave("Figures/UNEP_Curve_2021.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

## Curve by value
price <- read.csv("Inputs/Value_per_material_UNEP.csv")
# Prices by ChatGPT best guess for raw material extracted, need to check

data_fig_value <- df |>
  filter(year == 2021) |>
  group_by(Label) |>
  reframe(tons = sum(tons)) |>
  ungroup() |>
  left_join(price) |>
  mutate(value = tons * Value_USD_per_t) |>
  arrange(desc(value)) |>
  mutate(cum_value = cumsum(value), cum_perc = cum_value / sum(value) * 100)


# add point to start at zero and labels at middle
data_fig_value <- data_fig_value |>
  mutate(x = row_number()) |>
  bind_rows(tibble(Label = NA, value = NA, cum_value = 0, cum_perc = 0, x = 0)) |>
  arrange(x) |>
  mutate(y_mid = (lag(cum_value) + cum_value) / 2)


elements_not_included <- data_fig_value |> filter(cum_perc > 95) |> pull(Label)
lab <- paste0(
  "Elements below P95 (n:32):\n",
  paste(
    sapply(split(elements_not_included, ceiling(seq_along(elements_not_included) / 5)), paste, collapse = "; "),
    collapse = "\n"
  )
)

ggplot(data_fig_value, aes(x = x)) +
  geom_line(aes(y = cum_value), linewidth = 0.8, col = "darkgrey") +
  geom_point(aes(y = cum_value), size = 1) +
  geom_text(
    data = filter(data_fig_value, cum_perc <= 95),
    aes(y = y_mid, label = Label),
    hjust = -0.1, size = 6 * 5 / 14 * 0.8,
    nudge_x = 0.2 * 1:30
  ) +
  annotate("text", x = 60, y = 2e12, label = lab, hjust = 1, vjust = 1, size = 4 * 5 / 14 * 0.8) +
  scale_x_continuous(name = "Number of elements", expand = c(0, 0)) +
  scale_y_continuous(
    name = "Cumulative value (USD)",
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / max(data_fig_value$cum_value), name = "Cumulative share", labels = scales::percent)
  ) +
  labs(title = "Cumulative curve by value of Domestic Material Extraction by Material (UNEP Data, 2021)") +
  theme_pb_wide()

# fmt: skip
ggsave("Figures/UNEP_Curve_2021_value.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


# Joint list
unique(c(
  filter(data_fig, cum_perc <= 95 & !is.na(Label))$Label,
  filter(data_fig_value, cum_perc <= 95 & !is.na(Label))$Label
)) # 32

setdiff(
  filter(data_fig, cum_perc <= 95 & !is.na(Label))$Label,
  filter(data_fig_value, cum_perc <= 95 & !is.na(Label))$Label
)

setdiff(
  filter(data_fig_value, cum_perc <= 95 & !is.na(Label))$Label,
  filter(data_fig, cum_perc <= 95 & !is.na(Label))$Label
)
