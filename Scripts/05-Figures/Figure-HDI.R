## =============================================================================
## Figure-HDI.R
## HDI vs DMC per capita country trajectories, 1990–2023
## Y: Human Development Index; X: Domestic Material Consumption per capita (t/cap)
## ~35 countries as paths; colour by country (semantically grouped by region);
## alpha gradient on decade dots; arrow at path end; direct country labels.
## Reads from Parameters/ and Inputs/; saves to Figures/
## =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

## Load data -------------------------------------------------------------------

df_dmc <- read_csv("Parameters/materials_country_DMC.csv", show_col_types = FALSE)
df_pop <- read_csv("Parameters/population_country_historical.csv", show_col_types = FALSE)
df_gdp <- read_csv("Parameters/gdp_country.csv", show_col_types = FALSE)
hdi_raw <- readxl::read_excel("Inputs/UNDP/hdr-data.xlsx")

## HDI: extract Human Development Index from long format -----------------------
hdi <- hdi_raw %>%
  filter(tolower(indicatorCode) == "hdi") %>%
  transmute(ISO3 = countryIsoCode, year = as.integer(year), hdi = as.numeric(value)) %>%
  filter(!is.na(hdi), !is.na(year))

## Aggregate DMC to total per country-year (sum all material categories) -------
dmc_total <- df_dmc %>% group_by(ISO3, year) %>% summarise(DMC_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop")

## Join DMC + population + HDI -------------------------------------------------
df <- dmc_total %>%
  left_join(df_pop, by = c("ISO3", "year")) %>%
  left_join(df_gdp, by = c("ISO3", "year")) |>
  left_join(hdi, by = c("ISO3", "year")) %>%
  filter(!is.na(hdi), !is.na(population), population > 0, DMC_Mt > 0) %>%
  mutate(dmc_pc = (DMC_Mt * 1e6) / population) |> # tonnes per capita
  mutate(dmc_gdp = (DMC_Mt * 1e9) / GDP_2015USD) # kg per USD

## Country subset and metadata -------------------------------------------------
# Semantically grouped by region; filtered to countries with good HDI coverage.

country_meta <- tibble::tribble(
  ~ISO3 , ~country_name , ~region                      ,
  # North America — dark navy
  "USA" , "USA"         , "North America"              ,
  "CAN" , "Canada"      , "North America"              ,
  # Europe & Russia — blues
  "DEU" , "Germany"     , "Europe & Russia"            ,
  # "FRA" , "France"      , "Europe & Russia"            ,
  # "ESP" , "Spain"       , "Europe & Russia"            ,
  "GBR" , "UK"          , "Europe & Russia"            ,
  "ITA" , "Italy"       , "Europe & Russia"            ,
  # "CHE" , "Switzerland" , "Europe & Russia"            ,
  "NOR" , "Norway"      , "Europe & Russia"            ,
  "RUS" , "Russia"      , "Europe & Russia"            ,
  # "UKR" , "Ukraine"     , "Europe & Russia"            ,
  "TUR" , "Turkey"      , "Europe & Russia"            ,
  # East Asia — reds / orange-reds
  "CHN" , "China"       , "East Asia"                  ,
  "JPN" , "Japan"       , "East Asia"                  ,
  "KOR" , "South Korea" , "East Asia"                  ,
  # "SGP" , "Singapore"   , "East Asia"                  ,
  # "VNM" , "Vietnam"     , "East Asia"                  ,
  # "IDN" , "Indonesia"   , "East Asia"                  ,
  # Latin America — greens
  "BRA" , "Brazil"      , "Latin America"              ,
  "MEX" , "Mexico"      , "Latin America"              ,
  "CHL" , "Chile"       , "Latin America"              ,
  # "COL" , "Colombia"    , "Latin America"              ,
  # Sub-Saharan Africa — amber / orange
  "ZAF" , "S. Africa"   , "Sub-Saharan Africa"         ,
  "NGA" , "Nigeria"     , "Sub-Saharan Africa"         ,
  "ETH" , "Ethiopia"    , "Sub-Saharan Africa"         ,
  "GHA" , "Ghana"       , "Sub-Saharan Africa"         ,
  # "COD" , "DR Congo"    , "Sub-Saharan Africa"         ,
  # Middle East & North Africa — gold / olive
  "EGY" , "Egypt"       , "Middle East & North Africa" ,
  # "MAR" , "Morocco"     , "Middle East & North Africa" ,
  "IRN" , "Iran"        , "Middle East & North Africa" ,
  # "ARE" , "UAE"         , "Middle East & North Africa" ,
  # "QAT" , "Qatar"       , "Middle East & North Africa" ,
  # "DZA" , "Algeria"     , "Middle East & North Africa" ,
  # South Asia — magentas
  "IND" , "India"       , "South Asia"                 ,
  # "BGD" , "Bangladesh"  , "South Asia"                 ,
  "PAK" , "Pakistan"    , "South Asia"                 ,
  # Oceania — cyan
  "AUS" , "Australia"   , "Oceania"
)

# Country-level palette: hue anchored to region, lightness/saturation varied
pal_countries <- c(
  # North America — dark navies
  "USA" = "#003A75",
  "CAN" = "#1565C0",
  # Europe & Russia — royal to sky blue spread
  "DEU" = "#0077BB",
  "FRA" = "#0033CC",
  "ESP" = "#55A0DD",
  "GBR" = "#004499",
  "ITA" = "#0099CC",
  "CHE" = "#00BBD4",
  "NOR" = "#115577",
  "RUS" = "#335588",
  "UKR" = "#6699BB",
  "TUR" = "#006699",
  # East Asia — strong red to orange-red
  "CHN" = "#CC2200",
  "JPN" = "#DD5522",
  "KOR" = "#881100",
  "SGP" = "#FF7755",
  "VNM" = "#BB4400",
  "IDN" = "#993311",
  # Latin America — forest to light green
  "BRA" = "#1B7B1B",
  "MEX" = "#44AA44",
  "CHL" = "#0D5C0D",
  "COL" = "#66CC66",
  # Sub-Saharan Africa — amber / burnt orange
  "ZAF" = "#EE7733",
  "NGA" = "#CC4411",
  "ETH" = "#FFAA44",
  "GHA" = "#DD7700",
  "COD" = "#DDAA00",
  # Middle East & North Africa — gold to olive
  "EGY" = "#CCAA00",
  "MAR" = "#886600",
  "IRN" = "#DDCC11",
  "ARE" = "#E8D044",
  "QAT" = "#AABB11",
  "DZA" = "#998833",
  # South Asia — magenta / deep pink
  "IND" = "#AA3377",
  "BGD" = "#CC55AA",
  "PAK" = "#882266",
  # Oceania — cyan
  "AUS" = "#00AADD"
)

countries_keep <- country_meta$ISO3

df_sel <- df %>% filter(ISO3 %in% countries_keep) %>% left_join(country_meta, by = "ISO3") %>% arrange(ISO3, year)

# Coverage check
df_sel %>%
  group_by(ISO3, country_name) %>%
  summarise(n = n(), yr_min = min(year), yr_max = max(year), .groups = "drop") %>%
  print(n = 40)

## Smooth: LOESS per country (span = 0.18, matching Figure 3 - Contour) -------
df_sel <- df_sel %>%
  group_by(ISO3) %>%
  arrange(year) %>%
  mutate(
    dmc_pc = tryCatch(predict(loess(dmc_pc ~ year, span = 0.18)), error = function(e) dmc_pc),
    hdi = tryCatch(predict(loess(hdi ~ year, span = 0.18)), error = function(e) hdi)
  ) %>%
  ungroup()

## Decade-mark points for alpha gradient (light = early, dark = recent) --------
pts_decade <- df_sel %>% filter(year %in% seq(1990, 2020, by = 10))

## Label data ------------------------------------------------------------------
labels_end <- df_sel %>% group_by(ISO3) %>% slice_max(year, n = 1) %>% ungroup()

label_pos <- tribble(
  ~ISO3 , ~nudge_x , ~nudge_y , ~hjust ,
  "USA" ,  3.0     ,  0.011   , 0      ,
  "AUS" , -3.0     ,  0.015   , 1      ,
  "NOR" , -3.5     ,  0.001   , 1      ,
  "CAN" ,  1.0     , -0.009   , 0      ,
  "CHL" ,  3.9     , -0.028   , 0      ,
  "DEU" ,  5.9     ,  0.012   , 0      ,
  "GBR" , -4.0     , -0.017   , 1      ,
  "ITA" , -3.5     , -0.014   , 1      ,
  "JPN" , -7.0     ,  0.005   , 1      ,
  "KOR" ,  6.0     ,  0.004   , 0      ,
  "RUS" , -2.5     ,  0.017   , 1      ,
  "TUR" ,  0.2     ,  0.006   , 0.5    ,
  "CHN" , -1.0     ,  0.025   , 1      ,
  "BRA" , -3.0     , -0.005   , 1      ,
  "MEX" , -2.5     , -0.005   , 1      ,
  "EGY" , -2.5     , -0.005   , 1      ,
  "IRN" , -2.0     ,  0.010   , 1      ,
  "ZAF" ,  2.0     ,  0.005   , 0      ,
  "GHA" ,  2.0     , -0.008   , 0      ,
  "IND" , -2.0     ,  0.005   , 1      ,
  "PAK" ,  2.0     , -0.020   , 0      ,
  "NGA" ,  2.0     , -0.010   , 0      ,
  "ETH" ,  2.0     , -0.077   , 0
)

labels_end <- labels_end %>% left_join(label_pos, by = "ISO3")

# Start and end year shown on one reference country (Norway: clear upward path)
labels_yr_ref <- df_sel %>% filter(ISO3 == "NOR") %>% slice(c(1, n()))

## Figure ----------------------------------------------------------------------

ggplot(df_sel, aes(x = dmc_pc, y = hdi, colour = ISO3, group = ISO3)) +
  # Trajectory paths with arrow at end
  geom_path(
    arrow = arrow(length = unit(0.11, "cm"), type = "closed", ends = "last"),
    linewidth = 0.5,
    lineend = "round",
    linejoin = "round"
  ) +
  # White background dots at decade marks
  geom_point(
    data   = pts_decade,
    shape  = 16,
    size   = 1.2,
    colour = "white",
    alpha  = 1,
    show.legend = FALSE
  ) +
  # Coloured decade dots: alpha gradient light (1990) → opaque (2020)
  geom_point(
    data = pts_decade,
    aes(alpha = year),
    shape = 16,
    size  = 1.2,
    show.legend = FALSE
  ) +
  # Country name labels at path endpoint
  geom_text(
  data = labels_end,
  aes(x = dmc_pc + nudge_x, y = hdi + nudge_y, label = country_name, hjust = hjust),
  size = 2.5,
  show.legend = FALSE
) +
  # Start and end year on reference country (Norway)
  # fmt: skip
  geom_text(data= labels_yr_ref,aes(label = year),colour   = "grey30",fontface = "bold",size= 3.5,vjust= c(1.8, -0.8),hjust= 0.5,show.legend = FALSE) +
  scale_colour_manual(values = pal_countries, guide = "none") +
  scale_alpha_continuous(range = c(0.25, 1), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(0, 70)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0.2, 1.0, 0.2), labels = label_number(accuracy = 0.1)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_pb_large() +
  labs(x = "Material Consumption per Capita (t/cap)", y = "Human Development Index") +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/Fig-HDI.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 , height = 8.7 )
ggsave("Figures/SVG/Fig-HDI.svg", ggplot2::last_plot(), units = "cm", width = 8.7, height = 8.7)
clean_svg("Figures/SVG/Fig-HDI.svg")

# EoF
