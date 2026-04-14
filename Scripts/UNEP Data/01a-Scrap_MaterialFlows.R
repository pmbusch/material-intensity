# =============================================================================
# Scrape Material Flows Sunburst Data
# https://visualisations.materialflows.net/mf-shiny/?#shiny-tab-sunburst
# Domestric Material Extraction by Material Full detail
# Requirements: install.packages(c("chromote", "jsonlite", "tidyverse"))
# PBH Feb 2026

library(chromote)
library(jsonlite)
library(tidyverse)

b <- ChromoteSession$new()
b$Page$navigate("https://visualisations.materialflows.net/mf-shiny/")
Sys.sleep(12)


b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath("Inputs/MaterialFlows"))

# activate sunburst tab
b$Runtime$evaluate("document.querySelector(\"a[data-value='sunburst']\").click()")
Sys.sleep(3)

# Function for loop
activate_sunburst <- function(b) {
  is_active <- b$Runtime$evaluate(
    "document.querySelector(\"li.active a[data-value='sunburst']\") !== null"
  )$result$value

  if (!isTRUE(is_active)) {
    b$Runtime$evaluate("document.querySelector(\"a[data-value='sunburst']\").click()")
    Sys.sleep(2)
  }
}

# extract country list properly
# countries <- c("Albania", "Germany") # Debug
# Countries obtained from HTML web selector
library(stringr)
html <- readLines("Inputs/MaterialFlows/Countries.html", warn = FALSE)
html <- paste(html, collapse = "\n")
countries <- unique(str_match_all(html, 'data-value="([^"]+)"')[[1]][, 2])
length(countries) # 240
countries
years <- 1970:2024


# get real combination of countries-year
# fmt: skip
continents <- c("Africa", "Asia + Pacific", "EECCA", "Europe", "Latin America + Caribbean", "North America","West Asia","World") # remove them
df <- read.csv("Inputs/UNEP/mfa13_export.csv")
df <- df |>
  filter(Flow.code == "DE") |> # Domestic Extraction
  filter(!Country %in% continents) |> # remove continents
  pivot_longer(c(-Country, -Category, -Flow.name, -Flow.code, -Flow.unit), names_to = 'year', values_to = 'value') |>
  mutate(year = str_remove(year, "X") |> as.integer()) |>
  filter(value > 0)
grid <- df |> group_by(Country, year) |> tally() |> filter(n > 0) |> ungroup()
nrow(grid) # 11810

# Avoid downloading already downloaded files
# grid <- expand.grid(year = 2024:1970, Country = rev(countries), stringsAsFactors = FALSE)
# nrow(grid) # 13200 or 55*240
files <- list.files("Inputs/MaterialFlows", pattern = "\\.csv$", full.names = FALSE)
done <- data.frame(
  country = sub("^.*DomesticExtractionof(.+)in[0-9]{4}bymaterialgroup\\.csv$", "\\1", files),
  year = as.integer(sub("^.*in([0-9]{4})bymaterialgroup\\.csv$", "\\1", files)),
  stringsAsFactors = FALSE
)
nrow(done) # 12660

grid$country_search <- str_remove_all(grid$Country, "[:space:]") |> str_remove_all(" ") |> str_replace_all("\\/", "_")
todo <- subset(grid, !(paste0(year, country_search) %in% paste0(done$year, done$country)))
todo <- todo |> filter(Country != "North Korea")

nrow(todo) # 0


for (i in seq_len(nrow(todo))) {
  cn <- todo$Country[i]
  y <- todo$year[i]
  tryCatch(
    {
      message("Country: ", cn, " Year: ", y)

      activate_sunburst(b)

      # country (selectize)
      b$Runtime$evaluate(sprintf("$('#sel_country')[0].selectize.setValue(%s)", toJSON(cn, auto_unbox = TRUE)))
      Sys.sleep(2) # def was 2, works with 1

      # year (Shiny slider input)
      b$Runtime$evaluate(sprintf("Shiny.setInputValue('sel_year', %s, {priority: 'event'})", y))
      Sys.sleep(2) # DEFAULT WAS 4, works with 1

      # download
      b$Runtime$evaluate("document.querySelector('#downloadData').click()")
      Sys.sleep(2) # DEFAULT WAS 5, works with 1
    },
    error = function(e) {
      message("FAILED: ", cn, " ", y)
    }
  )
}
