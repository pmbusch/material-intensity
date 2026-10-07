## =============================================================================
## 06-Aggregate_EIA_Capacity.R
## EIA International installed electricity capacity by country (1980-2024),
## aggregated to the 8 model regions.
##
## Input:  Inputs/EIA/INT-Export-10-06-2026_16-08-02.csv (EIA International,
##         "million kW" = GW; one header row per country, ISO3 inside the API code)
## Output: Parameters/EIA-Capacity/capacity_region_historical.csv
##           region, year, eia_tech, gw
##         eia_tech: nuclear, fossil, hydro, geothermal, solar, wind, biomass,
##                   tide_wave, pumped_storage (EIA aggregates; split into model
##                   technologies downstream, e.g. Figure 2)
## Missing values ("NA", "--") are 0: EIA reports most non-hydro renewables only
## from 2000 onward, when they were negligible.
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

EIA_FILE <- "Inputs/EIA/INT-Export-10-06-2026_16-08-02.csv"
OUT_DIR <- "Parameters/EIA-Capacity"

EIA_TECH <- c(
  "Nuclear (million kW)" = "nuclear",
  "Fossil fuels (million kW)" = "fossil",
  "Hydroelectricity (million kW)" = "hydro",
  "Geothermal (million kW)" = "geothermal",
  "Solar (million kW)" = "solar",
  "Wind (million kW)" = "wind",
  "Biomass and waste (million kW)" = "biomass",
  "Tide and wave (million kW)" = "tide_wave",
  "Hydroelectric pumped storage (million kW)" = "pumped_storage"
)
# EIA codes absent from Dict_Countries: pre-1991 Germany (West/East), Kosovo, Netherlands Antilles
EIA_EXTRA_REGION <- c("DEUW" = "Europe & Russia", "DDR" = "Europe & Russia", "XKS" = "Europe & Russia", "NLDA" = "Latin America")


# Step 1: Country -> model region --------------------------------------------------

country_region <- readxl::read_excel("Inputs/Dict_Countries.xlsx", sheet = "R10_Agg") |>
  dplyr::left_join(
    readxl::read_excel("Inputs/Dict_Countries.xlsx", sheet = "UNEP_Agg") |> dplyr::select(UNEP_Name = UNEP_name, ISO3),
    by = "UNEP_Name"
  ) |>
  dplyr::distinct(ISO3, region = Region) |>
  dplyr::filter(!is.na(ISO3), !is.na(region), region != "NA") |>
  dplyr::bind_rows(tibble::tibble(ISO3 = names(EIA_EXTRA_REGION), region = unname(EIA_EXTRA_REGION)))
stopifnot(!any(duplicated(country_region$ISO3)))


# Step 2: Read EIA, long format by country -------------------------------------------

eia_country <- readr::read_csv(EIA_FILE, skip = 1, col_types = readr::cols(.default = "c"), na = c("", "NA", "--")) |>
  dplyr::rename(api = 1, var = 2) |>
  dplyr::mutate(var = stringr::str_trim(var), ISO3 = stringr::str_match(api, "-7-([A-Z0-9]+)-MK")[, 2]) |>
  dplyr::filter(!is.na(ISO3), var %in% c(names(EIA_TECH), "Capacity (million kW)")) |>
  tidyr::pivot_longer(dplyr::matches("^[0-9]{4}$"), names_to = "year", values_to = "gw") |>
  dplyr::mutate(year = as.integer(year), gw = dplyr::coalesce(suppressWarnings(as.numeric(gw)), 0))

# Coverage: mapped countries vs EIA World total capacity
world_total <- eia_country |> dplyr::filter(ISO3 == "WORL", var == "Capacity (million kW)") |> dplyr::select(year, world_gw = gw)
mapped_total <- eia_country |>
  dplyr::filter(var == "Capacity (million kW)") |>
  dplyr::inner_join(country_region, by = "ISO3") |>
  dplyr::group_by(year) |>
  dplyr::summarise(mapped_gw = sum(gw), .groups = "drop")
coverage <- world_total |> dplyr::inner_join(mapped_total, by = "year") |> dplyr::mutate(share = mapped_gw / world_gw)
cat("EIA total capacity mapped to model regions (share of EIA World): min", round(min(coverage$share), 4), "| max", round(max(coverage$share), 4), "\n")
cat("Unmapped EIA codes:", paste(setdiff(unique(eia_country$ISO3), c(country_region$ISO3, "WORL")), collapse = ", "), "\n")


# Step 3: Aggregate to region x year x technology -------------------------------------

capacity_region <- eia_country |>
  dplyr::filter(var %in% names(EIA_TECH)) |>
  dplyr::inner_join(country_region, by = "ISO3") |>
  dplyr::mutate(eia_tech = unname(EIA_TECH[var])) |>
  dplyr::group_by(region, year, eia_tech) |>
  dplyr::summarise(gw = sum(gw), .groups = "drop") |>
  dplyr::arrange(region, eia_tech, year)

cat("\nWorld capacity by technology (GW), 1980 / 2000 / 2024:\n")
print(
  capacity_region |>
    dplyr::filter(year %in% c(1980L, 2000L, 2024L)) |>
    dplyr::group_by(eia_tech, year) |>
    dplyr::summarise(gw = round(sum(gw)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = year, values_from = gw) |>
    as.data.frame()
)


# Step 4: Save -------------------------------------------------------------------

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(capacity_region, file.path(OUT_DIR, "capacity_region_historical.csv"))
cat("\nSaved:", file.path(OUT_DIR, "capacity_region_historical.csv"), "(", nrow(capacity_region), "rows )\n")

# EoF
