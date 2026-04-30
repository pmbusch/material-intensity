## =============================================================================
## 03a_preprocess_ssp_drivers.R
## Produces a clean annual GDP and population dataframe by region and SSP
## scenario, reusable by all material forecast modules.
##
## Input:
##   Inputs/IIASA_SSP/ssp_basic_drivers_release_3.2_full.xlsx  (sheet: data)
##   Inputs/Dict_Countries.xlsx                                  (sheet: IIASA_Agg)
##
## Outputs (Parameters/IIASA/):
##   ssp_drivers.csv  — long format: scenario, region, year, variable, value, index
##   ssp_drivers.rds  — same content, RDS format for fast downstream loading
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")

DRIVER_VARIABLES <- c("Population", "GDP|PPP [per capita]", "GDP|PPP")
BASE_YEAR <- 2024L


# Step 1: Load IIASA SSP basic drivers ────────────────────────────────────────

cat("STEP 1: Load IIASA SSP basic drivers\n")

# Read all columns as text first: readxl silently coerces year columns 2030+
# to logical when the first rows look like binary values. Convert explicitly.
iiasa_raw <- read_excel("Inputs/IIASA_SSP/ssp_basic_drivers_release_3.2_full.xlsx", sheet = "data", col_types = "text")
cat("Raw file:", nrow(iiasa_raw), "rows x", ncol(iiasa_raw), "cols\n")

names(iiasa_raw) <- tolower(names(iiasa_raw))
yr_cols <- names(iiasa_raw)[grepl("^[0-9]{4}$", names(iiasa_raw))]
iiasa_raw <- iiasa_raw %>% mutate(across(all_of(yr_cols), as.numeric))
str(iiasa_raw)

cat("Year columns:", paste(yr_cols, collapse = ", "), "\n")
unique(iiasa_raw$variable)

# Step 2: Filter to SSP marker models and driver variables ────────────────────

cat("\nSTEP 2: Filter to marker models and driver variables\n")

cat("Scenarios in file:", paste(sort(unique(iiasa_raw$scenario)), collapse = ", "), "\n")

# Detect scenario names directly from the file — no hardcoded strings
HIST_SCENARIO <- unique(iiasa_raw$scenario)[str_detect(unique(iiasa_raw$scenario), "(?i)hist")]
SSP_SCENARIOS <- sort(unique(iiasa_raw$scenario)[str_detect(unique(iiasa_raw$scenario), "^SSP[1-5]")])

cat("Historical scenario(s) detected:", paste(HIST_SCENARIO, collapse = ", "), "\n")
cat("SSP scenarios detected:", paste(SSP_SCENARIOS, collapse = ", "), "\n")

iiasa_filtered <- iiasa_raw %>%
  filter(variable %in% DRIVER_VARIABLES, scenario %in% c(HIST_SCENARIO, SSP_SCENARIOS)) %>%
  mutate(ssp = scenario)

cat("Rows after model + variable filter:", nrow(iiasa_filtered), "\n")
cat("SSPs present:", paste(sort(unique(iiasa_filtered$ssp)), collapse = ", "), "\n")
cat("Variables:", paste(sort(unique(iiasa_filtered$variable)), collapse = ", "), "\n")


# Step 3: Load IIASA_Agg country → region mapping ─────────────────────────────

cat("\nSTEP 3: Load IIASA_Agg mapping\n")

dict_iiasa <- read_excel("Inputs/Dict_Countries.xlsx", sheet = "IIASA_Agg")
cat("IIASA_Agg columns:", paste(names(dict_iiasa), collapse = ", "), "\n")

# Auto-detect the IIASA country name key column (first column that is not Region or ISO3)
iiasa_name_col <- setdiff(names(dict_iiasa), c("Region", "ISO3"))[1]
cat("Country name key column detected:", iiasa_name_col, "\n")

dict_clean <- dict_iiasa %>%
  rename(iiasa_name = all_of(iiasa_name_col)) %>%
  filter(!is.na(Region)) %>%
  select(iiasa_name, Region)

cat("Countries with region assignment:", nrow(dict_clean), "\n")
cat("Regions:", paste(sort(unique(dict_clean$Region)), collapse = ", "), "\n")


# Step 4: Pivot years to long and join region mapping ─────────────────────────

cat("\nSTEP 4: Pivot to long and join region mapping\n")

iiasa_long_raw <- iiasa_filtered %>%
  pivot_longer(cols = all_of(yr_cols), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(value)) %>%
  rename(iiasa_region = region) %>%
  select(ssp, iiasa_region, variable, year, value)

# Historical (≤ BASE_YEAR) anchors the 2024 base value; SSPs diverge after.
# Replicate the historical prefix for every SSP so each is a complete series.
iiasa_hist_prefix <- iiasa_long_raw %>%
  filter(ssp %in% HIST_SCENARIO, year <= BASE_YEAR) %>%
  select(iiasa_region, variable, year, value)

iiasa_long <- bind_rows(
  purrr::map_dfr(SSP_SCENARIOS, \(s) iiasa_hist_prefix %>% mutate(ssp = s)),
  iiasa_long_raw %>% filter(ssp %in% SSP_SCENARIOS, year > BASE_YEAR)
) %>%
  left_join(dict_clean, by = c("iiasa_region" = "iiasa_name")) %>%
  filter(Region != "NA")

# Diagnostic: report IIASA country names not found in the dictionary
unmatched_regions <- iiasa_filtered %>%
  distinct(region) %>%
  anti_join(dict_clean, by = c("region" = "iiasa_name")) %>%
  pull(region) %>%
  sort()
if (length(unmatched_regions) > 0) {
  cat(
    "[NOTE] IIASA regions not matched in IIASA_Agg (",
    length(unmatched_regions),
    "):",
    paste(head(unmatched_regions, 10), collapse = ", "),
    if (length(unmatched_regions) > 10) "..." else "",
    "\n"
  )
}

iiasa_long <- iiasa_long %>% filter(!is.na(Region))
cat("Rows after dropping unmatched regions:", nrow(iiasa_long), "\n")


# Step 5: Aggregate countries to project regions ───────────────────────────────

cat("\nSTEP 5: Aggregate to project regions by summing country values\n")

iiasa_region <- iiasa_long %>%
  group_by(ssp, Region, variable, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

cat("Regional rows:", nrow(iiasa_region), "\n")
cat("Regions:", paste(sort(unique(iiasa_region$Region)), collapse = ", "), "\n")

head(iiasa_region)

# Step 6: Interpolate 5-year grid to annual resolution (linear) ───────────────

cat("\nSTEP 6: Interpolate 5-year intervals to annual (linear)\n")

annual_years <- seq(min(iiasa_region$year), max(iiasa_region$year))

iiasa_annual <- iiasa_region %>%
  group_by(ssp, Region, variable) %>%
  nest() %>%
  mutate(
    annual_df = purrr::map(data, function(d) {
      d <- d %>% arrange(year)
      interp <- approx(x = d$year, y = d$value, xout = annual_years, method = "linear", rule = 2)
      tibble(year = as.integer(interp$x), value = interp$y)
    })
  ) %>%
  select(-data) %>%
  unnest(annual_df) %>%
  ungroup()

cat("Annual rows:", nrow(iiasa_annual), "\n")
cat("Year range:", min(iiasa_annual$year), "-", max(iiasa_annual$year), "\n")


# Step 7: Compute index normalised to BASE_YEAR = 1 ───────────────────────────

cat("\nSTEP 7: Compute index (base year", BASE_YEAR, "= 1)\n")


base_vals <- iiasa_annual %>% filter(year == BASE_YEAR) %>% select(ssp, Region, variable, base_value = value)

iiasa_indexed <- iiasa_annual %>%
  left_join(base_vals, by = c("ssp", "Region", "variable")) %>%
  mutate(index = if_else(!is.na(base_value) & base_value > 0, value / base_value, NA_real_)) %>%
  select(-base_value) %>%
  rename(scenario = ssp, region = Region)

na_index <- sum(is.na(iiasa_indexed$index))
if (na_index > 0) {
  cat("[NOTE]", na_index, "rows with NA index (zero or missing base value)\n")
}


# Step 8: Save outputs ────────────────────────────────────────────────────────

cat("\nSTEP 8: Save outputs to Parameters/IIASA/\n")


write_csv(iiasa_indexed, "Parameters/IIASA/ssp_drivers.csv")
cat("  Saved: Parameters/IIASA/ssp_drivers.csv (", nrow(iiasa_indexed), "rows )\n")


# Step 9: Summary check ───────────────────────────────────────────────────────

cat("\n── Sanity check ────────────────────────────────────────────────────────────\n")
cat(
  "  Rows:",
  nrow(iiasa_indexed),
  "| Year range:",
  min(iiasa_indexed$year),
  "-",
  max(iiasa_indexed$year),
  "| Scenarios:",
  paste(sort(unique(iiasa_indexed$scenario)), collapse = ", "),
  "\n"
)
cat("  Regions:", paste(sort(unique(iiasa_indexed$region)), collapse = ", "), "\n")
cat("  NA check — value:", sum(is.na(iiasa_indexed$value)), "| index:", sum(is.na(iiasa_indexed$index)), "\n")

cat("\n  GDP index at base year", BASE_YEAR, "(should all equal 1.0):\n")
print(
  iiasa_indexed %>%
    filter(variable == "GDP|PPP", year == BASE_YEAR) %>%
    select(scenario, region, index) %>%
    pivot_wider(names_from = scenario, values_from = index) %>%
    arrange(region),
  n = 30
)

# Figure ---------------
library(geomtextpath)

library(geomtextpath)

iiasa_indexed |>
  filter(year <= 2060) |>
  filter(year >= 1970) |>
  filter(variable != "GDP|PPP") |>
  mutate(variable = factor(variable, levels = c("Population", "GDP|PPP [per capita]"))) |>
  ggplot(aes(year, index)) +
  # Historical line (1960–2024): black, all scenarios overlap so just use SSP1 to avoid duplication
  geom_line(data = ~ filter(.x, year <= 2024, scenario == "SSP1"), color = "black") +
  # Projections (2024–2060): colored by scenario
  geom_line(data = ~ filter(.x, year >= 2024), aes(col = scenario)) +
  # Labels only on East Asia facet
  geom_textline(
    data = ~ filter(.x, year >= 2024, region == "Sub-Saharan Africa", variable == "GDP|PPP [per capita]"),
    aes(col = scenario, label = scenario),
    hjust = 0.85,
    vjust = -0.1, # above the line; negative = above
    text_smoothing = 30,
    show.legend = FALSE,
    linewidth = NA # suppress duplicate line (already drawn above)
  ) +
  facet_grid(variable ~ region, scales = "free") +
  scale_color_manual(values = SSP_COLORS) +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Year", y = "Base 2024 = 1", color = "") +
  theme_pb_large() +
  theme(legend.position = c(0.05, 0.9))

# fmt: skip
ggsave("Figures/IIASA/BasicDrivers.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*4.2, height = 8.7*1.6)

# EoF
