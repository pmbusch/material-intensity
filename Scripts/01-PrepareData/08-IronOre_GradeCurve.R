## 08-IronOre_GradeCurve.R
## Cumulative ore vs ore grade curve for known iron ore deposits (S&P Capital IQ
## export), to gauge how far the iron ore grade could decline as future primary
## iron demand depletes the best deposits. Demand is not taken from the model
## (avoids circularity: model results depend on the grade).
##
## Per deposit, contained Fe and ore grade follow a hierarchy (first available):
##   1. Reserves & Resources (R&R)
##   2. Reserves + Total Resources excl Reserves
##   3. Measured & Indicated incl Reserves + Inferred
##   4. Measured & Indicated incl Reserves
##   5. Reserves
## Ore = contained Fe / grade. "Contained" in the export is Fe metal (checked:
## M&I contained = M&I ore tonnage x grade).
##
## Curve order: development group first, then grade high -> low. Group from
## DEV_STAGE: producing or in construction (operating, expansion, satellite,
## limited/residual production, commissioning, preproduction, construction)
## before all other stages. ACTV_STATUS is kept in the output but not used.
## All grades are kept (no plausibility filter).
##
## Tonnage scaled to USGS world resources (>800 Gt ore, >230 Gt Fe): every
## deposit's ore and Fe x (800 / S&P total ore). Grades and order unchanged.
##
## Demand cases: 2024 world primary Fe (UNEP ferrous ore DMC x GRADE_ORE_FE_2024)
## changing by -1, 0, +1, +2, +3 %/yr over 2025-2060. Vertical lines mark each
## case's cumulative Fe on the curve.
##
## Option B (default, b_* columns): grade in year t = average grade of the
## producing group's ore remaining after cumulative extraction to t (then of the
## other stages once it is used up), relative to the producing group's average.
## Option A (comparison): the grade in year t = Fe / ore of the slice mined over
## the 3 years t-2..t; relative grade = that grade / the case's 2025-2027 grade.
## 2060 grade target per case = GRADE_ORE_FE_2024 x option B relative grade 2060
## (collected with Cu and bauxite in 09-NonFerrous_GradeCurve.R).
##
## Inputs:  Inputs/SP/IronOre_*.xls
##          Parameters/UNEP-Materials/materials_region_DMC.csv
## Outputs: Parameters/SP/IronOre_deposits.csv
##          Parameters/SP/IronOre_GradeIndex.csv (growth_pct, year, option A and B grade and relative grade)
##          Figures/SP/IronOre_GradeCurve.png
##          Figures/SP/IronOre_GradeIndex.png

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
pb_set_geom_defaults("small")

STAGE_PRODUCING <- c(
  "Operating", "Expansion", "Satellite", "Limited Production", "Residual Production",
  "Commissioning", "Preproduction", "Construction Started", "Construction Planned"
)
DEMAND_START <- 2025L
DEMAND_END <- FORECAST_END # 2060
WINDOW <- 3L # years of production per grade slice
BASE_YEAR <- DEMAND_START + WINDOW - 1L # first full window = 2025-2027 ("today")
BASE_LABEL <- paste0(DEMAND_START, "-", BASE_YEAR)
SCALE_TO_ORE_GT <- 800 # USGS world resources: >800 Gt ore containing >230 Gt Fe
GROWTH_CASES <- c(-1, 0, 1, 2, 3) # %/yr change of primary Fe from its 2024 level


# Step 1: Read S&P export ------------------------------------------------

cat("STEP 1: Read S&P export\n")

sp_file <- tail(sort(list.files("Inputs/SP", pattern = "^IronOre.*\\.xls$", full.names = TRUE)), 1)
sp_raw <- read_excel(sp_file, col_names = FALSE, .name_repair = "minimal")

# Header rows: 1 = label, 2 = field code, 3 = year basis, 4 = commodity. Several
# fields are exported twice (identical), keep the first occurrence.
sp_codes <- unlist(sp_raw[2, ])
dep <- sp_raw[-(1:4), !duplicated(sp_codes)]
names(dep) <- sp_codes[!duplicated(sp_codes)]
dep <- dep %>%
  mutate(across(-c(PROP_NAME, PRIMARY_COMMODITY, ACTV_STATUS, DEV_STAGE), as.numeric)) %>%
  mutate(across(-c(PROP_NAME, PROP_ID, PRIMARY_COMMODITY, ACTV_STATUS, DEV_STAGE), ~ coalesce(.x, 0)))

cat("Deposits in file:", nrow(dep), "| Primary commodity = Iron Ore:", sum(dep$PRIMARY_COMMODITY == "Iron Ore"), "\n")


# Step 2: Hierarchy for contained Fe and grade --------------------------

cat("STEP 2: Contained Fe and ore per deposit\n")

# Ore of each component = contained Fe / grade; a component with contained Fe
# but no grade invalidates the tier (ore unknown)
dep <- dep %>%
  mutate(
    ore_rr = if_else(GRD_R_AND_R_PCT_TONNE > 0, CONTAINED_R_AND_R_PCT_TONNE / GRD_R_AND_R_PCT_TONNE * 100, NA_real_),
    ore_resv = if_else(GRD_RESV_PCT_TONNE > 0, CONTAINED_RESV_PCT_TONNE / GRD_RESV_PCT_TONNE * 100, if_else(CONTAINED_RESV_PCT_TONNE > 0, NA_real_, 0)),
    ore_res = if_else(GRD_TOTAL_RESRC_PCT_TONNE > 0, CONTAINED_TOTAL_RESRC_PCT_TONNE / GRD_TOTAL_RESRC_PCT_TONNE * 100, if_else(CONTAINED_TOTAL_RESRC_PCT_TONNE > 0, NA_real_, 0)),
    ore_mi = if_else(GRD_MEAS_IND_INCL_RESV_PCT_TONNE > 0, CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE / GRD_MEAS_IND_INCL_RESV_PCT_TONNE * 100, if_else(CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE > 0, NA_real_, 0)),
    ore_inf = if_else(GRD_INF_PCT_TONNE > 0, CONTAINED_INF_PCT_TONNE / GRD_INF_PCT_TONNE * 100, if_else(CONTAINED_INF_PCT_TONNE > 0, NA_real_, 0)),
    fe_t2 = CONTAINED_RESV_PCT_TONNE + CONTAINED_TOTAL_RESRC_PCT_TONNE,
    fe_t3 = CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE + CONTAINED_INF_PCT_TONNE,
    source = case_when(
      CONTAINED_R_AND_R_PCT_TONNE > 0 & !is.na(ore_rr) ~ "R&R",
      fe_t2 > 0 & !is.na(ore_resv + ore_res) ~ "Reserves + Resources",
      fe_t3 > 0 & !is.na(ore_mi + ore_inf) ~ "M&I + Inferred",
      CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE > 0 & !is.na(ore_mi) ~ "M&I",
      CONTAINED_RESV_PCT_TONNE > 0 & !is.na(ore_resv) ~ "Reserves",
      TRUE ~ NA_character_
    ),
    fe_Mt = case_when(
      source == "R&R" ~ CONTAINED_R_AND_R_PCT_TONNE,
      source == "Reserves + Resources" ~ fe_t2,
      source == "M&I + Inferred" ~ fe_t3,
      source == "M&I" ~ CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE,
      source == "Reserves" ~ CONTAINED_RESV_PCT_TONNE
    ) / 1e6,
    ore_Mt = case_when(
      source == "R&R" ~ ore_rr,
      source == "Reserves + Resources" ~ ore_resv + ore_res,
      source == "M&I + Inferred" ~ ore_mi + ore_inf,
      source == "M&I" ~ ore_mi,
      source == "Reserves" ~ ore_resv
    ) / 1e6,
    grade_pct = fe_Mt / ore_Mt * 100,
    group = if_else(DEV_STAGE %in% STAGE_PRODUCING, "Producing or in construction", "Other stages")
  )

cat("Deposits by source tier:\n")
print(dep %>% count(source, wt = NULL) %>% left_join(dep %>% group_by(source) %>% summarise(fe_Gt = sum(fe_Mt, na.rm = TRUE) / 1e3), by = "source"))

cat("Contained Fe (Gt) by primary commodity, deposits with data:\n")
print(dep %>% filter(!is.na(source)) %>% group_by(PRIMARY_COMMODITY) %>% summarise(n = n(), fe_Gt = sum(fe_Mt) / 1e3, grade = sum(fe_Mt) / sum(ore_Mt) * 100) %>% arrange(desc(fe_Gt)), n = 20)

dep_curve <- dep %>% filter(!is.na(source), PRIMARY_COMMODITY == "Iron Ore")

# Scale every deposit's ore and Fe by the same factor so total ore matches USGS
# (grades unchanged; 800 Gt ore at S&P grades also gives >230 Gt Fe)
scale_f <- SCALE_TO_ORE_GT * 1e3 / sum(dep_curve$ore_Mt)
cat("Tonnage scale factor:", round(scale_f, 2), "(S&P total", round(sum(dep_curve$ore_Mt) / 1e3), "Gt ore,", round(sum(dep_curve$fe_Mt) / 1e3), "Gt Fe)\n")
dep_curve <- dep_curve %>% mutate(ore_Mt = ore_Mt * scale_f, fe_Mt = fe_Mt * scale_f)

cat("Deposits by development stage:\n")
print(dep_curve %>% group_by(group, DEV_STAGE) %>% summarise(n = n(), fe_Gt = sum(fe_Mt) / 1e3, .groups = "drop") %>% arrange(group, desc(fe_Gt)), n = 30)


# Step 3: Cumulative curve ----------------------------------------------

cat("STEP 3: Cumulative curve\n")

dep_curve <- dep_curve %>%
  mutate(group = factor(group, levels = c("Producing or in construction", "Other stages"))) %>%
  arrange(group, desc(grade_pct)) %>%
  mutate(
    ore_end_Gt = cumsum(ore_Mt) / 1e3,
    ore_start_Gt = lag(ore_end_Gt, default = 0),
    fe_end_Gt = cumsum(fe_Mt) / 1e3,
    fe_start_Gt = lag(fe_end_Gt, default = 0)
  )

print(dep_curve %>% group_by(group) %>% summarise(n = n(), ore_Gt = sum(ore_Mt) / 1e3, fe_Gt = sum(fe_Mt) / 1e3, grade = fe_Gt / ore_Gt * 100))

dir.create("Parameters/SP", showWarnings = FALSE, recursive = TRUE)
dep_curve %>%
  dplyr::select(PROP_ID, PROP_NAME, PRIMARY_COMMODITY, ACTV_STATUS, DEV_STAGE, group, source, fe_Mt, ore_Mt, grade_pct, ore_start_Gt, ore_end_Gt, fe_end_Gt) %>%
  write_csv("Parameters/SP/IronOre_deposits.csv")


# Step 4: Primary Fe demand cases from the 2024 level -------------------

cat("STEP 4: Primary Fe demand cases\n")

# 2024 primary Fe = world ferrous ore DMC x 2024 ore-to-metal grade. No model
# results are used, so the grade decline does not depend on its own outcome.
fe_2024_Gt <- read_csv("Parameters/UNEP-Materials/materials_region_DMC.csv", show_col_types = FALSE) %>%
  filter(material_category == "Ferrous ores", year == 2024L) %>%
  summarise(fe_Gt = sum(DMC_Mt) * GRADE_ORE_FE_2024 / 1e3) %>%
  pull(fe_Gt)
cat("2024 primary Fe:", round(fe_2024_Gt, 2), "Gt Fe\n")

demand_year <- expand_grid(growth_pct = GROWTH_CASES, year = DEMAND_START:DEMAND_END) %>%
  mutate(fe_Gt = fe_2024_Gt * (1 + growth_pct / 100)^(year - 2024L))

# Ore position on the curve where cumulative contained Fe reaches the demand
# (linear within a deposit: constant grade)
demand <- demand_year %>%
  group_by(growth_pct) %>%
  summarise(fe_Gt = sum(fe_Gt), .groups = "drop") %>%
  mutate(ore_Gt = approx(c(0, dep_curve$fe_end_Gt), c(0, dep_curve$ore_end_Gt), xout = fe_Gt)$y)
print(demand)


# Step 5: Grade change per demand case ----------------------------------

cat("STEP 5: Grade change, option B (default) and option A\n")

# Option A: the Fe mined over years t-2..t takes a slice of the curve; ore of
# the slice from the cumulative Fe -> cumulative ore relation
grade_year <- demand_year %>%
  arrange(growth_pct, year) %>%
  group_by(growth_pct) %>%
  mutate(cum_end_Gt = cumsum(fe_Gt), cum_start_Gt = lag(cum_end_Gt, WINDOW, default = 0)) %>%
  ungroup() %>%
  mutate(
    ore_Gt = approx(c(0, dep_curve$fe_end_Gt), c(0, dep_curve$ore_end_Gt), xout = cum_end_Gt)$y -
      approx(c(0, dep_curve$fe_end_Gt), c(0, dep_curve$ore_end_Gt), xout = cum_start_Gt)$y,
    grade_pct = if_else(year >= BASE_YEAR, (cum_end_Gt - cum_start_Gt) / ore_Gt * 100, NA_real_)
  ) %>%
  group_by(growth_pct) %>%
  mutate(grade_rel = grade_pct / grade_pct[year == BASE_YEAR]) %>%
  ungroup()

# Option B (default): grade = average grade of the producing group's ore that
# remains after cumulative extraction (best first); once that group is used up,
# of the remaining ore in other stages. Today = the producing group's average.
# NA once all known resources are used up.
prod_end <- dep_curve %>% filter(group == "Producing or in construction") %>% summarise(ore = max(ore_end_Gt), fe = max(fe_end_Gt))
tot_end <- dep_curve %>% summarise(ore = max(ore_end_Gt), fe = max(fe_end_Gt))
grade_b_today <- prod_end$fe / prod_end$ore * 100
grade_year <- grade_year %>%
  mutate(
    ore_cum_Gt = approx(c(0, dep_curve$fe_end_Gt), c(0, dep_curve$ore_end_Gt), xout = cum_end_Gt)$y,
    b_grade_pct = case_when(
      ore_cum_Gt < prod_end$ore ~ (prod_end$fe - cum_end_Gt) / (prod_end$ore - ore_cum_Gt),
      ore_cum_Gt < tot_end$ore ~ (tot_end$fe - cum_end_Gt) / (tot_end$ore - ore_cum_Gt)
    ) * 100,
    b_rel = b_grade_pct / grade_b_today
  )
cat("Option B, today (producing-group average):", round(grade_b_today, 1), "% Fe\n")

grade_index <- grade_year %>% dplyr::select(growth_pct, year, fe_Gt, grade_pct, grade_rel, b_grade_pct, b_rel)
print(grade_index %>% filter(year %in% c(2030, 2040, 2050, 2060)) %>% arrange(year, growth_pct), n = 20)
write_csv(grade_index, "Parameters/SP/IronOre_GradeIndex.csv")

# 2060 grade target per case (all metals collected in 09-NonFerrous_GradeCurve.R)
cat("2060 grade target (GRADE_ORE_FE_2024 x option B relative grade):\n")
print(grade_index %>% filter(year == DEMAND_END) %>% transmute(growth_pct, rel_grade_2060 = b_rel, grade_2060 = GRADE_ORE_FE_2024 * b_rel))


# Figure: grade curve ---------------------------------------------------

dir.create("Figures/SP", showWarnings = FALSE, recursive = TRUE)

central <- demand %>% filter(growth_pct == 0)
rel_central <- grade_index$b_rel[grade_index$year == DEMAND_END & grade_index$growth_pct == 0]
# Band ends: lowest (-1 %/yr) and highest (+3 %/yr) demand case
rel_band <- grade_index$b_rel[grade_index$year == DEMAND_END & grade_index$growth_pct %in% range(GROWTH_CASES)]
group_lab <- dep_curve %>%
  group_by(group) %>%
  summarise(x = (max(min(ore_start_Gt), max(demand$ore_Gt)) + max(ore_end_Gt)) / 2, y = 8, label = paste0(sub(" or ", " or\n", first(group)), "\n(", n(), " deposits)"), .groups = "drop")

ggplot(dep_curve) +
  annotate("rect", xmin = min(demand$ore_Gt), xmax = max(demand$ore_Gt), ymin = 0, ymax = Inf, fill = "grey85") +
  geom_rect(aes(xmin = ore_start_Gt, xmax = ore_end_Gt, ymin = 0, ymax = grade_pct, fill = group)) +
  geom_step(
    data = bind_rows(dep_curve, tail(dep_curve, 1) %>% mutate(ore_start_Gt = ore_end_Gt)),
    aes(x = ore_start_Gt, y = grade_pct), direction = "hv", linewidth = 0.2, colour = "black"
  ) +
  geom_vline(xintercept = central$ore_Gt, linetype = "dashed", linewidth = 0.4) +
  annotate(
    "text", x = max(demand$ore_Gt) + 5, y = 84, hjust = 0, vjust = 1,
    label = paste0("Primary Fe ", DEMAND_START, "-", DEMAND_END, " at 2024 level:\n", round(central$fe_Gt), " Gt Fe (shaded: -1 to +3%/yr)\n",
                   "Grade ", DEMAND_END, " vs today: ", sprintf("%+.0f%%", (rel_central - 1) * 100),
                   "\n(band: ", sprintf("%+.0f%%", (rel_band[1] - 1) * 100), " to ", sprintf("%+.0f%%", (rel_band[2] - 1) * 100), ")"),
    size = pb_annot_size("small")
  ) +
  geom_text(data = group_lab, aes(x = x, y = y, label = label), colour = "white", size = pb_annot_size("small")) +
  scale_fill_manual(values = c("Producing or in construction" = "#8B2E1F", "Other stages" = "#D08770"), guide = "none") +
  scale_x_continuous(labels = comma, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 70, 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 85)) + # headroom for the demand label
  labs(
    title = "Iron ore grade curve",
    subtitle = paste0("By stage, then grade; scaled to USGS ", SCALE_TO_ORE_GT, " Gt ore"),
    x = "Cumulative ore (Gt)", y = "Ore grade (% Fe)"
  ) +
  theme_pb_small()

ggsave("Figures/SP/IronOre_GradeCurve.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)


# Figure: grade relative to today, option B -----------------------------

index_band <- grade_index %>%
  group_by(year) %>%
  summarise(lo = min(b_rel), hi = max(b_rel), mid = b_rel[growth_pct == 0], .groups = "drop")

ggplot(index_band, aes(x = year)) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#8B2E1F", alpha = 0.25) +
  geom_line(aes(y = mid), colour = "#8B2E1F", linewidth = 0.8) +
  annotate(
    "text", x = 2040, y = 0.87, hjust = 0.5,
    label = paste0("2024 level held: ", sprintf("%+.0f%%", (rel_central - 1) * 100), " by ", DEMAND_END, "\n(shaded: -1 to +3%/yr)"),
    colour = "#8B2E1F", size = pb_annot_size("small")
  ) +
  scale_x_continuous(breaks = c(DEMAND_START, 2030, 2040, 2050, 2060), expand = expansion(mult = c(0, 0.04))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Iron ore grade change",
    subtitle = "Average grade of the remaining producing-group ore",
    x = NULL, y = "Grade relative to today"
  ) +
  theme_pb_small()

ggsave("Figures/SP/IronOre_GradeIndex.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)

# EoF
