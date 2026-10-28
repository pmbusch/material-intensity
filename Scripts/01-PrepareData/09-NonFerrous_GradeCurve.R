## 09-NonFerrous_GradeCurve.R
## Same analysis as 08-IronOre_GradeCurve.R for copper and bauxite: a
## cumulative ore vs ore grade curve of known deposits (S&P Capital IQ exports)
## and the grade decline as future ore extraction depletes them. Collects the
## 2060 grade targets of iron, copper and bauxite.
## Gold was dropped: S&P coverage too low for the demand.
##
## Per deposit, contained metal and ore grade follow the same hierarchy as iron
## (R&R -> Reserves + Resources -> M&I + Inferred -> M&I -> Reserves); ore =
## contained / grade. Units: copper % Cu, bauxite % Al2O3. Only deposits whose
## primary commodity is the material. Curve order: producing or in construction
## (DEV_STAGE) first, then grade high -> low. All grades kept.
##
## Tonnage scaled to USGS totals (grades and order unchanged; every deposit's
## ore and contained metal x the same factor):
##   Bauxite: S&P 36 Gt ore (106 of 278 deposits report tonnage) -> 65 Gt ore
##            (midpoint of USGS 55-75 Gt), factor 1.8.
##   Copper:  S&P 3.0 Gt contained Cu -> 5.0 Gt Cu (USGS 2015 assessment: 1.5 Gt
##            identified + 3.5 Gt undiscovered), factor 1.64. Assumes
##            undiscovered deposits share the known grade distribution.
##
## Demand (gross ore, no model results, so no circularity): 2024 world ore DMC
## of the material (UNEP non-ferrous ore DMC split by 07-NonFerrous_Split.R),
## changing by -1, 0, +1, +2, +3 %/yr over 2025-2060.
##
## Option B (default, b_* columns): grade in year t = average grade of the
## producing group's remaining ore after cumulative extraction to t (then of the
## other stages), relative to the producing group's average today.
## Option A (comparison): demand is ore, so the curve is sliced by ore: grade in
## year t = metal / ore of the slice mined over t-2..t, relative to 2025-2027.
## Grades beyond the end of known resources are NA.
## 2060 grade targets: 2024 model grade (GRADE_ORE_*_2024) x option B relative
## grade 2060, per case, for iron, copper and bauxite (bauxite: the relative
## decline of % Al2O3 is applied to the Al grade).
##
## Inputs:  Inputs/SP/{Copper,Bauxite}_*.xls
##          Parameters/MaterialFlows/nonfe_split_region.csv (07-NonFerrous_Split.R)
##          Parameters/SP/IronOre_GradeIndex.csv (08-IronOre_GradeCurve.R)
## Outputs: Parameters/SP/{Copper,Bauxite}_deposits.csv (tonnage scaled)
##          Parameters/SP/NonFerrous_GradeIndex.csv (material, growth_pct, year, option A and B grades)
##          Parameters/SP/GradeTargets_2060.csv (metal, growth_pct, rel_grade_2060, grade_2024, grade_2060)
##          Figures/SP/{Copper,Bauxite}_GradeCurve.png
##          Figures/SP/Metals_GradeIndex.png (option B), Metals_GradeIndex_AvsB.png (iron, copper, bauxite)

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
pb_set_geom_defaults("small")

STAGE_PRODUCING <- c(
  "Operating", "Expansion", "Satellite", "Limited Production", "Residual Production",
  "Commissioning", "Preproduction", "Construction Started", "Construction Planned"
)
DEMAND_START <- 2025L
DEMAND_END <- FORECAST_END # 2060
WINDOW <- 3L # years of production per grade slice
BASE_YEAR <- DEMAND_START + WINDOW - 1L # first full window = 2025-2027
BASE_LABEL <- paste0(DEMAND_START, "-", BASE_YEAR)
GROWTH_CASES <- c(-1, 0, 1, 2, 3) # %/yr change of ore demand from its 2024 level

# grade_div: grade unit -> mass fraction (% -> /100)
# scale_to_Gt / scale_basis: USGS total the curve is scaled to, in ore or in
# contained metal (NA = S&P tonnage as reported). Copper = 5.0 Gt contained Cu
# (USGS 2015 assessment: 1.5 identified + 3.5 undiscovered); bauxite = 55-75 Gt
# ore, midpoint. nonfe_sub: category in nonfe_split_region.csv
# fmt: skip
MATS <- tribble(
  ~material,  ~file_pattern, ~nonfe_sub, ~unit,      ~grade_div, ~scale_to_Gt, ~scale_basis,
  "Copper",   "^Copper",     "Copper",   "% Cu",     100,        1.5 + 3.5,    "metal",
  "Bauxite",  "^Bauxite",    "Bauxite",  "% Al2O3",  100,        65,           "ore"
)


# Step 1: 2024 ore demand by material -----------------------------------

cat("STEP 1: 2024 world ore DMC by material\n")

ore_2024 <- read_csv("Parameters/MaterialFlows/nonfe_split_region.csv", show_col_types = FALSE) %>%
  filter(year == 2024L) %>%
  group_by(nonfe_sub) %>%
  summarise(ore_2024_Gt = sum(DMC_Mt) / 1e3, .groups = "drop")
MATS <- MATS %>% left_join(ore_2024, by = "nonfe_sub")
print(MATS %>% dplyr::select(material, ore_2024_Gt))

dir.create("Parameters/SP", showWarnings = FALSE, recursive = TRUE)
dir.create("Figures/SP", showWarnings = FALSE, recursive = TRUE)
grade_index_all <- list()

# Loop over materials ---------------------------------------------------

for (m in seq_len(nrow(MATS))) {
  mat <- MATS$material[m]
  cat("\n=====", mat, "=====\n")

  ## Read S&P export ----

  # Header rows: 1 = label, 2 = field code, 3 = year basis, 4 = commodity.
  # Duplicated fields kept once.
  sp_file <- tail(sort(list.files("Inputs/SP", pattern = paste0(MATS$file_pattern[m], ".*\\.xls$"), full.names = TRUE, ignore.case = TRUE)), 1)
  sp_raw <- read_excel(sp_file, col_names = FALSE, .name_repair = "minimal")
  sp_codes <- unlist(sp_raw[2, ])
  dep <- sp_raw[-(1:4), !duplicated(sp_codes)]
  names(dep) <- sp_codes[!duplicated(sp_codes)]
  dep <- dep %>%
    mutate(across(-c(PROP_NAME, PRIMARY_COMMODITY, ACTV_STATUS, DEV_STAGE, COUNTRY_NAME), as.numeric)) %>%
    mutate(across(-c(PROP_NAME, PROP_ID, PRIMARY_COMMODITY, ACTV_STATUS, DEV_STAGE, COUNTRY_NAME), ~ coalesce(.x, 0)))

  cat("Deposits in file:", nrow(dep), "| Primary commodity =", mat, ":", sum(dep$PRIMARY_COMMODITY == mat), "\n")

  ## Hierarchy for contained metal and grade ----

  # Ore of each component = contained / grade; a component with contained metal
  # but no grade invalidates the tier (ore unknown)
  gd <- MATS$grade_div[m]
  dep <- dep %>%
    mutate(
      ore_rr = if_else(GRD_R_AND_R_PCT_TONNE > 0, CONTAINED_R_AND_R_PCT_TONNE / GRD_R_AND_R_PCT_TONNE * gd, NA_real_),
      ore_resv = if_else(GRD_RESV_PCT_TONNE > 0, CONTAINED_RESV_PCT_TONNE / GRD_RESV_PCT_TONNE * gd, if_else(CONTAINED_RESV_PCT_TONNE > 0, NA_real_, 0)),
      ore_res = if_else(GRD_TOTAL_RESRC_PCT_TONNE > 0, CONTAINED_TOTAL_RESRC_PCT_TONNE / GRD_TOTAL_RESRC_PCT_TONNE * gd, if_else(CONTAINED_TOTAL_RESRC_PCT_TONNE > 0, NA_real_, 0)),
      ore_mi = if_else(GRD_MEAS_IND_INCL_RESV_PCT_TONNE > 0, CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE / GRD_MEAS_IND_INCL_RESV_PCT_TONNE * gd, if_else(CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE > 0, NA_real_, 0)),
      ore_inf = if_else(GRD_INF_PCT_TONNE > 0, CONTAINED_INF_PCT_TONNE / GRD_INF_PCT_TONNE * gd, if_else(CONTAINED_INF_PCT_TONNE > 0, NA_real_, 0)),
      metal_t2 = CONTAINED_RESV_PCT_TONNE + CONTAINED_TOTAL_RESRC_PCT_TONNE,
      metal_t3 = CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE + CONTAINED_INF_PCT_TONNE,
      source = case_when(
        CONTAINED_R_AND_R_PCT_TONNE > 0 & !is.na(ore_rr) ~ "R&R",
        metal_t2 > 0 & !is.na(ore_resv + ore_res) ~ "Reserves + Resources",
        metal_t3 > 0 & !is.na(ore_mi + ore_inf) ~ "M&I + Inferred",
        CONTAINED_MEAS_IND_INCL_RESV_PCT_TONNE > 0 & !is.na(ore_mi) ~ "M&I",
        CONTAINED_RESV_PCT_TONNE > 0 & !is.na(ore_resv) ~ "Reserves",
        TRUE ~ NA_character_
      ),
      metal_Mt = case_when(
        source == "R&R" ~ CONTAINED_R_AND_R_PCT_TONNE,
        source == "Reserves + Resources" ~ metal_t2,
        source == "M&I + Inferred" ~ metal_t3,
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
      grade = metal_Mt / ore_Mt * gd, # in the material's grade unit
      group = if_else(DEV_STAGE %in% STAGE_PRODUCING, "Producing or in construction", "Other stages")
    )

  cat("Deposits by source tier:\n")
  print(dep %>% filter(!is.na(source)) %>% group_by(source) %>% summarise(n = n(), ore_Gt = sum(ore_Mt) / 1e3, .groups = "drop"))
  cat("Ore (Gt) by primary commodity, deposits with data:\n")
  print(dep %>% filter(!is.na(source)) %>% group_by(PRIMARY_COMMODITY) %>% summarise(n = n(), ore_Gt = sum(ore_Mt) / 1e3, grade = sum(metal_Mt) / sum(ore_Mt) * gd) %>% arrange(desc(ore_Gt)), n = 8)

  ## Cumulative curve ----

  dep_curve <- dep %>% filter(!is.na(source), PRIMARY_COMMODITY == mat)

  # Scale every deposit's ore and contained metal by the same factor so the
  # total matches an external resource estimate (grades unchanged)
  scale_f <- if (is.na(MATS$scale_to_Gt[m])) 1 else if (MATS$scale_basis[m] == "metal") MATS$scale_to_Gt[m] * 1e3 / sum(dep_curve$metal_Mt) else MATS$scale_to_Gt[m] * 1e3 / sum(dep_curve$ore_Mt)
  cat("Tonnage scale factor:", round(scale_f, 2), "(S&P total", round(sum(dep_curve$ore_Mt) / 1e3, 1), "Gt ore,", round(sum(dep_curve$metal_Mt) / 1e3, 2), "Gt metal)\n")

  dep_curve <- dep_curve %>%
    mutate(ore_Mt = ore_Mt * scale_f, metal_Mt = metal_Mt * scale_f) %>%
    mutate(group = factor(group, levels = c("Producing or in construction", "Other stages"))) %>%
    arrange(group, desc(grade)) %>%
    mutate(
      ore_end_Gt = cumsum(ore_Mt) / 1e3,
      ore_start_Gt = lag(ore_end_Gt, default = 0),
      metal_end_Gt = cumsum(metal_Mt) / 1e3
    )

  print(dep_curve %>% group_by(group) %>% summarise(n = n(), ore_Gt = sum(ore_Mt) / 1e3, metal_Gt = sum(metal_Mt) / 1e3, grade = metal_Gt / ore_Gt * gd))

  dep_curve %>%
    dplyr::select(PROP_ID, PROP_NAME, PRIMARY_COMMODITY, COUNTRY_NAME, ACTV_STATUS, DEV_STAGE, group, source, metal_Mt, ore_Mt, grade, ore_start_Gt, ore_end_Gt, metal_end_Gt) %>%
    write_csv(paste0("Parameters/SP/", mat, "_deposits.csv"))

  ## Ore demand cases ----

  ore_year <- expand_grid(growth_pct = GROWTH_CASES, year = DEMAND_START:DEMAND_END) %>%
    mutate(ore_Gt = MATS$ore_2024_Gt[m] * (1 + growth_pct / 100)^(year - 2024L))
  demand <- ore_year %>% group_by(growth_pct) %>% summarise(ore_Gt = sum(ore_Gt), .groups = "drop")
  ore_known_Gt <- max(dep_curve$ore_end_Gt)
  cat("Cumulative ore", DEMAND_START, "-", DEMAND_END, "(Gt):\n"); print(demand)
  cat("Known resources:", round(ore_known_Gt, 1), "Gt ore\n")

  ## Grade change per demand case ----

  # Option A: ore mined over years t-2..t takes a slice of the curve; metal of
  # the slice from the cumulative ore -> cumulative metal relation
  grade_year <- ore_year %>%
    arrange(growth_pct, year) %>%
    group_by(growth_pct) %>%
    mutate(cum_end_Gt = cumsum(ore_Gt), cum_start_Gt = lag(cum_end_Gt, WINDOW, default = 0)) %>%
    ungroup() %>%
    mutate(
      metal_Gt = approx(c(0, dep_curve$ore_end_Gt), c(0, dep_curve$metal_end_Gt), xout = cum_end_Gt)$y -
        approx(c(0, dep_curve$ore_end_Gt), c(0, dep_curve$metal_end_Gt), xout = cum_start_Gt)$y,
      grade = if_else(year >= BASE_YEAR, metal_Gt / (cum_end_Gt - cum_start_Gt) * gd, NA_real_)
    ) %>%
    group_by(growth_pct) %>%
    mutate(grade_rel = grade / grade[year == BASE_YEAR]) %>%
    ungroup()

  # Option B (default): grade = average grade of the producing group's ore that
  # remains after cumulative extraction (best first); once that group is used
  # up, of the remaining ore in other stages. NA beyond known resources.
  prod_end <- dep_curve %>% filter(group == "Producing or in construction") %>% summarise(ore = max(ore_end_Gt), metal = max(metal_end_Gt))
  tot_end <- dep_curve %>% summarise(ore = max(ore_end_Gt), metal = max(metal_end_Gt))
  grade_b_today <- prod_end$metal / prod_end$ore * gd
  grade_year <- grade_year %>%
    mutate(
      metal_cum_Gt = approx(c(0, dep_curve$ore_end_Gt), c(0, dep_curve$metal_end_Gt), xout = cum_end_Gt)$y,
      b_grade = case_when(
        cum_end_Gt < prod_end$ore ~ (prod_end$metal - metal_cum_Gt) / (prod_end$ore - cum_end_Gt),
        cum_end_Gt < tot_end$ore ~ (tot_end$metal - metal_cum_Gt) / (tot_end$ore - cum_end_Gt)
      ) * gd,
      b_rel = b_grade / grade_b_today
    )
  cat("Option B, today (producing-group average):", signif(grade_b_today, 3), MATS$unit[m], "\n")

  grade_index <- grade_year %>%
    transmute(material = mat, unit = MATS$unit[m], growth_pct, year, ore_Gt, grade, grade_rel, b_grade, b_rel)
  print(grade_index %>% filter(year %in% c(2030, 2040, 2050, 2060)) %>% arrange(year, growth_pct), n = 20)
  grade_index_all[[mat]] <- grade_index

  ## Figure: grade curve ----

  central <- demand %>% filter(growth_pct == 0)
  rel_end <- grade_index$b_rel[grade_index$year == DEMAND_END & grade_index$growth_pct == 0]
  # Band ends: lowest (-1 %/yr) and highest (+3 %/yr) demand case (NA beyond known resources)
  rel_band <- grade_index$b_rel[grade_index$year == DEMAND_END & grade_index$growth_pct %in% range(GROWTH_CASES)]
  band_lab <- if_else(is.na(rel_band), "beyond resources", sprintf("%+.0f%%", (rel_band - 1) * 100))
  # Y axis: grade of the top 0.5% of ore x 1.4 (headroom for the label); a few
  # tiny very-high-grade deposits would otherwise flatten the curve
  y_top <- approx(c(0, dep_curve$ore_end_Gt), c(dep_curve$grade[1], dep_curve$grade), xout = 0.005 * ore_known_Gt, method = "constant", f = 1)$y * 1.4
  x_end <- max(ore_known_Gt, max(demand$ore_Gt)) * 1.02
  # Group labels above the curve: just over the highest grade after the group's
  # first 5% of ore (skips the few very-high-grade deposits at its start)
  group_lab <- dep_curve %>%
    group_by(group) %>%
    summarise(
      x = (min(ore_start_Gt) + max(ore_end_Gt)) / 2,
      y = max(grade[ore_start_Gt >= min(ore_start_Gt) + 0.05 * (max(ore_end_Gt) - min(ore_start_Gt))]) + 0.06 * y_top,
      n = n(), .groups = "drop"
    ) %>%
    mutate(label = paste0(sub(" or ", " or\n", group), "\n(", comma(n), " deposits)"))

  ggplot(dep_curve) +
    annotate("rect", xmin = min(demand$ore_Gt), xmax = max(demand$ore_Gt), ymin = 0, ymax = Inf, fill = "grey85") +
    geom_rect(aes(xmin = ore_start_Gt, xmax = ore_end_Gt, ymin = 0, ymax = grade, fill = group)) +
    geom_step(
      data = bind_rows(dep_curve, tail(dep_curve, 1) %>% mutate(ore_start_Gt = ore_end_Gt)),
      aes(x = ore_start_Gt, y = grade), direction = "hv", linewidth = 0.2, colour = "black"
    ) +
    geom_vline(xintercept = central$ore_Gt, linetype = "dashed", linewidth = 0.4) +
    annotate(
      "label", x = x_end, y = y_top, hjust = 1, vjust = 1, fill = "white", border.colour = NA,
      label = paste0(
        "Ore ", DEMAND_START, "-", DEMAND_END, " at 2024 level: ", comma(round(central$ore_Gt)), " Gt\n(shaded: -1 to +3%/yr)\n",
        "Grade ", DEMAND_END, " vs today: ",
        if_else(is.na(rel_end), "beyond known resources", sprintf("%+.0f%%", (rel_end - 1) * 100)),
        "\n(band: ", band_lab[1], " to ", band_lab[2], ")"
      ),
      size = pb_annot_size("small")
    ) +
    geom_text(data = group_lab, aes(x = x, y = y, label = label), vjust = 0, colour = "grey15", size = pb_annot_size("small")) +
    scale_fill_manual(values = c("Producing or in construction" = unname(PALETTE_METAL_ORES[mat]), "Other stages" = colorspace::lighten(unname(PALETTE_METAL_ORES[mat]), 0.4)), guide = "none") +
    scale_x_continuous(labels = comma, expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, x_end), ylim = c(0, y_top)) +
    labs(
      title = paste(mat, "ore grade curve"),
      subtitle = if_else(
        scale_f == 1, "Known deposits sorted by development stage, then grade",
        paste0("By stage, then grade; scaled to USGS ", MATS$scale_to_Gt[m], " Gt ", if_else(MATS$scale_basis[m] == "metal", "Cu", "ore"))
      ),
      x = "Cumulative ore (Gt)", y = paste0("Ore grade (", MATS$unit[m], ")")
    ) +
    theme_pb_small()

  ggsave(paste0("Figures/SP/", mat, "_GradeCurve.png"), ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)
}

grade_index_all <- bind_rows(grade_index_all)
write_csv(grade_index_all, "Parameters/SP/NonFerrous_GradeIndex.csv")


# 2060 grade targets for the Monte Carlo --------------------------------

# Model 2024 grade x option B relative grade 2060, per demand case. For
# reference only: MC_Assumptions.xlsx is filled by hand from this table.
iron_index <- read_csv("Parameters/SP/IronOre_GradeIndex.csv", show_col_types = FALSE) %>%
  transmute(material = "Iron", unit = "% Fe", growth_pct, year, grade = grade_pct, grade_rel, b_grade = b_grade_pct, b_rel)
grade_index_metals <- bind_rows(iron_index, grade_index_all) %>%
  mutate(material = factor(material, levels = c("Iron", MATS$material)))

grade_2024 <- c(Iron = GRADE_ORE_FE_2024, Copper = GRADE_ORE_CU_2024, Bauxite = GRADE_ORE_AL_2024)
grade_targets <- grade_index_metals %>%
  filter(year == DEMAND_END) %>%
  transmute(metal = material, growth_pct, rel_grade_2060 = b_rel, grade_2024 = grade_2024[as.character(material)], grade_2060 = grade_2024 * b_rel)
cat("\n2060 grade targets (option B):\n")
print(grade_targets, n = 20)
write_csv(grade_targets, "Parameters/SP/GradeTargets_2060.csv")


# Figure: grade relative to today, option B, all metals -----------------

# Same y axis across panels for comparison. Band = -1 to +3 %/yr demand cases,
# line = 2024 level held constant
index_band <- grade_index_metals %>%
  group_by(material, year) %>%
  summarise(lo = min(b_rel), hi = max(b_rel), mid = b_rel[growth_pct == 0], .groups = "drop")

end_lab <- index_band %>%
  filter(year == DEMAND_END) %>%
  mutate(label = if_else(is.na(mid), paste0(DEMAND_END, ": beyond\nknown resources"), paste0(DEMAND_END, ": ", sprintf("%+.0f%%", (mid - 1) * 100))))

ggplot(index_band, aes(x = year, colour = material, fill = material)) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.25) +
  geom_line(aes(y = mid), linewidth = 0.8) +
  geom_text(data = end_lab, aes(label = label), x = DEMAND_END, y = 1.19, hjust = 1, vjust = 1, size = pb_annot_size("wide")) +
  facet_wrap(~material, nrow = 1) +
  scale_colour_manual(values = PALETTE_METAL_ORES, guide = "none") +
  scale_fill_manual(values = PALETTE_METAL_ORES, guide = "none") +
  scale_x_continuous(breaks = c(2030, 2045, 2060), expand = expansion(mult = c(0.03, 0.1))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1.2, 0.2)) +
  coord_cartesian(ylim = c(0, 1.2)) +
  labs(
    title = "Ore grade change with future extraction",
    subtitle = "Remaining producing-group ore; line: 2024 demand held, band: -1 to +3%/yr",
    x = NULL, y = "Grade relative to today"
  ) +
  theme_pb_wide()

ggsave("Figures/SP/Metals_GradeIndex.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 7)


# Figure: option B vs option A ------------------------------------------

# B = remaining producing-group ore vs its average today; A = 3-year slice,
# best first, vs 2025-2027. Lines: 2024 demand held; ribbons: -1 to +3 %/yr.
grade_ab <- bind_rows(
  grade_index_metals %>% transmute(material, growth_pct, year, method = "B", rel = b_rel),
  grade_index_metals %>% transmute(material, growth_pct, year, method = "A", rel = grade_rel)
) %>%
  group_by(material, year, method) %>%
  summarise(lo = min(rel), hi = max(rel), mid = rel[growth_pct == 0], .groups = "drop") %>%
  mutate(method = factor(method, levels = c("B", "A"), labels = c("Remaining producing ore", "Best-first slice")))

ab_lab <- grade_ab %>%
  filter(year == DEMAND_END) %>%
  group_by(material) %>%
  summarise(
    b = mid[method == "Remaining producing ore"], a = mid[method == "Best-first slice"], .groups = "drop"
  ) %>%
  mutate(label = paste0(
    "2060, 2024 level\nB: ", if_else(is.na(b), "beyond resources", sprintf("%+.0f%%", (b - 1) * 100)),
    "\nA: ", if_else(is.na(a), "beyond resources", sprintf("%+.0f%%", (a - 1) * 100))
  ))

# Direct method labels in the iron panel only
method_lab <- tibble(
  material = factor("Iron", levels = levels(grade_index_metals$material)),
  method = factor(c("Remaining producing ore", "Best-first slice"), levels = levels(grade_ab$method)),
  x = BASE_YEAR, y = c(0.42, 0.30)
)

ggplot(grade_ab, aes(x = year)) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = method), alpha = 0.25) +
  geom_line(aes(y = mid, colour = method, linetype = method), linewidth = 0.7) +
  geom_text(data = ab_lab, aes(label = label), x = DEMAND_END, y = 1.39, hjust = 1, vjust = 1, lineheight = 0.9, size = pb_annot_size("wide")) +
  geom_text(data = method_lab, aes(x = x, y = y, label = method, colour = method), hjust = 0, size = pb_annot_size("wide")) +
  facet_wrap(~material, nrow = 1) +
  scale_colour_manual(values = c("Best-first slice" = "#8B2E1F", "Remaining producing ore" = "#2F5D8A"), guide = "none") +
  scale_fill_manual(values = c("Best-first slice" = "#8B2E1F", "Remaining producing ore" = "#2F5D8A"), guide = "none") +
  scale_linetype_manual(values = c("Best-first slice" = "dashed", "Remaining producing ore" = "solid"), guide = "none") +
  scale_x_continuous(breaks = c(2030, 2045, 2060), expand = expansion(mult = c(0.03, 0.1))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1.4)) + # headroom for the 2060 labels
  labs(
    title = "Ore grade change: two definitions",
    subtitle = paste0("B (default): remaining producing-group ore vs its average today; A: 3-year slice vs ", BASE_LABEL),
    x = NULL, y = "Grade relative to today"
  ) +
  theme_pb_wide()

ggsave("Figures/SP/Metals_GradeIndex_AvsB.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17, height = 7.5)

# EoF
