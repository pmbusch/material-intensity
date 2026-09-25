## Table 1 - Results.R
## Compiles headline Stock / Flow / Outflow results by material category and
## detail, at 2024 (current/historical) and FORECAST_END (MC median + 95% CI).
## Rows: Total -> big category -> detail (metals split Ferrous/Non-ferrous;
## minerals split by end-use). Columns: category, then Stock, Flow, Outflow,
## each with a 2024 and a FORECAST_END sub-column. Rendered as an HTML gt table.
##
## Units: Gt (stock) / Gt per year (flow, outflow).
## "Flow" = primary_consumption_Mt + secondary_supply_Mt (DMC-consistent,
##   ore-equivalent for metals -- same convention as the rest of the project).
## "Outflow" = waste_Mt (material leaving in-use stock at end of life).
## "Stock" = in_use_stock_Mt. Biomass and fossil fuels have no in-use stock in
##   this model (Kaya flow-through), so their Stock/Outflow cells are blank.
##
## 2024 values use real historical/accounting data wherever available
## (stock_2024_total.csv; materials_region_DMC.csv; historical_secondary_flows.csv).
## Where history has no split matching this table's detail rows (minerals by
## end-use; metals Fe/Non-Fe outflow), the real 2024 category TOTAL is kept
## from history and apportioned across details using the MC ensemble's own
## year-2025 (first simulated year, closest available) median shares.

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(arrow)
library(gt)

cat("=== Table 1 - Results ===\n\n")

TARGET_YR <- FORECAST_END # 2060 (Scripts/00-CommonParameters.R)
PROXY_YR <- 2025L # first simulated year -- closest MC proxy to 2024 for shares
CI_LO <- 0.025
CI_HI <- 0.975

ENDUSE_MAP <- c(
  "Residential" = "Buildings", "Non-residential" = "Buildings",
  "Roads" = "Civil infrastructure", "Civil engineering" = "Civil infrastructure",
  "Machinery" = "Machinery", "Vehicles" = "Machinery",
  "Durables" = "Short-lived products", "Packaging" = "Short-lived products"
)

CATEGORY_ORDER <- c("Biomass", "Fossil fuels", "Metals", "Non-metallic minerals")
DETAIL_ORDER <- list(
  "Biomass" = c("Crops", "Grazed biomass and fodder crops", "Wood", "Crop Residues", "Other biomass"),
  "Fossil fuels" = c("Coal", "Natural Gas", "Petroleum", "Other fossil fuels"),
  "Metals" = c("Ferrous ores", "Non-ferrous ores"),
  "Non-metallic minerals" = c("Buildings", "Civil infrastructure", "Machinery", "Short-lived products")
)


# Load data --------

mc_results <- arrow::read_parquet("Results/MC/mc_results.parquet")
stock_2024_total <- readr::read_csv("Parameters/stock_2024_total.csv", show_col_types = FALSE)
dmc_region <- readr::read_csv("Parameters/materials_region_DMC.csv", show_col_types = FALSE)
secondary_2024 <- readr::read_csv("Parameters/Intermediate/historical_secondary_flows.csv", show_col_types = FALSE)
dict_mat <- readxl::read_excel("Inputs/Dict_Materials.xlsx", sheet = "Categories") |>
  dplyr::select(material_category = Material_22, Material_group)


# Assign category/detail labels to every mc_results row (year filtered later) --------

mc_labeled <- mc_results |>
  dplyr::mutate(
    category = dplyr::case_when(
      material_group == "biomass" ~ "Biomass",
      material_group == "fossil_fuels" ~ "Fossil fuels",
      material_group %in% c("metal_fe", "metal_nonfe") ~ "Metals",
      material_group == "nonmetallic_minerals" ~ "Non-metallic minerals"
    ),
    detail = dplyr::case_when(
      material_group %in% c("biomass", "fossil_fuels") ~ material_key,
      material_group == "metal_fe" ~ "Ferrous ores",
      material_group == "metal_nonfe" ~ "Non-ferrous ores",
      material_group == "nonmetallic_minerals" ~ unname(ENDUSE_MAP[material_key])
    ),
    flow_Mt = primary_consumption_Mt + secondary_supply_Mt
  )

mc_target <- mc_labeled |> dplyr::filter(year == TARGET_YR)
mc_proxy <- mc_labeled |> dplyr::filter(year == PROXY_YR)


# ===========================================================================
# STOCK -- 2024 (exact, stock_2024_total.csv) and FORECAST_END (MC) ---------
# ===========================================================================

stock_2024_detail <- stock_2024_total |>
  dplyr::mutate(
    category = dplyr::if_else(material %in% c("Metal_Fe", "Metal_NonFe"), "Metals", "Non-metallic minerals"),
    detail = dplyr::case_when(
      material == "Metal_Fe" ~ "Ferrous ores",
      material == "Metal_NonFe" ~ "Non-ferrous ores",
      super_category == "buildings" ~ "Buildings",
      super_category == "civil_infrastructure" ~ "Civil infrastructure",
      super_category == "machinery" ~ "Machinery",
      super_category == "short_lived" ~ "Short-lived products"
    )
  ) |>
  dplyr::group_by(category, detail) |>
  dplyr::summarise(stock_2024_Gt = sum(stock_Mt, na.rm = TRUE) / 1e3, .groups = "drop")

stock_2024_category <- stock_2024_detail |>
  dplyr::group_by(category) |>
  dplyr::summarise(stock_2024_Gt = sum(stock_2024_Gt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

stock_2024_grand <- tibble::tibble(
  category = "Total", detail = "Total",
  stock_2024_Gt = sum(stock_2024_category$stock_2024_Gt)
)

stock_2024_all <- dplyr::bind_rows(stock_2024_grand, stock_2024_category, stock_2024_detail)

# MC (FORECAST_END): per-run world sum by detail, then rolled up to category
# and grand totals BEFORE taking quantiles -- preserves each run's own
# cross-category correlation instead of pooling independent per-detail CIs.
stock_mc_detail_byrun <- mc_target |>
  dplyr::filter(category %in% c("Metals", "Non-metallic minerals")) |>
  dplyr::group_by(run_id, category, detail) |>
  dplyr::summarise(value_Mt = sum(in_use_stock_Mt, na.rm = TRUE), .groups = "drop")

stock_mc_category_byrun <- stock_mc_detail_byrun |>
  dplyr::group_by(run_id, category) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

stock_mc_grand_byrun <- stock_mc_category_byrun |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(category = "Total", detail = "Total")

stock_target_all <- dplyr::bind_rows(stock_mc_grand_byrun, stock_mc_category_byrun, stock_mc_detail_byrun) |>
  dplyr::group_by(category, detail) |>
  dplyr::summarise(
    stock_target_med = median(value_Mt) / 1e3,
    stock_target_lo = quantile(value_Mt, CI_LO, names = FALSE) / 1e3,
    stock_target_hi = quantile(value_Mt, CI_HI, names = FALSE) / 1e3,
    .groups = "drop"
  )


# ===========================================================================
# FLOW -- 2024 (historical DMC) and FORECAST_END (MC) -----------------------
# ===========================================================================

dmc_2024 <- dmc_region |>
  dplyr::filter(year == 2024) |>
  dplyr::left_join(dict_mat, by = "material_category")

BIOMASS_NAMED <- c("Crops", "Grazed biomass and fodder crops", "Wood", "Crop Residues")
FOSSIL_NAMED <- c("Coal", "Natural Gas", "Petroleum")

flow_2024_biomass <- dmc_2024 |>
  dplyr::filter(Material_group == "Biomass") |>
  dplyr::mutate(detail = dplyr::if_else(material_category %in% BIOMASS_NAMED, material_category, "Other biomass")) |>
  dplyr::group_by(detail) |>
  dplyr::summarise(flow_2024_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(category = "Biomass")

flow_2024_fossil <- dmc_2024 |>
  dplyr::filter(Material_group == "Fossil fuels") |>
  dplyr::mutate(detail = dplyr::if_else(material_category %in% FOSSIL_NAMED, material_category, "Other fossil fuels")) |>
  dplyr::group_by(detail) |>
  dplyr::summarise(flow_2024_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(category = "Fossil fuels")

flow_2024_metal <- dmc_2024 |>
  dplyr::filter(material_category %in% c("Ferrous ores", "Non-ferrous ores")) |>
  dplyr::group_by(detail = material_category) |>
  dplyr::summarise(flow_2024_Mt = sum(DMC_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(category = "Metals")

mineral_2024_total_Mt <- dmc_2024 |>
  dplyr::filter(material_category %in% c(
    "Non-metallic minerals - construction dominant",
    "Non-metallic minerals - industrial or agricultural dominant"
  )) |>
  dplyr::summarise(x = sum(DMC_Mt, na.rm = TRUE)) |>
  dplyr::pull(x)

# No historical end-use split exists for minerals -- apportion the real 2024
# total using the MC ensemble's own year-2025 median end-use share of flow.
mineral_flow_shares <- mc_proxy |>
  dplyr::filter(category == "Non-metallic minerals") |>
  dplyr::group_by(run_id, detail) |>
  dplyr::summarise(flow_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(share = flow_Mt / sum(flow_Mt)) |>
  dplyr::ungroup() |>
  dplyr::group_by(detail) |>
  dplyr::summarise(share = median(share), .groups = "drop")

flow_2024_mineral <- mineral_flow_shares |>
  dplyr::transmute(detail, category = "Non-metallic minerals", flow_2024_Mt = share * mineral_2024_total_Mt)

flow_2024_detail <- dplyr::bind_rows(flow_2024_biomass, flow_2024_fossil, flow_2024_metal, flow_2024_mineral) |>
  dplyr::mutate(flow_2024_Gt = flow_2024_Mt / 1e3) |>
  dplyr::select(category, detail, flow_2024_Gt)

flow_2024_category <- flow_2024_detail |>
  dplyr::group_by(category) |>
  dplyr::summarise(flow_2024_Gt = sum(flow_2024_Gt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

flow_2024_grand <- tibble::tibble(category = "Total", detail = "Total", flow_2024_Gt = sum(flow_2024_category$flow_2024_Gt))

flow_2024_all <- dplyr::bind_rows(flow_2024_grand, flow_2024_category, flow_2024_detail)

# MC (FORECAST_END): all four material groups contribute a flow.
flow_mc_detail_byrun <- mc_target |>
  dplyr::group_by(run_id, category, detail) |>
  dplyr::summarise(value_Mt = sum(flow_Mt, na.rm = TRUE), .groups = "drop")

flow_mc_category_byrun <- flow_mc_detail_byrun |>
  dplyr::group_by(run_id, category) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

flow_mc_grand_byrun <- flow_mc_category_byrun |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(category = "Total", detail = "Total")

flow_target_all <- dplyr::bind_rows(flow_mc_grand_byrun, flow_mc_category_byrun, flow_mc_detail_byrun) |>
  dplyr::group_by(category, detail) |>
  dplyr::summarise(
    flow_target_med = median(value_Mt) / 1e3,
    flow_target_lo = quantile(value_Mt, CI_LO, names = FALSE) / 1e3,
    flow_target_hi = quantile(value_Mt, CI_HI, names = FALSE) / 1e3,
    .groups = "drop"
  )


# ===========================================================================
# OUTFLOW -- 2024 (historical waste) and FORECAST_END (MC) ------------------
# ===========================================================================

secondary_2024_filt <- secondary_2024 |> dplyr::filter(year == 2024)

metal_waste_2024_Mt <- secondary_2024_filt |>
  dplyr::filter(material_group == "metal_ores") |>
  dplyr::summarise(x = sum(waste_Mt, na.rm = TRUE)) |>
  dplyr::pull(x)

mineral_waste_2024_Mt <- secondary_2024_filt |>
  dplyr::filter(material_group == "nonmetallic_minerals") |>
  dplyr::summarise(x = sum(waste_Mt, na.rm = TRUE)) |>
  dplyr::pull(x)

# No historical Fe/Non-Fe or end-use split exists for waste -- apportion the
# real 2024 category totals using the MC ensemble's year-2025 median shares.
metal_waste_shares <- mc_proxy |>
  dplyr::filter(category == "Metals") |>
  dplyr::group_by(run_id, detail) |>
  dplyr::summarise(waste_Mt = sum(waste_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(share = waste_Mt / sum(waste_Mt)) |>
  dplyr::ungroup() |>
  dplyr::group_by(detail) |>
  dplyr::summarise(share = median(share), .groups = "drop")

mineral_waste_shares <- mc_proxy |>
  dplyr::filter(category == "Non-metallic minerals") |>
  dplyr::group_by(run_id, detail) |>
  dplyr::summarise(waste_Mt = sum(waste_Mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(run_id) |>
  dplyr::mutate(share = waste_Mt / sum(waste_Mt)) |>
  dplyr::ungroup() |>
  dplyr::group_by(detail) |>
  dplyr::summarise(share = median(share), .groups = "drop")

outflow_2024_metal <- metal_waste_shares |>
  dplyr::transmute(detail, category = "Metals", outflow_2024_Gt = share * metal_waste_2024_Mt / 1e3)

outflow_2024_mineral <- mineral_waste_shares |>
  dplyr::transmute(detail, category = "Non-metallic minerals", outflow_2024_Gt = share * mineral_waste_2024_Mt / 1e3)

outflow_2024_detail <- dplyr::bind_rows(outflow_2024_metal, outflow_2024_mineral)

outflow_2024_category <- outflow_2024_detail |>
  dplyr::group_by(category) |>
  dplyr::summarise(outflow_2024_Gt = sum(outflow_2024_Gt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

outflow_2024_grand <- tibble::tibble(
  category = "Total", detail = "Total",
  outflow_2024_Gt = sum(outflow_2024_category$outflow_2024_Gt)
)

outflow_2024_all <- dplyr::bind_rows(outflow_2024_grand, outflow_2024_category, outflow_2024_detail)

# MC (FORECAST_END): waste_Mt only exists for metals/minerals (NA for biomass/fossil).
outflow_mc_detail_byrun <- mc_target |>
  dplyr::filter(category %in% c("Metals", "Non-metallic minerals")) |>
  dplyr::group_by(run_id, category, detail) |>
  dplyr::summarise(value_Mt = sum(waste_Mt, na.rm = TRUE), .groups = "drop")

outflow_mc_category_byrun <- outflow_mc_detail_byrun |>
  dplyr::group_by(run_id, category) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(detail = "Total")

outflow_mc_grand_byrun <- outflow_mc_category_byrun |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(value_Mt = sum(value_Mt), .groups = "drop") |>
  dplyr::mutate(category = "Total", detail = "Total")

outflow_target_all <- dplyr::bind_rows(outflow_mc_grand_byrun, outflow_mc_category_byrun, outflow_mc_detail_byrun) |>
  dplyr::group_by(category, detail) |>
  dplyr::summarise(
    outflow_target_med = median(value_Mt) / 1e3,
    outflow_target_lo = quantile(value_Mt, CI_LO, names = FALSE) / 1e3,
    outflow_target_hi = quantile(value_Mt, CI_HI, names = FALSE) / 1e3,
    .groups = "drop"
  )


# ===========================================================================
# Assemble table in display row order --------
# ===========================================================================

row_order <- dplyr::bind_rows(
  tibble::tibble(category = "Total", detail = "Total"),
  purrr::map_dfr(CATEGORY_ORDER, function(cat) {
    dplyr::bind_rows(
      tibble::tibble(category = cat, detail = "Total"),
      tibble::tibble(category = cat, detail = DETAIL_ORDER[[cat]])
    )
  })
)

table_data <- row_order |>
  dplyr::left_join(stock_2024_all, by = c("category", "detail")) |>
  dplyr::left_join(stock_target_all, by = c("category", "detail")) |>
  dplyr::left_join(flow_2024_all, by = c("category", "detail")) |>
  dplyr::left_join(flow_target_all, by = c("category", "detail")) |>
  dplyr::left_join(outflow_2024_all, by = c("category", "detail")) |>
  dplyr::left_join(outflow_target_all, by = c("category", "detail")) |>
  dplyr::mutate(
    category_label = category,
    detail_label = dplyr::case_when(
      category == "Total" & detail == "Total" ~ "All materials",
      detail == "Total" ~ "Total",
      TRUE ~ detail
    )
  )

# Order-of-magnitude rounding: <10 -> 1 decimal, >=10 -> whole numbers,
# >=1,000 -> comma-separated whole numbers. NA (no stock/outflow concept) -> "-".
# (12 call sites below, no clean inline alternative -- kept as one small helper.)
fmt_oom <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "–",
    abs(x) < 10 ~ formatC(round(x, 1), format = "f", digits = 1, big.mark = ","),
    TRUE ~ formatC(round(x, 0), format = "f", digits = 0, big.mark = ",")
  )
}

table_fmt <- table_data |>
  dplyr::mutate(
    stock_2024_disp = fmt_oom(stock_2024_Gt),
    stock_target_disp = dplyr::if_else(
      is.na(stock_target_med), "–",
      paste0(fmt_oom(stock_target_med), " (", fmt_oom(stock_target_lo), " to ", fmt_oom(stock_target_hi), ")")
    ),
    flow_2024_disp = fmt_oom(flow_2024_Gt),
    flow_target_disp = dplyr::if_else(
      is.na(flow_target_med), "–",
      paste0(fmt_oom(flow_target_med), " (", fmt_oom(flow_target_lo), " to ", fmt_oom(flow_target_hi), ")")
    ),
    outflow_2024_disp = fmt_oom(outflow_2024_Gt),
    outflow_target_disp = dplyr::if_else(
      is.na(outflow_target_med), "–",
      paste0(fmt_oom(outflow_target_med), " (", fmt_oom(outflow_target_lo), " to ", fmt_oom(outflow_target_hi), ")")
    )
  ) |>
  dplyr::select(
    category_label, detail_label, detail,
    stock_2024_disp, stock_target_disp,
    flow_2024_disp, flow_target_disp,
    outflow_2024_disp, outflow_target_disp
  )


# Build gt table --------

tbl <- gt::gt(table_fmt, groupname_col = "category_label", rowname_col = "detail_label") |>
  gt::cols_hide(columns = "detail") |>
  gt::tab_header(title = "Global material stocks and flows, 2024 and 2060") |>
  gt::tab_spanner(label = "Stock (Gt)", columns = c("stock_2024_disp", "stock_target_disp")) |>
  gt::tab_spanner(label = "Flow (Gt/yr)", columns = c("flow_2024_disp", "flow_target_disp")) |>
  gt::tab_spanner(label = "Outflow (Gt/yr)", columns = c("outflow_2024_disp", "outflow_target_disp")) |>
  gt::cols_label(
    stock_2024_disp = "2024", stock_target_disp = paste0(TARGET_YR, " (median, 95% CI)"),
    flow_2024_disp = "2024", flow_target_disp = paste0(TARGET_YR, " (median, 95% CI)"),
    outflow_2024_disp = "2024", outflow_target_disp = paste0(TARGET_YR, " (median, 95% CI)")
  ) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_stub(rows = table_fmt$detail == "Total")
  ) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |>
  gt::tab_footnote(
    footnote = paste0(
      "2024 minerals end-use split and metals outflow Fe/Non-Fe split have no historical breakdown; ",
      "apportioned from the real 2024 category total using the MC ensemble's year-", PROXY_YR,
      " median shares. Biomass and fossil fuels have no in-use stock in this model (flow-through), so ",
      "their Stock/Outflow cells are blank."
    )
  ) |>
  gt::opt_align_table_header(align = "left") |>
  gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(3))

gt::gtsave(tbl, "Figures/Table1_Results.html")

cat("  Saved: Figures/Table1_Results.html\n")
cat("=== Table 1 done ===\n")

# EoF
