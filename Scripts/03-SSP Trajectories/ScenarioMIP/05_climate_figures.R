## =============================================================================
## 05_climate_figures.R
## Raw-level (NOT intensity) climate outcome figures from the IIASA
## ScenarioMIP CMIP7 climate assessment extract (World only, MAGICC v7.6.0a3):
##   01_forcing     -- Effective Radiative Forcing (W/m²): 33rd / Median / 67th percentile panels
##   02_temperature -- Surface Temperature GSAT (°C vs 1850-1900): 33rd / Median / 67th panels,
##                     with Paris Agreement 1.5 °C / 2 °C reference lines
##   03_emissions   -- Harmonized & infilled CO₂, CH₄, N₂O, Kyoto gases
## Each panel: every (model, family, SSP) run as a line, end-of-horizon
## (FORECAST_END) min-max bar per colour group, table below with the
## END_YEAR level (min/max) and the absolute change vs RATIO_BASE_YEAR --
## ratios are not used here (temperature is additive; CO₂ crosses zero).
## Temperature table adds the share of runs whose PEAK warming over
## BASE_YEAR-END_YEAR exceeds 1.5 °C / 2 °C (Paris yardstick).
## Two versions of every figure: coloured by SSP (main), and coloured by
## ScenarioMIP forcing family (the policy level, which drives most of the
## climate spread -- SSP ranges partly reflect each SSP's family mix).
## Annual data (not the 5-yr grid of 01-04); emissions start in 2023.
##
## Input:  Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-0e7dfab0-....csv
## Outputs:
##   Figures/IIASA/Climate/{BySSP,ByFamily}/{01_forcing,02_temperature,03_emissions}.png (+ SVG mirror)
##   Parameters/SSP_ScenarioMIP/climate_summary_{END_YEAR}.csv -- numbers behind the tables
## =============================================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)

BASE_YEAR <- 2020L # plot x-axis start (matches 01-04)
RATIO_BASE_YEAR <- 2025L # reference year for the absolute-change columns (matches 03-04)
END_YEAR <- FORECAST_END # 2060, project horizon
PARIS_LEVELS <- c(1.5, 2.0) # °C above 1850-1900 (GSAT baseline of the MAGICC assessment)

CLIMATE_FILE <- "Inputs/IIASA_SSP/2026-MIP-CMIP7/climate_iamc_data-0e7dfab0-46ee-486b-b50d-4faf3de27da4.csv"
OUT_DIR <- "Parameters/SSP_ScenarioMIP"
FIG_DIR <- "Figures/IIASA/Climate"
FIG_DIR_SVG <- "Figures/SVG/IIASA/Climate"

for (dd in c(FIG_DIR, FIG_DIR_SVG)) {
  dir.create(file.path(dd, "BySSP"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(dd, "ByFamily"), showWarnings = FALSE, recursive = TRUE)
}


# Step 1: Load, pivot long, parse scenario into family / SSP / marker ---------

cat("STEP 1: Load climate extract\n")

clim_raw <- read_csv(CLIMATE_FILE, col_types = cols(.default = "c"))
yr_cols <- names(clim_raw)[grepl("^[0-9]{4}$", names(clim_raw))]

clim_long <- clim_raw %>%
  distinct() %>%
  pivot_longer(cols = all_of(yr_cols), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year), value = as.numeric(value)) %>%
  filter(!is.na(value), year >= BASE_YEAR, year <= END_YEAR) %>%
  mutate(
    marker = str_detect(scenario, fixed("(Marker)")),
    scenario_clean = str_squish(str_remove(scenario, fixed("(Marker)"))),
    ssp = str_extract(scenario_clean, "SSP[1-5]"),
    family = str_squish(str_remove(scenario_clean, "-\\s*SSP[1-5]$"))
  )

cat("  Rows:", nrow(clim_long), "| runs:", n_distinct(paste(clim_long$model, clim_long$scenario)),
    "| families:", paste(sort(unique(clim_long$family)), collapse = ", "), "\n")


# Step 2: Variable metadata (group, panel label, unit rescale, table digits) --

# CO₂ / Kyoto gases rescaled Mt -> Gt (/1000) for readable axes
VAR_META <- tibble::tribble(
  ~variable, ~group, ~panel, ~p_idx, ~unit_label, ~scale, ~digits,
  "Climate Assessment|Effective Radiative Forcing|33rd Percentile [MAGICC v7.6.0a3]", "forcing", "33rd percentile", 1L, "W/m²", 1, 2,
  "Climate Assessment|Effective Radiative Forcing|Median [MAGICC v7.6.0a3]", "forcing", "Median", 2L, "W/m²", 1, 2,
  "Climate Assessment|Effective Radiative Forcing|67th Percentile [MAGICC v7.6.0a3]", "forcing", "67th percentile", 3L, "W/m²", 1, 2,
  "Climate Assessment|Surface Temperature (GSAT)|33rd Percentile [MAGICC v7.6.0a3]", "temperature", "33rd percentile", 1L, "°C vs 1850–1900", 1, 2,
  "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICC v7.6.0a3]", "temperature", "Median", 2L, "°C vs 1850–1900", 1, 2,
  "Climate Assessment|Surface Temperature (GSAT)|67th Percentile [MAGICC v7.6.0a3]", "temperature", "67th percentile", 3L, "°C vs 1850–1900", 1, 2,
  "Climate Assessment|Harmonized and Infilled|Emissions|CO2", "emissions", "CO₂", 1L, "Gt CO₂/yr", 1 / 1000, 1,
  "Climate Assessment|Harmonized and Infilled|Emissions|CH4", "emissions", "CH₄", 2L, "Mt CH₄/yr", 1, 0,
  "Climate Assessment|Harmonized and Infilled|Emissions|N2O", "emissions", "N₂O", 3L, "kt N₂O/yr", 1, 0,
  "Climate Assessment|Harmonized and Infilled|Emissions|Kyoto Gases [AR6GWP100]", "emissions", "Kyoto gases (AR6 GWP100)", 4L, "Gt CO₂-eq/yr", 1 / 1000, 1
)

GROUPS <- tibble::tribble(
  ~group, ~title, ~filename,
  "forcing", "Effective Radiative Forcing", "01_forcing",
  "temperature", "Global Surface Air Temperature (GSAT)", "02_temperature",
  "emissions", "Emissions (harmonized & infilled)", "03_emissions"
)

clim_long <- clim_long %>%
  inner_join(VAR_META, by = "variable") %>%
  mutate(value = value * scale)

stopifnot(n_distinct(clim_long$variable) == nrow(VAR_META))


# Step 3: Colour schemes -- SSP (project palette) and forcing family ----------

SSP_LEVELS <- names(SSP_COLORS)

# Ordered from lowest to highest forcing; cool -> warm, non-bright
FAMILY_LEVELS <- c("Very Low", "Low-to-Negative", "Low", "Medium-to-Low", "Medium", "High-to-Low", "High")
# fmt: skip
FAMILY_COLORS <- c(
  "Very Low"        = "#1f4e79",
  "Low-to-Negative" = "#3a7dbf",
  "Low"             = "#7fb3d5",
  "Medium-to-Low"   = "#8c8c5a",
  "Medium"          = "#d9a13b",
  "High-to-Low"     = "#cc6a2f",
  "High"            = "#9e2a2b"
)
FAMILY_SHORT <- c(
  "Very Low" = "VL", "Low-to-Negative" = "LN", "Low" = "L", "Medium-to-Low" = "ML",
  "Medium" = "M", "High-to-Low" = "HL", "High" = "H"
)
stopifnot(all(unique(clim_long$family) %in% FAMILY_LEVELS))

COLOUR_SCHEMES <- tibble::tribble(
  ~colour_by, ~folder, ~key_name,
  "ssp", "BySSP", "SSP",
  "family", "ByFamily", "Family"
)

pb_set_geom_defaults("wide")

TBL_THEME <- gridExtra::ttheme_minimal(
  base_size = 7,
  padding = grid::unit(c(1.6, 1.2), "mm"),
  core = list(fg_params = list(fontsize = 7, col = "#222222")),
  colhead = list(fg_params = list(fontsize = 7, fontface = "bold", col = "#222222"))
)


# Step 4: One figure per (colour scheme, group); one panel per sub-variable ---

cat("\nSTEP 4: Build figures\n")

summary_rows <- list()

for (cs in seq_len(nrow(COLOUR_SCHEMES))) {
  scheme <- COLOUR_SCHEMES[cs, ]
  key_levels <- if (scheme$colour_by == "ssp") SSP_LEVELS else FAMILY_LEVELS
  key_colors <- if (scheme$colour_by == "ssp") SSP_COLORS else FAMILY_COLORS

  for (g in seq_len(nrow(GROUPS))) {
    grp <- GROUPS[g, ]
    vars_here <- VAR_META %>% filter(group == grp$group) %>% arrange(p_idx)
    panel_list <- list()

    for (v in seq_len(nrow(vars_here))) {
      vm <- vars_here[v, ]
      d <- clim_long %>%
        filter(variable == vm$variable) %>%
        mutate(key = factor(if (scheme$colour_by == "ssp") ssp else family, levels = key_levels)) %>%
        droplevels()

      # Emphasis: marker run, OR the only run of its colour group (see 03)
      n_runs_key <- d %>% distinct(model, family, ssp, key) %>% count(key, name = "n_runs")
      d <- d %>%
        left_join(n_runs_key, by = "key") %>%
        mutate(emphasize = marker | n_runs == 1)

      # End-of-horizon min-max bar per colour group; single-run groups -> point
      bar_step <- if (scheme$colour_by == "ssp") 2.2 else 1.6
      end_by_key <- d %>%
        filter(year == END_YEAR) %>%
        group_by(key) %>%
        summarise(ymin = min(value), ymax = max(value), .groups = "drop") %>%
        mutate(
          x = END_YEAR + 2 + (as.integer(key) - 1) * bar_step,
          label = if (scheme$colour_by == "ssp") as.character(key) else FAMILY_SHORT[as.character(key)]
        )
      end_by_key_range <- end_by_key %>% filter(ymax > ymin)
      end_by_key_single <- end_by_key %>% filter(ymax == ymin)

      p_line <- ggplot(d, aes(year, value, colour = key, group = interaction(model, family, ssp))) +
        geom_line(aes(linewidth = emphasize, alpha = emphasize)) +
        geom_segment(
          data = end_by_key_range, aes(x = x, xend = x, y = ymin, yend = ymax, colour = key),
          inherit.aes = FALSE, linewidth = 1.1, lineend = "round"
        ) +
        geom_point(data = end_by_key_single, aes(x = x, y = ymax, colour = key), inherit.aes = FALSE, size = 2.2) +
        geom_text(
          data = end_by_key, aes(x = x, y = ymax, label = label, colour = key),
          inherit.aes = FALSE, angle = 90, hjust = -0.1, vjust = 0.5, fontface = "bold",
          size = pb_annot_size("wide", 7)
        ) +
        scale_colour_manual(values = key_colors, guide = "none") +
        scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.4), guide = "none") +
        scale_alpha_manual(values = c(`TRUE` = 0.95, `FALSE` = 0.4), guide = "none") +
        scale_x_continuous(
          breaks = seq(BASE_YEAR, END_YEAR, by = 10),
          limits = c(BASE_YEAR, max(end_by_key$x) + 2), expand = expansion(mult = c(0.02, 0.01))
        ) +
        scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.03, 0.12))) +
        coord_cartesian(clip = "off") +
        labs(x = NULL, y = vm$unit_label, title = vm$panel) +
        theme_pb_wide() +
        theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4))

      # Zero line where emissions cross into net-negative territory
      if (min(d$value) < 0) p_line <- p_line + geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3)

      # Paris Agreement yardstick on temperature panels
      if (grp$group == "temperature") {
        p_line <- p_line +
          geom_hline(yintercept = PARIS_LEVELS, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
          annotate(
            "text", x = BASE_YEAR + 0.5, y = PARIS_LEVELS, label = paste0(PARIS_LEVELS, " °C (Paris)"),
            hjust = 0, vjust = -0.4, colour = "grey30", size = pb_annot_size("wide", 7)
          )
      }

      # Per-run END_YEAR level, absolute change vs RATIO_BASE_YEAR, and peak
      # over BASE_YEAR-END_YEAR (for the Paris exceedance columns)
      run_stats <- d %>%
        group_by(model, family, ssp, key) %>%
        summarise(
          v_end = value[year == END_YEAR][1],
          v_ref = value[year == RATIO_BASE_YEAR][1],
          v_peak = max(value),
          .groups = "drop"
        ) %>%
        mutate(delta = v_end - v_ref)

      tbl_num <- bind_rows(
        run_stats %>% mutate(key = "All Runs"),
        run_stats %>% mutate(key = as.character(key))
      ) %>%
        mutate(key = factor(key, levels = c("All Runs", key_levels))) %>%
        group_by(key) %>%
        summarise(
          N = n(),
          end_min = min(v_end), end_max = max(v_end),
          delta_min = min(delta), delta_max = max(delta),
          pct_peak_15 = mean(v_peak > PARIS_LEVELS[1]) * 100,
          pct_peak_20 = mean(v_peak > PARIS_LEVELS[2]) * 100,
          .groups = "drop"
        ) %>%
        filter(!is.na(key))

      summary_rows[[length(summary_rows) + 1]] <- tbl_num %>%
        mutate(colour_by = scheme$colour_by, group = grp$group, panel = vm$panel, unit = vm$unit_label, .before = 1)

      tbl_show <- tbl_num %>%
        mutate(
          `N` = N,
          `min` = formatC(end_min, format = "f", digits = vm$digits, big.mark = ","),
          `max` = formatC(end_max, format = "f", digits = vm$digits, big.mark = ","),
          `Δmin` = formatC(delta_min, format = "f", digits = vm$digits, big.mark = ",", flag = "+"),
          `Δmax` = formatC(delta_max, format = "f", digits = vm$digits, big.mark = ",", flag = "+"),
          `>1.5 °C` = paste0(round(pct_peak_15), "%"),
          `>2 °C` = paste0(round(pct_peak_20), "%")
        ) %>%
        dplyr::select(key, N, `min`, `max`, `Δmin`, `Δmax`, `>1.5 °C`, `>2 °C`)
      names(tbl_show)[1] <- scheme$key_name
      names(tbl_show)[3:4] <- paste0(END_YEAR, " ", names(tbl_show)[3:4])
      if (grp$group != "temperature") tbl_show <- tbl_show %>% dplyr::select(-`>1.5 °C`, -`>2 °C`)

      tbl_grob <- gridExtra::tableGrob(tbl_show, rows = NULL, theme = TBL_THEME)

      panel_list[[vm$panel]] <- (p_line / patchwork::wrap_elements(full = tbl_grob)) +
        patchwork::plot_layout(heights = c(3, 1.7))

      cat("  [OK]", scheme$colour_by, "|", grp$group, "|", vm$panel, "| runs:", nrow(run_stats), "\n")
    }

    GRID_NCOL <- if (length(panel_list) == 4) 2L else 3L
    n_rows <- ceiling(length(panel_list) / GRID_NCOL)
    caption_txt <- paste0(
      "Lines: individual model × scenario runs (thick = IIASA marker run). Bars at right: ", END_YEAR,
      " min–max per ", tolower(scheme$key_name), ". Table: ", END_YEAR, " level (min/max) and Δ = change vs ",
      RATIO_BASE_YEAR, " (per run, then min/max)",
      if (grp$group == "temperature") {
        paste0("; >1.5 °C / >2 °C = share of runs whose peak warming ", BASE_YEAR, "–", END_YEAR, " exceeds the Paris levels.")
      } else {
        "."
      },
      if (scheme$colour_by == "family") " Families: VL Very Low, LN Low-to-Negative, L Low, ML Medium-to-Low, M Medium, HL High-to-Low, H High." else ""
    )

    p_grid <- patchwork::wrap_plots(panel_list, ncol = GRID_NCOL) +
      patchwork::plot_annotation(
        title = paste0(grp$title, " by ", scheme$key_name, ", ", BASE_YEAR, "–", END_YEAR),
        caption = caption_txt,
        theme = theme(
          plot.title = element_text(size = 9.8, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 7, hjust = 0, colour = "#666666")
        )
      )

    out_png <- file.path(FIG_DIR, scheme$folder, paste0(grp$filename, ".png"))
    out_svg <- file.path(FIG_DIR_SVG, scheme$folder, paste0(grp$filename, ".svg"))
    ggsave(out_png, p_grid, units = "cm", dpi = 300, width = 11 * GRID_NCOL, height = 11 * n_rows + 1)
    ggsave(out_svg, p_grid, units = "cm", width = 11 * GRID_NCOL, height = 11 * n_rows + 1)
    group_svg_layers(out_svg)

    cat("  [OK] figure:", out_png, "\n")
  }
}


# Step 5: Save the numbers behind the tables ------------------------------------

climate_summary <- bind_rows(summary_rows) %>%
  mutate(key = as.character(key))
# Paris exceedance only meaningful for temperature
climate_summary <- climate_summary %>%
  mutate(across(c(pct_peak_15, pct_peak_20), ~ if_else(group == "temperature", .x, NA_real_)))

summary_file <- file.path(OUT_DIR, paste0("climate_summary_", END_YEAR, ".csv"))
write_csv(climate_summary, summary_file)
cat("\n  Saved", summary_file, "(", nrow(climate_summary), "rows )\n")

cat("\nDone ->", FIG_DIR, "\n")

# EoF
