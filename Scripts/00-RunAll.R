## =============================================================================
## 00-RunAll.R
## Reproduces the full analysis from raw Inputs/ to Figures/.
## Each script runs in a fresh Rscript process (no shared global state), in
## pipeline order. Stops at the first failure.
##
## Usage (from the project root):
##   Rscript Scripts/00-RunAll.R          # run everything
##   Rscript Scripts/00-RunAll.R 12       # resume from script #12 of the list
## =============================================================================

scripts <- c(
  # Stage 1: aggregate raw data to regions (-> Parameters/UNEP-Materials, UN-Population, Worldbank-GDP, MISO-Stock)
  "Scripts/01-PrepareData/01-Aggregate_UNEP.R",
  "Scripts/01-PrepareData/02-Aggregate_UN.R",
  "Scripts/01-PrepareData/03-Aggregate_GDP.R",
  "Scripts/01-PrepareData/04-Aggregate_MISO.R",
  "Scripts/01-PrepareData/05-Aggregate_EIA_Capacity.R",
  # Local data not in git (materialflows.net raw files, S&P deposits); their
  # outputs are tracked in Parameters/MaterialFlows and Parameters/SP:
  # "Scripts/01-PrepareData/06-Aggregate_MaterialFlows.R",
  "Scripts/01-PrepareData/07-NonFerrous_Split.R",
  # "Scripts/01-PrepareData/08-IronOre_GradeCurve.R",
  # "Scripts/01-PrepareData/09-NonFerrous_GradeCurve.R",

  # Stage 2: historical end-use split, ore grade, stock-flow model (-> Parameters/Intermediate, MISO-Stock)
  "Scripts/02-HistoricalStock/01-UNEP_enduse_shares.R",
  "Scripts/02-HistoricalStock/02-UNEP_enduse_detail.R",
  "Scripts/02-HistoricalStock/03-OreGrade_Factor_g.R",
  "Scripts/02-HistoricalStock/04-MISO_scope_factor.R",
  "Scripts/02-HistoricalStock/05-UNEP_stock_flow_model.R",
  "Scripts/02-HistoricalStock/06-AgePyramid.R",
  "Scripts/02-HistoricalStock/07-HistoricalSecondaryFlows.R",
  "Scripts/02-HistoricalStock/08-stock_intensity_analysis.R",

  # Stage 3: SSP drivers and ScenarioMIP intensity trajectories (-> Parameters/IIASA-Trajectories)
  "Scripts/03-SSP Trajectories/01-preprocess_ssp_drivers.R",
  "Scripts/03-SSP Trajectories/02-load_and_check.R",
  "Scripts/03-SSP Trajectories/03-compute_metrics.R",
  "Scripts/03-SSP Trajectories/04-trajectory_figures.R",
  "Scripts/03-SSP Trajectories/05-trajectory_by_ssp_region.R",
  "Scripts/03-SSP Trajectories/06-primary_energy_figures.R",
  "Scripts/03-SSP Trajectories/07-capacity_figures.R",
  "Scripts/03-SSP Trajectories/08-summary_ratio.R",
  "Scripts/03-SSP Trajectories/09-climate_figures.R",

  # Stage 4: Monte Carlo simulation (-> Parameters/Simulation, Results/MC)
  "Scripts/04-Simulation/01-Sampling.R",
  "Scripts/04-Simulation/02-PowerSector.R",
  "Scripts/04-Simulation/03-RunSimulations.R",
  "Scripts/04-Simulation/04-Diagnostics.R",
  "Scripts/04-Simulation/05-Decoupling.R",

  # Stage 5: main figures and table (-> Figures/)
  "Scripts/05-Figures/Figure 1 - HistoricalTimeseries.R",
  "Scripts/05-Figures/Figure 2 - Assumptions.R",
  "Scripts/05-Figures/Figure 2 Detail - Material.R",
  "Scripts/05-Figures/Figure 2 Detail - Region.R",
  "Scripts/05-Figures/Figure 2 Detail - Region Summary.R",
  "Scripts/05-Figures/Figure 3 - Contours-GrowthRate.R",
  "Scripts/05-Figures/Figure 4 - Climate - PrepareData.R",
  "Scripts/05-Figures/Figure 5 - Sensitivity - PrepareData.R", # before Figure 4: its panel a reads Figure5_Scatter.csv
  "Scripts/05-Figures/Figure 4 - Climate.R",
  "Scripts/05-Figures/Figure 5 - Sensitivity.R",
  "Scripts/05-Figures/Figure 5 - Alt1 - PrepareData.R",
  "Scripts/05-Figures/Figure 5 - Alt1.R",
  "Scripts/05-Figures/Table 1 - Results.R",

  # Stage 6: supporting figures not produced above (-> Figures/Supporting-Figures)
  "Scripts/05-Figures/Supporting-Figures/S01-S02 - CumulativeIntensityCurve.R",
  "Scripts/05-Figures/Supporting-Figures/S10 - SamplingBounds.R",
  "Scripts/05-Figures/Supporting-Figures/S11 - FlowTimeSeries.R",
  "Scripts/05-Figures/Supporting-Figures/S12 - TimeSeriesProjection.R",
  "Scripts/05-Figures/Supporting-Figures/S13-S14 - MaterialContribution.R",
  "Scripts/05-Figures/Supporting-Figures/S15 - RegionContribution.R",
  "Scripts/05-Figures/Supporting-Figures/S16 - Density.R",
  "Scripts/05-Figures/Supporting-Figures/S18 - RegionalAnalysis.R",
  "Scripts/05-Figures/Supporting-Figures/S19 - CumulativeExtraction.R",
  "Scripts/05-Figures/Supporting-Figures/S27-S28 - RegionalTimeseries.R",
  "Scripts/05-Figures/Supporting-Figures/S29 - CircularityStockGrowth.R",
  "Scripts/05-Figures/Supporting-Figures/S30 - MetalOreExtraction.R",
  "Scripts/05-Figures/Supporting-Figures/S31 - MaterialGroupExtraction.R"
)

start_from <- if (length(commandArgs(TRUE)) > 0) as.integer(commandArgs(TRUE)[1]) else 1L
rscript <- file.path(R.home("bin"), "Rscript")

for (i in seq(start_from, length(scripts))) {
  cat(sprintf("\n[%02d/%02d] %s  (%s)\n", i, length(scripts), scripts[i], format(Sys.time(), "%H:%M:%S")))
  t0 <- Sys.time()
  status <- system2(rscript, shQuote(scripts[i]))
  cat(sprintf("        exit %d, %.1f min\n", status, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  if (status != 0) stop("Failed at script #", i, ": ", scripts[i], " -- fix and resume with: Rscript Scripts/00-RunAll.R ", i)
}

cat("\nAll", length(scripts), "scripts completed.\n")
