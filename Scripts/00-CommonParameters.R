## =============================================================================
## 00-CommonParameters.R
## Shared named colour palettes for regions, material categories, and material
## groups, plus the central projection-horizon parameter. Sourced
## automatically via 00-Libraries.R -- this is the FIRST config file every
## script gets, before any figure- or simulation-specific sourcing, so
## FORECAST_END is guaranteed to already be in scope everywhere.
##
## Usage in ggplot:
##   scale_fill_manual(values = PALETTE_REGIONS)
##   scale_colour_manual(values = PALETTE_MATERIALS)
##   scale_fill_manual(values = PALETTE_MATERIAL_GROUPS)
##
## Unknown categories fall back to grey via na.value = "#999999".
## =============================================================================

# ── Projection horizon (central parameter) ─────────────────────────────────────
# Single source of truth for how far every projected simulation (MC + the
# deterministic DSM forecast scripts) and every projected figure runs.
# Change this ONE number to extend/shorten the horizon everywhere downstream.
# BASE_YEAR and TARGET_YEAR (intensity convergence, held at 2050 as a modelling
# choice independent of the horizon) live in Scripts/model_parameters.R.
FORECAST_END <- 2100L

# ── Regions (Analysis_group, 8 levels) ────────────────────────────────────────
# Based on Paul Tol's Vibrant palette with geographic-semantic adjustments.

PALETTE_REGIONS <- c(
  "East Asia" = "#CC3311",
  "Europe & Russia" = "#0077BB",
  "North America" = "#004488",
  "Latin America" = "#228B22",
  "Sub-Saharan Africa" = "#EE7733",
  "Middle East & North Africa" = "#CCBB44",
  "South Asia" = "#AA3377",
  "Oceania" = "#33BBEE"
)

# ── Material categories (21 confirmed; add 22nd if it appears in data) ────────
# Colours are grouped by material family with lightness variation within groups:
#   Fossil fuels  → brown / black / grey  (Coal darkest → plastics lightest)
#   Biomass       → green / yellow / tan / ocean-blue
#   Metals        → rust-red / copper / steel-blue
#   Minerals      → stone grey family
#   Mixed / Waste → purple / dark charcoal

PALETTE_MATERIALS <- c(
  # — Fossil fuels —
  "Coal" = "#1C1C1C",
  "Natural Gas" = "#78909C",
  "Petroleum" = "#5D4037",
  "Oil shale and tar sands" = "#8D6E63",
  "Refined fossil fuels mainly for fuel e.g. LPG gasoline diesel" = "#A1887F",
  "Other products mainly from fossil fuels e.g. plastics" = "#BCAAA4",
  "Other fossil fuels" = "#9E9D88",
  # — Biomass —
  "Crops" = "#F9A825",
  "Crop Residues" = "#C8A415",
  "Grazed biomass and fodder crops" = "#558B2F",
  "Wild catch and harvest" = "#0277BD",
  "Wood" = "#4E342E",
  "Non-wild animal products" = "#EF6C00",
  "Products mainly from biomass nec." = "#AED581",
  "Other biomass" = "#7BAE43",
  # — Metals —
  "Ferrous ores" = "#B71C1C",
  "Non-ferrous ores" = "#C77B00",
  "Products mainly from metals nec." = "#455A64",
  # — Non-metallic minerals —
  "Non-metallic minerals - construction dominant" = "#9E9E9E",
  "Non-metallic minerals - industrial or agricultural dominant" = "#CFD8DC",
  "Products mainly from non-metallic minerals" = "#607D8B",
  # — Mixed / Waste —
  "Mixed / complex products nec." = "#7B1FA2",
  "Waste for final treatment and disposal" = "#37474F"
)

# ── Material groups (6 levels) ────────────────────────────────────────────────
# Summary-level palette: each colour is the anchor hue from its material family.

# fmt: skip
PALETTE_MATERIAL_GROUPS <- c(
  "Fossil fuels"           = "#5D4037",   # dark oil brown  (fossil fuel family)
  "Biomass"                = "#558B2F",   # grass green      (biomass family)
  "Metal ores"        = "#B71C1C",   # iron rust red    (metals family)
  "Non-metallic minerals" = "#78909C",   # stone grey-blue  (minerals family)
  "Mixed"                  = "#7B1FA2",   # purple
  "Waste"                  = "#37474F"    # dark charcoal
)

# Longest-lived at bottom of stack, shortest at top.
PALETTE_ENDUSE <- c(
  "Buildings"            = "#1B4F8A",
  "Civil infrastructure" = "#7A5230",
  "Machinery"            = "#2D6A4F",
  "Short-lived products" = "#7B2D8B"
)

# Sub-end-use palette: 2 tones per main category, keyed by display label.
PALETTE_SUBENDUSE <- c(
  "Residential"           = "#1B4F8A",
  "Residential Bldg"      = "#1B4F8A",
  "Non-residential"       = "#5285C0",
  "Non-residential Bldg"  = "#5285C0",
  "Roads"                 = "#7A5230",
  "Civil engineering"     = "#B8896A",
  "Civil eng."            = "#B8896A",
  "Machinery"             = "#2D6A4F",
  "Vehicles"              = "#5DAA80",
  "Durables"              = "#7B2D8B",
  "Packaging"             = "#B068C0"
)

SSP_COLORS <- c("SSP1" = "#2d7d46", "SSP2" = "#8db84e", "SSP3" = "#e8a628", "SSP4" = "#d4622a", "SSP5" = "#c0392b")

# ── Decoupling classification (4 levels) ──────────────────────────────────────
# Used by 19-Decoupling.R, 19b-Decoupling-ScenarioDiscovery.R and Figure 3.
# "Peak decoupling": per-capita material consumption peaks before TARGET_YEAR
# and is declining by TARGET_YEAR, but the endpoint-to-endpoint CAGR is still
# >= 0 (net level at TARGET_YEAR still above BASE_YEAR) -- distinct from
# "Absolute decoupling", which requires a net decrease over the full window.

PALETTE_DECOUPLING <- c(
  "Absolute decoupling" = "#1B7837",
  "Peak decoupling" = "#74C476",
  "Relative decoupling" = "#D9A404",
  "No decoupling" = "#B2182B"
)
