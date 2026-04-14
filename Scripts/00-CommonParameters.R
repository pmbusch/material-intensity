## =============================================================================
## 00-CommonParameters.R
## Shared named colour palettes for regions, material categories, and material
## groups. Sourced automatically via 00-Libraries.R.
##
## Usage in ggplot:
##   scale_fill_manual(values = PALETTE_REGIONS)
##   scale_colour_manual(values = PALETTE_MATERIALS)
##   scale_fill_manual(values = PALETTE_MATERIAL_GROUPS)
##
## Unknown categories fall back to grey via na.value = "#999999".
## =============================================================================

# ── Regions (Analysis_group, 9 levels) ────────────────────────────────────────
# Based on Paul Tol's Vibrant palette with geographic-semantic adjustments.

PALETTE_REGIONS <- c(
  "East Asia" = "#CC3311",
  "Europe & Russia" = "#0077BB",
  "North America" = "#004488",
  "Latin America" = "#228B22",
  "Sub-Saharan Africa" = "#EE7733",
  "North Africa & Middle East" = "#CCBB44",
  "South Asia" = "#AA3377",
  "Southeast Asia" = "#009988",
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
  # — Biomass —
  "Crops" = "#F9A825",
  "Crop Residues" = "#C8A415",
  "Grazed biomass and fodder crops" = "#558B2F",
  "Wild catch and harvest" = "#0277BD",
  "Wood" = "#4E342E",
  "Non-wild animal products" = "#EF6C00",
  "Products mainly from biomass nec." = "#AED581",
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
  "Energy fuels"           = "#5D4037",   # dark oil brown  (fossil fuel family)
  "Biomass"                = "#558B2F",   # grass green      (biomass family)
  "Metals and ores"        = "#B71C1C",   # iron rust red    (metals family)
  "Construction materials" = "#78909C",   # stone grey-blue  (minerals family)
  "Mixed"                  = "#7B1FA2",   # purple
  "Waste"                  = "#37474F"    # dark charcoal
)
