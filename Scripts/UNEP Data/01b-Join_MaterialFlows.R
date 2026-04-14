# =============================================================================
# Join Material Flows Domestic Extraction Data
# https://visualisations.materialflows.net/mf-shiny/?#shiny-tab-sunburst
# Domestric Material Extraction by Material Full detail
# Need to download all files first
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(data.table)
library(future.apply)

# To check and erase wrongly downloaded filed
# files <- list.files("Inputs/MaterialFlows/", recursive = TRUE, full.names = TRUE)
# matches <- grepl("1970-20", files)
# (matching_strings <- files[matches])
material_flow_files <- list.files("Inputs/MaterialFlows/", pattern = "bymaterialgroup", full.names = TRUE)
material_flow_files <- material_flow_files[!str_detect(material_flow_files, "\\.crdownload$")]
length(material_flow_files) # 12660

# Function to read a single file
read_material_flow <- function(f) {
  filename <- basename(f)
  country <- str_match(filename, "DomesticExtractionof(.+)in[0-9]{4}bymaterialgroup\\.csv$")[, 2]
  year <- as.integer(str_match(filename, "in([0-9]{4})bymaterialgroup\\.csv$")[, 2])
  dt <- fread(f) # very fast
  dt[, filename := filename]
  dt[, country := country]
  dt[, year := year]
  dt
}

output_file <- "Inputs/MaterialFlows_all.csv"
chunk_size <- 250 # number of files to read per batch
file_chunks <- split(material_flow_files, ceiling(seq_along(material_flow_files) / chunk_size))


# Parallel read all files - takes a while
# read in loops to clear memory, still takes a while
if (file.exists(output_file)) {
  file.remove(output_file)
}
process_chunk <- function(files_chunk) {
  dt_list <- lapply(files_chunk, read_material_flow)
  rbindlist(dt_list)
}

output_file_temp <- "Inputs/Temp/MaterialFlows_all_%s.csv"

# Loop through chunks
# Takes time, and sometimes fails so needs to be run in batches
for (i in 40:length(file_chunks)) {
  # for (i in 20:25) {
  cat("Processing chunk", i, "of", length(file_chunks), "\n")
  chunk_dt <- process_chunk(file_chunks[[i]])

  # Append to CSV
  # fwrite(chunk_dt, output_file, append = file.exists(output_file)) # OLD, but now throws error
  fwrite(chunk_dt, sprintf(output_file_temp, i), append = FALSE) # write separate files to avoid memory issues

  # Clean up memory
  rm(chunk_dt)
  gc()
}

# Read file
# material_flows_all <- fread(output_file)
files <- list.files("Inputs/Temp", pattern = "\\.csv$", full.names = TRUE)
material_flows_all <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
nrow(material_flows_all) / 1e6 # 0.44M

# sanity check
unique(material_flows_all$Label)
unique(material_flows_all$Parent)
unique(material_flows_all$country)
range(material_flows_all$year)

# Filter for repeated files, keep most recent
material_flows_all <- material_flows_all %>%
  mutate(
    download_date = ymd(str_extract(filename, "^[0-9]{4}-[0-9]{2}-[0-9]{2}")), # extract YYYY-MM-DD
    country = str_match(filename, "DomesticExtractionof(.+)in[0-9]{4}bymaterialgroup\\.csv$")[, 2]
  ) %>%
  group_by(country) %>%
  filter(download_date == max(download_date)) %>% # keep only the latest per country
  ungroup()

# Classify materials (Parent) by level
# to copy/paste and analyze
material_flows_all |> group_by(Label, Parent) |> tally()
material_flows_all <- material_flows_all |>
  mutate(
    level_material = case_when(
      Label == "Domestic Extraction" ~ "Level 0",
      Label %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals") ~ "Level 1",
      Parent %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals") ~ "Level 2",
      T ~ "Level 3"
    )
  )

write_csv(material_flows_all, output_file)

# EoF
