## Load all required libraries to use
## Common file to run from multiple scripts
## PBH Feb 2023

# Library -----
list_libraries <- c(
  "tidyverse",
  "tidyverse",
  "geomtextpath",
  "readr",
  "readxl",
  "ggplot2",
  "data.table",
  "dplyr",
  "gridExtra",
  "glmnet",
  "openxlsx",
  "reshape2",
  "scales",
  "RColorBrewer",
  "sf",
  "ggrepel",
  "geomtextpath"
) # maps

# Install libraries if they are not present
# UNCOMMENT THE CODE TO INSTALL LIBRARIES THE FIRST TIME
# new_libraries <- list_libraries[!(list_libraries %in% installed.packages()[,"Package"])]
# lapply(new_libraries, install.packages)
# rm(new_libraries)

lapply(list_libraries, require, character.only = TRUE)

rm(list_libraries)

source("Scripts/00b-Theme.R", encoding = "UTF-8")
source("Scripts/00-CommonParameters.R", encoding = "UTF-8")

# Functions -----
# load all required functions automatically
file.sources = list.files("Scripts/00-Functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)
rm(file.sources)

# Common function to save SVG files with text without the auto-text size autoformatting
clean_svg <- function(filename, output = filename) {
  svg_content <- readLines(filename, warn = FALSE)
  # Handle both single and double quotes
  svg_content <- gsub('textLength="[^"]*"', '', svg_content)
  svg_content <- gsub("textLength='[^']*'", '', svg_content)
  svg_content <- gsub('lengthAdjust="[^"]*"', '', svg_content)
  svg_content <- gsub("lengthAdjust='[^']*'", '', svg_content)
  writeLines(svg_content, output)
}

group_svg_layers <- function(input, output = input, flat = FALSE) {
  doc <- xml2::read_xml(input)
  xml2::xml_set_attr(xml2::xml_root(doc), "xmlns:inkscape", "http://www.inkscape.org/namespaces/inkscape")

  classify <- function(node) {
    tag <- xml2::xml_name(node)
    sty <- xml2::xml_attr(node, "style")
    if (is.na(sty)) {
      sty <- ""
    }
    switch(
      tag,
      text = "labels",
      polygon = "data",
      circle = "data",
      path = "data",
      rect = "frame",
      line = "frame",
      polyline = {
        if (grepl("stroke-dasharray", sty)) {
          "grids"
        } else {
          n <- length(strsplit(trimws(xml2::xml_attr(node, "points")), "\\s+")[[1]])
          if (n <= 2) "frame" else "data"
        }
      },
      NA_character_
    )
  }

  # Panel content groups: <g> children of <g class='svglite'> with >2 elements
  groups <- xml2::xml_find_all(doc, "/*[local-name()='svg']/*[local-name()='g']/*[local-name()='g']")
  panels <- groups[vapply(
    groups,
    function(g) {
      length(xml2::xml_children(g)) > 2
    },
    logical(1)
  )]

  cat_order <- c("frame", "grids", "data", "labels")

  for (i in seq_along(panels)) {
    p <- panels[[i]]
    kids <- xml2::xml_children(p)
    cats <- vapply(kids, classify, character(1))

    for (cat in cat_order) {
      idx <- which(cats == cat)
      if (!length(idx)) {
        next
      }
      gname <- if (flat) cat else sprintf("panel%d.%s", i, cat)
      wrapper <- xml2::xml_add_child(p, "g")
      xml2::xml_set_attr(wrapper, "id", gname)
      xml2::xml_set_attr(wrapper, "inkscape:label", gname)
      xml2::xml_set_attr(wrapper, "inkscape:groupmode", "layer")
      for (k in idx) {
        xml2::xml_add_child(wrapper, kids[[k]], .copy = TRUE)
        xml2::xml_remove(kids[[k]])
      } # moves node
    }
  }

  xml2::write_xml(doc, output)
  clean_svg(output)
  invisible(output)
}

# EoF
