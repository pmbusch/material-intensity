# =====================================================================
# Structured SVG export + editor-friendly grouping for ggplot2 figures
# ---------------------------------------------------------------------
# STATUS: NOT tested in the environment where it was written. The xml2
#   node-moving and namespace calls are written from knowledge. The two
#   fragile points, called out inline, are:
#     (A) the default id/class PATTERNS below (gridSVG appends unique
#         suffixes; your ggplot2 version may name grobs differently) --
#         verify with svg_element_table() before trusting the defaults;
#     (B) the inkscape namespace attribute injection in .set_layer_attrs()
#         (xml2 namespace handling is finicky and untested here).
#
# Dependencies: gridSVG, xml2, ggplot2, grid
# =====================================================================

# ---------------------------------------------------------------------
# 1. EXPORT  ----------------------------------------------------------
#    Prints a ggplot to a gridSVG device and writes labelled SVG.
#    addClasses=TRUE  -> each grob/viewport also gets an SVG `class`.
#    uniqueNames=TRUE -> ids get numeric suffixes to stay XML-valid.
# ---------------------------------------------------------------------
export_svg_structured <- function(plot, file, width = 7, height = 5, addClasses = TRUE) {
  stopifnot(inherits(plot, "ggplot"))
  requireNamespace("gridSVG")
  requireNamespace("grid")

  gridSVG::gridsvg(name = file, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot) # draws grobs to the device
  grid::grid.force() # realise the full grob tree
  invisible(file)
}
# NOTE: on some gridSVG/ggplot2 combos the gridsvg() device route fails and
# you instead do:  print(plot); gridSVG::grid.export(file, addClasses = TRUE)
# If export_svg_structured() errors, try that two-line form directly.

# ---------------------------------------------------------------------
# 2. DIAGNOSTIC  ------------------------------------------------------
#    Read-only. Lists every element with an id/class AND its cumulative
#    ancestor transform chain, so you can (a) see the REAL names in your
#    output and fix the patterns, and (b) confirm which elements share a
#    parent (only same-parent elements can be safely grouped).
# ---------------------------------------------------------------------
svg_element_table <- function(file) {
  requireNamespace("xml2")
  doc <- xml2::read_xml(file)
  # local-name() avoids all SVG default-namespace XPath pain:
  nodes <- xml2::xml_find_all(doc, "//*[@id or @class]")

  transform_chain <- function(node) {
    ts <- character(0)
    p <- node
    repeat {
      t <- xml2::xml_attr(p, "transform")
      if (!is.na(t)) {
        ts <- c(t, ts)
      }
      p <- xml2::xml_parent(p)
      if (inherits(p, "xml_missing") || length(p) == 0) break
    }
    paste(ts, collapse = " | ")
  }

  data.frame(
    tag = vapply(nodes, function(n) xml2::xml_name(n), character(1)),
    id = vapply(nodes, function(n) xml2::xml_attr(n, "id"), character(1)),
    class = vapply(nodes, function(n) xml2::xml_attr(n, "class"), character(1)),
    parent_id = vapply(nodes, function(n) xml2::xml_attr(xml2::xml_parent(n), "id"), character(1)),
    ancestor_xform = vapply(nodes, transform_chain, character(1)),
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------
# 3. GROUP  -----------------------------------------------------------
#    Safe grouping: for each logical group, matched nodes are re-wrapped
#    under a NEW <g> inserted as a child of their EXISTING parent. This
#    preserves inherited transforms. Members living under different
#    parents produce one sub-group per parent (reported via message()).
#
#    (A) FRAGILE: these default patterns are informed guesses about
#        ggplot/gridSVG grob names, NOT verified. Run svg_element_table()
#        first and adjust. Matching is case-insensitive regex on id.
# ---------------------------------------------------------------------
default_patterns <- list(
  frame = c("panel\\.background", "panel\\.border", "plot\\.background", "^background"),
  grids = c("panel\\.grid", "grill"),
  labels = c("axis", "title", "xlab", "ylab", "lab\\.", "guide"),
  data = c("geom_", "GeomPoint", "GeomLine", "GeomBar", "GeomPath")
)

group_svg_elements <- function(
  file_in,
  file_out,
  patterns = default_patterns,
  editor = c("plain", "inkscape"),
  id_prefix = "grp-"
) {
  requireNamespace("xml2")
  editor <- match.arg(editor)
  doc <- xml2::read_xml(file_in)

  if (editor == "inkscape") {
    xml2::xml_set_attr(xml2::xml_root(doc), "xmlns:inkscape", "http://www.inkscape.org/namespaces/inkscape")
  }

  # order matters: assign each node to the FIRST group it matches, so a
  # panel.grid node isn't also swept into a broad "data" pattern.
  assigned <- character(0) # ids already claimed

  for (gname in names(patterns)) {
    rx <- paste(patterns[[gname]], collapse = "|")
    hits <- xml2::xml_find_all(doc, "//*[@id]")
    ids <- vapply(hits, function(n) xml2::xml_attr(n, "id"), character(1))
    keep <- grepl(rx, ids, ignore.case = TRUE) & !(ids %in% assigned)
    hits <- hits[keep]
    if (length(hits) == 0) {
      message("group '", gname, "': 0 matches")
      next
    }
    assigned <- c(assigned, ids[keep])

    # bucket by parent so we never cross a transform boundary
    parents <- lapply(hits, xml2::xml_parent)
    pkey <- vapply(parents, function(p) xml2::xml_path(p), character(1))

    for (k in unique(pkey)) {
      members <- hits[pkey == k]
      parent <- xml2::xml_parent(members[[1]])

      # create the wrapper as a child of the shared parent
      wrapper <- xml2::xml_add_child(parent, "g", id = paste0(id_prefix, gname))
      if (editor == "inkscape") {
        .set_layer_attrs(wrapper, gname)
      }

      # move each member: copy under wrapper, then remove original.
      # (xml_add_child copies an existing node; z-order among moved
      #  members is preserved, but the wrapper is appended LAST among
      #  the parent's children -> see z-order caveat in the notes.)
      for (m in members) {
        xml2::xml_add_child(wrapper, m)
        xml2::xml_remove(m)
      }
      if (length(unique(pkey)) > 1) {
        message("group '", gname, "': split across ", length(unique(pkey)), " parents (one sub-group each)")
      }
    }
  }

  xml2::write_xml(doc, file_out)
  invisible(file_out)
}

# (B) FRAGILE / UNTESTED: inkscape sublayer attributes. A nested group
#     with groupmode=layer behaves as an Inkscape *sublayer*, not a
#     top-level layer. If xml2 rejects the prefixed attr, the fallback is
#     to write them post-hoc by string edit.
.set_layer_attrs <- function(node, label) {
  xml2::xml_set_attr(node, "inkscape:groupmode", "layer")
  xml2::xml_set_attr(node, "inkscape:label", label)
}

# ---------------------------------------------------------------------
# USAGE
# ---------------------------------------------------------------------
# library(ggplot2)
# p <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
#
# export_svg_structured(p, "fig_raw.svg")
#
# ids <- svg_element_table("fig_raw.svg")   # <-- INSPECT THIS FIRST
# print(ids[, c("tag","id","parent_id")])   #     fix patterns to match
#
# group_svg_elements("fig_raw.svg", "fig_grouped.svg",
#                    editor = "inkscape")    # or editor = "plain"
