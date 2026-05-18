#!/usr/bin/env Rscript
# =====================================================================
# Title: Run compact RFID particle-tracking method example
# Author: Matt Jenkin
#
# Purpose:
#   Machine-readable entry point for the minimal RFID method capsule. For a
#   guided explanation, render method_walkthrough.Rmd or open the included
#   walkthrough HTML.
#
# Core logic:
#   - check that required files are present
#   - run the numbered scripts in order
#   - write compact transport-distance outputs to outputs/
# =====================================================================

required_files <- c(
  "R/packages.R",
  "R/data_io.R",
  "R/geometry.R",
  "R/roving.R",
  "R/stationary.R",
  "R/combine.R",
  "R/validate.R",
  "R/plots.R",
  "data/antennas.gpkg",
  "data/channel_line.gpkg",
  "data/dGNSS.gpkg",
  "data/glacier_outline.gpkg",
  "data/roving_antenna.gpkg",
  "data/stationary_antennas.csv",
  "data/survey_times.csv",
  "data/tags.csv",
  "scripts/00_setup.R",
  "scripts/01_roving_heuristic.R",
  "scripts/02_kde_localisation.R",
  "scripts/03_stationary_antennas.R",
  "scripts/04_outputs.R"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n",
    paste(" -", missing_files, collapse = "\n"),
    call. = FALSE
  )
}

run_script <- function(path) {
  message("\n--- Running ", path, " ---")
  source(path, local = .GlobalEnv)
}

scripts <- c(
  "scripts/00_setup.R",
  "scripts/01_roving_heuristic.R",
  "scripts/02_kde_localisation.R",
  "scripts/03_stationary_antennas.R",
  "scripts/04_outputs.R"
)

for (script in scripts) {
  run_script(script)
}

message("\nDone. Outputs written to outputs/.")
