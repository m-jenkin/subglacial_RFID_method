# =====================================================================
# Title: Run minimal RFID particle-tracking method example
# Author: Matt Jenkin
#
# Purpose:
#   Machine-readable entry point for the public method demonstration. For
#   a guided explanation, render method_walkthrough.Rmd or open
#   method_walkthrough.html.
#
# Core logic:
#   1. Check that required workflow files are present.
#   2. Source each numbered script in method order.
#   3. Write checked CSV outputs to outputs/.
#
# Inputs:
#   scripts/*.R
#   R/*.R
#   data/*
#
# Outputs:
#   outputs/roving_transport_distances.csv
#   outputs/stationary_transport_records.csv
#   outputs/combined_transport_records.csv
# =====================================================================

required_files <- c(
  "scripts/00_setup.R",
  "scripts/01_roving_heuristic.R",
  "scripts/02_kde_localisation.R",
  "scripts/03_stationary_antennas.R",
  "scripts/04_outputs.R",
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
  "data/tags.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s): ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

run_section <- function(label, path) {
  message("\n==> ", label)
  source(path, local = .GlobalEnv)
}

run_section("Setup and input loading", "scripts/00_setup.R")
run_section("Roving antenna heuristic", "scripts/01_roving_heuristic.R")
run_section("KDE localisation and along-channel distance", "scripts/02_kde_localisation.R")
run_section("Stationary antenna interpretation", "scripts/03_stationary_antennas.R")
run_section("Output tables and validation", "scripts/04_outputs.R")

message("\nDone. Outputs written to outputs/.")
