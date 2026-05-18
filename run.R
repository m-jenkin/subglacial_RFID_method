# Reproduce the minimal RFID particle-tracking method example.
#
# This script is the machine-readable entry point. For a guided explanation,
# render method_walkthrough.Rmd or open run.html.

source("scripts/00_setup.R")
source("scripts/01_roving_heuristic.R")
source("scripts/02_kde_localisation.R")
source("scripts/03_stationary_antennas.R")
source("scripts/04_outputs.R")

message("Done. Outputs written to outputs/.")
