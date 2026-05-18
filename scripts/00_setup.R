#!/usr/bin/env Rscript
# =====================================================================
# Title: Set up RFID method-example workflow
# Author: Matt Jenkin
#
# Purpose:
#   Load packages, helper functions, input data, and site-specific constants
#   for the compact RFID particle-tracking method example.
#
# Core logic:
#   - source helper functions from R/
#   - load required packages
#   - define CRS, localisation ranges, seeding location, and plotting extent
#   - load cleaned input data from data/
#   - build along-channel sample points used for transport-distance mapping
#
# Inputs:
#   R/*.R helper files
#   data/*.csv and data/*.gpkg input files
#
# Outputs:
#   Objects in the R session used by later scripts.
# =====================================================================

source("R/packages.R")
source("R/data_io.R")
source("R/geometry.R")
source("R/roving.R")
source("R/stationary.R")
source("R/combine.R")
source("R/validate.R")
source("R/plots.R")

load_method_packages()

# Coordinate reference system: Swiss LV95.
crs_lv95 <- 2056

# Spatial assumptions used by the compact method example.
ci_buffer_range <- 28
stationary_detection_range <- 32
seed_x <- 2599065
seed_y <- 1087700
survey_end_distance <- 350
example_roving_day <- 231

analysis_bbox <- st_bbox(
  c(
    xmin = 2598650,
    xmax = 2599150,
    ymin = 1087300,
    ymax = 1087800
  ),
  crs = st_crs(crs_lv95)
)

load_data_files()

check_required_objects(c(
  "antennas",
  "channel_line",
  "dGNSS",
  "glacier_outline",
  "roving_antenna",
  "stationary_antennas",
  "survey_times",
  "tags"
))

# Along-channel sample points provide the distance axis used to convert
# planimetric RFID locations and stationary antenna ranges into transport
# distance along the inferred channel path.
channel_points <- build_channel_points(channel_line)
