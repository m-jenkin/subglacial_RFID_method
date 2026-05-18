# =====================================================================
# Title: Setup method demonstration inputs and constants
# Author: Matt Jenkin
#
# Purpose:
#   Load package dependencies, helper functions, cleaned input data and
#   shared constants for the minimal RFID method demonstration.
#
# Core logic:
#   1. Source helper functions from R/.
#   2. Define CRS, spatial tolerances, example plotting settings and
#      site-specific constants.
#   3. Load cleaned input tables/geometries from data/.
#   4. Build a sampled channel centreline used for distance conversion.
#
# Inputs:
#   R/*.R
#   data/*.csv
#   data/*.gpkg
#
# Outputs:
#   Loaded data objects, workflow constants and channel_points in the
#   global workflow environment.
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

# ---------------------------------------------------------------------
# 1. Site and method settings
# ---------------------------------------------------------------------

crs_lv95 <- 2056
ci_buffer_range <- 28
stationary_detection_range <- 32
seed_x <- 2599065
seed_y <- 1087700
survey_end_distance <- 350
example_roving_day <- 232

# Analysis extent used for the roving antenna grid. This is deliberately
# site-specific and should be reviewed before adapting the method elsewhere.
analysis_bbox <- st_bbox(
  c(
    xmin = 2598650,
    xmax = 2599150,
    ymin = 1087300,
    ymax = 1087800
  ),
  crs = st_crs(crs_lv95)
)

# ---------------------------------------------------------------------
# 2. Load cleaned input data and construct channel distance lookup
# ---------------------------------------------------------------------

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

# The channel line is sampled into approximately metre-spaced points so
# later spatial locations can be converted to along-channel distance.
channel_points <- build_channel_points(channel_line)
