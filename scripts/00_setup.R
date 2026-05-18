source("R/packages.R")
source("R/data_io.R")
source("R/geometry.R")
source("R/roving.R")
source("R/stationary.R")
source("R/combine.R")
source("R/validate.R")
source("R/plots.R")

load_method_packages()

crs_lv95 <- 2056
ci_buffer_range <- 28
stationary_detection_range <- 32
seed_x <- 2599065
seed_y <- 1087700
survey_end_distance <- 350

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

channel_points <- build_channel_points(channel_line)
