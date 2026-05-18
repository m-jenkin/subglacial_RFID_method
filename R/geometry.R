# =====================================================================
# Title: Spatial geometry helpers
# Author: Matt Jenkin
#
# Purpose:
#   Provide small spatial helpers for gridding, channel-distance lookup and
#   warning-controlled sf operations used by the method demonstration.
# =====================================================================

safe_st_intersection <- function(x, y) {
  suppressWarnings(st_intersection(x, y))
}

safe_st_centroid <- function(x) {
  suppressWarnings(st_centroid(x))
}

build_channel_points <- function(channel_line) {
  # Approximate along-channel distance by sampling the interpreted channel
  # centreline into closely spaced points and accumulating distance along it.
  channel_line |>
    st_line_sample(n = as.integer(st_length(channel_line))) |>
    st_cast("POINT") |>
    st_as_sf() |>
    rename(geometry = x) |>
    mutate(
      step_dist = st_distance(geometry, lag(geometry), by_element = TRUE) |>
        as.numeric() |>
        round(),
      dist = cumsum(coalesce(step_dist, 0))
    ) |>
    select(-step_dist)
}

build_analysis_grid <- function(bbox, cellsize = 2.5) {
  # Hexagonal cells are used for the roving antenna weighting surface.
  st_make_grid(bbox, cellsize = cellsize, square = FALSE) |>
    st_as_sf() |>
    mutate(cell = row_number()) |>
    rename(geometry = x)
}
