#!/usr/bin/env Rscript
# =====================================================================
# Title: Build roving detection weights
# Author: Matt Jenkin
#
# Purpose:
#   Convert raw roving antenna detections into gridded detection weights
#   for KDE localisation.
#
# Core logic:
#   - plot one raw roving survey day for inspection
#   - build a spatial analysis grid
#   - estimate survey effort as time spent in each grid cell
#   - weight detections by count, RSSI, and survey effort
#
# Inputs:
#   roving_antenna, dGNSS, glacier_outline, channel_line
#
# Outputs:
#   raw_roving_plot
#   grid
#   t_in_cell
#   roving_detection_weights
#   heuristic_plot
# =====================================================================

raw_roving_plot <- plot_raw_roving_day(
  roving_antenna = roving_antenna,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  dGNSS = dGNSS,
  example_day = example_roving_day,
  crs = crs_lv95
)

grid <- build_analysis_grid(analysis_bbox, cellsize = 2.5)

# Survey effort correction: estimate how long the moving antenna spent in
# each grid cell. Detection density is then interpreted relative to survey
# effort rather than as raw point density alone.
t_in_cell <- calculate_time_in_cell(dGNSS, grid, seed = 1)

# The `heur` column is retained as a data column because downstream KDE code
# uses it as the detection-weight field.
roving_detection_weights <- calculate_detection_heuristic(
  roving_antenna = roving_antenna,
  grid = grid,
  t_in_cell = t_in_cell
)

heuristic_plot <- plot_heuristic_example(
  roving_detection_weights = roving_detection_weights,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  example_id = 4,
  crs = crs_lv95
)
