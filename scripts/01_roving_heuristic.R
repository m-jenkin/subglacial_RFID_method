# =====================================================================
# Title: Build roving antenna detection weights
# Author: Matt Jenkin
#
# Purpose:
#   Convert raw roving RFID detections into gridded particle-day weights
#   that account for detection count, received signal strength and survey
#   effort.
#
# Core logic:
#   1. Plot raw roving detections for one example survey day.
#   2. Build a regular analysis grid over the survey area.
#   3. Estimate survey time spent in each grid cell from dGNSS positions.
#   4. Combine detection count, RSSI and survey effort into a relative
#      roving detection weight used by KDE localisation.
#
# Inputs:
#   roving_antenna
#   dGNSS
#   glacier_outline
#   channel_line
#   analysis_bbox
#
# Outputs:
#   raw_roving_plot
#   survey_grid
#   survey_time_by_cell
#   roving_detection_weights
#   heuristic_plot
# =====================================================================

raw_roving_plot <- plot_raw_roving_day(
  roving_antenna = roving_antenna,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  example_day = example_roving_day,
  crs = crs_lv95
)

# The grid scale is a judgement-based method parameter. It should be small
# enough to preserve local structure, but large enough that repeated dGNSS
# points and RFID detections can be meaningfully counted within cells.
survey_grid <- build_analysis_grid(analysis_bbox, cellsize = 2.5)

# Survey effort is estimated from the roving antenna dGNSS track. This
# corrects against over-weighting places where the antenna simply lingered.
survey_time_by_cell <- calculate_time_in_cell(dGNSS, survey_grid, seed = 1)

# This heuristic is intentionally simple and transparent: detections are
# stronger when there are more reads and higher RSSI, then down-weighted by
# the time spent surveying that cell.
roving_detection_weights <- calculate_detection_heuristic(
  roving_antenna = roving_antenna,
  grid = survey_grid,
  t_in_cell = survey_time_by_cell
)

heuristic_plot <- plot_heuristic_example(
  heur = roving_detection_weights,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  example_id = 4,
  crs = crs_lv95
)
