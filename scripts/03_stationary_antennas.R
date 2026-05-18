#!/usr/bin/env Rscript
# =====================================================================
# Title: Stationary antenna reach records
# Author: Matt Jenkin
#
# Purpose:
#   Convert stationary antenna detections into reach-scale transport records
#   that can be combined with roving transport-distance estimates.
#
# Core logic:
#   - buffer each stationary antenna by an approximate detection range
#   - intersect those buffers with the channel path
#   - summarise antenna-specific along-channel detection ranges
#   - convert stationary in/out detections to binned reach records
#
# Inputs:
#   antennas, stationary_antennas, channel_points, tags
#
# Outputs:
#   stationary_antenna_ranges
#   stationary_transport_records
#   combined_plot
# =====================================================================

stationary_antenna_ranges <- calculate_antenna_ranges(
  antennas = antennas,
  channel_points = channel_points,
  detection_range = stationary_detection_range
)

stationary_transport_records <- calculate_stationary_records(
  stationary_antennas = stationary_antennas,
  antenna_ranges = stationary_antenna_ranges,
  tags = tags
)

combined_plot <- plot_combined_records(
  roving_transport_distances = roving_transport_distances,
  stationary_transport_records = stationary_transport_records,
  survey_end_distance = survey_end_distance
)
