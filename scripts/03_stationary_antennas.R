# =====================================================================
# Title: Interpret stationary antenna detections
# Author: Matt Jenkin
#
# Purpose:
#   Convert stationary antenna in/out records into reach-scale downstream
#   distance constraints for each detected particle.
#
# Core logic:
#   1. Buffer each stationary antenna by an assumed horizontal detection
#      range.
#   2. Intersect buffered antenna zones with the channel centreline.
#   3. Convert stationary in/out detections into time-binned reach records.
#   4. Plot stationary records together with roving transport estimates.
#
# Inputs:
#   antennas
#   stationary_antennas
#   tags
#   channel_points
#   roving_transport_distances
#
# Outputs:
#   stationary_antenna_ranges
#   stationary_transport_records
#   combined_plot
# =====================================================================

# The detection range is a site/method assumption rather than a measured
# property of every individual read. It should be calibrated or justified
# before adapting the method elsewhere.
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
  t_dist = roving_transport_distances,
  stat = stationary_transport_records,
  survey_end_distance = survey_end_distance
)
