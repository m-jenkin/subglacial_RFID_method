antenna_ranges <- calculate_antenna_ranges(
  antennas = antennas,
  channel_points = channel_points,
  detection_range = stationary_detection_range
)

stat <- calculate_stationary_records(
  stationary_antennas = stationary_antennas,
  antenna_ranges = antenna_ranges,
  tags = tags
)

combined_plot <- plot_combined_records(
  t_dist = t_dist,
  stat = stat,
  survey_end_distance = survey_end_distance
)
