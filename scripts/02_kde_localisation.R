kde <- run_all_kdes(heur, crs = crs_lv95)

seeding <- make_seeding_points(
  tags = tags,
  seed_x = seed_x,
  seed_y = seed_y,
  crs = crs_lv95
)

point_loc <- build_point_locations(
  kde = kde,
  seeding = seeding,
  ci_buffer_range = ci_buffer_range
)

t_dist <- calculate_transport_distances(
  point_loc = point_loc,
  channel_points = channel_points,
  seeding = seeding,
  survey_times = survey_times,
  ci_buffer_range = ci_buffer_range
)

location_plot <- plot_point_locations(
  point_loc = point_loc,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  crs = crs_lv95
)

distance_plot <- plot_transport_distances(
  t_dist = t_dist,
  survey_end_distance = survey_end_distance
)
