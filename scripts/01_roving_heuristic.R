grid <- build_analysis_grid(analysis_bbox, cellsize = 2.5)

t_in_cell <- calculate_time_in_cell(dGNSS, grid, seed = 1)

heur <- calculate_detection_heuristic(
  roving_antenna = roving_antenna,
  grid = grid,
  t_in_cell = t_in_cell
)

heuristic_plot <- plot_heuristic_example(
  heur = heur,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  example_id = 4,
  crs = crs_lv95
)
