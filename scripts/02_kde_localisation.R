# =====================================================================
# Title: Localise roving detections and convert to channel distance
# Author: Matt Jenkin
#
# Purpose:
#   Use weighted KDE surfaces to estimate particle locations from roving
#   antenna detections, then convert those locations and uncertainty
#   contours to along-channel transport distances.
#
# Core logic:
#   1. Fit one weighted KDE surface for each particle-day.
#   2. Add seeding locations as known starting positions.
#   3. Extract particle point locations and uncertainty contours.
#   4. Intersect uncertainty contours with the channel centreline to
#      estimate downstream distance bounds.
#   5. Build plots for location uncertainty and transport distance.
#
# Inputs:
#   roving_detection_weights
#   tags
#   channel_points
#   survey_times
#   glacier_outline
#   channel_line
#
# Outputs:
#   roving_kde_surfaces
#   seeding_locations
#   roving_particle_locations
#   roving_transport_distances
#   location_plot
#   distance_plot
# =====================================================================

# KDE bandwidth selection is automatic here for a transparent method
# demonstration. For a new field site, bandwidth choice should be checked
# against survey geometry and detection density.
roving_kde_surfaces <- run_all_kdes(roving_detection_weights, crs = crs_lv95)

seeding_locations <- make_seeding_points(
  tags = tags,
  seed_x = seed_x,
  seed_y = seed_y,
  crs = crs_lv95
)

roving_particle_locations <- build_point_locations(
  kde = roving_kde_surfaces,
  seeding = seeding_locations,
  ci_buffer_range = ci_buffer_range
)

roving_transport_distances <- calculate_transport_distances(
  point_loc = roving_particle_locations,
  channel_points = channel_points,
  seeding = seeding_locations,
  survey_times = survey_times,
  ci_buffer_range = ci_buffer_range
)

location_plot <- plot_point_locations(
  point_loc = roving_particle_locations,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  crs = crs_lv95
)

distance_plot <- plot_transport_distances(
  t_dist = roving_transport_distances,
  survey_end_distance = survey_end_distance
)
