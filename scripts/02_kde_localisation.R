#!/usr/bin/env Rscript
# =====================================================================
# Title: KDE localisation and roving transport distances
# Author: Matt Jenkin
#
# Purpose:
#   Estimate particle locations from weighted roving detections and convert
#   those locations to along-channel transport distances.
#
# Core logic:
#   - run weighted KDE for each particle-day
#   - add seeding locations as known zero-distance observations
#   - retain point estimates and 95% uncertainty contours
#   - intersect uncertainty contours with the channel path
#   - calculate along-channel transport-distance estimates and bounds
#
# Inputs:
#   roving_detection_weights, tags, channel_points, survey_times
#
# Outputs:
#   roving_kde_surfaces
#   seeding_locations
#   roving_particle_locations
#   roving_transport_distances
#   location_plot
#   distance_plot
# =====================================================================

roving_kde_surfaces <- run_all_kdes(
  roving_detection_weights,
  crs = crs_lv95
)

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
  roving_particle_locations = roving_particle_locations,
  glacier_outline = glacier_outline,
  channel_line = channel_line,
  crs = crs_lv95
)

distance_plot <- plot_transport_distances(
  roving_transport_distances = roving_transport_distances,
  survey_end_distance = survey_end_distance
)
