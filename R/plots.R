# =====================================================================
# Title: Plot helpers for RFID method example
# Author: Matt Jenkin
#
# Purpose:
#   Provide lightweight diagnostic plots for the method walkthrough. These
#   plots illustrate the main stages of the RFID localisation workflow but
#   are not required for the transport-distance calculations themselves.
# =====================================================================

plot_raw_roving_day <- function(roving_antenna, glacier_outline, channel_line,
                                dGNSS, example_day = 233, crs = 2056) {
  roving_day <- roving_antenna |>
    filter(day == example_day)

  dGNSS_day <- dGNSS |>
    filter(day == example_day)

  ggplot() +
    geom_sf(data = glacier_outline, fill = "white", colour = "grey70") +
    geom_sf(data = dGNSS_day, colour = 'lightgrey', size = 0.13) +
    geom_sf(data = channel_line, colour = "blue", linewidth = 0.6) +
    geom_sf(
      data = roving_day,
      aes(colour = factor(ID), alpha = RSSI),
      size = 0.7
    ) +
    coord_sf(datum = crs) +
    theme_bw() +
    labs(
      title = paste("Raw roving RFID detections, day", example_day),
      subtitle = "Points are antenna positions at received transmissions; transparency is RSSI",
      x = NULL,
      y = NULL,
      colour = "Particle ID",
      size = "RSSI"
    )
}

plot_heuristic_example <- function(roving_detection_weights, glacier_outline,
                                   channel_line, example_id = 4, crs = 2056) {
  hex_data <- st_join(
    st_sf(st_make_grid(roving_detection_weights, cellsize = 8, square = FALSE)),
    filter(roving_detection_weights, ID == example_id),
    join = st_intersects
  ) |>
    drop_na()

  ggplot() +
    geom_sf(data = glacier_outline, fill = "white") +
    geom_sf(data = hex_data, aes(fill = factor(day), alpha = heur), col = NA) +
    geom_sf(data = channel_line, col = "blue") +
    facet_wrap(~ID) +
    labs(x = NULL, y = NULL, col = "Day", alpha = "Heuristic", size = "Heuristic") +
    coord_sf(datum = crs) +
    theme_bw() +
    labs(
      title = "Example of gridded heuristic for particle detection strength",
      subtitle = "Exaggerated grid scale"
    )
}

plot_point_locations <- function(roving_particle_locations, glacier_outline,
                                 channel_line, crs = 2056) {
  ggplot() +
    geom_sf(data = glacier_outline, fill = "white") +
    geom_sf(data = channel_line, col = "blue") +
    geom_sf(
      data = roving_particle_locations |> st_set_geometry("CI"),
      aes(col = factor(day)),
      fill = NA,
      lwd = 0.5
    ) +
    geom_sf(
      data = roving_particle_locations |> st_set_geometry("geometry"),
      aes(col = factor(day))
    ) +
    labs(x = NULL, y = NULL, colour = "Day") +
    coord_sf(datum = crs) +
    facet_wrap(~ID) +
    theme_bw() +
    ggtitle("Particle location with 95% uncertainty")
}

plot_transport_distances <- function(roving_transport_distances,
                                     survey_end_distance = 350) {
  ggplot(roving_transport_distances, aes(time, dist, col = factor(ID))) +
    geom_line(linetype = "dotted") +
    geom_errorbar(aes(ymin = min_dist, ymax = max_dist), width = 0.25) +
    geom_point() +
    geom_hline(yintercept = survey_end_distance, linetype = "dashed") +
    scale_y_reverse() +
    scale_x_continuous(breaks = breaks_pretty()) +
    labs(colour = "ID",
         x = 'Day',
         y = "Along-channel transport distance [m]",
         subtitle = "Black dashed line indicates glacier terminus"
         ) +
    facet_wrap(~ID, scales = 'free_x') +
    theme_bw()
}

plot_combined_records <- function(roving_transport_distances,
                                  stationary_transport_records,
                                  survey_end_distance = 350) {
  ggplot() +
    geom_rect(
      data = stationary_transport_records,
      aes(
        xmin = bin,
        xmax = bin + (1 / 24),
        ymin = min_dist,
        ymax = max_dist,
        alpha = log(duration)
      ),
      fill = "darkgrey",
      show.legend = TRUE
    ) +
    geom_point(
      data = filter(stationary_transport_records, antenna == 15),
      aes(x = bin, y = (max_dist + min_dist) / 2),
      col = "darkgrey",
      shape = 20,
      show.legend = FALSE
    ) +
    geom_line(
      data = roving_transport_distances,
      aes(time, dist, col = factor(ID)),
      linetype = "dashed",
      lwd = 0.8
    ) +
    geom_errorbar(
      data = roving_transport_distances,
      aes(x = time, ymin = min_dist, ymax = max_dist, col = factor(ID)),
      width = 0.25
    ) +
    geom_point(
      data = roving_transport_distances,
      aes(time, dist, col = factor(ID))
    ) +
    scale_y_reverse() +
    scale_x_continuous(breaks = breaks_pretty()) +
    geom_hline(yintercept = survey_end_distance, linetype = "dashed") +
    facet_wrap(~ID, scales = 'free_x') +
    theme_bw() +
    labs(
      title = "Particle transport distance over time",
      subtitle = "Grey bars indicate stationary antenna detections shaded by duration",
      alpha = "log duration [hrs]",
      colour = "Particle ID",
      x = 'Day',
      y = "Along-channel transport distance [m]"
      )
}
