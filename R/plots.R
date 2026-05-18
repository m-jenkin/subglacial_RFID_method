plot_heuristic_example <- function(heur, glacier_outline, channel_line,
                                   example_id = 4, crs = 2056) {
  hex_data <- st_join(
    st_sf(st_make_grid(heur, cellsize = 8, square = FALSE)),
    filter(heur, ID == example_id),
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

plot_point_locations <- function(point_loc, glacier_outline, channel_line, crs = 2056) {
  ggplot() +
    geom_sf(data = glacier_outline, fill = "white") +
    geom_sf(data = channel_line, col = "blue") +
    geom_sf(
      data = point_loc |> st_set_geometry("CI"),
      aes(col = factor(day)),
      fill = NA,
      lwd = 0.5
    ) +
    geom_sf(
      data = point_loc |> st_set_geometry("geometry"),
      aes(col = factor(day))
    ) +
    labs(x = NULL, y = NULL, colour = "Day") +
    coord_sf(datum = crs) +
    facet_wrap(~ID) +
    theme_bw() +
    ggtitle("Particle location with 95% uncertainty")
}

plot_transport_distances <- function(t_dist, survey_end_distance = 350) {
  ggplot(t_dist, aes(time, dist, col = factor(ID))) +
    geom_line(linetype = "dotted") +
    geom_errorbar(aes(ymin = min_dist, ymax = max_dist), width = 0.25) +
    geom_point() +
    geom_hline(yintercept = survey_end_distance, linetype = "dashed") +
    scale_y_reverse() +
    scale_x_continuous(breaks = breaks_pretty()) +
    labs(colour = "ID") +
    facet_wrap(~ID) +
    theme_bw()
}

plot_combined_records <- function(t_dist, stat, survey_end_distance = 350) {
  ggplot() +
    geom_rect(
      data = stat,
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
      data = filter(stat, antenna == 15),
      aes(x = bin, y = (max_dist + min_dist) / 2),
      col = "darkgrey",
      shape = 20,
      show.legend = FALSE
    ) +
    geom_line(
      data = t_dist,
      aes(time, dist, col = factor(ID)),
      linetype = "dashed",
      lwd = 0.8
    ) +
    geom_errorbar(
      data = t_dist,
      aes(x = time, ymin = min_dist, ymax = max_dist, col = factor(ID)),
      width = 0.25
    ) +
    geom_point(data = t_dist, aes(time, dist, col = factor(ID))) +
    scale_y_reverse() +
    geom_hline(yintercept = survey_end_distance, linetype = "dashed") +
    facet_wrap(~ID) +
    theme_bw() +
    labs(
      title = "Particle transport distance over time",
      subtitle = "Grey bars indicate stationary antenna detections shaded by duration\nBlack points highlight detection at the end of the survey zone",
      alpha = "log duration [hrs]",
      colour = "Particle ID"
    )
}
