calculate_antenna_ranges <- function(antennas, channel_points, detection_range = 32) {
  antennas |>
    st_buffer(detection_range) |>
    safe_st_intersection(channel_points) |>
    arrange(antenna, dist) |>
    group_by(antenna) |>
    summarise(
      min_dist = min(dist),
      max_dist = max(dist),
      .groups = "drop"
    ) |>
    st_drop_geometry()
}

calculate_stationary_records <- function(stationary_antennas, antenna_ranges, tags) {
  stationary_antennas |>
    left_join(
      select(tags, ID, dep_time),
      by = "ID",
      relationship = "many-to-one"
    ) |>
    arrange(ID, time, antenna) |>
    group_by(ID, antenna) |>
    mutate(duration = lead(time) - time) |>
    filter(IO == "In") |>
    mutate(day = floor(time)) |>
    select(time, day, ID, antenna, duration, dep_time) |>
    filter(time > dep_time) |>
    left_join(antenna_ranges, by = "antenna") |>
    mutate(
      bin = cut(
        time,
        breaks = seq(1, 365, 1 / 24),
        labels = seq(1, 365 - 1 / 24, 1 / 24)
      ),
      bin = as.numeric(paste(bin))
    ) |>
    group_by(ID, antenna, bin) |>
    reframe(
      duration = sum(duration),
      min_dist = min(min_dist),
      max_dist = max(max_dist)
    )
}
