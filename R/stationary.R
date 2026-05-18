# =====================================================================
# Title: Stationary antenna interpretation helpers
# Author: Matt Jenkin
#
# Purpose:
#   Convert stationary antenna positions and in/out detections into
#   along-channel reach constraints for the method demonstration.
# =====================================================================

calculate_antenna_ranges <- function(antennas, channel_points, detection_range = 32) {
  # Each antenna is interpreted as covering the part of the channel that
  # falls inside an assumed horizontal detection buffer.
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
    # Duration is inferred from each "In" record to the following record for
    # the same particle and antenna.
    mutate(duration = lead(time) - time) |>
    filter(IO == "In") |>
    mutate(day = floor(time)) |>
    select(time, day, ID, antenna, duration, dep_time) |>
    filter(time > dep_time) |>
    left_join(antenna_ranges, by = "antenna") |>
    mutate(
      # Hourly bins reduce repeated high-frequency stationary reads to
      # compact reach-time records for the combined output table.
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
