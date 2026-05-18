# =====================================================================
# Title: Combine roving and stationary transport records
# Author: Matt Jenkin
#
# Purpose:
#   Bind point-like roving transport estimates and reach-like stationary
#   detections into one compact table while preserving the evidence system.
# =====================================================================

combine_transport_records <- function(t_dist, stat) {
  roving_records <- t_dist |>
    mutate(system = "rov") |>
    select(-day)

  stationary_records <- stat |>
    select(ID, bin, duration, min_dist, max_dist) |>
    rename(time = bin) |>
    mutate(system = "stat")

  bind_rows(roving_records, stationary_records) |>
    arrange(ID, time) |>
    distinct() |>
    select(ID, time, dist, min_dist, max_dist, duration, system)
}
