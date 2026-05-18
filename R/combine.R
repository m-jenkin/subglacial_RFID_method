# =====================================================================
# Title: Combine roving and stationary transport records
# Author: Matt Jenkin
#
# Purpose:
#   Standardise the roving and stationary RFID transport-distance outputs
#   into one combined table. Roving records provide point estimates and
#   uncertainty bounds; stationary records provide reach-scale detection
#   constraints.
#
# Inputs:
#   roving_transport_distances
#       Along-channel transport-distance records derived from roving KDE
#       localisation.
#
#   stationary_transport_records
#       Reach-scale detections derived from stationary antenna records.
#
# Output:
#   Combined table with harmonised columns:
#     ID, time, dist, min_dist, max_dist, duration, system
# =====================================================================

combine_transport_records <- function(roving_transport_distances,
                                      stationary_transport_records) {
  roving_records <- roving_transport_distances |>
    mutate(system = "rov") |>
    select(-day)

  stationary_records <- stationary_transport_records |>
    select(ID, bin, duration, min_dist, max_dist) |>
    rename(time = bin) |>
    mutate(system = "stat")

  bind_rows(roving_records, stationary_records) |>
    arrange(ID, time) |>
    distinct() |>
    select(ID, time, dist, min_dist, max_dist, duration, system)
}
