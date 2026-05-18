combine_transport_records <- function(t_dist, stat) {
  t_dist2 <- t_dist |>
    mutate(system = "rov") |>
    select(-day)

  stat2 <- stat |>
    select(ID, bin, duration, min_dist, max_dist) |>
    rename(time = bin) |>
    mutate(system = "stat")

  bind_rows(t_dist2, stat2) |>
    arrange(ID, time) |>
    distinct() |>
    select(ID, time, dist, min_dist, max_dist, duration, system)
}
