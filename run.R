# Packages and data -------------------------------------------------------

pacman::p_load(tools, readr, tidyr, dplyr, lubridate, purrr, ggplot2, scales, sf, ks, eks)
files_delim <- list.files("./data", pattern = ".csv", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_csv(.x), envir = .GlobalEnv))
files_spatial <- list.files("./data", pattern = ".gpkg", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_sf(.x), envir = .GlobalEnv))

# Heuristic ---------------------------------------------------------------

bbox <- st_bbox(
  c(
    xmin = 2598650, xmax = 2599150, # spatial extent of the analysis
    ymin = 1087300, ymax = 1087800
  ),
  crs = st_crs(2056)
)

grid <- st_make_grid(bbox, square = F, cellsize = 5) |> # create a grid and number cells
  st_as_sf() |>
  mutate(cell = row_number()) |>
  rename(geometry = x)

t_in_cell <- dGNSS |> # find the time roving antenna in each cell per survey
  st_intersection(grid) |>
  group_by(day, cell) |>
  summarise(t_in_cell = n()) |>
  st_drop_geometry()

heur <- roving_antenna |> # calculate a heuristic accounting for signal strength, n observations and time in cell
  st_intersection(grid) |>
  group_by(ID, day, cell) |>
  reframe(
    n = n(),
    mean_RSSI = mean(RSSI)
  ) |>
  left_join(t_in_cell, by = c("day", "cell"), relationship = "many-to-many") |>
  left_join(grid, by = "cell") |>
  group_by(ID, day) |>
  mutate(
    mean_RSSI = rescale(mean_RSSI),
    heur = (n * mean_RSSI) / t_in_cell,
    heur = rescale(heur)
  ) |>
  st_as_sf() |>
  st_centroid() |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  )

# KDE ---------------------------------------------------------------------

kde <- heur |>
  group_by(ID, day) |>
  arrange(ID, day) |>
  eks::st_kde() # relatively slow but simple to work with the data

combinations <- heur |> # to add ID and day data back into `kde`
  group_by(ID, day) |>
  arrange(ID, day) |>
  st_drop_geometry() |>
  select(ID, day) |>
  distinct()

find_max_coords <- function(kde) { # function to extract the KDEmax point
  max_estimate <- max(kde$estimate)
  max_idx <- which(kde$estimate == max_estimate, arr.ind = TRUE)
  x_coord <- kde$eval.points[[1]][max_idx[1]]
  y_coord <- kde$eval.points[[2]][max_idx[2]]

  tibble(x = x_coord, y = y_coord, max_estimate = max_estimate)
}

kde_max <- map(kde$tidy_ks$ks, find_max_coords) |>
  bind_rows() |>
  bind_cols(combinations) |> # add the ID and day data back in
  st_as_sf(coords = c("x", "y"), crs = 2056) |>
  st_join(channel, join = st_nearest_feature) # KDEmax point gets along-channel distance of nearest point on channel

injection <- tags |> # add in data for when the particle was injected
  select(ID, dep_time) |>
  rename(day = dep_time) |>
  mutate(
    day = floor(day),
    dist = 0,
    x = 2599065,
    y = 1087700,
    z = NA
  ) |>
  st_as_sf(coords = c("x", "y"), crs = 2056)
kde_max <- bind_rows(injection, kde_max)

# Stationary antennas -----------------------------------------------------

antenna_range <- antennas |>
  st_buffer(30) |> # define a detection range of 30 m around each antenna
  st_intersection(channel) |>
  group_by(antenna) |>
  summarise(
    min_dist = round(min(dist)), # the upper and lower bounds of detection along the channel
    max_dist = round(max(dist))
  ) |>
  st_drop_geometry()

stationary_antennas <- stationary_antennas |>
  arrange(ID, time, antenna) |>
  group_by(ID, antenna) |>
  mutate(duration = lead(time) - time) |> # time particle in range of antenna
  filter(IO == "In") |>
  select(time, day, ID, antenna, duration, dep_time) |>
  filter(time > dep_time) |>
  left_join(antenna_range) |>
  mutate(
    bin = cut(time,
      breaks = seq(1, 365, 1 / 24), # cut data into hourly bins
      labels = seq(1, 365 - 1 / 24, 1 / 24)
    ),
    bin = as.numeric(paste(bin))
  ) |>
  group_by(ID, antenna, bin) |>
  reframe(
    duration = sum(duration), # summarise across hourly bins
    min_dist = min_dist,
    max_dist = max_dist
  )

# Plotting ----------------------------------------------------------------

ggplot() +
  geom_sf(
    data = channel |>
      st_union() |>
      st_cast("LINESTRING"),
    col = "blue", size = 0.3
  ) +
  geom_sf(data = glacier_outline, fill = NA) +
  geom_sf(data = kde_max, aes(col = factor(day))) +
  geom_sf(
    data = st_get_contour(kde, cont = c(94.99, 95)),
    aes(col = factor(day)), fill = NA
  ) +
  facet_wrap(~ID) +
  theme_bw() +
  labs(col = "Day") +
  ggtitle("Particle location on a given survey with 95% CI")

ggplot() +
  geom_rect(
    data = stationary_antennas,
    aes(
      xmin = bin, xmax = bin + (1 / 24),
      ymin = min_dist, ymax = max_dist,
      alpha = duration, fill = factor(antenna)
    ), show.legend = F
  ) +
  geom_point(data = kde_max, aes(day, dist)) +
  geom_step(data = kde_max, aes(day, dist)) +
  scale_y_reverse() +
  geom_hline(yintercept = 350, linetype = "dashed") +
  facet_wrap(~ID) +
  theme_bw() +
  ggtitle("Particle transport distance over time with a simple linear model linking roving antenna data",
    subtitle = "See Jenkin et al. in prep for automated modelling combining roving and stationary antennas"
  )
