#' ---
#' title: Tracking coarse sediment in an Alpine subglacial channel
#' author: Matt Jenkin
#' date: Feb 2024
#' format:
#'   html:
#'     self-contained: true
#' execute:
#'  warning: false
#' ---

pacman::p_load(tools, readr, tidyr, dplyr, lubridate, purrr, ggplot2, patchwork, scales, sf, ks, eks)
#'
#| include: false
files_delim <- list.files("./data", pattern = ".csv", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_csv(.x), envir = .GlobalEnv))
files_spatial <- list.files("./data", pattern = ".gpkg", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_sf(.x), envir = .GlobalEnv))

#' ## Heuristic
#' Here we derive a gridded heuristic accounting for RFID signal strength, number of observations and time spent surveying in a given grid cell.

bbox <- st_bbox(
  c(
    xmin = 2598650, xmax = 2599150, # spatial extent of the analysis
    ymin = 1087300, ymax = 1087800
  ),
  crs = st_crs(2056) # EPSG code for swiss CRS
)

grid <- st_make_grid(bbox, square = F, cellsize = 5) |> # create a hexagonal grid
  st_as_sf() |>
  mutate(cell = row_number()) |> # number grid cells
  rename(geometry = x)

t_in_cell <- dGNSS |> # find the time roving antenna in each cell per survey
  st_intersection(grid) |>
  group_by(day, cell) |>
  summarise(t_in_cell = n()) |>
  st_drop_geometry()

heur <- roving_antenna |> # calculate the heuristic
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

#'
#| echo: false
ggplot() +
  geom_sf(data = glacier_outline, fill = NA) +
  geom_sf(data = channel |>
            st_union() |>
            st_cast('LINESTRING'),
          col = 'blue') +
  geom_tile(data = heur, aes(x, y, fill = factor(day), alpha = heur),
            width = 10, height = 10) +
  labs(x = NULL, y = NULL, fill = 'Day', alpha = 'Heuristic') +
  coord_sf(datum = 2056) +
  facet_wrap(~ID) +
  theme_bw() +
  ggtitle('Gridded heuristic for particle observation strength',
          subtitle = '2x grid cell scale exaggeration')

#' ## KDE
#' We then perform a kernel smoothing to estimate the probability density distribution in 2D space of each tagged particle on each survey, weighted by the heuristic. The location with the highest probability density (KDE_max) is taken as the particle location, with a 95% confidence interval contour. The along-channel distance of the closest channel point to the KDE_max and the intersections of the 95% CI are computed. The `st_kde()` function is relatively slow but makes it easy to work with the data later on - I would advise parallel computing (e.g. `futures` and `furrr`) for many particles or adapting the code to use the `ks::kde()` function directly. The bandwidth is precomputed and can be changed.

bandwidths <- heur |>
  group_by(ID, day) |>
  arrange(ID, day) |>
  st_drop_geometry() |>
  group_split() |>
  map_dfr(~ {
    h_value <- Hscv.diag(.x[, 8:9])
    tibble(ID = unique(.x$ID), day = unique(.x$day), H = list(h_value))
  })

kde <- heur |>
  group_by(ID, day) |>
  arrange(ID, day) |>
  group_split() |>
  map(~ {
    current_id <- unique(.x$ID)[1]
    current_day <- unique(.x$day)[1]
    h_value <- bandwidths |>
      filter(ID == current_id, day == current_day) |>
      pull(H) |>
      first()

    st_kde(.x, w = .x$heur, H = h_value)
  })

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

find_CI <- function(kde) {
  CI <- kde |> filter(contlabel == 95)
  intersections <- st_intersection(CI, channel) |>
    summarise(lower_CI = max(dist),
              upper_CI = min(dist))
}

kde_max <- map(kde, ~find_max_coords(.x[['tidy_ks']][['ks']][[1]])) |>
  bind_rows() |>
  bind_cols(combinations) |> # add the ID and day data back in
  st_as_sf(coords = c("x", "y"), crs = 2056) |>
  st_join(channel, join = st_nearest_feature) # KDEmax point gets along-channel distance of nearest point on channel

kde_CI <- map(kde, ~find_CI(.x[['sf']])) |>
  bind_rows() |>
  bind_cols(combinations) |>
  st_drop_geometry()

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
kde_max <- bind_rows(injection, kde_max) |>
  left_join(kde_CI)

#'
#| echo: false
# To plot all localisations - correct approach using map() but not quite working
# plot_function <- function(kde, ID, day, channel, glacier_outline) {
#   data = st_get_contour(kde, cont = c(94.99, 95)) |>
#       mutate(ID = ID, day = day)
#
#   p <- ggplot() +
#     geom_sf(data = channel |>
#               st_union() |>
#               st_cast("LINESTRING"),
#             col = "blue", size = 0.3) +
#     geom_sf(data = glacier_outline, fill = NA) +
#     # geom_sf(data = kde_max, aes(col = factor(day))) +
#     geom_sf(data = data, aes(col = day), fill = NA) +
#     theme_bw() +
#     theme(axis.text = element_blank()) +
#     ggtitle("Particle location on a given survey with 95% CI")
#
#   return(p)
# }
#
# plots <- map2(kde, seq_along(kde), ~plot_function(.x, combinations$ID[.y], combinations$day[.y], channel, glacier_outline))
# wrap_plots(plots)

#' ## Stationary antennas
#' We then define an estimated horizontal detection range of each stationary antenna using a spatial buffer and determine the upper and lower limits of detection along the subglacial channel. The time particles spent in range of the antennas is calculated and binned by hourly timesteps.

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
  left_join(antenna_range, by = 'antenna') |>
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

#' Displaying both the roving antenna KDE_max data (black dots) and stationary antenna data (coloured bars) as a function of along-channel transport distance from the borehole.
#'
#| echo: false

ggplot() +
  geom_rect(
    data = stationary_antennas,
    aes(
      xmin = bin, xmax = bin + (1 / 24),
      ymin = min_dist, ymax = max_dist,
      alpha = duration, fill = factor(antenna)
    ), show.legend = F
  ) +
  geom_errorbar(data = kde_max, aes(day, ymin = lower_CI, ymax = upper_CI), width = 0.5) +
  geom_point(data = kde_max, aes(day, dist)) +
  scale_y_reverse() +
  geom_hline(yintercept = 350, linetype = "dashed") +
  facet_wrap(~ID) +
  theme_bw() +
  ggtitle("Particle transport distance over time",
          subtitle = "See Jenkin et al. in prep for automated modelling combining roving and stationary antennas"
  )

