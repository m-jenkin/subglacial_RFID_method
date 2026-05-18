#' ---
#' title: Tracking coarse sediment in an Alpine subglacial channel
#' author: Matt Jenkin
#' date: Feb 2024
#' format:
#'   html:
#'     self-contained: true
#'     toc: true
#'     toc-depth: 2
#'     code-fold: true
#'     code-summary: Show code
#'     theme: cosmo
#' execute:
#'  echo: true
#'  message: false
#'  warning: false
#'  fig-width: 8
#'  fig-height: 5
#' ---

pacman::p_load(
  tools, readr, tidyr, dplyr, lubridate, purrr, furrr, future, tibble,
  ggplot2, patchwork, scales, sf, ks, eks
)

options(readr.show_col_types = FALSE)
#'
#| include: false

crs_lv95 <- 2056
ci_buffer_range <- 28
stationary_detection_range <- 32
seed_x <- 2599065
seed_y <- 1087700
survey_end_distance <- 350

analysis_bbox <- st_bbox(
  c(
    xmin = 2598650,
    xmax = 2599150,
    ymin = 1087300,
    ymax = 1087800
  ),
  crs = st_crs(crs_lv95)
)

load_data_files <- function(data_dir = "data") {
  csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  gpkg_files <- list.files(data_dir, pattern = "\\.gpkg$", full.names = TRUE)

  walk(csv_files, \(path) {
    assign(
      file_path_sans_ext(basename(path)),
      read_csv(path, show_col_types = FALSE),
      envir = .GlobalEnv
    )
  })

  walk(gpkg_files, \(path) {
    assign(file_path_sans_ext(basename(path)), read_sf(path), envir = .GlobalEnv)
  })
}

check_required_objects <- function(required_objects) {
  missing_objects <- setdiff(required_objects, ls(envir = .GlobalEnv))

  if (length(missing_objects) > 0) {
    stop(
      "Missing required data object(s): ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }
}

safe_st_intersection <- function(x, y) {
  suppressWarnings(st_intersection(x, y))
}

safe_st_centroid <- function(x) {
  suppressWarnings(st_centroid(x))
}

build_channel_points <- function(channel_line) {
  channel_line |>
    st_line_sample(n = as.integer(st_length(channel_line))) |>
    st_cast("POINT") |>
    st_as_sf() |>
    rename(geometry = x) |>
    mutate(
      step_dist = st_distance(geometry, lag(geometry), by_element = TRUE) |>
        as.numeric() |>
        round(),
      dist = cumsum(coalesce(step_dist, 0))
    ) |>
    select(-step_dist)
}

load_data_files()

check_required_objects(c(
  "antennas",
  "channel_line",
  "dGNSS",
  "glacier_outline",
  "roving_antenna",
  "stationary_antennas",
  "survey_times",
  "tags"
))

channel_points <- build_channel_points(channel_line)

#' The data required are: roving antenna points (RFID detection with associated dGNSS point), all dGNSS survey points, tag metadata, survey times, channel centreline linestring, stationary antenna data and metadata (if available). This is intended as an example to aid generalisations to other RFID particle tracking projects. Consistent data CRS required.

#' ## Heuristic
#' Here we derive a gridded heuristic accounting for RFID signal strength, number of observations and time spent surveying in a given grid cell. It essentially reduces bias caused by differences in the amount of time the antenna spent surveying grid cells. The heuristic be modified or excluded depending on the study design.

grid <- st_make_grid(analysis_bbox, cellsize = 2.5, square = FALSE) |>
  st_as_sf() |>
  mutate(cell = row_number()) |>
  rename(geometry = x)

# Keep any overlap assignment deterministic. A dGNSS point can touch more than
# one cell boundary, and we only want to count it once per second.
set.seed(1)

t_in_cell <- dGNSS |>
  st_intersects(grid) |>
  as.data.frame() |>
  rename(cell = col.id) |>
  group_by(row.id) |>
  slice_sample(n = 1) |>
  ungroup() |>
  bind_cols(day = dGNSS$day) |>
  count(day, cell, name = "t_in_cell")

heur <- roving_antenna |>
  # Expected sf warning: attributes are assumed constant over split geometries.
  safe_st_intersection(grid) |>
  group_by(ID, day, cell) |>
  reframe(
    n = n(),
    mean_RSSI = mean(RSSI),
    .groups = "drop"
  ) |>
  left_join(t_in_cell, by = c("day", "cell"), relationship = "many-to-many") |>
  mutate(
    mean_RSSI = rescale(mean_RSSI, to = c(0, 1)),
    heur = (n * mean_RSSI) / t_in_cell
  ) |>
  group_by(ID, day) |>
  mutate(heur = rescale(heur)) |>
  ungroup() |>
  left_join(grid, by = "cell") |>
  st_sf() |>
  # Expected sf warning: cell attributes are carried to their centroid.
  safe_st_centroid() |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) |>
  drop_na()

#'
#| echo: false

hex_data <- st_join(
  st_sf(st_make_grid(heur, cellsize = 8, square = FALSE)),
  filter(heur, ID == 4),
  join = st_intersects
) |>
  drop_na()

ggplot() +
  geom_sf(data = glacier_outline, fill = "white") +
  geom_sf(data = hex_data, aes(fill = factor(day), alpha = heur), col = NA) +
  geom_sf(data = channel_line, col = "blue") +
  facet_wrap(~ID) +
  labs(x = NULL, y = NULL, col = "Day", alpha = "Heuristic", size = "Heuristic") +
  coord_sf(datum = crs_lv95) +
  theme_bw() +
  labs(
    title = "Example of gridded heuristic for particle detection strength",
    subtitle = "Exaggerated grid scale"
  )

#' ## KDE
#' We then perform a kernel density estimation (KDE) to estimate the probability density distribution in 2D space of each tagged particle on each survey, weighted by the heuristic. The location with the highest probability density (KDE_max) is taken as the particle location, with a 95% confidence interval contour. The along-channel distance of the closest channel point to the KDE_max and the intersections of the 95% CI are computed. This step requires a channel centreline point geometry feature, with the cumulative (along-channel) distance between points computed. I would advise parallel computing (e.g. `futures` and `furrr`) for many particles.

future::plan(multisession)

kde_func <- function(data) {
  try({
    current_id <- data$ID[1]
    current_day <- data$day[1]

    bandwidth_data <- data |>
      st_drop_geometry() |>
      select(x, y)

    h_value <- ks::Hscv.diag(bandwidth_data[, 1:2])
    # ks rescales weights internally; the repeated warning is expected here.
    kde_result <- suppressWarnings(eks::st_kde(data, w = data$heur, H = h_value))

    eval_points <- kde_result[["tidy_ks"]][["ks"]][[1]][["eval.points"]]
    estimate <- kde_result[["tidy_ks"]][["ks"]][[1]][["estimate"]]
    max_cell <- which(estimate == max(estimate), arr.ind = TRUE)

    ci_geometry <- eks::st_get_contour(kde_result, cont = 95) |>
      st_simplify(dTolerance = 0.5)

    tibble(
      ID = current_id,
      day = current_day,
      max_estimate = max(estimate),
      max_x = eval_points[[1]][max_cell[1]],
      max_y = eval_points[[2]][max_cell[2]],
      CI = ci_geometry$geometry
    ) |>
      st_as_sf(crs = crs_lv95, coords = c("max_x", "max_y"))
  })
}

kde <- heur |>
  arrange(ID, day) |>
  group_by(ID, day) |>
  group_split() |>
  future_map(kde_func, .progress = TRUE)

seeding <- tags |>
  select(ID, dep_time) |>
  rename(day = dep_time) |>
  mutate(
    day = floor(day),
    dist = 0,
    x = seed_x,
    y = seed_y
  ) |>
  st_as_sf(coords = c("x", "y"), crs = crs_lv95)

point_loc <- kde |>
  bind_rows(seeding) |>
  arrange(ID, day) |>
  group_by(ID, day) |>
  slice_max(max_estimate) |>
  mutate(
    CI_area = as.numeric(st_area(CI)),
    CI = if_else(
      CI_area < 500 | is.na(max_estimate),
      st_buffer(geometry, ci_buffer_range),
      CI
    )
  ) |>
  st_set_geometry("CI") |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON", warn = FALSE) |>
  mutate(CI_area = as.numeric(st_area(CI))) |>
  group_by(ID, day) |>
  slice_max(CI_area) |>
  mutate(CI = if_else(is.na(max_estimate), NA, CI))

t_dist <- point_loc |>
  group_by(ID, day) |>
  st_set_geometry("CI") |>
  # Expected sf warning: attributes are assumed constant over split geometries.
  safe_st_intersection(channel_points) |>
  group_by(ID, day) |>
  summarise(
    geometry = st_union(geometry),
    min_dist = round(min(dist.1)),
    max_dist = round(max(dist.1)),
    .groups = "drop"
  ) |>
  st_set_geometry("geometry") |>
  st_join(channel_points, join = st_nearest_feature) |>
  st_drop_geometry() |>
  mutate(
    dist = if_else(round(dist) < 3, 0, round(dist)),
    min_dist = if_else(min_dist < 3, 0, min_dist),
    dist = if_else(dist < min_dist | dist > max_dist, (min_dist + max_dist) / 2, dist),
    min_dist = if_else(is.na(min_dist), dist - ci_buffer_range, min_dist),
    max_dist = if_else(is.na(max_dist), dist + ci_buffer_range, max_dist)
  ) |>
  full_join(st_drop_geometry(seeding), by = c("ID", "day", "dist")) |>
  left_join(survey_times, by = "day")

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
  coord_sf(datum = crs_lv95) +
  facet_wrap(~ID) +
  theme_bw() +
  ggtitle("Particle location with 95% uncertainty")

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

#' ## Stationary antennas
#' We then define an estimated horizontal detection range of each stationary antenna using a spatial buffer and determine the upper and lower limits of detection along the subglacial channel. The time particles spent in range of the antennas is calculated and binned by hourly timesteps.

a_range <- antennas |>
  st_buffer(stationary_detection_range) |>
  # Expected sf warning: attributes are assumed constant over split geometries.
  safe_st_intersection(channel_points) |>
  arrange(antenna, dist) |>
  group_by(antenna) |>
  summarise(
    min_dist = min(dist),
    max_dist = max(dist),
    .groups = "drop"
  ) |>
  st_drop_geometry()

stat <- stationary_antennas |>
  arrange(ID, time, antenna) |>
  group_by(ID, antenna) |>
  mutate(duration = lead(time) - time) |>
  filter(IO == "In") |>
  select(time, day, ID, antenna, duration, dep_time) |>
  filter(time > dep_time) |>
  left_join(a_range, by = "antenna") |>
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

#' Displaying both the roving antenna KDE_max data (black dots) and stationary antenna data (coloured bars) as a function of along-channel transport distance from the borehole.
#'
#| echo: false

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

t_dist2 <- t_dist |>
  mutate(system = "rov") |>
  select(-day)

stat2 <- stat |>
  select(ID, bin, duration, min_dist, max_dist) |>
  rename(time = bin) |>
  mutate(system = "stat")

combi <- bind_rows(t_dist2, stat2) |>
  arrange(ID, time) |>
  distinct() |>
  select(ID, time, dist, min_dist, max_dist, duration, system)
