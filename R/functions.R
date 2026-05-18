load_method_packages <- function() {
  pacman::p_load(
    tools, readr, tidyr, dplyr, lubridate, purrr, furrr, future, tibble,
    ggplot2, patchwork, scales, sf, ks, eks
  )

  options(readr.show_col_types = FALSE)
}

load_data_files <- function(data_dir = "data", envir = .GlobalEnv) {
  csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  gpkg_files <- list.files(data_dir, pattern = "\\.gpkg$", full.names = TRUE)

  walk(csv_files, \(path) {
    assign(
      file_path_sans_ext(basename(path)),
      read_csv(path, show_col_types = FALSE),
      envir = envir
    )
  })

  walk(gpkg_files, \(path) {
    assign(file_path_sans_ext(basename(path)), read_sf(path), envir = envir)
  })

  invisible(c(csv_files, gpkg_files))
}

check_required_objects <- function(required_objects, envir = .GlobalEnv) {
  missing_objects <- setdiff(required_objects, ls(envir = envir))

  if (length(missing_objects) > 0) {
    stop(
      "Missing required data object(s): ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
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

build_analysis_grid <- function(bbox, cellsize = 2.5) {
  st_make_grid(bbox, cellsize = cellsize, square = FALSE) |>
    st_as_sf() |>
    mutate(cell = row_number()) |>
    rename(geometry = x)
}

calculate_time_in_cell <- function(dGNSS, grid, seed = 1) {
  set.seed(seed)

  dGNSS |>
    st_intersects(grid) |>
    as.data.frame() |>
    rename(cell = col.id) |>
    group_by(row.id) |>
    slice_sample(n = 1) |>
    ungroup() |>
    bind_cols(day = dGNSS$day) |>
    count(day, cell, name = "t_in_cell")
}

calculate_detection_heuristic <- function(roving_antenna, grid, t_in_cell) {
  roving_antenna |>
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
    safe_st_centroid() |>
    mutate(
      x = st_coordinates(geometry)[, 1],
      y = st_coordinates(geometry)[, 2]
    ) |>
    drop_na()
}

run_kde_localisation <- function(data, crs = 2056) {
  try({
    current_id <- data$ID[1]
    current_day <- data$day[1]

    bandwidth_data <- data |>
      st_drop_geometry() |>
      select(x, y)

    h_value <- ks::Hscv.diag(bandwidth_data[, 1:2])
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
      st_as_sf(crs = crs, coords = c("max_x", "max_y"))
  })
}

run_all_kdes <- function(heur, crs = 2056) {
  future::plan(multisession)

  heur |>
    arrange(ID, day) |>
    group_by(ID, day) |>
    group_split() |>
    future_map(\(x) run_kde_localisation(x, crs = crs), .progress = TRUE)
}

make_seeding_points <- function(tags, seed_x, seed_y, crs = 2056) {
  tags |>
    select(ID, dep_time) |>
    rename(day = dep_time) |>
    mutate(
      day = floor(day),
      dist = 0,
      x = seed_x,
      y = seed_y
    ) |>
    st_as_sf(coords = c("x", "y"), crs = crs)
}

build_point_locations <- function(kde, seeding, ci_buffer_range = 28) {
  kde |>
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
}

calculate_transport_distances <- function(point_loc, channel_points, seeding,
                                          survey_times, ci_buffer_range = 28) {
  point_loc |>
    group_by(ID, day) |>
    st_set_geometry("CI") |>
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
}

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

validate_method_outputs <- function(t_dist, stat, combi) {
  expected_systems <- c("rov", "stat")

  checks <- list(
    roving_records = nrow(t_dist) > 0,
    stationary_records = nrow(stat) > 0,
    combined_records = nrow(combi) > 0,
    combined_systems = all(expected_systems %in% unique(combi$system)),
    ordered_distances = all(combi$min_dist <= combi$max_dist, na.rm = TRUE)
  )

  failed_checks <- names(checks)[!unlist(checks)]

  if (length(failed_checks) > 0) {
    stop(
      "Validation failed: ",
      paste(failed_checks, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
