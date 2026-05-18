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

pacman::p_load(tools, readr, tidyr, dplyr, lubridate, purrr, furrr, future, ggplot2, patchwork, scales, sf, ks, eks)
#'
#| include: false
files_delim <- list.files("./data", pattern = ".csv", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_csv(.x), envir = .GlobalEnv))
files_spatial <- list.files("./data", pattern = ".gpkg", full.names = T) |>
  walk(~ assign(file_path_sans_ext(basename(.x)), read_sf(.x), envir = .GlobalEnv))
range <- 28

# interpolate the channel line to 1m resolution and calculate cumulative along-channel distance
channel_points <- channel_line |>
  st_line_sample(n = as.integer(st_length(channel_line))) |>
  st_cast("POINT") |>
  st_as_sf() |>
  rename(geometry = x) |>
  mutate(dist = st_distance(geometry, lag(geometry), by_element = T) |>
           as.numeric() |>
           round(),
         dist = cumsum(coalesce(dist, 0)))

#' The data required are: roving antenna points (RFID detection with associated dGNSS point), all dGNSS survey points, tag metadata, survey times, channel centreline linestring, stationary antenna data and metadata (if available). This is intended as an example to aid generalisations to other RFID particle tracking projects. Consistent data CRS required.

#' ## Heuristic
#' Here we derive a gridded heuristic accounting for RFID signal strength, number of observations and time spent surveying in a given grid cell. It essentially reduces bias caused by differences in the amount of time the antenna spent surveying grid cells. The heuristic be modified or excluded depending on the study design.

bbox <- st_bbox( # spatial extent of the analysis
  c(xmin = 2598650, xmax = 2599150,
    ymin = 1087300, ymax = 1087800),
  crs = st_crs(2056) # EPSG code for Swiss CRS (CH1903+/LV95)
)

grid <- st_make_grid(bbox, cellsize = 2.5, square = F) |> # create a hexagonal grid with 2.5 m cells
  st_as_sf() |>
  mutate(cell = row_number()) |> # ID number of grid cell
  rename(geometry = x)

t_in_cell <- dGNSS |> # find the time roving antenna in each grid cell per survey
  st_intersects(grid) |> # assign each dGNSS point to a grid cell
  as.data.frame() |>
  rename(cell = col.id) |>
  group_by(row.id) |>
  slice_sample(n = 1) |> # if point covers multiple cells select one randomly
  bind_cols(day = dGNSS$day) |> # add survey day data back in
  group_by(day, cell) |>
  count(name = "t_in_cell") |> # count n seconds in cell per survey
  st_drop_geometry() # drop sf spatial column

heur <- roving_antenna |> # calculate the particle detection strength heuristic
  st_intersection(grid) |> # assign each RFID point to a grid cell
  group_by(ID, day, cell) |>
  reframe(n = n(), # n observations of tag in each cell per survey
          mean_RSSI = mean(RSSI)) |> # mean received signal strength indication metric
  left_join(t_in_cell, by = c("day", "cell"), # add time spent surveying each grid cell
            relationship = "many-to-many") |>
  mutate(mean_RSSI = rescale(mean_RSSI, to = c(0,1)), # min-max signal strength scaling
         heur = (n * mean_RSSI) / t_in_cell) |> # signal strength and number versus survey time
  group_by(ID, day) |>
  mutate(heur = rescale(heur)) |> # min-max scaling for inter-comparisons
  left_join(grid, by = "cell") |> # add grid sf geometry column
  st_sf() |>
  st_centroid() |> # simplify grid geometry to cell centroids
  mutate(x = st_coordinates(geometry)[, 1], # split grid cell centroid coords for later analysis
         y = st_coordinates(geometry)[, 2]) |>
  drop_na() # zero tolerance for missing data

#'
#| echo: false

# Plotting an example
hex_data <- st_join(st_sf(heur |> st_make_grid(cellsize = 8, square = F)),
                    heur |> filter(ID == 4),
                    join = st_intersects) |> drop_na()

ggplot() +
  geom_sf(data = glacier_outline, fill = 'white') +
  geom_sf(data = hex_data, aes(fill = factor(day), alpha = heur), col = NA)  +
  geom_sf(data = channel_line, col = 'blue') +
  facet_wrap(~ID) +
  labs(x = NULL, y = NULL, col = 'Day', alpha = 'Heuristic', size = 'Heuristic') +
  coord_sf(datum = 2056) +
  theme_bw() +
  labs(title = 'Example of gridded heuristic for particle detection strength',
       subtitle = 'Exaggerated grid scale')

#' ## KDE
#' We then perform a kernel density estimation (KDE) to estimate the probability density distribution in 2D space of each tagged particle on each survey, weighted by the heuristic. The location with the highest probability density (KDE_max) is taken as the particle location, with a 95% confidence interval contour. The along-channel distance of the closest channel point to the KDE_max and the intersections of the 95% CI are computed. This step requires a channel centreline point geometry feature, with the cumulative (along-channel) distance between points computed. I would advise parallel computing (e.g. `futures` and `furrr`) for many particles.

future::plan(multisession)

kde_func <- function(data) { # kde function taking heuristic data grouped by ID and day
  try({
    current_id <- data$ID[1] # tag ID
    current_day <- data$day[1] # survey date

    bandwidth_data <- data |> # format data for bandwidth selection
      st_drop_geometry() |>
      select(x,y)
    h_value <- ks::Hscv.diag(bandwidth_data[, 1:2]) # run smoothed cross validation selector

    kde_result <- eks::st_kde(data, w = data$heur, H = h_value) # run the weighted geospatial KDE

    eval_points <- kde_result[["tidy_ks"]][["ks"]][[1]][["eval.points"]] # the coordinates for which the kde was evaluated
    estimate <- kde_result[["tidy_ks"]][["ks"]][[1]][["estimate"]] # probability density estimates
    max_cell <- which(estimate == max(estimate), arr.ind = TRUE) # coordinates of maximum probability density
    CI <- eks::st_get_contour(kde_result, cont = 95) |> # extract the 95% confidence interval contour
      st_simplify(dTolerance = 0.5) # simplify contours to save memory

    result <- tibble(ID = current_id,
                     day = current_day,
                     max_estimate = max(estimate), # value of max. prob. dens.
                     max_x = eval_points[[1]][max_cell[1]], # max. prob. dens. easting
                     max_y = eval_points[[2]][max_cell[2]], # max. prob. dens. northing
                     CI = CI$geometry) |> # CI contour spatial features
      st_as_sf(crs = 2056, coords = c('max_x', 'max_y'))

    return(result) # return the results
  })
}

kde <- heur |> # run the kde function on each ID-day combination
  arrange(ID, day) |>
  group_by(ID, day) |>
  group_split() |> # split the heuristic by ID-day combinations
  future_map(~ kde_func(.x), .progress = TRUE) # run the kde function for each grouping

seeding <- tags |> # create particle seeding location spatial data
  select(ID, dep_time) |>
  rename(day = dep_time) |>
  mutate(day = floor(day),
         dist = 0, # zero transport distance
         x = 2599065, # seeding location easting
         y = 1087700) |> # seeding location northing
  st_as_sf(coords = c("x", "y"), crs = 2056)

point_loc <- kde |> # processing particle point location timeseries with uncertainty
  bind_rows(seeding) |> # add particle seeding data
  arrange(ID, day) |>
  group_by(ID, day) |>
  slice_max(max_estimate) |>  # if multiple point locations per ID-day, keep one with highest probability
  mutate(CI_area = as.numeric(st_area(CI)), # if CI contour area implausibly small or NA/zero, assign larger circular CI
         CI = if_else((CI_area < 500 | is.na(max_estimate)), st_buffer(geometry, range), CI)) |>
  st_set_geometry('CI') |> # switch to the CI contour geometries
  st_cast('MULTIPOLYGON') |> # necessary for whatever reason
  st_cast('POLYGON', warn = F) |> # split MULTIPOLYGON contours into individual POLYGONs
  mutate(CI_area = as.numeric(st_area(CI))) |> # recalculate contour areas per polygon
  group_by(ID, day) |>
  slice_max(CI_area) |> # extract the largest distinct CI contour per ID-day
  mutate(CI = if_else(is.na(max_estimate), NA, CI)) # remove uncertainty around the seeding point

t_dist <- point_loc |> # conversion of point locations to along-channel transport distances with uncertainty
  group_by(ID, day) |>
  st_set_geometry('CI') |> # switch to the CI contour geometries
  st_intersection(channel_points) |> # extract the points at which CI contours intersect the channel
  group_by(ID, day) |>
  summarise(geometry = st_union(geometry), # combine geometries to prevent duplicates
            min_dist = round(min(dist.1)),# upper along-channel uncertainty bound
            max_dist = round(max(dist.1)), # lower along-channel uncertainty bound
            .groups = 'drop') |>
  st_set_geometry('geometry') |> # switch to the point location geometries
  # snap the point locations to the nearest point on the channel centreline (along-channel distance assigned here).
  st_join(channel_points, join = st_nearest_feature) |>
  st_drop_geometry() |> # drop spatial data
  # cleaning up edge-case transport distances after snapping
  mutate(dist = if_else(round(dist) < 3, 0, round(dist)), # transport distances < 3 m assigned to seeding point
         min_dist = if_else(min_dist < 3, 0, min_dist),
         # if snapped point lies outside CI contour, assign transport distance to middle of transport distance CI
         dist = if_else(dist < min_dist | dist > max_dist, (min_dist + max_dist) / 2, dist),
         # if no transport distance CI available, assign one
         min_dist = if_else(is.na(min_dist), dist - range, min_dist),
         max_dist = if_else(is.na(max_dist), dist + range, max_dist)) |>
  full_join(seeding |> st_drop_geometry()) |>
  left_join(survey_times, by = 'day')

ggplot() +
  geom_sf(data = glacier_outline, fill = 'white') +
  geom_sf(data = channel_line, col = 'blue') +
  geom_sf(data = point_loc |> st_set_geometry('CI'), aes(col = factor(day)),
          fill = NA, lwd = 0.5) +
  geom_sf(data = point_loc |> st_set_geometry('geometry'),
          aes(col = factor(day))) +
  labs(x = NULL, y = NULL, colour = 'Day') +
  coord_sf(datum = 2056) +
  facet_wrap(~ID) +
  theme_bw() +
  ggtitle('Particle location with 95% uncertainty')

ggplot(t_dist, aes(time, dist, col = factor(ID))) +
  geom_line(linetype = 'dotted') +
  geom_errorbar(aes(ymin = min_dist, ymax = max_dist), width = .25) +
  geom_point() +
  geom_hline(yintercept = 350, linetype = 'dashed') +
  scale_y_reverse() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(colour = 'ID') +
  facet_wrap(~ID) +
  theme_bw()

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

det_range <- 32

a_range <- antennas |>
  st_buffer(det_range) |> # define the horizontal detection range around each antenna
  st_intersection(channel_points) |>
  arrange(antenna, dist) |>
  group_by(antenna) |>
  summarise(
    min_dist = min(dist), # the upper and lower bounds of detection along the channel
    max_dist = max(dist)
  ) |>
  st_drop_geometry()

stat <- stationary_antennas |>
  arrange(ID, time, antenna) |>
  group_by(ID, antenna) |>
  mutate(duration = lead(time) - time) |> # time particle in range of antenna
  filter(IO == "In") |>
  select(time, day, ID, antenna, duration, dep_time) |>
  filter(time > dep_time) |>
  left_join(a_range, by = 'antenna') |>
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
    min_dist = min(min_dist),
    max_dist = max(max_dist)
  )

#' Displaying both the roving antenna KDE_max data (black dots) and stationary antenna data (coloured bars) as a function of along-channel transport distance from the borehole.
#'
#| echo: false

ggplot() +
  geom_rect(data = stat,
            aes(xmin = bin, xmax = bin + (1 / 24),
                ymin = min_dist, ymax = max_dist,
                alpha = log(duration)),
            fill = 'darkgrey', show.legend = T) +
  geom_point(data = stat |> filter(antenna == 15),
             aes(x = bin, y = (max_dist + min_dist) / 2),
             col = 'darkgrey', shape = 20, show.legend = F) +
  geom_line(data = t_dist,
            aes(time, dist, col = factor(ID)),
            linetype = 'dashed', lwd = .8) +
  geom_errorbar(data = t_dist,
                aes(x = time, ymin = min_dist, ymax = max_dist,
                    col = factor(ID)), width = .25) +
  geom_point(data = t_dist,
             aes(time, dist, col = factor(ID))) +
  scale_y_reverse() +
  geom_hline(yintercept = 350, linetype = "dashed") +
  facet_wrap(~ID) +
  theme_bw() +
  labs(title = "Particle transport distance over time",
       subtitle = 'Grey bars indicate stationary antenna detections shaded by duration\nBlack points highlight detection at the end of the survey zone',
       alpha = 'log duration [hrs]',
       colour = 'Particle ID')

t_dist2 <- t_dist |>
  mutate('system' = 'rov') |>
  select(-day)

stat2 <- stat |>
  select(ID, bin, duration, min_dist, max_dist) |>
  rename(time = bin) |>
  mutate('system' = 'stat')

combi <- bind_rows(t_dist2, stat2) |>
  arrange(ID, time) |>
  distinct() |>
  select(ID, time, dist, min_dist, max_dist, duration, system)

