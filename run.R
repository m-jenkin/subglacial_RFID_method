# Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)

pacman::p_load(dplyr, tidyr, readr, purrr, fs, ggplot2, stringr, forcats,
       lubridate, # date and time manipulation
       ks, # kernel smoothing
       scales, # scaling functions
       pracma,
       sf, # simple features (sf)
       smoothr, # sf smoothing
       rgeos, # GEOS spatial geometry operations
       raster, # RasterLayers
       viridis) # perceptually uniform colour palettes

map(list.files('./functions/', full.names=T), source)

antennas <- read_sf('./data/antennas.gpkg')
channel <- read_sf('./data/channel.gpkg')
glacier_outline <- read_sf('./data/glacier_outline.gpkg')
dGPS_track <- read_sf('./data/dGPS_track.gpkg')
roving_antenna_data <- read_sf('./data/roving_antenna_data.gpkg')
stationary_antenna_data <- read_csv('./data/stationary_antenna_data.csv')
tag_list <- read_csv('./data/tag_list.csv')
survey_times <- read_csv('./data/survey_times.csv')
bbox <- # generate a bounding box over the study area
  sf::st_bbox(c(xmin = 2598650, xmax = 2599150,
            ymin = 1087300, ymax = 1087800),
          crs = st_crs(2056)) # set coordinate reference system (CH1903+ LV95 used here)
combinations <- # list unique combinations of particle ID codes (Tag_ID) and survey dates
  roving_antenna_data |>
  nest_by(Tag_ID, Date) |>
  unique() |>
  dplyr::select(Tag_ID, Date)


TagID <- '075107' # ID code of the tagged particle (character)
Survey_date <- '2021-08-20' # date of the survey [yyyy-mm-dd]
antenna_range <- 38
grid_size <- 5 # size of the grid squares used for calculating heuristic index [meters]
weights <- TRUE # KDE weighting [TRUE or FALSE]
CI <- 5 # KDE confidence interval contour value [100-CI]

print(filter(combinations, Tag_ID == TagID)) # print the Survey_date values on which TagID observed (does not guarantee successful localisation)

# Localise particle in a single roving antenna survey ---------------------

try({
  output_single <- # list of the objects created with the KDE_single function, descriptions provided in the function code.
    KDE_single(
      TagID = TagID,
      Survey_date = Survey_date,
      grid_size = grid_size,
      weights = weights,
      CI = CI
    )
})

# Localise particle in all roving antenna surveys -------------------------

output_multiple <- # list of the objects created with the KDE_multiple function, descriptions provided in the function code.
  KDE_multiple(
    TagID = TagID,
    grid_size = grid_size,
    weights = weights,
    CI = CI
  )

# Localise particle with stationary antennas ------------------------------

output_stationary <- # slices and dices the stationary antenna data
  stationary_ant(
    TagID = TagID,
    timestep = 1, # time interval to cut data into [hours]
    detection_range = antenna_range, # the estimated detection range of the antennas [meters]
    in_range_threshold = filter(tag_list, Tag_ID == TagID)$in_range_threshold_s) # the noise threshold value (obtain manually using trial and error for each particle)

# Plotting ----------------------------------------------------------------

RFID_points_plot <- plots(name = 'RFID_points')
KDE_single_plot <- plots(name = 'KDE_single')
KDE_multiple_plot <- plots(name = 'KDE_multiple')
Distance_probability_plot <- plots(name = 'Distance_probability')
Stationary_antenna_plot <- plots(name = 'Stationary_antenna')

