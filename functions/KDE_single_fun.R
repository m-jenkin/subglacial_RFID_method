# Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)

KDE_single <-
  function(TagID, Survey_date, grid_size, weights, CI) {
    
    ID_date <- # filter the data for a single tagged particle in a single survey
      roving_antenna_data %>%
      filter(Tag_ID == TagID &
        Date == Survey_date) %>%
      arrange(Timestamp) %>%
      st_as_sf()

    n_frames <- # number of detections of TagID on Survey_date
      nrow(ID_date) %>%
      as.numeric()

    dep_timestamp <- # time of tagged particle deployment - requires 'tag_list' metadata file
      tag_list %>%
      filter(Tag_ID == TagID) %>%
      pull(Deployment_timestamp) %>%
      ymd_hms()

    dGPS_line <- # the survey lines on Survey_date
      dGPS_track %>%
      filter(Date == Survey_date) %>%
      st_combine() %>%
      st_cast("LINESTRING") %>%
      st_simplify(dTolerance = 0.2) %>% # reduce number of vertices
      smoothr::smooth(method = "ksmooth", smoothness = 2) # light kernel smoothing for plotting

    dGPS_buffer <- # buffer around the survey track to estimate survey extent
      dGPS_line %>%
      st_buffer(10) %>% # buffer value in meters
      st_simplify(dTolerance = 0.5) %>% # reduce number of vertices
      smoothr::smooth(method = "ksmooth", smoothness = 5) # moderate kernel smoothing for plotting

    # Binned summary statistics -----------------------------------------------

    kde_grid_blank <- # generate a blank grid over the survey area with a predefined cell size
      st_make_grid(bbox, c(grid_size, grid_size),
        what = "polygons", square = T
      ) %>%
      st_sf() %>%
      mutate(grid_id = rownames(.)) # assign an ID number to each grid square

    kde_grid <- # bin RFID data into grid squares and calculate heuristic index
      kde_grid_blank %>%
      st_join(ID_date) %>% # assign each RFID point to a grid square
      filter(!is.na(Tag_ID)) %>% # remove empty grid squares
      group_by(grid_id) %>% # grouping by grid square for following calculations
      summarise(mean_RSSI = mean(RSSI)) %>% # mean and max RSSI (rescaled from 0 to 1 during data cleaning)
      mutate(n_detections = lengths(st_intersects(., ID_date))) %>% # the number of detections per square
      mutate(
        time_in_square = # number of seconds antenna situated in each square (simply counts the number of 1s interval GPS points)
          lengths(st_intersects(., filter(dGPS_track, Date == Survey_date[[1]])))
      ) %>%
      mutate(detection_rate = (n_detections / (time_in_square * 5))) %>% # number of RFID points observed versus the number transmitted (5x per sec)
      mutate(heuristic = mean_RSSI * detection_rate) %>% # heuristic index for KDE weighting - accounts for variable RSSI and detection rate
      mutate(centroid = st_centroid(geometry)) %>% # get the grid square centroid coordinates
      cbind(st_coordinates(.$centroid)) %>% # extract as X, Y variables
      as_tibble() %>% # convert to df for KDE function
      dplyr::select(any_of(c(
        "grid_id", "n_detections", "mean_RSSI",
        "time_in_square", "heuristic", "X", "Y"
      )))

    # Weighted 2D KDE ---------------------------------------------------------
    
    H <- Hscv.diag(kde_grid[,6:7])
      
    invisible(
      ifelse(weights == TRUE,
        {
          kde_tag <- # weighted 2D KDE
            suppressWarnings(
              ks::kde(
                x = kde_grid[, 6:7], # grid square centroids (X,Y columns in kde_grid)
                H = H, # the KDE bandwidth
                w = kde_grid$heuristic, # the weights, scaled automatically by the kde function (warning message suppressed)
                compute.cont = T,
                binned = T,
                gridsize = c(500 / grid_size, 500 / grid_size), # bounding box length divided by grid square size in meters
                xmin = c(2598650, 1087300), # lower left coordinate of bbox
                xmax = c(2599150, 1087800)
              ) # lower right coordinate of bbox
            )
        },
        {
          kde_tag <- # unweighted 2D KDE
            suppressWarnings(
              ks::kde(
                x = kde_grid[, 6:7],
                # H = H,
                compute.cont = T,
                binned = T,
                gridsize = c(500 / grid_size, 500 / grid_size),
                xmin = c(2598650, 1087300),
                xmax = c(2599150, 1087800)
              )
            )
        }
      )
    )

    kde_raster <- # rasterise the KDE for following analysis
      kde_tag %>%
      raster::raster()

    raster_df <- # convert raster to tibble for plotting
      as(kde_raster, "SpatialPixelsDataFrame") %>%
      as_tibble() %>%
      rename("value" = "layer") %>%
      mutate(value = scales::rescale(value, to = c(0, 1))) # rescale 0 to 1 for inter-comparison across surveys

    KDE_max <- raster::xyFromCell(kde_raster, which.max(kde_raster)) # centroid of grid square containing maximum probability density value

    # Confidence interval contours 95% --------------------------------------------

    contour.95_list <- # calculate contour lines corresponding to the confidence interval (CI) chosen
      with(
        kde_tag,
        contourLines(
          x = eval.points[[1]],
          y = eval.points[[2]],
          z = estimate,
          levels = cont[as.character(paste0(CI, "%"))]
        )
      )

    contour.95_list_sizes <- data.frame() # empty data frame to populate with CI data
    for (i in 1:length(contour.95_list)) { # calculate the area inside each CI contour
      contour.95_list_sizes_temp <-
        data.frame(i = abs(with(contour.95_list[[i]], pracma::polyarea(x, y))))
      contour.95_list_sizes <-
        rbind(contour.95_list_sizes, contour.95_list_sizes_temp)
      rm(contour.95_list_sizes_temp)
    }

    contour.95_all <- data.frame() # empty data frame to populate with CI data
    for (i in 1:length(contour.95_list)) { # format the CI contour line data as sf for further analysis
      contour.95_all_temp <-
        contour.95_list[[i]] %>%
        data.frame(.$x, .$y) %>%
        st_as_sf(coords = c("x", "y"), crs = 2056) %>%
        summarise(do_union = FALSE) %>%
        st_cast("LINESTRING")
      contour.95_all <-
        rbind(contour.95_all, contour.95_all_temp)
      rm(contour.95_all_temp)
    }

    contour.95 <- # calculate and format the largest CI contour (useful if there are multiple due to a multi-modal KDE)
      with(
        kde_tag,
        contourLines(
          x = eval.points[[1]],
          y = eval.points[[2]],
          z = estimate,
          levels = cont[as.character(paste0(CI, "%"))]
        )[[which.max(contour.95_list_sizes$i)]]
      ) %>%
      data.frame(.$x, .$y) %>%
      st_as_sf(coords = c("x", "y"), crs = 2056) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING") %>%
      smoothr::smooth(method = "ksmooth", smoothness = 3) # light kernel smoothing for plotting remove if not required.
    rm(contour.95_list, contour.95_list_sizes)

    contour.95_polygon <- # format CI contours as sf polygons for spatial analysis and plotting
      st_polygonize(contour.95)
    contour.95_all_polygon <-
      st_polygonize(contour.95_all)
    rm(contour.95, contour.95_all)

    if (st_intersects(contour.95_polygon, channel, sparse = F) == TRUE) { # if the CI contour intersects the channel...

      kde_raster_mask <- # extract the KDE probability density values within the CI contour
        mask(kde_raster, contour.95_all_polygon, inverse = F)

      raster_df_mask <- # convert masked raster to data frame for plotting
        as(kde_raster_mask, "SpatialPixelsDataFrame") %>%
        as_tibble() %>%
        rename("value" = "layer") %>%
        mutate(value = scales::rescale(value, to = c(0, 1))) # rescale 0 to 1 for inter-comparison across surveys
    }

    # Output ------------------------------------------------------------------

    output <-
      # the output of this custom function
      list(
        "TagID" = TagID, # ID of the selected particle
        "Survey_date" = Survey_date, # date of the selected survey
        "ID_date" = ID_date, # RFID point data for TagID on Date
        "n_frames" = n_frames, # number of RFID points in ID_Date
        "dep_timestamp" = dep_timestamp, # time of TagID deployment in subglacial channel
        "dGPS_line" = dGPS_line, # survey track from dGPS
        "dGPS_buffer" = dGPS_buffer, # survey extent using spatial buffer around dGPS_line
        "kde_grid_blank" = kde_grid_blank, # blank spatial grid used for data binning
        "kde_grid" = kde_grid, # binned RFID data with summary statistics and heuristic
        "kde_tag" = kde_tag, # kernel density estimate (KDE) for TagID on Date, based on kde_grid
        "kde_raster" = kde_raster, # raster version of kde_tag, probability density rescaled from 0 to 1
        "raster_df" = raster_df, # data frame version of kde_raster for plotting
        "KDE_max" = KDE_max, # xy coordinates of estimated particle location
        "contour.95_polygon" = contour.95_polygon, # largest CI contour
        "contour.95_all_polygon" = contour.95_all_polygon, # all CI contours
        "kde_raster_mask" = kde_raster_mask, # kde_raster masked by the 95% CIs
        "raster_df_mask" = raster_df_mask
      ) # data frame version of kde_mask for plotting

    return(output)
  }
