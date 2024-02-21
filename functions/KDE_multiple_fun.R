# Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)

KDE_multiple <-
  function(TagID, grid_size, weights, CI) {
    
    Tag_Date <- # determine in which surveys TagID was observed
      combinations %>%
      filter(Tag_ID == TagID)

    l <- list() # empty list to populate with output data
    for (i in 1:nrow(Tag_Date)) { # loop to perform the KDE_single_survey function for each survey

      try(
        {
          Survey_date <- Tag_Date[i, 2] # dates of all surveys in which TagID was observed

          l[[paste(as.character(Survey_date[[1]]))]] <- # append the output list to l (nested list)
            KDE_single(
              TagID = TagID, # function to localise the particle
              Survey_date = Survey_date,
              grid_size = grid_size,
              weights = weights,
              CI = CI
            )
        },
        silent = TRUE
      )
    }

    output_multiple <- tibble() # generate empty tibble to populate with relevant data from list l
    for (i in 1:length(l)) { # loop over all Survey_date values

      try(
        {
          l3 <- # extract relevant elements from list l
            tibble(
              "TagID" = TagID, # particle / tag ID code
              "Survey_date" = l[[i]]["Survey_date"][[1]][[1]], # date of the survey
              "n_frames" = pluck(l[[i]], "n_frames"), # number of RFID points for TagID on Survey_date
              "KDE_max_x" = l[[i]]["KDE_max"][[1]][1], # x coordinates of estimated particle location
              "KDE_max_y" = l[[i]]["KDE_max"][[1]][2], # y coordinates of estimated particle location
              "contour.95_polygon" = l[[i]]["contour.95_polygon"][1]
            ) # largest CI contour

          output_multiple <- bind_rows(output_multiple, l3) # append l3 to master list
        },
        silent = T
      )
    }

    try(
      {
        contours <- # extract and format CI contour geometries from list
          Reduce(rbind, output_multiple$contour.95_polygon) %>%
          st_as_sf() %>%
          st_set_crs(2056)

        output_multiple <- # add CI contours back into output list
          output_multiple %>%
          mutate("contour.95_polygon" = contours)
      },
      silent = TRUE
    )


    # Calculate along-channel distance to CI intersections --------------------

    l_dist <- tibble() # empty tibble to populate with mapping data
    distances <- tibble() # empty interpolated distance table

    for (i in 1:nrow(output_multiple)) { # for each survey...

      try(
        {
          Survey_date <- output_multiple$Survey_date[[i]] # set the Survey_date variable

          if (st_intersects(output_multiple$contour.95_polygon[[1]][i], channel, sparse = F) == "TRUE") { # if the CI contour intersects the channel

            intersection <- # extract the subset of the channel between intersections and format as Spatial*
              st_intersection(channel, output_multiple$contour.95_polygon[[1]][i]) %>%
              st_cast("POINT") %>%
              as_Spatial()

            channel_estimated_sp <- # convert channel to Spatial* format for next step
              channel %>%
              as_Spatial()

            dist_channel_intersection <- # using GEOS, find the along channel distance of all points comprising the subsetted channel feature
              rgeos::gProject(channel_estimated_sp, intersection, normalized = F)

            l_dist_temp <- # temporary data object to bind to l_dist
              tibble(
                "int_upper" = dist_channel_intersection[which.min(dist_channel_intersection)], # obtain the upper CI intersection distance
                "int_lower" = dist_channel_intersection[which.max(dist_channel_intersection)], # obtain the lower CI intersection distance
                "Survey_date" = as.Date(Survey_date[[1]])
              ) # set the Survey_date

            l_dist <- # add l_dist_temp data to master l_dist list
              bind_rows(l_dist, l_dist_temp)

            rm(l_dist_temp) # remove the temporary l_dist_temp object

            # Extract probability density values along channel reach inside CI contour -

            sp <- # extract probability density values of grid squares underlying channel and format as spatial points
              raster::extract(pluck(l[[i]], "kde_raster"), intersection,
                cellnumbers = T,
                df = T, along = T, sp = T
              ) %>%
              st_as_sf(coords = c("X", "Y"), crs = 2056) %>%
              cbind(., st_coordinates(.)) %>%
              as_Spatial()

            dist_channel <- # calculate along-channel distance to each grid square
              rgeos::gProject(channel_estimated_sp, sp, normalized = F)

            sp2 <- # extract probability density values of grid squares underlying channel and add along-channel distance to each square
              raster::extract(pluck(l[[i]], "kde_raster"), intersection,
                cellnumbers = T,
                df = T, along = T, sp = T
              ) %>%
              as.data.frame() %>%
              mutate(distance = dist_channel) %>%
              arrange(layer) %>%
              st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 2056)

            interp <- # interpolate along-channel probability density to 1 m resolution and rescale 0 to 1 for intercomparison between surveys
              approx(sp2$distance,
                y = sp2$layer,
                xout = seq(min(sp2$distance),
                  max(sp2$distance),
                  by = 1
                )
              ) %>%
              as_tibble() %>%
              rename(distance = x) %>%
              rename(layer = y) %>%
              mutate(rescale_01 = scales::rescale(layer, to = c(0, 1))) %>%
              mutate(Survey_date = Survey_date)

            distances <- # add interpolated data to master distances object
              distances %>%
              rbind(interp)
          }
        },
        silent = TRUE
      )
    }

    try(
      {
        distances <- # add survey times
          distances %>%
          left_join(survey_times, by = "Survey_date")

        output_multiple <- # add distances to intersections
          output_multiple %>%
          left_join(l_dist, by = "Survey_date")

        kde_max_adjusted <- # format KDEmax points as sf object and map to nearest point on the channel (only for example plots, not analysis)
          output_multiple %>%
          dplyr::select(Survey_date, KDE_max_x, KDE_max_y) %>%
          st_as_sf(coords = c("KDE_max_x", "KDE_max_y"), crs = 2056) %>%
          st_cast("POINT") %>%
          st_nearest_points(channel)

        return(list(
          output_multiple, # list of selected output from KDE_single for each Survey_date
          distances, # the probability density of transport distance on each Survey_date
          kde_max_adjusted
        )) # the KDEmax points mapped to the nearest point on the channel centreline
      },
      silent = TRUE
    )

    # if (length(l) == 0) {
    #   print("No data")
    # } # if there is no usable data, print "No data"
  }
