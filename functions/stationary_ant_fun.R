# Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)

stationary_ant <-
  function(TagID, timestep, detection_range, in_range_threshold) {
    ID <- # subset stationary antenna detection data by TagID
      stationary_antenna_data %>%
      arrange(Timestamp) %>%
      filter(Tag_ID == TagID) %>%
      arrange(Reader_ID, Timestamp)

    timediff <- diff(ID$Timestamp) # Time difference between successive rows, shift 1 row with paste NA.
    timediff <- c(paste(timediff), NA) # NA to fill the final row
    ID$timediff <- as.numeric(timediff) # Add to ID

    if (nrow(ID) > 0) { # if there is data ...

      ID <- # calculate time particle was in-range (Out)
        ID %>%
        filter(In_Out == "In") %>% # extract the start time of each in-range period
        mutate(Out = Timestamp + timediff) # calculate the end of each in-range period by adding the timediff to Out

      count <- # group data into hourly timesteps, sum timediffs to get number of seconds in range per timestep (Time_in_range)
        ID %>%
        mutate(Timestep = cut(Timestamp, breaks = paste(timestep, "hour"))) %>%
        group_by(Reader_ID, Timestep) %>%
        summarise(
          Time_in_range = sum(as.numeric(timediff)),
          .groups = "keep"
        ) # seconds in range

      min_time <- ID$Timestamp[which.min(ID$Timestamp)] # minimum timestamp
      max_time <- ID$Timestamp[which.max(ID$Timestamp)] # maximum timestamp

      sequence <- # generate a sequence of timestamps that will define timestep groups (using POSIX)
        seq.POSIXt(
          from = floor_date(min_time, unit = paste(1, "hour")),
          to = ceiling_date(max_time, unit = paste(1, "hour")),
          by = paste(timestep, "hours")
        ) %>%
        as.POSIXct()

      # Group the data into timesteps, sum the timediffs to get number of seconds in range per timestep.
      # Set 'noise' threshold by removing any 'in-range periods' < in_range_threshold value.

      Counts_s <- ## USE FOR PLOTTING WHERE TOTAL DURATION <24 hrs. New code required for < 25 hrs.
        ID %>%
        mutate(Timestep = cut(Timestamp, breaks = sequence)) %>%
        mutate(Timestep = ymd_hms(Timestep)) %>%
        group_by(Reader_ID, Timestep) %>%
        summarise(
          Time_in_range = sum(as.numeric(timediff)),
          .groups = "keep"
        ) %>%
        mutate(cs = cumsum(Time_in_range)) %>% # cumulative time in range per antenna
        mutate(Timestep_end = Timestep + (timestep * 3600)) %>%
        relocate(Timestep_end, .after = Timestep) %>%
        mutate(Timestep_end2 = case_when(
          Time_in_range > 3600 ~ Timestep_end + (Time_in_range - 3600),
          Time_in_range < 3600 ~ Timestep_end
        )) %>%
        filter(Time_in_range > in_range_threshold) %>% # remove any periods of less than x seconds per hour.
        mutate(Reader_ID = as.factor(Reader_ID)) %>%
        left_join(antennas, by = "Reader_ID") %>%
        mutate(Tag_ID = TagID) %>%
        dplyr::select(Tag_ID, Reader_ID, Timestep, Timestep_end, Distance)

      Counts_s$Reader_ID <- # reorder the factor levels for plotting.
        factor(Counts_s$Reader_ID,
          levels = c(
            "1", "2", "3", "4", "5", "6", "7",
            "8", "9", "10", "11", "12", "13", "14", "15"
          )
        )

      # Antenna intersections with channel --------------------------------------

      ant_buff <- # buffer around each stationary antenna - detection range estimate
        antennas %>%
        st_buffer(dist = detection_range)

      Fixed_boundaries <- tibble() # empty data frame to populate with intersection data

      for (i in 1:15) { # for each supraglacial antenna...

        intersection <- # extracts the subset of the channel between intersections
          st_intersection(channel, ant_buff$geom[i]) %>%
          st_cast("POINT") %>%
          as_Spatial()

        channel_estimated_sp <- # spatial format for next step
          channel %>%
          as_Spatial()

        dist_channel_intersection <- # finds the distance of the CI intersections from A1 (at BH4)
          rgeos::gProject(channel_estimated_sp, intersection, normalized = F)

        Fixed_boundaries_temp <-
          tibble(
            int_upper = dist_channel_intersection[which.min(dist_channel_intersection)], # upper CI intersection
            int_lower = dist_channel_intersection[which.max(dist_channel_intersection)], # lower CI intersection
            Antenna = i
          )

        Fixed_boundaries <- # the upper and lower intersections of the antenna buffers with the channel
          bind_rows(Fixed_boundaries, Fixed_boundaries_temp)
      }

      list2 <- list(
        "TagID" = TagID, # particle ID code
        "ID" = ID, # stationary antenna data for TagID
        "timestep" = timestep, # timestep used
        "detection_range" = detection_range, # antenna detection range used
        "in_range_threshold" = in_range_threshold, # noise threshold used
        "Counts_s" = Counts_s, # time TagID in-range per timestep
        "Fixed_boundaries" = Fixed_boundaries
      ) # the upper and lower CI intersections of the antenna buffers with the channel

      return(list2)
    }
    if (nrow(ID) <= 0) {
      print("No data")
    } # if there is no usable data, print "No data"
  }
