# Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)

plots <- 
  function(name){
    
    if(name == "RFID_points"){    
      p <- # run KDE_single_survey in advance
        ggplot() +
        geom_sf(data = glacier_outline, colour = "black", 
                fill = "NA", linetype = "solid", linewidth = 0.5) +
        geom_sf(data = output_single$dGPS_line, 
                colour = "light grey", linetype = "solid", linewidth = 0.25) +
        geom_sf(data = output_single$dGPS_buffer, colour = "black", 
                fill = NA, linetype = "dotted", linewidth = 0.55) +
        geom_sf(data = channel, colour = "blue", linetype = "solid", linewidth = 0.75) +
        geom_sf(data = (output_single$ID_date %>% 
                          dplyr::arrange(RSSI)), 
                aes(colour = RSSI), size = 0.5) +
        coord_sf(crs = 2056, datum = 2056, xlim = c(2598650, 2599150), ylim = c(1087275, 1087775)) +
        scale_colour_viridis(option = "B") +
        xlab(NULL) + ylab(NULL) + 
        labs(fill = "RSSI", subtitle = c(paste0("Tag ID: ", output_single$TagID[[1]], "     n = ", output_single$n_frames), 
                                         paste0("Survey date: ", output_single$Survey_date[[1]]))) + 
        theme_linedraw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.subtitle = element_text(hjust = c(0, 1)))
    }
    
    if(name == "KDE_single"){    
      p <- # run KDE_single_survey in advance
        ggplot() +
        geom_tile(data = output_single$raster_df_mask, 
                    aes(x, y, fill = value)) + # the KDE in raster format, masked by the CI contour
        geom_sf(data = glacier_outline, colour = "black", 
                fill = "NA", linetype = "solid", linewidth = 0.5) +
        geom_sf(data = channel, colour = "blue", linetype = "solid", linewidth = 0.75) +
        geom_sf(data = output_single$dGPS_line, colour = "grey", 
                linetype = "solid", linewidth = 0.25) +
        # geom_sf(data = output_single$dGPS_buffer, colour = "black", 
        #         fill = NA, linetype = "dotted", linewidth = 0.55) +
        geom_sf(data = output_single$contour.95_polygon, colour = "red", 
                fill = NA, linetype = "solid", linewidth = 1) +
        geom_point(data = as.data.frame(output_single$KDE_max), aes(x, y), 
                   colour = "black", pch = 10, stroke = 1, size = 3) +
        coord_sf(crs = 2056, datum = 2056, 
                 xlim = c(2598650, 2599150), 
                 ylim = c(1087275, 1087775)) +
        scale_fill_viridis(breaks = c(0.05, 0.5, 1), 
                           limits = c(0, 1), option = "C") +
        scale_colour_viridis(option = "H") +
        xlab(NULL) + ylab(NULL) + 
        labs(fill = "Prob.\ndensity", subtitle = c(paste0("Tag ID: ", output_single$TagID[[1]]), 
                                                   paste0("Survey date: ", output_single$Survey_date[[1]]))) +
        theme_linedraw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.subtitle = element_text(hjust = c(0, 1)))
    }
    
    if(name == "KDE_multiple"){
      p <- # run KDE_multiple_survey in advance
        ggplot() +
        geom_sf(data = glacier_outline, colour = "black", fill = "NA", 
                linetype = "solid", linewidth = 0.5) +
        geom_sf(data = channel, colour = "blue", 
                linetype = "solid", linewidth = 0.75) +
        geom_sf(data = output_multiple[1][[1]]$contour.95_polygon, # 95% confidence intervals
                aes(colour = factor(output_multiple[1][[1]]$Survey_date)), 
                fill = NA, linetype = "solid", linewidth = 1) +
        geom_point(data = output_multiple[1][[1]], # KDEmax points
                   aes(x = KDE_max_x, y = KDE_max_y, colour = factor(Survey_date)), 
                   pch = 10, stroke = 1, size = 3) +
        geom_sf(data = output_multiple[3][[1]]) + # mapping to channel
        coord_sf(crs = 2056, datum = 2056, xlim = c(2598650, 2599150), ylim = c(1087275, 1087775)) +
        scale_colour_viridis_d(option = "H", end = 0.9) +
        xlab(NULL) + ylab(NULL) + labs(colour = "Date",
                                       subtitle = c(paste0("Tag ID: ", output_multiple$TagID[[1]]))) +
        theme_linedraw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    if(name == "Distance_probability"){
      p <- # run KDE_multiple_survey in advance.
        ggplot() + 
        
        geom_hline(aes(yintercept = max(antennas$Distance)), # most downstream antenna
                   linewidth = 0.7, linetype = "dotted") + 
        geom_hline(aes(yintercept = 350), linewidth = 0.5, linetype = "dashed") + # glacier terminus
        geom_point(data = (output_multiple[2][[1]] %>% arrange(rescale_01)), # probability density mapped to channel
                   aes(Survey_time, distance, colour = rescale_01)) + 
        geom_point(aes(x = (tag_list %>% filter(Tag_ID == TagID) %>% # injection time
                              dplyr::select(Deployment_timestamp))[[1]], y = 0), 
                   colour = "green", size = 2.5) +
        geom_point(data = (output_multiple[2][[1]] %>% # KDEmax points mapped to channel
                             group_by(Survey_date) %>% 
                             filter(rescale_01 == max(rescale_01)) %>% 
                             slice(1)), 
                   aes(Survey_time, distance), pch = 10, stroke = 1, size = 3) + 
        scale_colour_viridis(option = "C") +
        scale_y_reverse(limits = c(562, 0)) +
        xlab("Date") + ylab("Along-channel distance [m]") + 
        labs(colour = "Probability\ndensity") +
        theme_bw()
    }
    
    if(name == "Stationary_antenna"){
      p <- # run KDE_multiple_function and Stationary_ant_function in advance
        ggplot() + 
        geom_rect(data = (output_stationary[7][[1]] %>%
                            mutate(Reader_ID = as.factor(Antenna)) %>%
                            left_join(antennas) %>%
                            right_join(output_stationary[6][[1]], by = "Reader_ID")), 
                  aes(xmin = Timestep, xmax = Timestep_end,
                      ymin = int_upper, ymax = int_lower), 
                  fill = "darkgrey", colour = NA, linewidth = 1, alpha = 0.45) +
        geom_hline(aes(yintercept = antennas$Distance), # most downstream antenna
                   linewidth = 0.25, linetype = "dotted") +
        geom_hline(aes(yintercept = 350), linewidth = 0.5, linetype = "dashed") + # glacier terminus
        geom_point(aes(x = (tag_list %>% filter(Tag_ID == TagID) %>% # injection time
                              dplyr::select(Deployment_timestamp))[[1]], y = 0), 
                   colour = "green", size = 2.5) +
        geom_point(data = (output_multiple[2][[1]] %>% arrange(rescale_01)), 
                   aes(Survey_time, distance, colour = rescale_01), size = 1) + 
        geom_point(data = (output_multiple[2][[1]] %>%
                             group_by(Survey_date) %>%
                             filter(rescale_01 == max(rescale_01)) %>% 
                             slice(1)), 
                   aes(Survey_time, distance), pch = 10, stroke = 1, size = 3) + 
        scale_colour_viridis(option = "C") +
        scale_y_reverse(limits = c(562, 0)) +
        xlab("Date") + ylab("Along-channel distance [m]") + 
        labs(colour = "Probability\ndensity") +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    return(p)
  }