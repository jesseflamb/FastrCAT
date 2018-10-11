#' @title Temperature/Salinity by Depth Plots
#'
#' @description Once make_dataframe_fc has been run, then plot_ts_fc
#' can be used. This function creates a depth by salinity and temperature
#' plot for each station. These are all .png files which will be located
#' in the plot folder within the current folder. It only needs to be run
#' once to generate a plot for each station. Each dot is a data point.
#' Check the profile for each station/haul.
#' @param current_path The path to directory where all .up files
#' are located for a cruise.
#' @return A plot of temperature and salinity by depth for each
#' station of a cruise. Plots are written in the plot folder and
#' are in the .png format.




plot_ts_fc <- function(current_path){


  #FOR TESTING
  #current_path <- "G:/SeaCatData/Processed/1AE10"


  to_datadframe <- list.files(path = current_path, pattern = "\\EcoDATT.csv$", ignore.case = TRUE,
                              include.dirs = TRUE, full.names = TRUE)


  fc_dataframe <- read_csv(to_datadframe, col_types = cols(CRUISE = col_character(),
                                                          STATION_NAME = col_integer(),
                                                          HAUL_NAME = col_integer(),
                                                          FOCI_GRID = col_character(),
                                                          DEPTH = col_integer(),
                                                          TEMPERATURE1 = col_double(),
                                                          SALINITY1 = col_double()))%>%
                             dplyr::select(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID, DEPTH,
                                           TEMPERATURE1, SALINITY1,DIRECTORY)%>%
    unite(Station_haul,STATION_NAME,HAUL_NAME,sep = "_", remove = FALSE)%>%
    gather("TYPE","MEASURMENT",c(TEMPERATURE1,SALINITY1))

  unique_stations_one <- unique(fc_dataframe$Station_haul)
  unique_foci_grid <- unique(fc_dataframe$FOCI_GRID)
  cruise_name <- unique(fc_dataframe$CRUISE)

  plot_colors <- c("#1565C0","#b92b27")
  names(plot_colors) <- c("SALINITY1", "TEMPERATURE1")

  unique_stations <- if(length(unique_stations_one) < 2){
    unique_stations <- unique_foci_grid
    } else {
    unique_stations <- unique_stations_one
    }


  #For testing
  #i <- 12


  for(i in 1:length(unique_stations)){

  name_ts_plot <- paste(current_path,"/plots/",cruise_name, "_Station_",
                        unique_stations[i], ".png",sep = "")

  plot_title <- if(length(unique_stations_one) < 2){
      plot_title <- paste("Cruise ",cruise_name,"\nFOCI GRID ", unique_foci_grid[i], sep = "")
    } else {
      plot_title <- paste("Cruise ",cruise_name,"\n", "Station ",str_replace(unique_stations[i],
                                                        "_", " Haul "), sep = "")
    }

  filtered_data <- if(length(unique_stations_one) < 2){
      filtered_data <- fc_dataframe %>% filter(FOCI_GRID == unique_stations[i])
    } else {
      filtered_data <- fc_dataframe %>% filter(Station_haul == unique_stations[i])
    }

  depth_breaks <- if(max(filtered_data$DEPTH, na.rm = TRUE) >= 200){
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 50)
   } else if (max(filtered_data$DEPTH, na.rm = TRUE) > 100 &
              max(filtered_data$DEPTH, na.rm = TRUE) < 200){
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 20)
   } else {
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 4)
   }

  depth_labels <- if(max(filtered_data$DEPTH, na.rm = TRUE) >= 200){
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 50))
  } else if(max(filtered_data$DEPTH, na.rm = TRUE) > 100 &
            max(filtered_data$DEPTH, na.rm = TRUE) < 200) {
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 20))
  } else {
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0,by = 4))
  }


  ts_plot <- ggplot(data = filtered_data)+
                               geom_path(aes(MEASURMENT,-(DEPTH), color = TYPE),
                                           size = 2,alpha = 0.6)+
    geom_point(aes(MEASURMENT,-(DEPTH), color = TYPE), shape = 1, size = 4)+
    scale_y_continuous(breaks = depth_breaks,
                       labels = depth_labels)+
    scale_color_manual(values = plot_colors)+
    theme_bw()+
    theme( axis.text.y = element_text(face = "bold", size = 12),
           axis.text.x = element_text(face = "bold", size = 12),
           axis.title.x  = element_text(face = "bold", size = 14),
           axis.title.y  = element_text(face = "bold", size = 14),
           title = element_text(face = "bold", size = 18),
           strip.background = element_blank(),
           strip.text = element_blank(),
           legend.position = "none")+
    ylab(label = "Depth [m]")+
    xlab(label = expression(bold(paste("Salinity[PSU]",
                                       "\t\t\t\t\t\t\t\t\t\t\t",
                                       paste("Temperature","["~degree~C,"]"))))) +
    ggtitle(label = plot_title)+
    facet_wrap(~ TYPE, nrow = 1, scales = "free_x")

  png(filename = name_ts_plot, width = 500, height = 600, units = "px",
      bg = "transparent")

  print(ts_plot)

  dev.off()

  }

}
