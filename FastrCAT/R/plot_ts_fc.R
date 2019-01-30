#' @title Temperature/Salinity by Depth Plots
#'
#' @description Once make_dataframe_fc has been run, then plot_ts_fc
#' can be used. This function creates a depth by salinity and temperature
#' plot for each station. These are all .png files which will be located
#' in the plot folder within the current folder. If a plot folder hasn't been
#' created, it will create on. It only needs to be run
#' once to generate a plot for each station. Each dot is a data point.
#' Check the profile for each station/haul.
#' @param current_path The path to directory where dataframe created from
#' make_dataframe_fc() is located.
#' @inheritParams make_dataframe_fc()
#' @return A plot of temperature and salinity by depth for each
#' station of a cruise. Plots are written in the plot folder and
#' are in the .png format.
#' @export plot_ts_fc


plot_ts_fc <- function(current_path){


# Find file -------------------------------------------------------------------

  to_datadframe <- list.files(path = current_path, pattern = "\\EcoDAAT.csv$",
                              ignore.case = TRUE,
                              include.dirs = TRUE, full.names = TRUE)

# Check for plot folder--------------------------------------------------------

  if(dir.exists(paste(current_path,"/plots",sep = "")) == FALSE){

    dir.create(paste(current_path,"/plots",sep = ""))
  }


# Load data -------------------------------------------------------------------

  fc_dataframe <- readr::read_csv(to_datadframe,
                                  col_types = readr::cols(
                                    CRUISE = readr::col_character(),
                                    STATION_NAME = readr::col_integer(),
                                    HAUL_NAME = readr::col_integer(),
                                    FOCI_GRID = readr::col_character(),
                                    DEPTH = readr::col_integer(),
                                    TEMPERATURE1 = readr::col_double(),
                                    SALINITY1 = readr::col_double()))%>%
                    dplyr::select(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID,
                                 DEPTH, TEMPERATURE1, SALINITY1, DIRECTORY)%>%
                    tidyr::unite(Station_haul,STATION_NAME,HAUL_NAME,sep = "_",
                                 remove = FALSE)%>%
                    tidyr::gather("TYPE","MEASURMENT",c(TEMPERATURE1,SALINITY1))

# Determine Station or Foci grid unique indentifiers --------------------------

  unique_stations_one <- unique(fc_dataframe$Station_haul)
  unique_foci_grid <- unique(fc_dataframe$FOCI_GRID)
  cruise_name <- unique(fc_dataframe$CRUISE)

# Define plot colors ----------------------------------------------------------

  plot_colors <- c("#1565C0","#b92b27")
  names(plot_colors) <- c("SALINITY1", "TEMPERATURE1")

# Determine use of Station or Foci grid ---------------------------------------

  unique_stations <- if(length(unique_stations_one) < 2){
    unique_stations <- unique_foci_grid
    } else {
    unique_stations <- unique_stations_one
    }

# Plot each station/foci grid -------------------------------------------------

  for(i in 1:length(unique_stations)){

# Make unique file name -------------------------------------------------------

  name_ts_plot <- paste(current_path,"/plots/",cruise_name, "_Station_",
                        unique_stations[i], ".png",sep = "")

# Make plot name (station or foci grid) ---------------------------------------

  plot_title <- if(length(unique_stations_one) < 2){
      plot_title <- paste("Cruise ",cruise_name,"\nFOCI GRID ",
                          unique_foci_grid[i], sep = "")
    } else {
      plot_title <- paste("Cruise ",cruise_name,"\n", "Station ",
                          stringr::str_replace(unique_stations[i],
                                      "_", " Haul "), sep = "")
    }

# Determine filter for unique station/ foci grid ------------------------------

  filtered_data <- if(length(unique_stations_one) < 2){
      filtered_data <- fc_dataframe %>%
        dplyr::filter(FOCI_GRID == unique_stations[i])
    } else {
      filtered_data <- fc_dataframe %>%
        dplyr::filter(Station_haul == unique_stations[i])
    }

# Make pretty plot breaks for depth -------------------------------------------

  depth_breaks <- if(max(filtered_data$DEPTH, na.rm = TRUE) >= 200){
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 , by = 50)
   } else if (max(filtered_data$DEPTH, na.rm = TRUE) > 100 &
              max(filtered_data$DEPTH, na.rm = TRUE) < 200){
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 , by = 20)
   } else {
     depth_breaks <- seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 ,by = 4)
   }

  depth_labels <- if(max(filtered_data$DEPTH, na.rm = TRUE) >= 200){
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 ,
                            by = 50))
  } else if(max(filtered_data$DEPTH, na.rm = TRUE) > 100 &
            max(filtered_data$DEPTH, na.rm = TRUE) < 200) {
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 ,
                            by = 20))
  } else {
    depth_labels <- abs(seq(-(max(filtered_data$DEPTH, na.rm = TRUE)),0 ,
                            by = 4))
  }

# Plot data --------------------------------------------------------------------

  ts_plot <- ggplot2::ggplot(data = filtered_data)+
    ggplot2::geom_path(ggplot2::aes(MEASURMENT, -(DEPTH), color = TYPE), size = 2,
                      alpha = 0.6)+
    ggplot2::geom_point(ggplot2::aes(MEASURMENT,-(DEPTH), color = TYPE), shape = 1,
                        size = 4)+
    ggplot2::scale_y_continuous(breaks = depth_breaks,
                       labels = depth_labels)+
    ggplot2::scale_color_manual(values = plot_colors)+
    ggplot2::theme_bw()+
    ggplot2::theme(
           axis.text.y = ggplot2::element_text(face = "bold", size = 12),
           axis.text.x = ggplot2::element_text(face = "bold", size = 12),
           axis.title.x  = ggplot2::element_text(face = "bold", size = 14),
           axis.title.y  = ggplot2::element_text(face = "bold", size = 14),
           title = ggplot2::element_text(face = "bold", size = 18),
           strip.background = ggplot2::element_blank(),
           strip.text = ggplot2::element_blank(),
           legend.position = "none")+
    ggplot2::ylab(label = "Depth [m]")+
    ggplot2::xlab(label = expression(bold(paste("Salinity[PSU]",
                                       "\t\t\t\t\t\t\t\t\t\t\t",
                                       paste("Temperature",
                                             "["~degree~C, "]"))))) +
    ggplot2::ggtitle(label = plot_title)+
    ggplot2::facet_wrap(~ TYPE, nrow = 1, scales = "free_x")

# Write plot to file ----------------------------------------------------------

  grDevices::png(filename = name_ts_plot, width = 500, height = 600,
                 units = "px", bg = "transparent")

  print(ts_plot)

  grDevices::dev.off()

  }

}
