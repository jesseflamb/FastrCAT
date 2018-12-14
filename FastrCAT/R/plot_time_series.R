#' @title GOA Time Series Plots
#' @description Creates either temperature or salinity plots of core EcoFOCI
#' stations in the Gulf of Alaska. Each plot displays the average temperature
#' or salinity for each meter of depth of the core stations for each year for
#' the months when peak sampling of these regions occured. Line 8 and Semidi
#' area are most commonly sampled in May and June, which is considered
#' Spring. Summer sampling in the Gulf of Alaska has been less frequent and
#' starts in the early 2000's. This summer sampling is in the Semidi core
#' area, summer is considered August and September.Post 2010, these core
#' stations were only sampled in odd numbered years. In the future more core
#' areas will be added.
#' @param hist_data Supply the path and .csv file name of the historical data
#' in quotations. The historical data must be in the format created by
#' the FastrCAT::make_dataframe_fc.
#' @param core_stations There are three core areas for the Gulf of Alaska which
#' have been regularly sampled and are representative of the Gulf of Alaska.
#' Core areas are all in bottom depth at or greater than 100 and less than 150
#' meters. Salintiy or temperature data from the surface to 100m will be shown.
#' core_stations are as follows: "Line 8 FOX", "Line 8", "Semidi Spring" and
#' "Semidi Summer".
#' Line 8 FOX is a set of six stations  with a bounding box of `57.52, -155.2,
#' 57.72, -154.85`. Line 8 are the 4 core stations of Line 8 FOX. Semidi are
#' 6- 8 stations centrally located in the Semidi area and have been consistently
#' sampled, with a bounding box of '55.1, -158.5, 55.9, -158.0'.
#' @param plot_type will accept on of two quoted characters "temperature" or
#' "salinity".
#' @param fastcat_data An optional argument if you want to add the current years
#' fastcat data. Supply the path and .csv file name to the current years
#' fastcat data. This must be in the format created by the FastrCAT function
#' make_dataframe_fc.
#' @param anomaly An optional argument if you want the anomaly of the plot_type
#' selected. This argument is set to FALSE, when set to TRUE then the anomaly
#' will be plotted. The anomoly is calculated using the ...something equation.
#' @return A depth by year tile plot of temperature or salinity. The plot will
#' be written to the folder designated by the historical data file path. The
#' plot will be in .png format. It should be noted that the plot throws out
#' the 0 depth value. 0 depth can and has been problematic for fastcat data.


plot_time_series <- function(hist_data, core_stations, plot_type,
                             fastcat_data = FALSE, anomaly = FALSE){

fast_col_types <- readr::cols_only(
                                   DATE = readr::col_date(format = "%Y-%m-%d"),
                                   DEPTH_BOTTOM = readr::col_integer(),
                                   DEPTH = readr::col_integer(),
                                   TEMPERATURE1 = readr::col_double(),
                                   SALINITY1 = readr::col_double(),
                                   LAT = readr::col_double(),
                                   LON = readr::col_double())

old_data <- readr::read_csv(hist_data, col_types = fast_col_types)


time_data <- if(fastcat_data == FALSE){
  time_data <- old_data
} else if (fastcat_data == TRUE){

  time_data <- fastcat_data <- readr::read_csv(fastcat_data,
                                        col_types = fast_col_types)%>%
                                        dplyr::bind_rows(old_data)
}


range_filter <- if(core_stations == "Line 8"){ # 4 FOX stations + 2
  time_data %>%
    dplyr::filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
    dplyr::filter(LON >= -155.2673 & LON <= -154.7725)%>%
    dplyr::filter(month(DATE) %in% c(5,6)) #May and June
} else if (core_stations == "Line 8 FOX"){ # 4 Stations
  time_data %>%
    dplyr::filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
    dplyr::filter(LON >= -155.2 & LON <= -154.85)%>%
    dplyr::filter(month(DATE) %in% c(5,6)) #May and June
} else if(core_stations == "Semidi Spring"){ # .5 x 1 degree area
  time_data %>%
    dplyr::filter(LAT >= 55.2 & LAT <= 57.2)%>%
    dplyr::filter(LON >= -159 & LON <= -158)%>%
    dplyr::filter(month(DATE) %in% c(5,6))%>% #May and June
    dplyr::filter(DEPTH_BOTTOM >= 100 & DEPTH_BOTTOM <= 150)

} else if(core_stations == "Semidi Summer"){
  time_data %>%
    dplyr::filter(LAT >= 55.2 & LAT <= 57.2)%>%
    dplyr::filter(LON >= -159 & LON <= -158)%>%
    dplyr::filter(month(DATE) %in% c(8,9))%>% #August and September
    dplyr::filter(DEPTH_BOTTOM >= 100 & DEPTH_BOTTOM <= 150)
}



plot_data <- if(plot_type == "temperature"){

  if(anomaly == FALSE){

  range_filter %>%
    dplyr::filter(DEPTH <= 100 & DEPTH > 0)%>%
    dplyr::group_by(year(DATE), DEPTH)%>%
    dplyr::summarise(mean_yr = mean(TEMPERATURE1, na.rm = TRUE))
  }else if(anomaly == TRUE){
    # place anomaly equation here
  }

} else if(plot_type == "salinity"){

  if(anomaly == FALSE){

  range_filter %>%
    dplyr::filter(DEPTH <= 100 & DEPTH > 0)%>%
    dplyr::group_by(year(DATE), DEPTH)%>%
    dplyr::summarise(mean_yr = mean(SALINITY1, na.rm = TRUE))
  }else if(anomaly == TRUE){
    # place anomaly equation here
  }
}



# temperature color pallete red to blue, did not use oce for temperature
# it was decided red to blue was more intuitive. Salinity color ramp
# is the salinity oce color ramp. ---------------------------------------------
plot_color <- if(plot_type == "temperature"){

  c( "#22325E","#214985", "#136495", "#107EA0", #BLUE "#132037",
    "#2E97A8","#65ACAE","#95BEBC", "#C0D4CF",
    "#E8EDE3", # Eggshell
    "#f5af19","#f5af19","#f12711","#ff2525", "#f10000",
    "#bd0000", "#a30000" ,"#560000") # RED ,"#230000"

   }else if(plot_type == "salinity"){

  c("#2B1470", "#2C1D8A", "#212F96", "#114293", "#08518F", "#0E5E8B",
    "#1A6989", "#267488", "#318088", "#3A8B88", "#439787", "#4BA385",
    "#56AF81", "#64BA7B", "#77C574", "#91CF6C", "#B0D66C", "#CCDE78",
    "#E6E58A", "#FEEEA0")
}



# expressions for either salinity or temperature legend -----------------------
legend_name <- if(plot_type == "temperature"){
  expression(bold( ~degree~C ))
} else if (plot_type == "salinity"){
  expression(bold("PSU"))
}

# time range of plot data, names different for each year ----------------------
time_range <- paste(min(plot_data$`year(DATE)`), "_",
                    max(plot_data$`year(DATE)`), sep = "")

# the directory to send the plot to -------------------------------------------
current_path <- unlist(stringr::str_split(hist_data, "/"))
current_path <- paste(current_path[ 1:length(current_path)-1], collapse = "/" )

# name time series plot to write to file --------------------------------------
name_time_series_plot <- paste(current_path, "/", core_stations, "_", plot_type,
                               "_", time_range, ".png",sep = "")

# Time series plot ------------------------------------------------------------

time_series_plot <- ggplot2::ggplot()+
  ggplot2::geom_tile(aes(x = `year(DATE)`, y = -(DEPTH), fill = mean_yr),
                     data = plot_data)+
  ggplot2::scale_fill_gradientn(colors = plot_color, name = legend_name)+
  ggplot2::scale_y_continuous(breaks = -(seq(0,100 ,by = 10)),
                              labels = abs(seq(0,100 ,by = 10)))+
  ggplot2::theme_bw()+
  ggplot2::ylab(label = "Depth m")+
  ggplot2::xlab(label = "Year")+
  ggplot2::ggtitle(label = core_stations)+
  ggplot2::theme(
    axis.text.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.x  = element_text(face = "bold", size = 14),
    axis.title.y  = element_text(face = "bold", size = 14),
    title = element_text(face = "bold", size = 18),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.75, "cm"),
    legend.text = element_text(face = "bold", size = 12))


grDevices::png(filename = name_time_series_plot, width = 600, height = 600,
               units = "px", bg = "transparent")

print(time_series_plot)

grDevices::dev.off()

}
