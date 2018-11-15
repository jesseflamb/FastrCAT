#' @title Compare Temperature and Salinity to GAK1 line



library(dataone)

compare_gak1 <- function(current_path, foci_header = TRUE){

# node object for production level dataone environment-------------------------
cn <- dataone::CNode("PROD")

# The Reasearch Workspace node is the location of the GAK1 line data-----------
mn <- dataone::getMNode(cn, "urn:node:RW")

# General ---------------------------------------------------------------------
# Cited Identifier: https://doi.org/10.24431/rw1k1b
# Abstract
# This data is part of the Gulf Watch Alaska (GWA), Environmental Drivers
# component of the Exxon Valdez Oil Spill Trustee Council, project numbers
# 12120114P, 13120114P, 14120114P, 15120114P, 16120114P. Gulf Watch Alaska is
# the long-term ecosystem monitoring program of the Exxon Valdez Oil Spill
# Trustee Council for the marine ecosystem affected by the 1989 oil spill.
# This dataset describes CTD readings from the GAK1 oceanographic station in
# Prince William Sound(59°50.7’N, 149°28.0’W). Located at the mouth of
# Resurrection Bay near Seward,
# Alaska, temperature and salinity versus depth profiles have been taken at GAK1
# since December, 1970. This multi-decade time series is one of the longest
# running oceanographic time series in the North Pacific. This dataset includes
# a single ASCII-formatted '.dat' file containing CTD profile data 1970-2016.
# This file provides the code for the cruise that collected the profile, the
# station number of the GAK1 CTD profile on the cruise that collected the data,
# the date (in decimal year) of the profile, as well as depth (m),
# temperature (C), salinity (ppt), seawater density (sigma-t), and delta-D.

# Resource Constraints---------------------------------------------------------
# This data may be freely used. If you do choose to use it, please contact
# Seth Danielson (sldanielson@alaska.edu) with information about your
# application and publications that result so that we can keep our website
# updated and so we can continue keeping this time series funded. This material
# is based upon work funded by multiple entities, including the Exxon Valdez Oil
# Spill Trustee Council. Any opinions, findings, conclusions, or
# recommendations expressed herein are those of the author(s) and do not
# necessarily reflect the views or positions of funding entities. Standard
# scientific norms for attribution, credit, and potential co-authorship should be
# followed when using these data including to the Owners, Exxon Valdez Oil Spill
# Trustee Council and other sources of funding. Please let the Owner know when
# these data are used.

gak1_data <- dataone::getObject(mn,"e24107a7-8905-4f55-8c75-8b0831e47a83")%>%
  rawToChar(.)%>%
  textConnection(.)%>%
  read.table(., sep = "\t", stringsAsFactors = FALSE)



headers <- unlist(stringr::str_split(gak1_data[1,], pattern = "[:space:]+"))%>%
                            stringr::str_trim(.)
headers <- headers[1:8]


gak_data <- gak1_data %>%
  dplyr::slice(-c(1:3))%>%
  tidyr::separate(V1, headers, extra = "merge", sep = "[:space:]+")%>%
  dplyr::mutate(Year = as.double(Year),
         Depth = as.integer(Depth),
         Temp = as.double(Temp),
         Sal = as.double(Sal),
         Sigma = as.double(Sigma),
         `Delta-D` = as.double(`Delta-D`))%>%
  dplyr::mutate(Year = format(lubridate::date_decimal(Year),"%Y-%m-%d %H:%M:%OS"))%>%
  dplyr::mutate(Year = lubridate::ymd_hms(Year))

# Match Foci headers names?----------------------------------------------------
if(foci_header == TRUE){

  colnames(gak_data) <- c("CRUISE","STATION_NAME","DATE","DEPTH",
                          "TEMPERATURE1","SALINITY1","SIGMA_T","DELTA-D")

  gak_data <- gak_data %>%
    dplyr::mutate(TIME = paste(lubridate::hour(DATE),":",
                               lubridate::minute(DATE), ":",
                               lubridate::second(DATE), sep = ""),
                  DATE = lubridate::date(DATE),
# Add LAT/LON to dataframe for mapping-----------------------------------------
# GAK1 oceanographic station in Prince William Sound(59°50.7’N, 149°28.0’W)
                  LAT = as.numeric("59.8353"),
                  LON = as.numeric("-149.4667"))


  }

# find and read in the file created by make_dataframe_fc()---------------------

fc_data <- readr::read_csv(list.files(path = current_path,
                                      pattern = "\\EcoDAAT.csv$",
                                      ignore.case = TRUE,
                                      include.dirs = TRUE,
                                      full.names = TRUE),
                           col_types = cols(CRUISE = col_character(),
                                            STATION_NAME = col_integer(),
                                            HAUL_NAME = col_integer(),
                                            FOCI_GRID = col_character(),
                                            DATE = col_date(),
                                            LAT = col_double(),
                                            LON = col_double(),
                                            DEPTH_BOTTOM = col_integer(),
                                            DEPTH = col_integer(),
                                            TEMPERATURE1 = col_double(),
                                            SALINITY1 = col_double()))%>%
  dplyr::select(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID,
                DATE, LAT, LON, DEPTH_BOTTOM, DEPTH,
                TEMPERATURE1, SALINITY1)%>%
# filter for GAK1 box ---------------------------------------------------------
  dplyr::filter(LAT <= 60.3353 & LAT >= 59.3353)%>%
  dplyr::filter(LON >= -149.9667 & LON <= -148.9667)

# filter for min and max of cruise dates---------------------------------------
gak_plot_data_all <- gak_data%>%
  dplyr::filter(DEPTH <= max(fc_data$DEPTH, na.rm = TRUE))%>%
  dplyr::filter(lubridate::month(DATE) >=
                  min(lubridate::month(fc_data$DATE), na.rm = TRUE) &
                lubridate::month(DATE) <=
                  max(lubridate::month(fc_data$DATE), na.rm = TRUE))%>%
  tidyr::gather("TYPE","MEASURMENT",c(TEMPERATURE1,SALINITY1))%>%
  dplyr::group_by(year(DATE),DEPTH, TYPE)%>% #gets the each years average
  dplyr::summarise(year_MEAN = mean(MEASURMENT, na.rm = TRUE))%>%
  dplyr::group_by(DEPTH, TYPE)%>%
  # Calculates mean and 95% confidence intervals for plot -----------------------
  dplyr::summarise(MEAN = mean(year_MEAN, na.rm = TRUE),
          CI_95 = mean(year_MEAN, na.rm = TRUE) +
            qnorm(0.975)*sd(year_MEAN,
                            na.rm = TRUE)/sqrt(length(year_MEAN)),
          CI_5 = mean(year_MEAN, na.rm = TRUE) -
            qnorm(0.975)*sd(year_MEAN,
                            na.rm = TRUE)/sqrt(length(year_MEAN)))
# gak1 plot data for year of cruise, no CI's-----------------------------------
gak_plot_data_year <- gak_data%>%
  dplyr::filter(DEPTH <= max(fc_data$DEPTH, na.rm = TRUE))%>%
  dplyr::filter(lubridate::year(DATE) %in% lubridate::year(fc_data$DATE))%>%
  dplyr::filter(lubridate::month(DATE) >=
                  min(lubridate::month(fc_data$DATE), na.rm = TRUE) &
                  lubridate::month(DATE) <=
                  max(lubridate::month(fc_data$DATE), na.rm = TRUE))%>%
  tidyr::gather("TYPE","MEASURMENT",c(TEMPERATURE1,SALINITY1))%>%
  dplyr::group_by(year(DATE),DEPTH, TYPE)%>% #gets the each years average
  dplyr::summarise(year_MEAN = mean(MEASURMENT, na.rm = TRUE))%>%
  dplyr::group_by(DEPTH, TYPE)%>%
# Calculates mean and 95% confidence intervals for plot ---------------------
  dplyr::summarise(MEAN = mean(year_MEAN, na.rm = TRUE))

# Plot information for cruise report ------------------------------------------
plot_colors <- c("#1565C0","#b92b27")
names(plot_colors) <- c("SALINITY1", "TEMPERATURE1")

fc_plot_data <- fc_data%>% dplyr::select(STATION_NAME, HAUL_NAME,
                                               DEPTH, TEMPERATURE1,
                                               SALINITY1)%>%
  tidyr::unite(Station_haul,STATION_NAME,HAUL_NAME,sep = "_",
               remove = FALSE)%>%
  tidyr::gather("TYPE","MEASURMENT",c(TEMPERATURE1,SALINITY1))%>%
  dplyr::group_by(DEPTH, TYPE)%>%
# Calculates mean and 95% confidence intervals for plot -----------------------
  dplyr::summarise(MEAN = mean(MEASURMENT, na.rm = TRUE),
                 CI_95 = mean(MEASURMENT, na.rm = TRUE) +
                   qnorm(0.975)*sd(MEASURMENT,
                                   na.rm = TRUE)/sqrt(length(MEASURMENT)),
                 CI_5 = mean(MEASURMENT, na.rm = TRUE) -
                   qnorm(0.975)*sd(MEASURMENT,
                                   na.rm = TRUE)/sqrt(length(MEASURMENT)))%>%
  dplyr::filter(!is.na(MEAN))%>%
  dplyr::filter(!is.na(CI_95))%>%
  dplyr::filter(!is.na(CI_5))


ts_plot <- ggplot2::ggplot()+
  geom_ribbon(aes(-(DEPTH), ymin = CI_5, ymax = CI_95),
              alpha = 0.5,color = "gray", fill = "gray",
              data = gak_plot_data_all)+
  geom_point(aes(-(DEPTH), MEAN), shape = 21, size = 3, color = 'gray',
             fill = "gray", data = gak_plot_data_year)+
  geom_ribbon(aes(-(DEPTH), ymin = CI_5, ymax = CI_95, color = TYPE, fill = TYPE),
              alpha = 0.5, data = fc_plot_data)+
  geom_point(aes(-(DEPTH), MEAN, color = TYPE, fill = TYPE), shape = 21, size = 3,
             alpha = 0.8, data = fc_plot_data)+
  ggplot2::scale_color_manual(values = plot_colors)+
  ggplot2::scale_fill_manual(values = plot_colors)+
  ggplot2::coord_flip()+
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x  = element_text(face = "bold", size = 14),
    axis.title.y  = element_text(face = "bold", size = 14),
    title = element_text(face = "bold", size = 18),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "none")+
  ggplot2::xlab(label = "Depth [m]")+
  ggplot2::ylab(label = expression(bold(paste("Salinity[PSU]",
                                              "\t\t\t\t\t\t\t\t\t\t\t",
                                              paste("Temperature",
                                                    "["~degree~C, "]"))))) +
  ggplot2::facet_wrap(~ TYPE, nrow = 1, scales = "free_x")

# make the file name ----------------------------------------------------------

name_ts_plot <- paste(current_path,"/plots/",unique(fc_data$CRUISE), "_toGAK1_",
                      ".png",sep = "")

# Write plot to file ----------------------------------------------------------

grDevices::png(filename = name_ts_plot, width = 700, height = 800,
               units = "px", bg = "transparent")

print(ts_plot)

grDevices::dev.off()

}
