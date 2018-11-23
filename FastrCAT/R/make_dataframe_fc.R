#' @title Create a Dataframe from .up files
#'
#' @description This function writes a single data frame in .csv format to file
#' containing oceanographic data collected by the FastCat during a cruise.
#' The format and column naming conventions are specific to the needs of EcoDAAT.
#' This is the primary function of the FastrCAT package and must be run prior to
#' all other functions. All other functions depend on the data frame generated.
#' Other outputs of the function are two text files. This first is a cruise
#' summary.The summary contains basic information about the data and some summary
#' statistics.The second is a warnings file, which tells users if data or
#' information is missing incase reprocessing is necessary.
#' @param current_path The path to directory where all .up files
#' are located for a cruise.
#' @param GE A logical value, Returns the dataframe to the global R environment.
#' By defulaut it is set to FALSE. Set to TRUE if you would like the data
#' available in the global environment.
#' @param Cruise_report A logical value set to TRUE. When TRUE a cruise report
#' will be generated. When set to false a cruise report will not be
#' generated.
#' @return .csv file of all .up file data. An .html file with a cruise
#' summary and data QAQC output.



make_dataframe_fc <- function(current_path, GE = FALSE, Cruise_report = TRUE){

# Get list of files -----------------------------------------------------------
  temp <- list.files(path = current_path, pattern = "\\.up$",
                     ignore.case = TRUE, include.dirs = TRUE,
                     full.names = TRUE)

# Create empty list for each file ---------------------------------------------
  cruise_data <- list()

# Create empty lists for files with problems-----------------------------------
  no_data_files <- list()
  no_head_files <- list()

# Retrieve data from each file ------------------------------------------------
  for(i in 1:length(temp)){

    full_table <- read_lines(temp[i])

# Determines if file contains data --------------------------------------------
    if(length(file.size(full_table)) == 0){

      no_data_files[[i]] <- paste("This file", temp[i] ,"is empty.", sep = " ")

      warning(paste("This file", temp[i], "is empty.", sep = " "))
      next()
    }

# Determines if file has header information -----------------------------------
    if(length(grep("@ ", full_table,ignore.case = TRUE)) == 0){

      no_head_files[[i]] <- paste("This file", temp[i],
                                 "has no header info needs to be reprocesed.",
                                 sep = " ")

      warning(paste("This file", temp[i],
                    "has no header info, needs to be reprocesed.", sep = " "))

      next()

    }else{

# When file has header information --------------------------------------------

# Get latitude ----------------------------------------------------------------
      lat_id <- if(is.na(stringr::str_trim(unlist(strsplit(
                    full_table[grep("@ Latitude?",
                    full_table,ignore.case = TRUE)], ":")))[2])){
        lat_id <- as.double(NA)
        }else{
        lat_id <-stringr::str_trim(unlist(strsplit(
                                   full_table[grep("@ Latitude?", full_table,
                                   ignore.case = TRUE)], ":"))[2],
                                   side = "both")}

# Get longitude ---------------------------------------------------------------
      long_id <- if(is.na(stringr::str_trim(unlist(strsplit(
                    full_table[grep("@ Longitude?", full_table,
                    ignore.case = TRUE)], ":")))[2])){
        long_id <- as.double(NA)
      }else{
        long_id <- stringr::str_trim(unlist(strsplit(
                                     full_table[grep("@ Longitude?",
                                     full_table,ignore.case = TRUE)],
                                     ":"))[2], side = "both")}

# Get the date ----------------------------------------------------------------
      date_id <- if(is.na(stringr::str_trim(unlist(strsplit(
                    full_table[grep("@ Date?", full_table,ignore.case = TRUE)],
                    ":")))[2])){
        date_id <- as.character(NA) #If the date is missing subs an NA.
      }else{
        date_id <- stringr::str_trim(unlist(strsplit(
                            full_table[grep("@ Date?", full_table,
                            ignore.case = TRUE)], ":"))[2], side = "both")}

# Get the time ----------------------------------------------------------------
      time_id <-  if(is.na(stringr::str_trim(unlist(strsplit(
                    full_table[grep("@ Cast?", full_table,ignore.case = TRUE)],
                    ":")))[2])){
        time_id <- as.character(NA)
      }else{
        time_id <- strftime(as.POSIXct(paste(date_id,
                            stringr::str_pad(stringr::str_trim(unlist(
                            strsplit(full_table[grep("@ Cast?",
                            full_table, ignore.case = TRUE)],":"))[2],
                            side = "both"), width = 4, side = "left", pad = "0"),
                            sep = "-"), format = "%d%b%y-%H%M", tz = "UTC"),
                            format = "%H:%M:%S", tz = "UTC")}

# Get the cruise name ---------------------------------------------------------
      cruise_id <- if(is.na(stringr::str_trim(unlist(strsplit(
                      full_table[grep("@ Cruise?",
                      full_table,ignore.case = TRUE)], ":")))[2])){
        cruise_id <- as.character(NA)#If Cruise is missing subs an NA
      }else{
        cruise_id <- toupper(stringr::str_trim(unlist(strsplit(
                             full_table[grep("@ Cruise?", full_table,
                             ignore.case = TRUE)], ":"))[2], side = "both"))}

# Get the station name --------------------------------------------------------
      station_id_p <- if(length(full_table[grep("@ Station?",
                         full_table,ignore.case = TRUE)])!=0){
        station_id_p <- stringr::str_trim(unlist(strsplit(
                                 full_table[grep("@ Station?",
                                 full_table,ignore.case = TRUE)],
                                 ":"))[2], side = "both")
        }else{
          station_id_p <- as.integer(NA)}
# Get the haul name -----------------------------------------------------------

      haul_id_p <- if(length(full_table[grep("@ Haul?",
                      full_table, ignore.case = TRUE)]) != 0 ){
        haul_id <- stringr::str_trim(unlist(strsplit(
                            full_table[grep("@ Haul?", full_table,
                            ignore.case = TRUE)], ":"))[2], side = "both")
        }else{
        haul_id <- as.integer(NA) }

# Get foci grid name ----------------------------------------------------------
      grid_id <- if(length(full_table[grep("@ Grid?",
                    full_table,ignore.case = TRUE)]) == 0){
        grid_id <- as.character(NA)
      }else if (is.na(stringr::str_trim(unlist(strsplit(
                full_table[grep("@ Grid?", full_table,ignore.case = TRUE)],
                ":")))[2])){
        grid_id <- as.character(NA)
      }else if(stringr::str_detect(stringr::str_trim(unlist(strsplit(
               full_table[grep("@ Grid?", full_table,ignore.case = TRUE)],
               ":"))[2], side = "both"), "BON") == TRUE){
        grid_id <- as.character(NA)
      }else {
        grid_id <- toupper(stringr::str_trim(unlist(strsplit(
                           full_table[grep("@ Grid?",
                           full_table,ignore.case = TRUE)], ":"))[2],
                           side = "both"))}

# Fixes Station name if it is actually foci grid-------------------------------
      station_id <- if(is.na(suppressWarnings(
                       as.numeric(station_id_p))) == TRUE){
        station_id <- as.integer(NA)
      }else if(stringr::str_detect(station_id_p,"[.]") == TRUE){
        station_id <- unlist(strsplit(station_id_p,"[.]"))[1]
      }else{
        station_id <- station_id_p
      }

      grid_id <- if(is.na(suppressWarnings(as.numeric(station_id))) == TRUE &
                   is.na(grid_id) == TRUE){
        grid_id <- station_id_p
      }else{
        grid_id <- grid_id
      }

# Fixes station name if it is in station.haul format --------------------------
      haul_id <- if(is.na(haul_id_p) == TRUE &
                   !is.na(station_id_p) &
                   stringr::str_detect(station_id_p, "[.]") == TRUE){
        haul_id <- unlist(strsplit(station_id_p, "[.]"))[2]
      }else if(is.na(haul_id_p) == TRUE){
        haul_id <- as.integer(NA)
      }else{
        haul_id <- haul_id_p
      }

# Get the depth ---------------------------------------------------------------
      depth_id <- if(length(full_table[grep("@ Water?", full_table,
                     ignore.case = TRUE)]) != 0 ){
        depth_id <- stringr::str_trim(unlist(strsplit(
                             full_table[grep("@ Water?", full_table,
                             ignore.case = TRUE)], ":"))[2], side = "both")
      }else{
        depth_id <- as.integer(NA) }

# Get instrument type ---------------------------------------------------------
      instrument_id <- if(length(full_table[grep("@ Instrument?", full_table,
                          ignore.case = TRUE)]) != 0){
        instrument_id <- stringr::str_trim(unlist(strsplit(
                                  full_table[grep("@ Instrument??", full_table,
                                  ignore.case = TRUE)], ":"))[2], side = "both")
      }else{
        instrument_id <- as.character(NA)
      }

# Reformat latitude and longitude to prep for conversion ----------------------
      lat_id <- paste(unlist(strsplit(lat_id, " "))[1],
                      unlist(strsplit(lat_id, " "))[2], sep = " ")

      long_id <- paste(unlist(strsplit(long_id, " "))[1],
                       unlist(strsplit(long_id, " "))[2], sep = " ")
    }

# Get columnar data -----------------------------------------------------------
    if (grep("END", full_table) >= length(full_table)){

# If data is missing ----------------------------------------------------------
      bongo_data <- data.frame(PRESSURE = as.double(NA),
                             TEMPERATURE1 = as.double(NA),
                             CONDUCTIVITY1 = as.double(NA),
                             SALINITY1 = as.double(NA),
                             SIGMA_T = as.double(NA),
                             FLAG = as.character(NA))

      no_data_files[[i]] <- paste("This file", temp[i],
                                  "has no data associated with it.", sep = " ")

      warning(paste("This file", temp[i],
                    "has no data associated with it.", sep = " "))

    }else{

# Gets data -------------------------------------------------------------------
      bongo_table <- read.table(temp[i], skip = grep("END", full_table),
                                sep = "", header = FALSE)

      col_length <- length(names(bongo_table))

      col_nums <- as.character(seq(0,(col_length -1), by = 1))

# These are all the differnt ways we have selected names for measuments in SeaSoft.
      pref_col_names<- c("timeS: time [s]",
                         "timeS: Time, Elapsed [seconds]",
                         "timeJ: Julian Days",
                         "timeH: Time, Elapsed [hours]",
                         "timeS: Time, Elapsed [seconds]",
                         "timeJ: time [julian days]",
                         "pr: pressure [db]",
                         "prSM: Pressure, Strain Guage [db]",
                         "prSM: Pressure, Strain Gauge [db]",
                         "prdM: Pressure, Strain Gauge [db]",
                         "prDM: Pressure, Strain Gauge [db]",
                         "t068: temperature, IPTS-68 [deg C]",
                         "t090: temperature, ITS-90 [deg C]",
                         "t090C: temperature, ITS-90 [deg C]",
                         "t090C: Temperature [ITS-90, deg C]",
                         "t4990C: Temperature [ITS-90, deg C]",
                         "tv290C: Temperature [ITS-90, deg C]",
                         "c0mS/cm: conductivity [mS/cm]",
                         "c0mS/cm: Conductivity [mS/cm]",
                         "c0S/m: Conductivity [S/m]",
                         "sal00: salinity, PSS-78 [PSU]",
                         "sal00: Salinity, Practical [PSU]",
                         "sal00: Salinity [PSU]",
                         "density00: Density [density, Kg/m^3]",
                         "sigma-t00: density, sigma-t [kg/m^3]",
                         "sigma-t00: Density [sigma-t, Kg/m^3 ]",
                         "sigma-t00: Density [sigma-t, kg/m^3 ]",
                         "flag:  0.000e+00",
                         "flag: flag",
                         "nbin: number of scans per bin",
                         "depSM: Depth [salt water, m]",
                         "depS: depth, salt water [m]",
                         "svCM: Sound Velocity [Chen-Millero, m/s]",
                         "scan: scan number",
                         "dm: dynamic meters")

# EcoDAAT specific column names -----------------------------------------------
      names(pref_col_names) <-  c("Time1","Time2","Time3",
                                  "Time4","Time5", "Time6",
                                 "PRESSURE","PRESSURE","PRESSURE",
                                 "PRESSURE","PRESSURE",
                                 "TEMPERATURE1","TEMPERATURE1","TEMPERATURE1",
                                 "TEMPERATURE1", "TEMPERATURE1", "TEMPERATURE1",
                                 "CONDUCTIVITY1","CONDUCTIVITY1", "CONDUCTIVITY1",
                                 "SALINITY1","SALINITY1","SALINITY1",
                                 "SIGMA_T","SIGMA_T","SIGMA_T","SIGMA_T",
                                 "FLAG","FLAG","Bins","depth","depth",
                                 "sound","scan","dm")

            colum_names <- vector()

# Gets all data column names --------------------------------------------------
            for(n in 1:col_length){

            name0 <- unlist(strsplit(
                            full_table[grep(paste("# name",col_nums[n],
                            sep = " "), full_table, ignore.case = TRUE)],
                            "= "))[2]

            colum_names[n] <- names(pref_col_names[pmatch(name0,
                                    pref_col_names)])}


      colnames(bongo_table) <- colum_names


# Tests for depth and pressure ------------------------------------------------
      dep_and_press <- sum(colum_names %in% c("depth", "PRESSURE")) == 2 #TRUE
      dep_only <- sum(colum_names %in% c("depth")) == 1 #TRUE
      press_only <- sum(colum_names %in% c("PRESSURE")) == 1 #TRUE

# Handles if depth and pressure columns were present --------------------------
      bongo_data <- if(nrow(bongo_table) != 0 & press_only == TRUE){

        bongo_data <- bongo_table %>%
          dplyr::mutate(PRESSURE = as.double(PRESSURE),
                        TEMPERATURE1 = as.double(TEMPERATURE1),
                        CONDUCTIVITY1 = as.double(CONDUCTIVITY1),
                        SALINITY1 = as.double(SALINITY1),
                        SIGMA_T = as.double(SIGMA_T),
                        FLAG = as.character(FLAG))

# Handles if only depth is present so no conversion ---------------------------
      }else if (nrow(bongo_table) != 0 & dep_only == TRUE &
                press_only == FALSE){

        bongo_data <- bongo_table %>%
          dplyr::mutate(PRESSURE = as.double(depth),
                        TEMPERATURE1 = as.double(TEMPERATURE1),
                        CONDUCTIVITY1 = as.double(CONDUCTIVITY1),
                        SALINITY1 = as.double(SALINITY1),
                        SIGMA_T = as.double(SIGMA_T),
                        FLAG = as.character(FLAG))

      }else{

        bongo_data <- data.frame(
          PRESSURE = as.double(NA),
          TEMPERATURE1 = as.double(NA),
          CONDUCTIVITY1 = as.double(NA),
          SALINITY1 = as.double(NA),
          SIGMA_T = as.double(NA),
          FLAG = as.character(NA))
      }



    }


# Formats bongo data ----------------------------------------------------------
    bongo_data <- bongo_data %>%
      dplyr::mutate(LAT = lat_id,
                    LAT = as.numeric(measurements::conv_unit(LAT,
                              from = 'deg_dec_min', to = 'dec_deg')),
                    LON = long_id,
                    LON = -1*(as.numeric(measurements::conv_unit(LON,
                                  from = 'deg_dec_min', to = 'dec_deg'))),
                    DATE = as.POSIXct(date_id,format = "%d%b%y", tz = "UTC"),
                    TIME = time_id,
                    CRUISE = cruise_id,
                    STATION_NAME = as.character(station_id),
                    HAUL_NAME = as.integer(haul_id),
                    DIRECTORY =  as.character(temp[i]),
                    FOCI_GRID = as.character(grid_id),
                    DEPTH_BOTTOM = suppressWarnings(as.integer(depth_id)),
                    INSTRUMENT = as.character(instrument_id))



    bongo_data <- if(nrow(bongo_table) > 1 &
                   dep_only == TRUE &
                   press_only == FALSE){

      bongo_data <- bongo_data %>% dplyr::mutate(DEPTH = as.integer(PRESSURE))

    }else if (nrow(bongo_table) > 1 &
              dep_only == FALSE &
              press_only == TRUE){
# Calculate depth from pressure -----------------------------------------------
#Depth calaculated using AN69:Conversion of Pressure to Depth,
#requires Decimal degrees.
#http://www.seabird.com/document/an69-conversion-pressure-depth
      bongo_data <- bongo_data %>% dplyr::mutate(
                                          DEPTH = as.integer((((((-1.82e-15  *
                                          PRESSURE + 2.279e-10 ) * PRESSURE -
                                          2.2512e-5 ) * PRESSURE + 9.72659) *
                                          PRESSURE) / (9.780318 *
                                          (1.0 + (5.2788e-3 + 2.36e-5 *
                                          (sin(LAT/57.29578)^2)) *
                                          sin(LAT/57.29578)^2)) +
                                          1.092e-6 * PRESSURE)))
    }else{
      bongo_data <- bongo_data %>% dplyr::mutate(DEPTH = as.integer(NA))
    }

# Find which casts have bad salinities ----------------------------------------

    bad_salinity <- bongo_data %>% dplyr::select(CRUISE, STATION_NAME,
                                                 HAUL_NAME,
                                                 FOCI_GRID, SALINITY1)%>%
      dplyr::filter(SALINITY1 < 0.05 | SALINITY1 > 38)%>%
      dplyr::group_by(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID)%>%
      dplyr::summarise(Min_value = ifelse(min(SALINITY1, na.rm = TRUE) < 0.05,
                                          min(SALINITY1, na.rm = TRUE), NA),
                       Max_value = ifelse(max(SALINITY1, na.rm = TRUE) > 38,
                                          max(SALINITY1, na.rm = TRUE), NA))%>%
      dplyr::mutate(ERROR_TYPE = "Salinity")


# Find which casts have bad temperature ----------------------------------------

    bad_temperature <- bongo_data %>% dplyr::select(CRUISE, STATION_NAME,
                                                    HAUL_NAME,
                                                    FOCI_GRID, TEMPERATURE1)%>%
      dplyr::filter(TEMPERATURE1 < -3 | TEMPERATURE1 > 20)%>%
      dplyr::group_by(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID)%>%
      dplyr::summarise(Min_value = ifelse(min(TEMPERATURE1, na.rm = TRUE) < -3,
                                          min(TEMPERATURE1, na.rm = TRUE), NA),
                       Max_value = ifelse(max(TEMPERATURE1, na.rm = TRUE) > 20,
                                          max(TEMPERATURE1, na.rm = TRUE), NA))%>%
      dplyr::mutate(ERROR_TYPE = "Temperature")

# Bind together bad salinity and temperature for error table ------------------

    bad_sal_temp <- bad_temperature %>% dplyr::bind_rows(bad_salinity)%>%
      as.data.frame(.)


# Rearrange columns for EcoDAAT format ----------------------------------------
    bongo_data <- bongo_data %>% dplyr::select(CRUISE, STATION_NAME, HAUL_NAME,
                                               FOCI_GRID, DATE, TIME, LAT, LON,
                                               DEPTH_BOTTOM, DEPTH,PRESSURE,
                                               TEMPERATURE1, SALINITY1,
                                               CONDUCTIVITY1, SIGMA_T,
                                               INSTRUMENT, DIRECTORY)%>%
# Filters out bad temperatures ------------------------------------------------
                  dplyr::filter(TEMPERATURE1 >= -3 & TEMPERATURE1 <= 20)%>%
# Filters out bad salinities --------------------------------------------------
                  dplyr::filter(SALINITY1 >= 0.05  & SALINITY1 <= 38)


# Appends finished files to cruise list ---------------------------------------
    cruise_data[[i]] <- bongo_data

  }

# Make problem files text -----------------------------------------------------
  no_data_files <- data.frame(unlist(no_data_files))
  No_head_files <- data.frame(unlist(no_head_files))

# Make a single file of all cruise data ---------------------------------------
  cruise_data_all <- as.data.frame(dplyr::bind_rows(cruise_data))%>%
    dplyr::mutate(CONDUCTIVITY2 = as.double(NA),
                  FLOUROMETER = as.double(NA),
                  OXYGEN1 = as.double(NA),
                  OXYGEN2 = as.double(NA),
                  PAR = as.double(NA),
                  PH = as.double(NA),
                  POTENTIAL_DENSITY = as.double(NA),
                  SALINITY2 = as.double(NA),
                  TEMPERATURE2 = as.double(NA),
                  TRANS = as.double(NA),
                  COMMENTS_SEACAT_CTD = as.character(NA),
                  GEAR_NAME = as.character("CAT"),
                  NET = as.integer(0))

# Generate the Cruise report if set to TRUE------------------------------------
  if(Cruise_report == TRUE){
# Make cruise summary and summary stats text ----------------------------------
  summary_fc <- summary(cruise_data_all %>%
                        select(LAT, LON, DEPTH_BOTTOM, DEPTH, PRESSURE,
                               TEMPERATURE1, SALINITY1,  CONDUCTIVITY1, SIGMA_T))
  summary_fc <- summary_fc[-c(2,5),]


  how_many_tows <- unique(paste(cruise_data_all$STATION_NAME,
                                cruise_data_all$HAUL_NAME, sep = "."))

  tows <- if(length(how_many_tows) < 2){
      tows <- length(unique(cruise_data_all$FOCI_GRID))
    } else {
      tows <- length(how_many_tows)
    }

# Create variables for cruise summary text ------------------------------------
  cruise_name_check <- unique(cruise_data_all$CRUISE)

  station_name_num <- length(unique(cruise_data_all$STATION_NAME))

  foci_grid_name_check <- unique(cruise_data_all$FOCI_GRID)

  haul_name_check <- unique(cruise_data_all$HAUL_NAME)

  start_month <- lubridate::month(min(
    lubridate::month(cruise_data_all$DATE),
    na.rm = TRUE), label = TRUE, abbr = FALSE)

  start_day <- lubridate::day(min(cruise_data_all$DATE, na.rm = TRUE))

  end_month <- lubridate::month(max(
    lubridate::month(cruise_data_all$DATE),
    na.rm = TRUE), label = TRUE, abbr = FALSE)

  end_day <- lubridate::day(max(cruise_data_all$DATE, na.rm = TRUE))

  year <- lubridate::year(max(cruise_data_all$DATE, na.rm = TRUE))

  temp_range_check <- round(range(cruise_data_all$TEMPERATURE1, na.rm = TRUE),
                            digits = 1)

  temp_mean <- round(mean(cruise_data_all$TEMPERATURE1, na.rm = TRUE),
                     digits = 1)

  sal_range_check <- round(range(cruise_data_all$SALINITY1, na.rm = TRUE),
                           digits = 1)

  sal_mean <- round(mean(cruise_data_all$SALINITY1, na.rm = TRUE), digits = 2)

  depth_range_check <- round(range(cruise_data_all$DEPTH, na.rm = TRUE),
                             digits = 1)
  lat_range_check <- round(range(cruise_data_all$LAT, na.rm = TRUE),
                           digits = 2)

  lon_range_check <- round(range(cruise_data_all$LON, na.rm = TRUE),
                           digits = 2)

# Plot information for cruise report ------------------------------------------
  plot_colors <- c("#1565C0","#b92b27")
  names(plot_colors) <- c("SALINITY1", "TEMPERATURE1")

  plot_data <- cruise_data_all %>% dplyr::select(STATION_NAME, HAUL_NAME,
                                                  DEPTH, TEMPERATURE1,
                                                  SALINITY1,
                                                 DIRECTORY)%>%
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


  ts_plot <- ggplot2::ggplot(plot_data)+
    ggplot2::geom_pointrange(aes(-(DEPTH), MEAN, ymin = CI_5, ymax = CI_95,
                        color = TYPE),fatten = 6, alpha = 0.6)+
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


# Station map for Cruise summary-----------------------------------------------
# bring in the shape files to make the basemap --------------------------------

  MAP <- sf::st_read(dsn = "inst/extdata",layer = "Alaska_dcw_polygon_Project",
                     quiet = TRUE)

# tranform into WGS84 coordinate system----------------------------------------

  MAP <- sf::st_transform(MAP, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# bring in 200m bathymetry contour --------------------------------------------

  BATH_200 <- sf::st_read(dsn = "inst/extdata", layer = "ne_10m_bathymetry_K_200",
                          quiet = TRUE)
# transform into WGS84 coordinate system---------------------------------------

  BATH_200 <- sf::st_transform(BATH_200,
                               "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# coordinate bounding box for map----------------------------------------------

  fc_xlim <- c(min(cruise_data_all$LON, na.rm = TRUE) - 2,
               max(cruise_data_all$LON, na.rm = TRUE) + 2)
  fc_ylim <- c(min(cruise_data_all$LAT, na.rm = TRUE) - 2,
               max(cruise_data_all$LAT, na.rm = TRUE) + 2)


# station map data for Station haul--------------------------------------------
  Station_map <- cruise_data_all %>%
    dplyr::select(CRUISE, STATION_NAME, HAUL_NAME, LAT, LON)%>%
    tidyr::unite(STATION_HAUL, STATION_NAME, HAUL_NAME, sep = ".")%>%
    dplyr::distinct(STATION_HAUL, .keep_all = TRUE)

# station_map------------------------------------------------------------------
  fc_map <- ggplot2::ggplot()+
    ggplot2::geom_sf(color = "black", data = BATH_200[3], alpha = 0)+
    ggplot2::geom_sf(fill ="#a7ad94", color = "black", data = MAP[1])+
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5,
                                unit_category = "metric")+
    ggplot2::coord_sf(xlim = fc_xlim, ylim = fc_ylim)+
    ggplot2::geom_point(aes(LON, LAT), size = 4, shape = 21, color = "black",
                        fill = "gray", data = Station_map)+
    #ggrepel::geom_text_repel(aes(LON, LAT, label = STATION_HAUL), size = 4,
                             #color = "black",data = Station_map)+
    ggplot2::theme_bw()+
    ggplot2::xlab(label = "Longitude")+
    ggplot2::ylab(label = "Latitude")+
    ggplot2::theme(
      axis.text.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(face = "bold", size = 12),
      axis.title.x  = element_text(face = "bold", size = 14),
      axis.title.y  = element_text(face = "bold", size = 14))

# Cruise Summary, in Rmarkdown format -----------------------------------------
  cruise_report <- c(
    '---',
    'title: "Cruise Report for `r cruise_name_check[1]` " ',
    'output: htm_document',
    '---',
    '',
    '## Cruise Report for `r cruise_name_check[1]`',
    '',
    '### Quick Cruise FastCAT Summary',
    '',
    'During the cruise `r cruise_name_check[1]` there were `r tows` tows with',
    'depth, temperature, and salinity collected. Tows were taken ',
    'from `r start_month` `r start_day` to `r end_month` `r end_day` of `r year`.',
    'Temperature ranged from `r temp_range_check[1]` to `r temp_range_check[2]`',
    'with a mean of `r temp_mean` Celcius. Salinity ranged from `r sal_range_check[1]`',
    'to `r sal_range_check[2]` with a mean of `r sal_mean` PSU. The deepest',
    'tow was `r depth_range_check[2]`. The Southern most tow was ',
    '`r lat_range_check[1]` and the furthest North was `r lat_range_check[2]`',
    'Latitude. The Western most tow was `r lon_range_check[1]` and the',
    'Eastern most was `r lon_range_check[2]` Longitude.',
    '',
    '### Average Salinity and Temperature Profiles',
    '',
    'Plot shows average salinity and temperature (point) and 95% Confidence',
    'Intervals for each integer of depth.',
    '',
    '```{r, echo = FALSE, message = FALSE, results = "hide", fig.keep = "all"}',
    'print(suppressWarnings(ts_plot))',
    '```',
    '### Station Map',
    '',
    '```{r, echo = FALSE, message = FALSE, results = "hide", fig.keep = "all"}',
    'print(suppressWarnings(fc_map))',
    '```',
    '',
    'If any of this looks suspect, check for the values in the .csv file and',
    'then correct in MasterCOD. After this is done, re-run the Perl script and',
    'then re-run FastrCAT.',
    '',
    '### Summary Statistics',
    '```{r, echo = FALSE}',
    'print(summary_fc)',
    '```',
    '**List of Cruise Names**',
    'If there is more than one Cruise name, please fix in MasterCOD.',
    '',
    '```{r, echo = FALSE}',
    'print(unique(cruise_data_all$CRUISE))',
    '```',
    '',
    '**List of Station.hauls**',
    '',
    '```{r, echo = FALSE}',
    'print(how_many_tows)',
    '```',
    '',
    '**List of FOCI grid names**',
    '',
    '```{r, echo = FALSE}',
    'print(foci_grid_name_check)',
    '```',
    '',
    '### Anomalous Salinity and Temperature',
    '',
    'These values for salinity and temperature are outside of known ranges.',
    'They have been filtered out of the data, because they are outside the',
    'know ranges for salinity and temperature. NA\'s refer to no outlier',
    'values found either below the minimum cutoff or above the maximum cutoff.',
    'If there is no data below, then all readings were within known ranges.',
    '```{r,echo = FALSE}',
    'print(bad_sal_temp)',
    '```',
    '### No Header Files',
    '',
    'These files have been identified as having no header information. It is most',
    'likely that you ran the Perl script prior to entering the station COD form',
    'into MasterCOD. Please re-run Perl script and then make_dataframe_fc()',
    'after you have checked that you enterd the COD form. If there is not',
    'anything below then all header information was present.',
    '```{r, echo = FALSE}',
    'print(No_head_files)',
    '```',
    '### No Data Files',
    '',
    'Below are files which were identified as having no data. Make sure that this',
    'is true. Since all casts are processed by the perl script and the',
    'make_dataframe_fc() regardless if they were Good, Questionable, or "Failure.',
    'If there is not a dataframe below then all files contained data.',
    '```{r, echo = FALSE}',
    'print(no_data_files)',
    '```')

# Render Cruise Report --------------------------------------------------------
      markdown::markdownToHTML(text = knitr::knit(text = cruise_report),
                               output = paste(current_path, paste(cruise_id,
                                        "Cruise_Report.html", sep = "_"),
                                        sep = "/"))
  }
# End of Cruise report^--------------------------------------------------------

# Changes date to character to avoid Excel date-time errors--------------------
  cruise_data_all$DATE <- as.character(cruise_data_all$DATE)

# Data to the global environment ----------------------------------------------
  if(GE == TRUE){
    fastcat_data <<- as.data.frame(dplyr::bind_rows(cruise_data))
  }

# Makes the file name ---------------------------------------------------------
  file_name <- paste(cruise_id, "_forEcoDAAT", sep = "", ".csv")

# Write data to folder ------------------------------------------------------
  readr::write_csv(cruise_data_all, file.path(current_path, file_name))


}

