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
#' @return .csv file of all .up files. A .txt file of warnings and one
#' of summary statistics.



make_dataframe_fc <- function(current_path,GE = FALSE){

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
      lat_id <- if(is.na(stringr::str_trim(unlist(stringr::strsplit(
                    full_table[grep("@ Latitude?",
                    full_table,ignore.case = TRUE)], ":")))[2])){
        lat_id <- as.double(NA)
        }else{
        lat_id <-stringr::str_trim(unlist(stringr::strsplit(
                                   full_table[grep("@ Latitude?", full_table,
                                   ignore.case = TRUE)], ":"))[2],
                                   side = "both")}

# Get longitude ---------------------------------------------------------------
      long_id <- if(is.na(stringr::str_trim(unlist(stringr::strsplit(
                    full_table[grep("@ Longitude?", full_table,
                    ignore.case = TRUE)], ":")))[2])){
        long_id <- as.double(NA)
      }else{
        long_id <- stringr::str_trim(unlist(strsplit(
                                     full_table[grep("@ Longitude?",
                                     full_table,ignore.case = TRUE)],
                                     ":"))[2], side = "both")}

# Get the date ----------------------------------------------------------------
      date_id <- if(is.na(stringr::str_trim(unlist(stringr::strsplit(
                    full_table[grep("@ Date?", full_table,ignore.case = TRUE)],
                    ":")))[2])){
        date_id <- as.character(NA) #If the date is missing subs an NA.
      }else{
        date_id <- stringr::str_trim(unlist(stringr::strsplit(
                            full_table[grep("@ Date?", full_table,
                            ignore.case = TRUE)], ":"))[2], side = "both")}

# Get the time ----------------------------------------------------------------
      time_id <-  if(is.na(stringr::str_trim(unlist(stringr::strsplit(
                    full_table[grep("@ Cast?", full_table,ignore.case = TRUE)],
                    ":")))[2])){
        time_id <- as.character(NA)
      }else{
        time_id <- strftime(as.POSIXct(paste(date_id,
                            stringr::str_pad(stringr::str_trim(unlist(
                            stringr::strsplit(full_table[grep("@ Cast?",
                            full_table, ignore.case = TRUE)],":"))[2],
                            side = "both"), width = 4, side = "left", pad = "0"),
                            sep = "-"), format = "%d%b%y-%H%M", tz = "UTC"),
                            format = "%H:%M:%S", tz = "UTC")}

# Get the cruise name ---------------------------------------------------------
      cruise_id <- if(is.na(stringr::str_trim(unlist(stringr::strsplit(
                      full_table[grep("@ Cruise?",
                      full_table,ignore.case = TRUE)], ":")))[2])){
        cruise_id <- as.character(NA)#If Cruise is missing subs an NA
      }else{
        cruise_id <- toupper(stringr::str_trim(unlist(stringr::strsplit(
                             full_table[grep("@ Cruise?", full_table,
                             ignore.case = TRUE)], ":"))[2], side = "both"))}

# Get the station name --------------------------------------------------------
      station_id_p <- if(length(full_table[grep("@ Station?",
                         full_table,ignore.case = TRUE)])!=0){
        station_id_p <- stringr::str_trim(unlist(stringr::strsplit(
                                 full_table[grep("@ Station?",
                                 full_table,ignore.case = TRUE)],
                                 ":"))[2], side = "both")
        }else{
          station_id_p <- as.integer(NA)}
# Get the haul name -----------------------------------------------------------

      haul_id_p <- if(length(full_table[grep("@ Haul?",
                      full_table, ignore.case = TRUE)]) != 0 ){
        haul_id <- stringr::str_trim(unlist(stringr::strsplit(
                            full_table[grep("@ Haul?", full_table,
                            ignore.case = TRUE)], ":"))[2], side = "both")
        }else{
        haul_id <- as.integer(NA) }

# Get foci grid name ----------------------------------------------------------
      grid_id <- if(length(full_table[grep("@ Grid?",
                    full_table,ignore.case = TRUE)]) == 0){
        grid_id <- as.character(NA)
      }else if (is.na(stringr::str_trim(unlist(stringr::strsplit(
                full_table[grep("@ Grid?", full_table,ignore.case = TRUE)],
                ":")))[2])){
        grid_id <- as.character(NA)
      }else if(stringr::str_detect(stringr::str_trim(unlist(stringr::strsplit(
               full_table[grep("@ Grid?", full_table,ignore.case = TRUE)],
               ":"))[2], side = "both"), "BON\\") == TRUE){
        grid_id <- as.character(NA)
      }else {
        grid_id <- toupper(stringr::str_trim(unlist(stringr::strsplit(
                           full_table[grep("@ Grid?",
                           full_table,ignore.case = TRUE)], ":"))[2],
                           side = "both"))}

# Fixes Station name if it is actually foci grid-------------------------------
      station_id <- if(is.na(suppressWarnings(
                       as.numeric(station_id_p))) == TRUE){
        station_id <- as.integer(NA)
      }else if(stringr::str_detect(station_id_p,"[.]") == TRUE){
        station_id <- unlist(stringr::strsplit(station_id_p,"[.]"))[1]
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
        haul_id <- unlist(stringr::strsplit(station_id_p, "[.]"))[2]
      }else if(is.na(haul_id_p) == TRUE){
        haul_id <- as.integer(NA)
      }else{
        haul_id <- haul_id_p
      }

# Get the depth ---------------------------------------------------------------
      depth_id <- if(length(full_table[grep("@ Water?", full_table,
                     ignore.case = TRUE)]) != 0 ){
        depth_id <- stringr::str_trim(unlist(stringr::strsplit(
                             full_table[grep("@ Water?", full_table,
                             ignore.case = TRUE)], ":"))[2], side = "both")
      }else{
        depth_id <- as.integer(NA) }

# Get instrument type ---------------------------------------------------------
      instrument_id <- if(length(full_table[grep("@ Instrument?", full_table,
                          ignore.case = TRUE)]) != 0){
        instrument_id <- stringr::str_trim(unlist(stringr::strsplit(
                                  full_table[grep("@ Instrument??", full_table,
                                  ignore.case = TRUE)], ":"))[2], side = "both")
      }else{
        instrument_id <- as.character(NA)
      }

# Reformat latitude and longitude to prep for conversion ----------------------
      lat_id <- paste(unlist(stringr::strsplit(lat_id, " "))[1],
                      unlist(stringr::strsplit(lat_id, " "))[2], sep = " ")

      long_id <- paste(unlist(stringr::strsplit(long_id, " "))[1],
                       unlist(stringr::strsplit(long_id, " "))[2], sep = " ")
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

            name0 <- unlist(stringr::strsplit(
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
      dplyr::filter(SALINITY1 < 0.05  & SALINITY1 > 38)%>%
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
      dplyr::filter(TEMPERATURE1 < -3 & TEMPERATURE1 > 20)%>%
      dplyr::group_by(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID)%>%
      dplyr::summarise(Min_value = ifelse(min(TEMPERATURE1, na.rm = TRUE) < -3,
                                          min(TEMPERATURE1, na.rm = TRUE), NA),
                       Max_value = ifelse(max(TEMPERATURE1, na.rm = TRUE) > 20,
                                          max(TEMPERATURE1, na.rm = TRUE), NA))%>%
      dplyr::mutate(ERROR_TYPE = "Temperature")

# Bind together bad salinity and temperature for error table ------------------

    bad_sal_temp <- bad_temperature %>% dplyr::bind_rows(bad_salinity)

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
  no_data_files <- unlist(no_data_files)
  No_head_files <- unlist(no_head_files)

  problem_files <- append(no_data_files, no_head_files)

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


# Make cruise summary and summary stats text ----------------------------------
  summary_fc <- summary(cruise_data_all %>%
                        select(LAT, LON, DEPTH_BOTTOM, DEPTH, PRESSURE,
                               TEMPERATURE1, SALINITY1,  CONDUCTIVITY1, SIGMA_T))


  how_many_tows <- unique(paste(cruise_data_all$STATION_NAME,
                                cruise_data_all$HAUL_NAME, sep = "."))

  tows <- if(length(how_many_tows) < 2){
      tows <- length(unique(cruise_data_all$FOCI_GRID))
    } else {
      tows <- length(how_many_tows)
    }

# Create variables for summary text -------------------------------------------
  cruise_name_check <- unique(cruise_data_all$CRUISE)

  station_name_num <- length(unique(cruise_data_all$STATION_NAME))

  foci_grid_name_check <- unique(cruise_data_all$FOCI_GRID)

  haul_name_check <- unique(cruise_data_all$HAUL_NAME)

  time_range_check <- range(cruise_data_all$DATE, na.rm = TRUE)

  temp_range_check <- round(range(cruise_data_all$TEMPERATURE1, na.rm = TRUE),
                            digits = 2)

  temp_mean <- round(mean(cruise_data_all$TEMPERATURE1, na.rm = TRUE),
                     digits = 2)

  sal_range_check <- round(range(cruise_data_all$SALINITY1, na.rm = TRUE),
                           digits = 2)

  sal_mean <- round(mean(cruise_data_all$SALINITY1, na.rm = TRUE), digits = 2)

  depth_range_check <- round(max(cruise_data_all$DEPTH, na.rm = TRUE),
                             digits = 2)
  lat_range_check <- round(range(cruise_data_all$LAT, na.rm = TRUE),
                           digits = 3)

  lon_range_check <- round(range(cruise_data_all$LON, na.rm = TRUE),
                           digits = 3)

  cruise_summary <- paste(
        "Quick Cruise FastCat Summary.\n\n", "During the cruise, ",
         cruise_name_check[1], ", there were ", tows,
         " tows with depth, temperature, and salinity collected.",
         " Tows were taken during the time period from ",
        time_range_check[1], " to ", time_range_check[2], ". ",
        "Temperature ranged from ", temp_range_check[1], " to ",
        temp_range_check[2], " with a mean of ", temp_mean, "C. ",
        "Salinity ranged from ", sal_range_check[1], " to ",
        sal_range_check[2], " with a mean of ", sal_mean, " PSU. ",
        "The deepest tow was ", depth_range_check, " meters.",
        " The Southern most bongo tow was ", lat_range_check[1],
        " and the furthest North was ", lat_range_check[2],
        " Latitude. The Western most bongo tow was ", lon_range_check[1],
        " and the Eastern most was ", lon_range_check[2], " Longitude.",
        "\n\nIf any of this looks suspect,",
        "check for the values in the .csv file and then correct in MasterCOD.",
        " After this is done, re-run the Perl script and then re-run FastrCAT.\n\n",
        "SUMMARY\n\n", "CRUISE_NAMES\n",
        str_c(cruise_name_check, collapse = ", "),
        "\n\nSTATION_HAUL_NAMES\n",
        str_c(str_replace_na(how_many_tows), collapse = ", "),
        "\n\nFOCI_GRID_NAMES\n",
        str_c(str_replace_na(unique(cruise_data_all$FOCI_GRID)),
              collapse = ", "), "\n\nBOTTOM_DEPTHS\n",
        str_c(str_replace_na(summary_fc[-c(2,5),3]), collapse = "\n"),
        "\n\nTOW_DEPTHS\n",
        str_c(str_replace_na(summary_fc[-c(2,5),4]), collapse = "\n"),
        "\n\nPRESSURE\n",
        str_c(str_replace_na(summary_fc[-c(2,5),5]), collapse = "\n"),
        "\n\nTEMPERATURE\n",
        str_c(str_replace_na(summary_fc[-c(2,5),6]), collapse = "\n"),
        "\n\nSALINITY\n",
        str_c(str_replace_na(summary_fc[-c(2,5),7]), collapse = "\n"),
        "\n\nCONDUCTIVITY\n",
        str_c(str_replace_na(summary_fc[-c(2,5),8]), collapse = "\n"),
        "\n\nSIGMA_T\n",
        str_c(str_replace_na(summary_fc[-c(2,5),9]), collapse = "\n"),
        sep = "")

# Changes date to character to avoid Excel date-time errors--------------------
  cruise_data_all$DATE %<>% as.character()

# Data to the global environment ----------------------------------------------
  if(GE == TRUE){
    fastcat_data <<- as.data.frame(dplyr::bind_rows(cruise_data))
  }

# Makes the file name ---------------------------------------------------------
  file_name <- paste(cruise_id, "_forEcoDAAT", sep = "", ".csv")

# Writes files to folder ------------------------------------------------------
  readr::write_csv(cruise_data_all, file.path(current_path, file_name))
  write.table(problem_files, file = paste(current_path, paste(cruise_id,
                                          "warnings.txt", sep = "_")
                                         , sep = "/"), eol = "\n\n")
  write.table(cruise_summary, file = paste(current_path,
                                           paste(cruise_id,"cruise_summary.txt",
                                           sep = "_"), sep = "/"), eol = "\n\n")
}

