#' @title Fill in missing station, foci grid, and haul names.
#'
#' @description Takes the .csv file created with make_dataframe_fc() and
#' haul records queried from EcoDAAT and finds and replaces missing
#' station names, foci grid names, and haul numbers in the dataframe. Make
#' sure that haul records are available for cruise. This is part of the
#' quality/control process prior to the data being ready for EcoDAAT.
#' @param path_fc The path to the directory where the .csv file
#' generated from make_dataframe_fc() is located.
#' @param path_haul_records The path to the directory where the .csv file
#' of haul records queried from EcoDAAT is located.
#' @return a dataframe in .csv format where missing station, foci grid, and
#' hauls names have been corrected.
#' @importFrom magrittr %>%


fill_missing_stations <- function(path_fc, path_haul_records){

# get haul records file queried from EcoDAAT-----------------------------------
# Eventually this will an API to EcoDAAT.
replace_station <- readr::read_csv(path_haul_records,
                            col_types = readr::cols(
                              ALT_STATION_NAME = readr::col_character(),
                              CRUISE = readr::col_character(),
                              CRUISE_ID = readr::col_integer(),
                              FOCI_GRID = readr::col_character(),
                              GEAR_NAME = readr::col_character(),
                              GEOGRAPHIC_AREA = readr::col_character(),
                              GMT_DATE_TIME_TXT = readr::col_datetime(),
                              HAUL_ID = readr::col_character(),
                              HAUL_NAME = readr::col_integer(),
                              STATION_NAME = readr::col_character()))%>%
  dplyr::select(CRUISE,STATION_NAME,FOCI_GRID,HAUL_NAME)%>%
  tidyr::unite(Uniq_ID,CRUISE,STATION_NAME,FOCI_GRID,HAUL_NAME, sep = "_",
               remove = FALSE)%>%
  dplyr::distinct(Uniq_ID, .keep_all = TRUE)

# get file created by make_dataframe_fc()--------------------------------------
ALL_BON <- readr::read_csv(path_fc,
                           col_types = readr::cols(
                             CRUISE = readr::col_character(),
                             STATION_NAME = readr::col_integer(),
                             HAUL_NAME = readr::col_integer(),
                             FOCI_GRID = readr::col_character(),
                             DATE = readr::col_date(),
                             TIME = readr::col_time(),
                             LAT = readr::col_double(),
                             LON = readr::col_double(),
                             DEPTH_BOTTOM = readr::col_integer(),
                             DEPTH = readr::col_integer(),
                             PRESSURE = readr::col_double(),
                             TEMPERATURE1 = readr::col_double(),
                             SALINITY1 = readr::col_double(),
                             CONDUCTIVITY1 = readr::col_double(),
                             SIGMA_T = readr::col_number(),
                             INSTRUMENT = readr::col_character(),
                             DIRECTORY = readr::col_character(),
                             CONDUCTIVITY2 = readr::col_character(),
                             FLOUROMETER = readr::col_character(),
                             OXYGEN1 = readr::col_character(),
                             SALINITY2 = readr::col_character(),
                             TEMPERATURE2 = readr::col_character(),
                             TRANS = readr::col_character(),
                             COMMENTS_SEACAT_CTD = readr::col_character(),
                             GEAR_NAME = readr::col_character(),
                             NET = readr::col_integer()))


# Add missing foci grid and station names--------------------------------------
for(i in 1:nrow(ALL_BON)){

  get_grid_name <- replace_station %>%
    dplyr::filter(CRUISE == ALL_BON$CRUISE[i])%>%
    dplyr::filter(STATION_NAME == ALL_BON$STATION_NAME[i])%>%
    dplyr::collect %$% as.vector(FOCI_GRID)
  get_grid_name <- ifelse(length(get_grid_name) == 0, NA, get_grid_name)

  get_station_name <- replace_station%>%
    dplyr::filter(CRUISE == ALL_BON$CRUISE[i])%>%
    dplyr::filter(FOCI_GRID == ALL_BON$FOCI_GRID[i])%>%
    dplyr::collect %$% as.vector(STATION_NAME)
  get_station_name <- ifelse(length(get_station_name) == 0, NA,
                             get_station_name)

  if(is.na(ALL_BON$FOCI_GRID[i])){
    ALL_BON$FOCI_GRID[i] <- get_grid_name }

  if(is.na(ALL_BON$STATION_NAME[i])){
    ALL_BON$STATION_NAME[i] <- get_station_name
  }
}

# Add missing haul names ------------------------------------------------------
for(n in 1:nrow(ALL_BON)){
  get_haul_name <- replace_station %>%
    dplyr::filter(CRUISE == ALL_BON$CRUISE[n])%>%
    dplyr::filter(STATION_NAME ==
             ALL_BON$STATION_NAME[n] | FOCI_GRID == ALL_BON$FOCI_GRID[n])%>%
    dplyr::collect %$% as.vector(HAUL_NAME)

  get_haul_name <- ifelse(length(get_haul_name) == 0, NA, get_haul_name)

  if(is.na(ALL_BON$HAUL_NAME[n])){
    ALL_BON$HAUL_NAME[n] <- get_haul_name
  }
}


file_name <- paste(unique(ALL_BON$CRUISE),"_corrected_haul_rcds" , ".csv", sep = "")

readr::write_csv(ALL_BON, file_name)



}
