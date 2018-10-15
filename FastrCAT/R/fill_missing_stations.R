#' @title Fill in missing station, foci grid, and haul names
#'
#' @description Takes the .csv file created with make_dataframe_fc() and
#' a haul records queried from EcoDAAT and finds and replaces missing
#' station names, foci grid names, and haul numbers in the dataframe.
#' @param path_fc The path to the directory where the .csv file
#' generated from make_dataframe_fc() is located.
#' @param path_haul_records The path to the directory where the .csv file
#' of haul records queried from EcoDAAT is located.
#' @return a dataframe in .csv format where missing station, foci grid, and
#' hauls names have been corrected.


fill_missing_stations <- function(path_fc, path_haul_rcds){

# get haul records file queried from EcoDAAT-----------------------------------
# Eventually this will an API to EcoDAAT.
replace_station <- readr::read_csv(path_haul_rcds,
                            col_types = cols(
                              ALT_STATION_NAME = col_character(),
                              CRUISE = col_character(),
                              CRUISE_ID = col_integer(),
                              FOCI_GRID = col_character(),
                              GEAR_NAME = col_character(),
                              GEOGRAPHIC_AREA = col_character(),
                              GMT_DATE_TIME_TXT = col_datetime(),
                              HAUL_ID = col_character(),
                              HAUL_NAME = col_integer(),
                              STATION_NAME = col_character()))%>%
  dplyr::select(CRUISE,STATION_NAME,FOCI_GRID,HAUL_NAME)%>%
  dplyr::unite(Uniq_ID,CRUISE,STATION_NAME,FOCI_GRID,HAUL_NAME, sep = "_",
               remove = FALSE)%>%
  dplyr::distinct(Uniq_ID, .keep_all = TRUE)

# get file created by make_dataframe_fc()--------------------------------------
ALL_BON <- readr::read_csv(path_fc,
                           col_types = cols(CRUISE = col_character(),
                                            STATION_NAME = col_integer(),
                                            HAUL_NAME = col_integer(),
                                            FOCI_GRID = col_character(),
                                            DATE = col_date(),
                                            TIME = col_time(),
                                            LAT = col_double(),
                                            LON = col_double(),
                                            DEPTH_BOTTOM = col_integer(),
                                            DEPTH = col_integer(),
                                            PRESSURE = col_double(),
                                            TEMPERATURE1 = col_double(),
                                            SALINITY1 = col_double(),
                                            CONDUCTIVITY1 = col_double(),
                                            SIGMA_T = col_number(),
                                            INSTRUMENT = col_character(),
                                            DIRECTORY = col_character(),
                                            CONDUCTIVITY2 = col_character(),
                                            FLOUROMETER = col_character(),
                                            OXYGEN1 = col_character(),
                                            SALINITY2 = col_character(),
                                            TEMPERATURE2 = col_character(),
                                            TRANS = col_character(),
                                            COMMENTS_SEACAT_CTD = col_character(),
                                            GEAR_NAME = col_character(),
                                            NET = col_integer()))


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


file_name <- paste(unique(ALL_BON$CRUISE),"_add_haul_rcds" , ".csv", sep = "")

write_csv(ALL_BON, file_name)



}
