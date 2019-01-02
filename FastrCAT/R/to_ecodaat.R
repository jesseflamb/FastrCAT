#' @title Prepare FastrCAT data for EcoDAAT
#'
#' @description Prepares data for entry into EcoDAAT. Prior to using this
#' function make sure that all issues brought up in the Cruise Report were
#' handled and that missing station information (haul information) has been
#' fixed using the fill_missing_stations() function. This prepares all
#' FastrCAT data files present in a folder and will bind them together as
#' one .csv file for import into EcoDAAT.
#' @param fc_data_path Path to the folder where all FastrCAT data files are
#' located.
#' @return a single .csv files


to_ecodaat <- function(fc_data_path){

# Find files ------------------------------------------------------------------

  list_fc_data <- list.files(path = fc_data_path, pattern = "\\EcoDAAT.csv$",
                              ignore.case = TRUE,
                              include.dirs = TRUE, full.names = TRUE)

# Read in the files------------------------------------------------------------

  fc_data <- list()

  for(i in 1:length(list_fc_data)){

  fc_data[[i]] <- readr::read_csv(list_fc_data[i],
                             col_types = readr::cols_only(
                                         LAT = readr::col_double(),
                                         LON = readr::col_double(),
                                         DATE = readr::col_date(),
                                         TIME = readr::col_character(),
                                         PRESSURE = readr::col_double(),
                                         DEPTH = readr::col_integer(),
                                         TEMPERATURE1 = readr::col_double(),
                                         CONDUCTIVITY1 = readr::col_double(),
                                         SALINITY1 = readr::col_double(),
                                         SIGMA_T = readr::col_double(),
                                         CRUISE = readr::col_character(),
                                         STATION_NAME = readr::col_integer(),
                                         HAUL_NAME = readr::col_integer(),
                                         FOCI_GRID = readr::col_character()))

  }

fc_data_all <- do.call("rbind", fc_data)%>%
  dplyr::select(LAT, LON, DATE, TIME, PRESSURE, DEPTH, TEMPERATURE1,
                CONDUCTIVITY1, SALINITY1, SIGMA_T, CRUISE, STATION_NAME,
                HAUL_NAME, FOCI_GRID) %>%
  #If header names change in the future, edit here: New name = Old name--------
  dplyr::rename(Latitude = LAT,
                Longitude = LON,
                Date = DATE,
                Time = TIME,
                Pressure = PRESSURE,
                Depth = DEPTH,
                Temperature = TEMPERATURE1,
                Conductivity = CONDUCTIVITY1,
                Salinity = SALINITY1,
                `Sigma-T` = SIGMA_T,
                Cruise = CRUISE,
                Station = STATION_NAME,
                Haul = HAUL_NAME,
                Grid = FOCI_GRID)

#Make read me for file, summary------------------------------------------------

read_me <- c(
  '---',
  'title: "Summary for `r Sys.Date()` fastcat_data_EcoDAAT_ready" ',
  'output: htm_document',
  '---',
  '',
  '## Cruise Report for `r cruise_name_check[1]`',
  '',
  '### Quick Cruise FastCAT Summary'
)

# write to file----------------------------------------------------------------

new_fc_data_path <- paste(fc_data_path, "/", "fastcat_data_EcoDAAT_ready_",
                          Sys.Date(), ".csv", sep = "")

readr::write_csv(fc_data_all, new_fc_data_path)

# Render Read me --------------------------------------------------------------
markdown::markdownToHTML(text = knitr::knit(text = read_me),
                         output = paste(fc_data_path,
                                        paste("ReadMe", Sys.Date(),
                                        ".html", sep = "_"),
                                        sep = "/"))

}
