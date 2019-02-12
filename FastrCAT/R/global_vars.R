#' @docType package
#' @name FastrCAT

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "CI_5", "CI_95",
                                                        "CONDUCTIVITY1",
                                                        "CRUISE", "DATE",
                                                        "DEPTH", "DEPTH_BOTTOM",
                                                        "DIRECTORY", "FLAG",
                                                        "FOCI_GRID", "Flag",
                                                        "HAUL_NAME",
                                                        "INSTRUMENT", "LAT",
                                                        "LON", "MEAN",
                                                        "MEASURMENT",
                                                        "PRESSURE",
                                                        "SALINITY1", "SIGMA_T",
                                                        "STATION_HAUL",
                                                        "STATION_NAME",
                                                        "Station_haul",
                                                        "TEMPERATURE1",
                                                        "TIME", "TYPE",
                                                        "Uniq_ID", "depth",
                                                        "mean_yr",
                                                        "year(DATE)",
                                                        'lubridate::year(DATE)',
                                                        "SAMPLE_POINT", "VAR",
                                                        "YEAR"))
