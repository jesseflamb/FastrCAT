station_raw <- sf::st_as_sf(MAP_IDW, coords = c("LON","LAT"))

#%>%sf::st_crs(.)

station_deg <- sf::st_transform(station_raw, WGS84, use_gdal = FALSE)


station_buffer <- sf::st_buffer(station_raw, 20)
