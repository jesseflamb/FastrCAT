station_raw <- sf::st_as_sf(MAP_IDW, coords = c("LON","LAT"))

station_buffer <- sf::st_buffer(station_raw, .25)
plot(station_buffer)

convert <- sf::st_as_sf(station_buffer)

IDW_raster_crop <- raster::mask(IDW_raster, convert)

plot(IDW_raster_crop)

Raster_crop_pt <- raster::rasterToPoints(IDW_raster_crop, spatial = TRUE)

Out_crop <- cbind(Raster_crop_pt@coords, Raster_crop_pt@data)

names(Out_crop) <- c("LON","LAT","TEMPERATURE1")

Out_crop

plot(Out_crop)

######

# Convert station/point data to spatial object---------------------------------
WGS84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

MAP_IDW_SPAT <- sp::SpatialPointsDataFrame(coords = MAP_IDW[,c("LON","LAT")],
                                           data = MAP_IDW,
                                           proj4string = WGS84)

# Make grid for idw by 0.25 degrees and convert to spatial object--------------
x.range <- as.integer(c(min(MAP_IDW$LON, na.rm = TRUE)-1,
                        max(MAP_IDW$LON, na.rm = TRUE)+1))

y.range <- as.integer(c(min(MAP_IDW$LAT, na.rm = TRUE)-1,
                        max(MAP_IDW$LAT, na.rm = TRUE)+1))

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.25),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.25))

sp::coordinates(grd) <- ~ x + y

sp::gridded(grd) <- TRUE

sp::proj4string(grd) <- WGS84

# inverse distance weighting for variable--------------------------------------
idw <- gstat::idw(formula = MAP_IDW$TEMPERATURE1 ~ 1, locations = MAP_IDW_SPAT,
                  newdata = grd, idp = 3)

idw.output <- as.data.frame(idw)
names(idw.output)[1:3] <- c("LON","LAT","TEMPERATURE1")

# Makes a .35 degree buffer around each station--------------------------------
IDW_buff_WGS84 <- sf::st_as_sf(MAP_IDW, coords = c("LON","LAT"))%>%
  sf::st_buffer(., .35)%>%
  sf::st_as_sf(.)

# Converts the idw dataframe to a raster and masks non-buffer area-------------
IDW_raster <- raster::rasterFromXYZ(idw.output[,1:3],crs = WGS84)%>%
  raster::mask(., IDW_buff_WGS84)%>%
  raster::rasterToPoints(., spatial = TRUE)

Out_crop <- cbind(IDW_raster@coords, IDW_raster@data)

names(Out_crop) <- c("LON","LAT","TEMPERATURE1")

map_data <- Out_crop
