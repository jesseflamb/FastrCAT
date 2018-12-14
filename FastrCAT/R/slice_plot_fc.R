
# option "Line 8"--------------------------------------------------------------
# Line 8 is a defined box in the Gulf of Alaska
# LAT (min = 57.52417, max = 57.71517)
# LON (min = -155.2673, max = -154.7725)

# option "Line 8 FOX"----------------------------------------------------------
# Refine Line 8 to include only FOX57-FOX60 which are the regularly sampled
# deeper stations.
# LAT (min = 57.52417, max = 57.71517)
# LON (min = -155.2, max = -154.85)

# option "Semidi"--------------------------------------------------------------
# The Semidi box is for late summer midwater trawl surveys. This is a polygon
# c(-159.7,-158.3,-155,-156.3),c(55.9,54.7,56.1,57.2)

slice_plot <- function(fastcat_data, plot_type, slice){

# Get cruise name -------------------------------------------------------------
  cruise_name <- unique(fastcat_data$CRUISE)
# Which: temperature of salinity ----------------------------------------------

  measurment <- if(plot_type == "temperature"){
    "TEMPERATURE1"
  } else if(plot_type == "salinity"){
    "SALINITY1"
  } else{
    warning("For plot_type argument please use temperature or salinity")
  }


# Which of the Gulf of Alaska regions that are sampled frequently--------------
  range_filter <- if(slice == "Line 8"){
    fastcat_data %>%
    filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
    filter(LON >= -155.2673 & LON <= -154.7725)
  } else if (slice == "Line 8 FOX"){
    fastcat_data %>%
      filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
      filter(LON >= -155.2 & LON <= -154.85)
  } else if(slice == "Semidi"){
    fastcat_data %>%
      filter(LAT >= 54.7 & LAT <= 57.2)%>%
      filter(LON >= -159.7 & LON <= -155.0)
  }


# Set the latittude range -----------------------------------------------------
x.range <- as.numeric(c(min(range_filter$LAT, na.rm = TRUE),
                        max(range_filter$LAT, na.rm = TRUE)))

# Set the depth range ---------------------------------------------------------
y.range <- as.numeric(c(min(range_filter$DEPTH, na.rm = TRUE),
                        max(range_filter$DEPTH, na.rm = TRUE)))


# create gridded values to interpolte from ------------------------------------
idw_grd <- expand.grid(
                LAT = seq(from = x.range[1], to = x.range[2], by = 0.005),
                DEPTH = seq(from = y.range[1], to = y.range[2], by = 1))

# points for polygon to exlude area of rectangle where there are no data
# points ----------------------------------------------------------------------
point_vals_bot <- range_filter%>%
            group_by(LAT)%>%
            summarise(DEPTH = max(DEPTH, na.rm = TRUE))%>%
            arrange(desc(LAT))

point_vals_top <- range_filter%>%
          group_by(LAT)%>%
          summarise(DEPTH = min(DEPTH, na.rm = TRUE))%>%
          arrange(LAT)

in_poly <- data.frame(in_poly = sp::point.in.polygon(idw_grd$LAT,
                                                     idw_grd$DEPTH,
                             c(point_vals_top$LAT,point_vals_bot$LAT),
                             c(point_vals_top$DEPTH,point_vals_bot$DEPTH)))

# remove the negative space where there are no data points --------------------
idw_grd <- idw_grd %>% bind_cols(in_poly) %>% filter(in_poly != 0)

# interpolated using a Shepard with neighbours: A maximum ammount of N
# neighbours is allowed to the weight calculation following Shepard method.----

idw_mod <- phylin::idw(as.vector(as.matrix(range_filter[,c(measurment)])),
                         range_filter[,c("LAT","DEPTH")],
                         grid = idw_grd, method = "Neighbours",
                         p = 2, # Power to use in weight calculation
                         N = 15) # max number of neighbors

idw_grd <- idw_grd %>% bind_cols(idw_mod)


# color palletes for salinity and temperature are derived from the oce
# package. These colors are standardized for oceanographic plots.--------------
plot_color <- if(plot_type == "temperature"){

  c("#0B222E", "#062C46", "#013565", "#1C3983", "#3C3E87", "#524685",
    "#664D83", "#785383", "#8C5A82", "#A05F7F", "#B66478", "#CC686D",
    "#E0705D", "#EF7B4C", "#F78C41", "#FAA13D", "#F9B642", "#F5CD4D",
    "#EFE35B", "#E5FA6A")

}else if(plot_type == "salinity"){

  c("#2B1470", "#2C1D8A", "#212F96", "#114293", "#08518F", "#0E5E8B",
    "#1A6989", "#267488", "#318088", "#3A8B88", "#439787", "#4BA385",
    "#56AF81", "#64BA7B", "#77C574", "#91CF6C", "#B0D66C", "#CCDE78",
    "#E6E58A", "#FEEEA0")

}

legend_name <- if(plot_type == "temperature"){
  expression(bold( ~degree~C ))
} else if (plot_type == "salinity"){
  expression(bold("PSU"))
}

ggplot()+
  geom_raster(aes(LAT, -(DEPTH), fill = Z), data = idw_grd)+
  geom_ribbon(aes(x = LAT, ymin = -(max(DEPTH_BOTTOM, na.rm = TRUE)+10),
                  ymax = -(DEPTH_BOTTOM)), data = range_filter)+
  geom_point(aes(LAT,-(DEPTH)), shape = 21, size = 1, color = "orange", data = point_vals_bot)+
  geom_point(aes(LAT,-(DEPTH)), shape = 21, size = 1, color = "orange", data = point_vals_top)+
  scale_fill_gradientn(colors = plot_color, name = legend_name)+
  theme_bw()+
  ylab(label = "Depth m")+
  xlab(label = "Latitude")+
  ggtitle(label = cruise_name, subtitle = slice)

}
