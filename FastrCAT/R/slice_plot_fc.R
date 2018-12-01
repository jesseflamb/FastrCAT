
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


range_filter <- fastcat_data %>%
  filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
  filter(LON >= -155.2673 & LON <= -154.7725)



# Latittude
x.range <- as.numeric(c(min(range_filter$LAT, na.rm = TRUE),
                        max(range_filter$LAT, na.rm = TRUE)))
# depth
y.range <- as.numeric(c(min(range_filter$DEPTH, na.rm = TRUE),
                        max(range_filter$DEPTH, na.rm = TRUE)))

idw_grd <- expand.grid(
                LAT = seq(from = x.range[1], to = x.range[2], by = 0.005),
                DEPTH = seq(from = y.range[1], to = y.range[2], by = 1))

point_vals_bot <- range_filter%>%
            group_by(LAT)%>%
            summarise(DEPTH = max(DEPTH, na.rm = TRUE))%>%
            arrange(desc(LAT))

point_vals_top <- range_filter%>%
          group_by(LAT)%>%
          summarise(DEPTH = min(DEPTH, na.rm = TRUE))%>%
          arrange(LAT)




in_poly <- data.frame(in_poly = sp::point.in.polygon(idw_grd$LAT, idw_grd$DEPTH,
                             c(point_vals_top$LAT,point_vals_bot$LAT),
                             c(point_vals_top$DEPTH,point_vals_bot$DEPTH)))

idw_grd <- idw_grd %>% bind_cols(in_poly)

idw_grd <- idw_grd %>% filter(in_poly != 0)

idw_mod <- phylin::idw(range_filter[,c("TEMPERATURE1")],
                       range_filter[,c("LAT","DEPTH")], grid = idw_grd, method = "Shepard")

idw_grd <- idw_grd %>% bind_cols(idw_mod)



ggplot()+
  geom_raster(aes(LAT, -(DEPTH), fill = Z), data = idw_grd)+
  geom_ribbon(aes(x = LAT, ymin = -(max(DEPTH_BOTTOM, na.rm = TRUE)+10),
                  ymax = -(DEPTH_BOTTOM)), data = range_filter)+
  #geom_point(aes(LAT, -(DEPTH)), shape = 0, data = range_filter)+
  scale_fill_gradientn(colors = c("#0B222E","#01335C","#2D3B87","#564784",
                                  "#765383","#985D80","#BF6574","#E2705C","#F68842",
                                   "#FAAC3F","#F4D250","#E5FA6A"))+
  theme_bw()
