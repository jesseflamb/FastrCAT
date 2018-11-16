library(MODISTools)


df <- data.frame("site_name" = paste("test",1:2))
df$lat <- 40
df$lon <- -110

# test batch download
subsets <- mt_batch_subset(df = df,
                           product = "MOD11A2",
                           band = "LST_Day_1km",
                           internal = TRUE,
                           start = "2004-01-01",
                           end = "2004-02-01")




#   server <- "https://modis.ornl.gov/rst/"  The MODIS api's
# end_point <- "api/v1/products"

#https://khufkens.github.io/MODISTools/

#MODIS Aqua Level 3 SST Thermal IR Daily 4km Daytime v2014.0
