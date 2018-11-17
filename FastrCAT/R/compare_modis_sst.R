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


#error Error in checkForRemoteErrors(val) :
#2 nodes produced errors; first error: second argument must be a list
#found issues but no comments. Need to post issue on github

#   server <- "https://modis.ornl.gov/rst/"  The MODIS api's
# end_point <- "api/v1/products"

#https://khufkens.github.io/MODISTools/

#MODIS Aqua Level 3 SST Thermal IR Daily 4km Daytime v2014.0
#https://modis.gsfc.nasa.gov/tools/index.php
