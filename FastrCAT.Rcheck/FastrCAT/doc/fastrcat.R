## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE---------------------------------------------------------
htmltools::img(src = knitr::image_uri("hex_sticker_color.png"), 
               alt = 'logo', 
               style = 'position:absolute; top:0; right:0; padding:10px;')

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
library(FastrCAT)

## ----eval=FALSE----------------------------------------------------------
#   make_dataframe_fc("path/ to directory/ of .up files")

## ---- echo = FALSE-------------------------------------------------------
example_file <- read.csv("DY15-05_forEcoDAAT.csv")
head(example_file[,c(1:7)],4)

## ---- echo = FALSE-------------------------------------------------------
colnames(example_file)

## ---- echo = FALSE, results = 'asis'-------------------------------------
knitr::asis_output(htmltools::includeHTML("DY16-06_Cruise_Report.html"))

## ----eval=FALSE----------------------------------------------------------
#   plot_ts_fc("path/ to directory/ of fastrcat dataframe")

## ----echo=FALSE, fig.cap= "Correct Output", fig.show='hold'--------------

knitr::include_graphics("AE10-01_Station_2_2.png")


## ----echo=FALSE, fig.cap= "Incorrect Output", fig.show='hold'------------

knitr::include_graphics("AE10-01_Station_41_2.png")


## ----eval=FALSE----------------------------------------------------------
#   map_fc("path/ to directory/ of fastrcat dataframe", map_type = "Sample Intensity")

## ----echo=FALSE, fig.cap= "Sample Intensity Map", fig.show='hold'--------

knitr::include_graphics("DY1705_Sample_Intensity.png")


## ---- eval = FALSE-------------------------------------------------------
#  map_fc("path/ to directory/ of fastrcat dataframe", map_type = "Temperature", depth_range = c(5,10))
#  

## ----echo=FALSE, fig.cap= "Temperature Map", fig.show='hold'-------------

knitr::include_graphics("DY15-05_Temperature_5_10.png")


## ---- eval= FALSE--------------------------------------------------------
#  fill_missing_stations("path/ to directory/ of fastrcat dataframe",
#                        "path/ to directory/ of haul records dataframe")
#  

## ---- eval= FALSE--------------------------------------------------------
#  to_ecodaat("path/ to directory/of current years fastcat/ dataframes")
#  

## ---- eval= FALSE--------------------------------------------------------
#  plot_time_series(hist_data = "path/ to directory/of historical .csv",
#                   fastcat_data = "path to/ current years/fastcat .csv",
#                   core_stations = "Line 8",
#                   plot_type = "Temperature")
#  

