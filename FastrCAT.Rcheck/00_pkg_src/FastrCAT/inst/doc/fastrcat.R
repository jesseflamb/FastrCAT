## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE---------------------------------------------------------
htmltools::img(src = knitr::image_uri("mermaidcat.png"), 
               alt = 'mermaidcat', 
               style = 'position:absolute; top: 0; right:0; padding:5px;')

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
library(FastrCAT)

## ----eval=FALSE----------------------------------------------------------
#   make_dataframe_fc("path/ to directory/ of .up files")

## ----eval=FALSE----------------------------------------------------------
#   plot_ts_fc("path/ to directory/ of fastrcat dataframe")

## ----echo=FALSE, fig.cap= "Correct Output", fig.show='hold'--------------

knitr::include_graphics("AE10-01_Station_2_2.png")


## ----echo=FALSE, fig.cap= "Incorrect Output", fig.show='hold'------------

knitr::include_graphics("AE10-01_Station_41_2.png")


