#' @title Compare Temperature and Salinity to GAK1 line



library(dataone)

# node object for production level dataone environment-------------------------
cn <- dataone::CNode("PROD")

# The Reasearch Workspace node is the location of the GAK1 line data-----------
mn <- dataone::getMNode(cn, "urn:node:RW")
# General ---------------------------------------------------------------------
# Cited Identifier: https://doi.org/10.24431/rw1k1b
# Abstract
# This data is part of the Gulf Watch Alaska (GWA), Environmental Drivers
# component of the Exxon Valdez Oil Spill Trustee Council, project numbers
# 12120114P, 13120114P, 14120114P, 15120114P, 16120114P. Gulf Watch Alaska is
# the long-term ecosystem monitoring program of the Exxon Valdez Oil Spill
# Trustee Council for the marine ecosystem affected by the 1989 oil spill.
# This dataset describes CTD readings from the GAK1 oceanographic station in
# Prince William Sound. Located at the mouth of Resurrection Bay near Seward,
# Alaska, temperature and salinity versus depth profiles have been taken at GAK1
# since December, 1970. This multi-decade time series is one of the longest
# running oceanographic time series in the North Pacific. This dataset includes
# a single ASCII-formatted '.dat' file containing CTD profile data 1970-2016.
# This file provides the code for the cruise that collected the profile, the
# station number of the GAK1 CTD profile on the cruise that collected the data,
# the date (in decimal year) of the profile, as well as depth (m),
# temperature (C), salinity (ppt), seawater density (sigma-t), and delta-D.

# Resource Constraints---------------------------------------------------------
# This data may be freely used. If you do choose to use it, please contact
# Seth Danielson (sldanielson@alaska.edu) with information about your
# application and publications that result so that we can keep our website
# updated and so we can continue keeping this time series funded. This material
# is based upon work funded by multiple entities, including the Exxon Valdez Oil
# Spill Trustee Council. Any opinions, findings, conclusions, or
# recommendations expressed herein are those of the author(s) and do not
# necessarily reflect the views or positions of funding entities. Standard
# scientific norms for attribution, credit, and potential co-authorship should be
# followed when using these data including to the Owners, Exxon Valdez Oil Spill
# Trustee Council and other sources of funding. Please let the Owner know when
# these data are used.

gak1_data <- dataone::getObject(mn,"e24107a7-8905-4f55-8c75-8b0831e47a83")%>%
  rawToChar(.)%>%
  textConnection(.)%>%
  read.csv(., sep = "\t", stringsAsFactors = FALSE)
