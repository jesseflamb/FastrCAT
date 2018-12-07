# Line 8 FOX stations time series

time_data <- ALL_BON

slice <- "Line 8"
plot_type <- "temperature"

range_filter <- if(slice == "Line 8"){
  time_data %>%
    filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
    filter(LON >= -155.2673 & LON <= -154.7725)
} else if (slice == "Line 8 FOX"){
  time_data %>%
    filter(LAT >= 57.52417 & LAT <= 57.71517)%>%
    filter(LON >= -155.2 & LON <= -154.85)
} else if(slice == "Semidi"){
  time_data %>%
    filter(LAT >= 54.7 & LAT <= 57.2)%>%
    filter(LON >= -159.7 & LON <= -155.0)
}


plot_data <- range_filter %>%
  filter(month(DATE) %in% c(4,5))%>%
  filter(DEPTH <= 100)%>%
  group_by(year(DATE), DEPTH)%>%
  summarise(mean_yr = mean(TEMPERATURE1, na.rm = TRUE))

# color palletes for salinity and temperature are derived from the oce
# package. These colors are standardized for oceanographic plots.--------------
plot_color <- if(plot_type == "temperature"){

 # c("#0B222E", "#062C46", "#013565", "#1C3983", "#3C3E87", "#524685",
  #  "#664D83", "#785383", "#8C5A82", "#A05F7F", "#B66478", "#CC686D",
   # "#E0705D", "#EF7B4C", "#F78C41", "#FAA13D", "#F9B642", "#F5CD4D",
    #"#EFE35B", "#E5FA6A")

  c("#0c1524" ,"#132037", "#22325E","#214985", "#136495", "#107EA0", "#2E97A8","#65ACAE","#95BEBC", "#C0D4CF",
    "#E8EDE3",
    "#f5af19","#f5af19","#f12711","#ff2525", "#f10000","#bd0000", "#a30000" ,"#560000","#230000","#230023")

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
  geom_tile(aes(x = `year(DATE)`, y = -(DEPTH), fill = mean_yr), data = plot_data)+
  scale_fill_gradientn(colors = plot_color, name = legend_name)+
  theme_bw()+
  ylab(label = "Depth m")+
  xlab(label = "Year")
