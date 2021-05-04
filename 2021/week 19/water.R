library("tidytuesdayR")
library("dplyr")
library("ggplot2")
library("leaflet")

# Get the data

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

water <- tuesdata$water

# Check the data

water %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon_deg, 
             lat = ~lat_deg, 
             popup = ~as.character(water_source), 
             label = ~as.character(water_source),  
    clusterOptions = markerClusterOptions()
  )


(lng = ~location.longitude,
  lat = ~location.latitude)


iqa_cetesb %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    popup = ~klocal,
    clusterOptions = markerClusterOptions()
  )
