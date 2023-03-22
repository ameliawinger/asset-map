setwd("~/Desktop")

#Load libraries
x <- c("tidyverse","rvest","dplyr","lubridate","tidygeocoder","ggplot2",
        "stringr","readr","leaflet","htmltools","htmlwidgets","plotly","rgdal")
lapply(x, require, character.only = TRUE)

#Import data
data <- read_csv("data.csv")

#Create column combining latitude and longitude
data <- data %>% 
  mutate(LatLong = paste(Lat, CleanLong))

#Create a pivot table showing how many alerts were issued at each 
pivot <- data %>% 
  group_by(LatLong) %>% 
  summarize(Freq = n())

#Create columns for just the latitude and longitude (important for map)
pivot <- pivot %>% 
  mutate(Lat = as.numeric(gsub( " .*$", "", LatLong)),
         Long = as.numeric(gsub(".* ", "", LatLong)))

#Arrange pivot table from lowest frequency to highest (will be important for layering map dots)
pivot <- arrange(pivot, Freq)

#Create frequency categories (for color-coding on map)
pivot <- pivot %>% 
  mutate(Category = ifelse(pivot$Freq >= 10, "High",
                           ifelse(pivot$Freq >= 4, "Medium",
                                  ifelse(pivot$Freq >= 2, "Low", "Very Low"))))

#Create a color palette for map
pal <- colorFactor(palette = c("#003a6d", "#1192e8", "#82cfff", "#e5f6ff"),
                   levels = c("High", "Medium", "Low", "Very Low"))

#Create labels for map
labs <- lapply(seq(nrow(pivot)), function(i) {
  paste0("<b>Number of Shotspotter Alerts: </b>", pivot[i, "Freq"], '<br />')})

#Create map with images included in pop-ups
m <- leaflet(pivot) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(minZoom = 11, maxZoom = 19)) %>% 
  setView(lng = -79.99045863436507, lat = 40.439970708384806, zoom = 12) %>% 
  addCircleMarkers(lng = pivot$Long, lat = pivot$Lat, 
                   popup = paste0("<img src='https://www.r-project.org/logo/Rlogo.png'",
                                  "width='70'",
                                  "height='65'", ">"),
                   stroke = FALSE, fillOpacity = 1, color = ~pal(Category),
                   radius = 4) %>% 
  addLegend("bottomright", 
            colors = c("#e5f6ff", "#82cfff", "#1192e8", "#003a6d"),
            labels = c("1 Alert", "2-3 Alerts", "4-9 Alerts", "10+ Alerts"),
            opacity = .9)
m

#Just for reference- adding custom labels to a map
m2 <- leaflet(pivot) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(minZoom = 11, maxZoom = 19)) %>% 
  setView(lng = -79.99045863436507, lat = 40.439970708384806, zoom = 12) %>% 
  addCircleMarkers(lng = pivot$Long, lat = pivot$Lat, 
                   label = lapply(labs, htmltools::HTML),
                   stroke = FALSE, fillOpacity = 1, color = ~pal(Category),
                   radius = 4) %>% 
  addLegend("bottomright", 
            colors = c("#e5f6ff", "#82cfff", "#1192e8", "#003a6d"),
            labels = c("1 Alert", "2-3 Alerts", "4-9 Alerts", "10+ Alerts"),
            opacity = .9)
m2

#save map as html file
saveWidget(m, file="shotspotter_map.html")





