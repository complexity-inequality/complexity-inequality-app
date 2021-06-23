# R-script leaflet1.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)

# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(st)){install.packages("st")}
if(!require(leaflet)){install.packages("leaflet")}



source(file = "code/functions/fct_loc.R")
data_loc('SC')

source(file = "code/functions/fct_shapefile.R")
sc_sf <- get_sf()
plot(sc_sf['nm_mun'])

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = -48.521523, lat = -27.599015, popup = 'UFSC') %>%
  setView(lng = -48.521523, lat = -27.599015, zoom = 4)





leaflet(sc_sf) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
