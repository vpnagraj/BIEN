# install.packages("BIEN")
# install.packages("leaflet")
# install.packages("rgdal")
# install.packages("geojsonio")
# install.packages("htmltools")
# install.packages("DT")
library(BIEN)
library(leaflet)
library(rgdal)
library(geojsonio)
library(dplyr)
library(htmltools)
library(DT)

# occurence
# retrieve occurrenc data for western sumac
wsumac <- BIEN_occurrence_species(species = "Rhus copallinum")

# plot points on map
# popups for date collected where available
leaflet(data = wsumac) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addCircleMarkers(lng=wsumac$longitude, 
                   lat=wsumac$latitude,
                   col="firebrick",
                   stroke = FALSE,
                   opacity = 0.1,
                   label = ~htmlEscape(date_collected))
# ranges
BIEN_ranges_species("Rhus copallinum", directory = "shp")

wsumac <- readOGR(dsn = path.expand("shp"))

leaflet(data = wsumac) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons()

# traits
wsumac_traits <- BIEN_trait_species("Rhus copallinum")

summarised <-
  wsumac_traits %>%
  group_by(trait_name, unit) %>%
  summarise(average = mean(as.numeric(trait_value), na.rm = TRUE)) %>%
  filter(!is.nan(average)) %>%
  mutate(average = paste(round(average, 2), unit)) %>%
  select(-unit)

DT(summarised)
# stem
wsumac_stem <- BIEN_stem_species("Rhus copallinum")

summary(wsumac_stem$stem_dbh_cm)

# by states
# query BIEN for lists of species
state_species <- BIEN_list_state(country="United States", state=state.name)

# count up number of species by state
nspecies <- 
  state_species %>%
  group_by(state_province) %>%
  tally()

# get data for map
json_api <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"

states <- geojson_read(json_api, what = "sp")

# join in species count
states@data <-
  states@data %>%
  inner_join(nspecies, by = c("name" = "state_province"))

bins <- quantile(states@data$n, na.rm = TRUE)
pal <- colorBin("YlOrRd", domain = states@data$n, bins = bins)

leaflet(states) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(color = ~pal(n),
              stroke = FALSE
              ) %>%
  addLegend(pal = pal, 
            values = ~n,
            position = "bottomright") %>%
  setView(lat = 37, lng = -90, zoom = 3)
  
