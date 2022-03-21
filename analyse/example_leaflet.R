#Chargement des packages
library(sf)

ept_raw <- st_read('data/fondparis.gpkg', layer = "ept_sans")


library(leaflet)
ept <- st_transform(ept_raw, 4326)




leaflet() %>%
  addTiles(attribution = 'kjkljkljjkl') %>%
  addPolygons(data = ept, popup = ept$ID_EPT, col= "#000094")



content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE)
  )
