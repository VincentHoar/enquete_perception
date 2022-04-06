##Chargement des packages
library(sf)

basemap = st_read('data/cartelondres.gpkg')


basemap$inner  = "Outer London"


inn = c(
  "Camden", "City of London", 
  "Hackney", "Hammersmith and Fulham", 
  "Haringey", "Islington", "Kensington and Chelsea", 
  "Lambeth", "Lewisham",  "Newham", 
  "Southwark", "Tower Hamlets", 
  "Wandsworth", "Westminster")


basemap[basemap$BOROUGH %in% inn, "inner"]  = "Inner London"

hab = aggregate(basemap["BOROUGH"], by = list(cat = basemap$inner), head,1)
hab = hab[,-2]



##Enregistrement dans un seul geopackage : les EPT, les départements d'IdF et les communes d'IdF
st_write(obj = hab, dsn ="data/cartelondres.gpkg", layer = "inner", 
         delete_layer = TRUE, quiet = TRUE)



hab = st_read('data/cartelondres.gpkg', layer = "inner",quiet = TRUE)
basemap = st_read('data/cartelondres.gpkg', layer = "cartelondres", quiet = TRUE)


basemap = st_transform(basemap, "EPSG:27700")
hab = st_transform(hab, "EPSG:27700")

names(basemap)[1] = "ID_GEO"
basemap$NAME = basemap$ID_GEO
basemap = basemap[,-2]


st_write(obj = hab, dsn ="data/london.gpkg", layer = "hab", 
         delete_layer = TRUE, quiet = TRUE)

st_write(obj = basemap, dsn ="data/london.gpkg", layer = "basemap", 
         delete_layer = TRUE, quiet = TRUE)



