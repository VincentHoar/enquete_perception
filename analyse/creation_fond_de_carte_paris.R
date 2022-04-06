##Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

##Import du fond de carte, tiré de data.iledefrance.fr
com = st_read("data-raw/cog-communes.shp")


##Import du fichier csv avec les communes et EPT assossiés (+communes hors GP)
com_ept = read.csv("data/com_ept1.csv", sep = ";")

##Changement du nom de colonne (pour faciliter le merge)
com = com %>%
  rename(
    code_insee = com
  )

##Jointure du fond de carte et du csv
mergeparis = merge(com, com_ept, by = "code_insee", all.x = TRUE)


## Agrégation des communes selon les EPT
EPT = aggregate(mergeparis[, "code_insee"], by = list(ID_EPT = mergeparis$ID_EPT), length)

##Fond de carte des départements (source : data.iledefrance.fr)
dep <- aggregate(fond_carte_idf[,"dep"], by = list(fond_carte_idf$dep), length)


dep <- st_transform(dep, 2154)
com <- st_transform(com, 2154)
EPT <- st_transform(EPT, 2154)


mf_map(EPT_SANS)
mf_map(dep, add = T, col = NA)


EPT_SANS <- EPT[-1, ]
df <- data.frame(x = 2.202543, y = 48.71656)
pt <- st_as_sf(df, coords = c("x", "y"), crs = 4326)
pt <- st_transform(pt, 2154)

mf_map(pt, add = T)
poly <- st_buffer(pt, 2000, nQuadSegs = 2)
mf_map(poly, add = T)

EPT_SANS

poly$ID_EPT = "HORS GP"
poly$code_insee = 20

EPT_SANS <- rbind(poly, EPT_SANS)



##Enregistrement dans un seul geopackage : les EPT, les départements d'IdF et les communes d'IdF
st_write(obj = EPT, dsn ="data/fondparis.gpkg", layer = "ept", 
         delete_layer = TRUE, quiet = TRUE)
st_write(obj = EPT_SANS, dsn ="data/fondparis.gpkg", layer = "ept_sans", 
         delete_layer = TRUE, quiet = TRUE)
st_write(obj = dep, dsn ="data/fondparis.gpkg", layer = "dep", 
         delete_layer = TRUE, quiet=TRUE)
st_write(obj = com, dsn ="data/fondparis.gpkg", layer = "com", 
         delete_layer = TRUE, quiet=TRUE)



hab = st_read('data/fondparis.gpkg', layer = "dep", quiet = TRUE)
basemap = st_read('data/fondparis.gpkg', layer = "ept_sans", quiet = TRUE)
com = st_read('data/fondparis.gpkg', layer = "com", quiet = TRUE)

hab = st_transform(hab, "EPSG:2154")
com = st_transform(com, "EPSG:2154")
basemap = st_transform(basemap, "EPSG:2154")

dico = read.csv("data/com_ept1.csv", sep = ";")
com
basemap

x = unique(dico[,3:4])
basemap = merge(basemap, x, by = "ID_EPT", all.x = T)
basemap$NOM_EPT <- c("Hors Grand Paris", "Ville de Paris", "Paris-Est-Marne et Bois", 
                     "Grand Paris Sud Est Avenir", "Grand-Orly Seine Bièvre", "Vallée Sud Grand Paris", 
                     "Grand Paris Seine Ouest", "Paris Ouest La Défense", "Boucle Nord de Seine", 
                     "Plaine Commune", "Paris Terres d'Envol", "Est Ensemble", "Grand Paris - Grand Est"
)


basemap  = basemap[, c(1,3)]
names(basemap)[1:2] <- c("ID_GEO", "NAME")


st_write(obj = basemap, dsn ="data/paris.gpkg", layer = "basemap", 
         delete_layer = TRUE, quiet = TRUE)
st_write(obj = hab, dsn ="data/paris.gpkg", layer = "hab", 
         delete_layer = TRUE, quiet=TRUE)
st_write(obj = com, dsn ="data/paris.gpkg", layer = "com", 
         delete_layer = TRUE, quiet=TRUE)
