##Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier de l'enquête IPSOS pour Londres
ipsoslondres = read_sav(file = "data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - London UK SPSS V1_240621_Client Use Only.sav")

#Import du shapefile de Londres
fond_carte_londres = st_read("data-raw/londresborough.shp")

#Agréger pour obtenir le nombre de répondants par borough
agreglondon = aggregate(ipsoslondres$QMktSize_2_1, by = list(ipsoslondres$QMktSize_2_1),length)

#Changement du nom de la colonne du tableau agreglondon
agreglondon = agreglondon %>%
  rename(
    BOROUGH = Group.1
  )

#Fusion du shapefile et de l'agrégation
mergelondres = merge(fond_carte_londres, agreglondon, by = "BOROUGH")

#Enregistrement du fond de carte en geopackage
st_write(mergelondres, "data/cartelondres.gpkg")

cartelondres = st_read("data/cartelondres.gpkg")

mergelondres = st_read("data/cartelondres.gpkg")

#Création de la carte des répondants selon les Boroughs du Grand Londres
mf_export(mergelondres, "fig/carterepondantlondres.svg")
mf_shadow(mergelondres)
mf_map(x=mergelondres, border = "white", add=TRUE)

mf_map(
  x = mergelondres,
  var = "x",
  type = "prop",
  col = "blue4",
  inches = 0.29,
  leg_pos = "bottomleft2",
  leg_title = "Répondants"
)

mf_title(
  txt = "Répondants selon les 'Boroughs' de Londres",
  pos = "center",
  tab = FALSE,
  bg = "darkblue",
  fg = "aliceblue",
  cex = 1.2,
  line = 1.5,
  font = 1,
  inner = FALSE
)

mf_arrow(
  pos = "topleft"
)
mf_scale(
  size = 10,
  lwd = 2,
  cex = .8,
)
mf_credits("Source : IPSOS 2021  \nHoareau, 2022")
dev.off()
