##Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)


#Import du fichier IPSOS
IPSOSPARIS = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")

#Import du fichier csv des communes
com_ept = read.csv("data/com_ept1.csv", sep = ";")

#Changement de nom dans le fichier IPSOS pour faciliter le merge
IPSOSPARIS = IPSOSPARIS %>%
  rename(
    code_insee = QMktSize_1_1
  )

#Jointure des deux fichiers précédents
mergeparisipsos = merge(IPSOSPARIS, com_ept, by = "code_insee")

#Agrégation pour avoir le nombre de répondants par EPT
agregparisipsos = aggregate(mergeparisipsos$ID_EPT, by = list(mergeparisipsos$ID_EPT), length)

#Création de la carte du nombre dé répondants par EPT
##Import du geopackage
PARISEPT = st_read("data/fondparis.gpkg", layer = "EPT_SANS", quiet = TRUE)

mf_map(PARISEPT)
#Changement du nom de colonne pour faciliter le merge de l'agrégation et du fond de carte
agregparisipsos = agregparisipsos %>%
  rename(
    ID_EPT = Group.1
  )
##Jointure du fond de carte et du nombre de répondants
PARISREP = merge(PARISEPT, agregparisipsos, by = "ID_EPT")

##Création de la carte
mf_export(PARISREP, "fig/repondantparis.svg")
mf_shadow(PARISREP)
mf_map(x=PARISREP, border = "white", add=TRUE)

mf_map(
  x = PARISREP,
  var = "x",
  type = "prop",
  col = "lightsalmon3",
  inches = 0.24,
  leg_pos = "bottomleft2",
  leg_title = "Répondants"
)


mf_title(
  txt = "Répondants selon les EPT de Paris",
  pos = "center",
  tab = FALSE,
  bg = "lightsalmon3",
  fg = "aliceblue",
  cex = 1.25,
  line = 1.5,
  font = 1,
  inner = FALSE
)

mf_arrow(
  pos = "topleft"
)
mf_scale(
  size = 5,
  lwd = 2,
  cex = 1.2,
  
)
mf_credits("Source : IPSOS 2021  \nHoareau, 2022")
dev.off()
