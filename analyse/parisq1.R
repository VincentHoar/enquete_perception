#Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier IPSOS
IPSOSPARIS = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")

#Import du fond de carte
dep <- st_read('data/fondparis.gpkg', layer = "dep")
ept <- st_read('data/fondparis.gpkg', layer = "ept_sans")



#Import du fichier csv des communes
com_ept = read.csv("data/com_ept1.csv", sep = ";")

#Changement de nom dans le fichier IPSOS pour faciliter le merge
IPSOSPARIS = IPSOSPARIS %>%
  rename(
    code_insee = QMktSize_1_1
  )

#Jointure des deux fichiers précédents
mergeparisipsos = merge(IPSOSPARIS, com_ept, by = "code_insee")

#Méthode 1
#Faire une matrice pour voir le nombre de total de répondants qui disent oui / non. Le faire 23 fois, mais pour comparer c'est compliqué.
Q1B1 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q1B_1)
Q1B1 = as.data.frame.matrix(Q1B1)
Q1B2 = as.data.frame.matrix(Q1B2)

Q1TEST = merge(Q1, Q1B2)

#Méthode 2
#tentative d'agréger les 23 modalités en fonction des EPT mais ce n'est pas des valeurs numériques donc je suis bloqué pour la suite et le FUN = lenght équivaut au nombre de répondants par EPT

test = aggregate(mergeparisipsos[,10:32], by=list(mergeparisipsos$ID_EPT),sum)

