#Q36. Thinking now about the future of Paris, how likely or unlikely would you say it is that each of the following will happen in the next five years? - Paris/Paris et sa région will bounce back from the coronavirus pandemic
##Chargement des packages
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


#Création de la table de contingence : EPT & Q36_7
Q36_7 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q36_7)
#Transformation de la table en dataframe
Q36_7 = as.data.frame.matrix(Q36_7)
Q36_7$id <- row.names(Q36_7)
Q36_7$total <- Q36_7$`1` + Q36_7$`2` + Q36_7$`3` + Q36_7$`4` +Q36_7$`5` +Q36_7$`6`
Q36_7$p1 <- Q36_7$`1` / Q36_7$total * 100
Q36_7$p2 <- Q36_7$`2` / Q36_7$total * 100
Q36_7$p3 <- Q36_7$`3` / Q36_7$total * 100
Q36_7$p4 <- Q36_7$`4` / Q36_7$total * 100
Q36_7$p5 <- Q36_7$`5` / Q36_7$total * 100
Q36_7$p6 <- Q36_7$`6` / Q36_7$total * 100
Q36_7$likely <- Q36_7$p1 + Q36_7$p2
Q36_7$unlikely <- Q36_7$p3 + Q36_7$p4

Q36_7$res <- Q36_7$likely / Q36_7$unlikely
res_ept <- merge(ept, Q36_7, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q36_7$p1, Q36_7$p2, Q36_7$p3, Q36_7$p4, Q36_7$p5, Q36_7$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très probable")
mf_map(res_ept, "p2", type = "choro", leg_title = "Probable")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Peu probable")
mf_map(res_ept, "p5", type = "choro", leg_title = "Très peu probable")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "likely", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Paris va probablement rebondir après le coronavirus",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "unlikely", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Paris ne va probablement pas rebondir après le coronavirus",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nProbable / Pas probable", leg_pos = "bottomleft2")
mf_layout(
  title = "Paris va rebondir du coronavirus dans les 5 prochaines années",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

