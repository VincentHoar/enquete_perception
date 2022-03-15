#Q28. Overall, would you say you have a positive or negative opinion of London?
  
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


#Création de la table de contingence : EPT & Q28
Q28 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q28)
#Transformation de la table en dataframe
Q28 = as.data.frame.matrix(Q28)
Q28$id <- row.names(Q28)
Q28$total <- Q28$`1` + Q28$`2` + Q28$`3` + Q28$`4` +Q28$`5` +Q28$`6`
Q28$p1 <- Q28$`1` / Q28$total * 100
Q28$p2 <- Q28$`2` / Q28$total * 100
Q28$p3 <- Q28$`3` / Q28$total * 100
Q28$p4 <- Q28$`4` / Q28$total * 100
Q28$p5 <- Q28$`5` / Q28$total * 100
Q28$p6 <- Q28$`6` / Q28$total * 100
Q28$daccord <- Q28$p1 + Q28$p2
Q28$pasdaccord <- Q28$p4 + Q28$p5
Q28$res <- Q28$daccord / Q28$pasdaccord
res_ept <- merge(ept, Q28, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q28$p1, Q28$p2, Q28$p3, Q28$p4, Q28$p5, Q28$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Positive")
mf_map(res_ept, "p2", type = "choro", leg_title = "Plutôt positive")
mf_map(res_ept, "p3", type = "choro", leg_title = "Plutôt négative")
mf_map(res_ept, "p4", type = "choro", leg_title = "Négative")
mf_map(res_ept, "p5", type = "choro", leg_title = "Pas assez de personnes")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")


# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Bonne opinion",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Mauvaise opinion",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "L'opinion sur Londres",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
