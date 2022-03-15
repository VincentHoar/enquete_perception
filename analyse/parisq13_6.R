### Q13. To what extent, if at all, would you say Paris/votre commune is a good place to live for the following groups of people? - People like me


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


#Création de la table de contingence : EPT & Q13_6
Q13_6 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q13_6)
#Transformation de la table en dataframe
Q13_6 = as.data.frame.matrix(Q13_6)
Q13_6$id <- row.names(Q13_6)
Q13_6$total <- Q13_6$`1` + Q13_6$`2` + Q13_6$`3` + Q13_6$`4` +Q13_6$`5` +Q13_6$`6`
Q13_6$p1 <- Q13_6$`1` / Q13_6$total * 100
Q13_6$p2 <- Q13_6$`2` / Q13_6$total * 100
Q13_6$p3 <- Q13_6$`3` / Q13_6$total * 100
Q13_6$p4 <- Q13_6$`4` / Q13_6$total * 100
Q13_6$p5 <- Q13_6$`5` / Q13_6$total * 100
Q13_6$p6 <- Q13_6$`6` / Q13_6$total * 100
Q13_6$daccord <- Q13_6$p1 + Q13_6$p2
Q13_6$pasdaccord <- Q13_6$p4 + Q13_6$p5
Q13_6$res <- Q13_6$daccord / Q13_6$pasdaccord
res_ept <- merge(ept, Q13_6, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q13_6$p1, Q13_6$p2, Q13_6$p3, Q13_6$p4, Q13_6$p5, Q13_6$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Un très bon endroit")
mf_map(res_ept, "p2", type = "choro", leg_title = "Un bon endroit")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Un mauvais endroit")
mf_map(res_ept, "p5", type = "choro", leg_title = "Un très mauvais endroit")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Ma commune est un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Ma commune n'est pas un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nd'accord / pas d'accord", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma commune est un bon endroit pour les riches",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  


