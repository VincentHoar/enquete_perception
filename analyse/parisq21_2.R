#Q21. How concerned, if at all, are you about each of the following? - Climate change, sometimes referred to as 'global warming'

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

#Création de la table de contingence : EPT & Q21_2
Q21_2 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q21_2)
#Transformation de la table en dataframe
Q21_2 = as.data.frame.matrix(Q21_2)
Q21_2$id <- row.names(Q21_2)
Q21_2$total <- Q21_2$`1` + Q21_2$`2` + Q21_2$`3` + Q21_2$`4` +Q21_2$`5`
Q21_2$p1 <- Q21_2$`1` / Q21_2$total * 100
Q21_2$p2 <- Q21_2$`2` / Q21_2$total * 100
Q21_2$p3 <- Q21_2$`3` / Q21_2$total * 100
Q21_2$p4 <- Q21_2$`4` / Q21_2$total * 100
Q21_2$p5 <- Q21_2$`5` / Q21_2$total * 100
Q21_2$concerned <- Q21_2$p1 + Q21_2$p2
Q21_2$notconcerned <- Q21_2$p3 + Q21_2$p4
Q21_2$res <- Q21_2$concerned / Q21_2$notconcerned
res_ept <- merge(ept, Q21_2, by.x = "ID_EPT", by.y = "id", all.x =TRUE)

# repartition
boxplot(Q21_2$p1, Q21_2$p2, Q21_2$p3, Q21_2$p4, Q21_2$p5, Q21_2$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très concerné")
mf_map(res_ept, "p2", type = "choro", leg_title = "Pas concerné")
mf_map(res_ept, "p3", type = "choro", leg_title = "Pas très concerné")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas concerné du tout")
mf_map(res_ept, "p5", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "concerned", type = "choro", leg_title = "En %")
mf_layout(
  title = "Concerné",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "notconcerned", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas ou peu concerné",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (Rapport)
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nConcerné / Pas concerné", leg_pos = "bottomleft2")
mf_layout(
  title = "Concerné ou non par le changement climatique",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)

