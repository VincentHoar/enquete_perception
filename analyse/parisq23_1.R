#Q23. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? 
#- Introducing a ‘congestion charge’ to reduce the number of cars driving into the centre of Paris on weekdays

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


#Création de la table de contingence : EPT & Q23_1
Q23_1 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q23_1)
#Transformation de la table en dataframe
Q23_1 = as.data.frame.matrix(Q23_1)
Q23_1$id <- row.names(Q23_1)
Q23_1$total <- Q23_1$`1` + Q23_1$`2` + Q23_1$`3` + Q23_1$`4` +Q23_1$`5` +Q23_1$`6`
Q23_1$p1 <- Q23_1$`1` / Q23_1$total * 100
Q23_1$p2 <- Q23_1$`2` / Q23_1$total * 100
Q23_1$p3 <- Q23_1$`3` / Q23_1$total * 100
Q23_1$p4 <- Q23_1$`4` / Q23_1$total * 100
Q23_1$p5 <- Q23_1$`5` / Q23_1$total * 100
Q23_1$p6 <- Q23_1$`6` / Q23_1$total * 100
Q23_1$daccord <- Q23_1$p1 + Q23_1$p2
Q23_1$pasdaccord <- Q23_1$p4 + Q23_1$p5
Q23_1$res <- Q23_1$daccord / Q23_1$pasdaccord
res_ept <- merge(ept, Q23_1, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q23_1$p1, Q23_1$p2, Q23_1$p3, Q23_1$p4, Q23_1$p5, Q23_1$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Soutient fortement")
mf_map(res_ept, "p2", type = "choro", leg_title = "Soutient")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Ne soutient pas")
mf_map(res_ept, "p5", type = "choro", leg_title = "Ne soutient pas du tout")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "En faveur d'un péage urbain",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "En opposition à un péage urbain",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Support d'un péage urbain en semaine dans le centre de Paris",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

