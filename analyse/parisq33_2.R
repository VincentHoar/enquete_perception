#Q33. How safe do you feel walking alone in your local area… - ...after dark?

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

#Création de la table de contingence : EPT & Q33_2
Q33_2 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q33_2)
#Transformation de la table en dataframe
Q33_2 = as.data.frame.matrix(Q33_2)
Q33_2$id <- row.names(Q33_2)
Q33_2$total <- Q33_2$`1` + Q33_2$`2` + Q33_2$`3` + Q33_2$`4` +Q33_2$`5` +Q33_2$`6`+Q33_2$`7`
Q33_2$p1 <- Q33_2$`1` / Q33_2$total * 100
Q33_2$p2 <- Q33_2$`2` / Q33_2$total * 100
Q33_2$p3 <- Q33_2$`3` / Q33_2$total * 100
Q33_2$p4 <- Q33_2$`4` / Q33_2$total * 100
Q33_2$p5 <- Q33_2$`5` / Q33_2$total * 100
Q33_2$p6 <- Q33_2$`6` / Q33_2$total * 100
Q33_2$p7 <- Q33_2$`7` / Q33_2$total * 100
Q33_2$safe <- Q33_2$p1 + Q33_2$p2
Q33_2$notsafe <- Q33_2$p3 + Q33_2$p4

Q33_2$res <- Q33_2$safe / Q33_2$notsafe
res_ept <- merge(ept, Q33_2, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q33_2$p1, Q33_2$p2, Q33_2$p3, Q33_2$p4, Q33_2$p5, Q33_2$p6, Q33_2$p7)

# Cartes par réponses
par(mfrow = c(3,3))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très sûre")
mf_map(res_ept, "p2", type = "choro", leg_title = "Sûre")
mf_map(res_ept, "p3", type = "choro", leg_title = "Pas sûre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas du tout sûre")
mf_map(res_ept, "p5", type = "choro", leg_title = "Ne sort pas à cette heure")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")
mf_map(res_ept, "p7", type = "choro", leg_title = "Préfère ne rien dire")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "safe", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Sûre",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "notsafe", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas sûre",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "La sûreté du quartier la nuit",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)
