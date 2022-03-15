#Q33C. To what extent are you worried or not about experiencing each of the following while living in Paris/votre commune? - Being stared at inappropriately

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

#Création de la table de contingence : EPT & Q33C_4
Q33C_4 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q33C_4)
#Transformation de la table en dataframe
Q33C_4 = as.data.frame.matrix(Q33C_4)
Q33C_4$id <- row.names(Q33C_4)
Q33C_4$total <- Q33C_4$`1` + Q33C_4$`2` + Q33C_4$`3` + Q33C_4$`4` +Q33C_4$`5` +Q33C_4$`6`
Q33C_4$p1 <- Q33C_4$`1` / Q33C_4$total * 100
Q33C_4$p2 <- Q33C_4$`2` / Q33C_4$total * 100
Q33C_4$p3 <- Q33C_4$`3` / Q33C_4$total * 100
Q33C_4$p4 <- Q33C_4$`4` / Q33C_4$total * 100
Q33C_4$p5 <- Q33C_4$`5` / Q33C_4$total * 100
Q33C_4$p6 <- Q33C_4$`6` / Q33C_4$total * 100
Q33C_4$worried <- Q33C_4$p1 + Q33C_4$p2
Q33C_4$notworried <- Q33C_4$p3 + Q33C_4$p4

Q33C_4$res <- Q33C_4$worried / Q33C_4$notworried
res_ept <- merge(ept, Q33C_4, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q33C_4$p1, Q33C_4$p2, Q33C_4$p3, Q33C_4$p4, Q33C_4$p5, Q33C_4$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très préoccupé")
mf_map(res_ept, "p2", type = "choro", leg_title = "Préoccupé")
mf_map(res_ept, "p3", type = "choro", leg_title = "Pas très préoccupé")
mf_map(res_ept, "p4", type = "choro", leg_title = "Très préoccupé")
mf_map(res_ept, "p5", type = "choro", leg_title = "Ne sait pas")
mf_map(res_ept, "p6", type = "choro", leg_title = "Préfère ne rien dire")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "worried", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Préoccupé par des regards inapropriés",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "notworried", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas préoccupé par des regards inapropriés",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nPréoccupé / Pas préoccupé", leg_pos = "bottomleft2")
mf_layout(
  title = "Préoccupé ou non par des regards inapropriés",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

