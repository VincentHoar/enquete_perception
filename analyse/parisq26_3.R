#Q26. Thinking now about immigration to Paris/votre commune, to what extent do you agree or disagree with the following statements? 
#- I think Paris/Ma commune needs more immigration than the present level
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


#Création de la table de contingence : EPT & Q26_3
Q26_3 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q26_3)
#Transformation de la table en dataframe
Q26_3 = as.data.frame.matrix(Q26_3)
Q26_3$id <- row.names(Q26_3)
Q26_3$total <- Q26_3$`1` + Q26_3$`2` + Q26_3$`3` + Q26_3$`4` +Q26_3$`5` +Q26_3$`6`
Q26_3$p1 <- Q26_3$`1` / Q26_3$total * 100
Q26_3$p2 <- Q26_3$`2` / Q26_3$total * 100
Q26_3$p3 <- Q26_3$`3` / Q26_3$total * 100
Q26_3$p4 <- Q26_3$`4` / Q26_3$total * 100
Q26_3$p5 <- Q26_3$`5` / Q26_3$total * 100
Q26_3$p6 <- Q26_3$`6` / Q26_3$total * 100
Q26_3$daccord <- Q26_3$p1 + Q26_3$p2
Q26_3$pasdaccord <- Q26_3$p4 + Q26_3$p5
Q26_3$res <- Q26_3$daccord / Q26_3$pasdaccord
res_ept <- merge(ept, Q26_3, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q26_3$p1, Q26_3$p2, Q26_3$p3, Q26_3$p4, Q26_3$p5, Q26_3$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "D'accord")
mf_map(res_ept, "p2", type = "choro", leg_title = "Plutôt d'accord")
mf_map(res_ept, "p3", type = "choro", leg_title = "Plutôt pas d'accord")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas du tout d'accord")
mf_map(res_ept, "p5", type = "choro", leg_title = "Pas assez de personnes")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")


# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "D'accord",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas d'accord",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma ville a besoin de plus d'immigration",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
