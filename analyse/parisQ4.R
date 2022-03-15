### Q4. Overall, how satisfied or dissatisfied are you with your local area as a place to live?

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






#Création de la table de contingence : EPT & Q4
Q4 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q4)
#Transformation de la table en dataframe
Q4 = as.data.frame.matrix(Q4)
Q4$id <- row.names(Q4)
Q4$total <- Q4$`1` + Q4$`2` + Q4$`3` + Q4$`4` +Q4$`5` +Q4$`6`
Q4$p1 <- Q4$`1` / Q4$total * 100
Q4$p2 <- Q4$`2` / Q4$total * 100
Q4$p3 <- Q4$`3` / Q4$total * 100
Q4$p4 <- Q4$`4` / Q4$total * 100
Q4$p5 <- Q4$`5` / Q4$total * 100
Q4$p6 <- Q4$`6` / Q4$total * 100
Q4$satisfait <- Q4$p1 + Q4$p2
Q4$passatisfait <- Q4$p4 + Q4$p5
Q4$res <- Q4$satisfait / Q4$passatisfait
res_ept <- merge(ept, Q4, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q4$p1, Q4$p2, Q4$p3, Q4$p4, Q4$p5, Q4$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très satisfait")
mf_map(res_ept, "p2", type = "choro", leg_title = "Satisfait")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas satisfait")
mf_map(res_ept, "p5", type = "choro", leg_title = "Pas du tout atisfait")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "satisfait", type = "choro", leg_title = "En %")
mf_layout(
  title = "Satisfait de leur lieu de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "passatisfait", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Non satisfait de leur lieu de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nsatisfait / n. insatisfait", leg_pos = "bottomleft2")
mf_layout(
  title = "Q4. Overall, how satisfied or dissatisfied are you with your local area as a place to live?",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
 