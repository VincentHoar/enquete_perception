### Q6. Thinking about local services in Paris/votre commune such as schools, transport and police, how satisfied or dissatisfied are you, in general, with the level of service provided?

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






#Création de la table de contingence : EPT & Q6
Q6 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q6)
#Transformation de la table en dataframe
Q6 = as.data.frame.matrix(Q6)
Q6$id <- row.names(Q6)
Q6$total <- Q6$`1` + Q6$`2` + Q6$`3` + Q6$`4` +Q6$`5` +Q6$`6`
Q6$p1 <- Q6$`1` / Q6$total * 100
Q6$p2 <- Q6$`2` / Q6$total * 100
Q6$p3 <- Q6$`3` / Q6$total * 100
Q6$p4 <- Q6$`4` / Q6$total * 100
Q6$p5 <- Q6$`5` / Q6$total * 100
Q6$p6 <- Q6$`6` / Q6$total * 100
Q6$satisfait <- Q6$p1 + Q6$p2
Q6$passatisfait <- Q6$p4 + Q6$p5
Q6$res <- Q6$satisfait / Q6$passatisfait
res_ept <- merge(ept, Q6, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q6$p1, Q6$p2, Q6$p3, Q6$p4, Q6$p5, Q6$p6)

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
  title = "Satisfait de la qualité des services publics",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "passatisfait", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Non satisfait de la qualité des services publics",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nsatisfait / n. insatisfait", leg_pos = "bottomleft2")
mf_layout(
  title = "Satisfaction de la qualité des services publics",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
