##Q3. And do you expect that your standard of living will improve, get worse, or stay the same, over the next 5 years?

#Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier IPSOS
IPSOSPARIS = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")

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


#Création de la table de contingence : EPT & Q3
Q3 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q3)
#Transformation de la table en dataframe
Q3 = as.data.frame.matrix(Q3)
Q3$id <- row.names(Q3)
Q3$total <- Q3$`1` + Q3$`2` + Q3$`3` + Q3$`4` +Q3$`5` +Q3$`6`
Q3$p1 <- Q3$`1` / Q3$total * 100
Q3$p2 <- Q3$`2` / Q3$total * 100
Q3$p3 <- Q3$`3` / Q3$total * 100
Q3$p4 <- Q3$`4` / Q3$total * 100
Q3$p5 <- Q3$`5` / Q3$total * 100
Q3$p6 <- Q3$`6` / Q3$total * 100
Q3$optimiste <- Q3$p1 + Q3$p2
Q3$pessimiste <- Q3$p4 + Q3$p5
Q3$res <- Q3$optimiste / Q3$pessimiste
res_ept <- merge(ept, Q3, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q3$p1, Q3$p2, Q3$p3, Q3$p4, Q3$p5, Q3$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très optimiste")
mf_map(res_ept, "p2", type = "choro", leg_title = "Optimiste")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pessimiste")
mf_map(res_ept, "p5", type = "choro", leg_title = "Très pessimiste")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "optimiste", type = "choro", leg_title = "En %")
mf_layout(
  title = "Vision optimiste de l'avenir de leur niveau de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pessimiste", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Vision pessimiste de l'avenir de leur niveau de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\noptimiste / pessimiste", leg_pos = "bottomleft2")
mf_layout(
  title = "Vision plutôt optimiste ou pessimiste de l'évolution du niveau de vie?",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
