##Q7. And do you expect that the quality of local services such as schools, transport and police will improve, get worse, or stay the same over the next five years?

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


#Création de la table de contingence : EPT & Q7
Q7 = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q7)
#Transformation de la table en dataframe
Q7 = as.data.frame.matrix(Q7)
Q7$id <- row.names(Q7)
Q7$total <- Q7$`1` + Q7$`2` + Q7$`3` + Q7$`4` +Q7$`5` +Q7$`6`
Q7$p1 <- Q7$`1` / Q7$total * 100
Q7$p2 <- Q7$`2` / Q7$total * 100
Q7$p3 <- Q7$`3` / Q7$total * 100
Q7$p4 <- Q7$`4` / Q7$total * 100
Q7$p5 <- Q7$`5` / Q7$total * 100
Q7$p6 <- Q7$`6` / Q7$total * 100
Q7$optimiste <- Q7$p1 + Q7$p2
Q7$pessimiste <- Q7$p4 + Q7$p5
Q7$res <- Q7$optimiste / Q7$pessimiste
res_ept <- merge(ept, Q7, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q7$p1, Q7$p2, Q7$p3, Q7$p4, Q7$p5, Q7$p6)

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
  title = "Les services publics vont s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pessimiste", type = "choro", pal = "Reds", leg_title = "En %")
mf_layout(
  title = "Les services publics vont se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\noptimiste / pessimiste", leg_pos = "bottomleft2")
mf_layout(
  title = "Quelle évolution pour la qualité des services publics?",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
