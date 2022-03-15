#Q21. How concerned, if at all, are you about each of the following? - Loos of green spaced and urban biodiversity

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

#Création de la table de contingence : EPT & Q22A
Q22A = table(mergeparisipsos$ID_EPT, mergeparisipsos$Q22A)
#Transformation de la table en dataframe
Q22A = as.data.frame.matrix(Q22A)
Q22A$id <- row.names(Q22A)
Q22A$total <- Q22A$`1` + Q22A$`2` + Q22A$`3` + Q22A$`4` +Q22A$`5`
Q22A$p1 <- Q22A$`1` / Q22A$total * 100
Q22A$p2 <- Q22A$`2` / Q22A$total * 100
Q22A$p3 <- Q22A$`3` / Q22A$total * 100
Q22A$p4 <- Q22A$`4` / Q22A$total * 100
Q22A$p5 <- Q22A$`5` / Q22A$total * 100
Q22A$important <- Q22A$p1 + Q22A$p2
Q22A$pasimportant <- Q22A$p3 + Q22A$p4
Q22A$res <- Q22A$important - Q22A$pasimportant
res_ept <- merge(ept, Q22A, by.x = "ID_EPT", by.y = "id", all.x =TRUE)

# repartition
boxplot(Q22A$p1, Q22A$p2, Q22A$p3, Q22A$p4, Q22A$p5, Q22A$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Très important")
mf_map(res_ept, "p2", type = "choro", leg_title = "Pas important")
mf_map(res_ept, "p3", type = "choro", leg_title = "Pas très important")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas important du tout")
mf_map(res_ept, "p5", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "important", type = "choro", leg_title = "En %")
mf_layout(
  title = "Important",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasimportant", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas ou peu important",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (Différence)
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Différence\nimportant / Pas important", leg_pos = "bottomleft2")
mf_layout(
  title = "Important d'être à moins de 20 minutes des installations locales",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)

