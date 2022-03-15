### Q11B. Thinking now about living in Paris/votre commune, to what extent do you agree or disagree with the following statements? 
#Paris/ma commune is a good place for people like me to start a career

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


#Création de la table de contingence : EPT & Q11_1
Q11_1 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q11B_5)
#Transformation de la table en dataframe
Q11_1 = as.data.frame.matrix(Q11_1)
Q11_1$id <- row.names(Q11_1)
Q11_1$total <- Q11_1$`1` + Q11_1$`2` + Q11_1$`3` + Q11_1$`4` +Q11_1$`5` +Q11_1$`6`
Q11_1$p1 <- Q11_1$`1` / Q11_1$total * 100
Q11_1$p2 <- Q11_1$`2` / Q11_1$total * 100
Q11_1$p3 <- Q11_1$`3` / Q11_1$total * 100
Q11_1$p4 <- Q11_1$`4` / Q11_1$total * 100
Q11_1$p5 <- Q11_1$`5` / Q11_1$total * 100
Q11_1$p6 <- Q11_1$`6` / Q11_1$total * 100
Q11_1$daccord <- Q11_1$p1 + Q11_1$p2
Q11_1$pasdaccord <- Q11_1$p4 + Q11_1$p5
Q11_1$res <- Q11_1$daccord / Q11_1$pasdaccord
res_ept <- merge(ept, Q11_1, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q11_1$p1, Q11_1$p2, Q11_1$p3, Q11_1$p4, Q11_1$p5, Q11_1$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Tout à fait d'accord")
mf_map(res_ept, "p2", type = "choro", leg_title = "D'accord")
mf_map(res_ept, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_ept, "p4", type = "choro", leg_title = "Pas d'accord")
mf_map(res_ept, "p5", type = "choro", leg_title = "Pas du tout d'accord")
mf_map(res_ept, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_ept, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Ma commune est un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Ma commune n'est pas un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_ept, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nd'accord / pas d'accord", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma commune est un bon endroit pour les gens comme moi au niveau de la carrière",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
