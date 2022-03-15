#Q13. To what extent, if at all, would you say Paris/votre commune is a good place to live for the following groups of people? - Poor people

##Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier IPSOS
ipsoslondres = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - London UK SPSS V1_240621_Client Use Only.sav")

#Import du fond de carte
fondlondres = st_read('data/cartelondres.gpkg')

#Changement de nom dans le fichier IPSOS pour faciliter le merge
ipsoslondres = ipsoslondres %>%
  rename(
    BOROUGH = QMktSize_2_1
  )

#Création de la table de contingence : EPT & Q13_5
Q13_5 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q13_5)
#Transformation de la table en dataframe
Q13_5 = as.data.frame.matrix(Q13_5)
Q13_5$id <- row.names(Q13_5)
Q13_5$total <- Q13_5$`1` + Q13_5$`2` + Q13_5$`3` + Q13_5$`4` +Q13_5$`5` +Q13_5$`6`
Q13_5$p1 <- Q13_5$`1` / Q13_5$total * 100
Q13_5$p2 <- Q13_5$`2` / Q13_5$total * 100
Q13_5$p3 <- Q13_5$`3` / Q13_5$total * 100
Q13_5$p4 <- Q13_5$`4` / Q13_5$total * 100
Q13_5$p5 <- Q13_5$`5` / Q13_5$total * 100
Q13_5$p6 <- Q13_5$`6` / Q13_5$total * 100
Q13_5$daccord <- Q13_5$p1 + Q13_5$p2
Q13_5$pasdaccord <- Q13_5$p4 + Q13_5$p5
Q13_5$res <- Q13_5$daccord / Q13_5$pasdaccord
res_borough <- merge(fondlondres, Q13_5, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q13_5$p1, Q13_5$p2, Q13_5$p3, Q13_5$p4, Q13_5$p5, Q13_5$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très bon endroit")
mf_map(res_borough, "p2", type = "choro", leg_title = "Bon endroit")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas bon endroit")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas du tout bon endroit")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "daccord", type = "choro", leg_title = "En %")
mf_layout(
  title = "Bon endroit pour les pauvres",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas bon endroit pour les pauvres",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (Rapport)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nBon endroit / Pas bon endroit", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma commune est un bon endroit pour les pauvres",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
