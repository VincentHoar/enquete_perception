#Q13. To what extent, if at all, would you say Paris/votre commune is a good place to live for the following groups of people? - People like me

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

#Création de la table de contingence : EPT & Q13_6
Q13_6 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q13_6)
#Transformation de la table en dataframe
Q13_6 = as.data.frame.matrix(Q13_6)
Q13_6$id <- row.names(Q13_6)
Q13_6$total <- Q13_6$`1` + Q13_6$`2` + Q13_6$`3` + Q13_6$`4` +Q13_6$`5` +Q13_6$`6`
Q13_6$p1 <- Q13_6$`1` / Q13_6$total * 100
Q13_6$p2 <- Q13_6$`2` / Q13_6$total * 100
Q13_6$p3 <- Q13_6$`3` / Q13_6$total * 100
Q13_6$p4 <- Q13_6$`4` / Q13_6$total * 100
Q13_6$p5 <- Q13_6$`5` / Q13_6$total * 100
Q13_6$p6 <- Q13_6$`6` / Q13_6$total * 100
Q13_6$daccord <- Q13_6$p1 + Q13_6$p2
Q13_6$pasdaccord <- Q13_6$p4 + Q13_6$p5
Q13_6$res <- Q13_6$daccord - Q13_6$pasdaccord
res_borough <- merge(fondlondres, Q13_6, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q13_6$p1, Q13_6$p2, Q13_6$p3, Q13_6$p4, Q13_6$p5, Q13_6$p6)

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
  title = "Bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (Différence)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Différence\nBon endroit / Pas bon endroit", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma commune est un bon endroit pour les gens comme moi",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  

