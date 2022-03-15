#Q36. Thinking now about the future of London, how likely or unlikely would you say it is that each of the following will happen in the next five years? - The level of immigration to Paris/Paris et sa région will increase
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

#Création de la table de contingence : EPT & Q36_2
Q36_2 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q36_2)
#Transformation de la table en dataframe
Q36_2 = as.data.frame.matrix(Q36_2)
Q36_2$id <- row.names(Q36_2)
Q36_2$total <- Q36_2$`1` + Q36_2$`2` + Q36_2$`3` + Q36_2$`4` +Q36_2$`5` +Q36_2$`6`
Q36_2$p1 <- Q36_2$`1` / Q36_2$total * 100
Q36_2$p2 <- Q36_2$`2` / Q36_2$total * 100
Q36_2$p3 <- Q36_2$`3` / Q36_2$total * 100
Q36_2$p4 <- Q36_2$`4` / Q36_2$total * 100
Q36_2$p5 <- Q36_2$`5` / Q36_2$total * 100
Q36_2$p6 <- Q36_2$`6` / Q36_2$total * 100
Q36_2$likely <- Q36_2$p1 + Q36_2$p2
Q36_2$unlikely <- Q36_2$p3 + Q36_2$p4

Q36_2$res <- Q36_2$likely / Q36_2$unlikely
res_borough <- merge(fondlondres, Q36_2, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q36_2$p1, Q36_2$p2, Q36_2$p3, Q36_2$p4, Q36_2$p5, Q36_2$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très probable")
mf_map(res_borough, "p2", type = "choro", leg_title = "Probable")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Peu probable")
mf_map(res_borough, "p5", type = "choro", leg_title = "Très peu probable")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "likely", type = "choro",  leg_title = "En %")
mf_layout(
  title = "L'immigration va probablement augmenter",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "unlikely", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "L'immigration ne va probablement pas augmenter",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nProbable / Pas probable", leg_pos = "bottomleft2")
mf_layout(
  title = "L'immigration va augmenter dans les 5 prochaines années",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
