#Q28. Overall, would you say you have a positive or negative opinion of London?

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

#Création de la table de contingence : EPT & Q28
Q28 = table(ipsoslondres$BOROUGH, ipsoslondres$Q28)
#Transformation de la table en dataframe
Q28 = as.data.frame.matrix(Q28)
Q28$id <- row.names(Q28)
Q28$total <- Q28$`1` + Q28$`2` + Q28$`3` + Q28$`4` +Q28$`5` +Q28$`6`
Q28$p1 <- Q28$`1` / Q28$total * 100
Q28$p2 <- Q28$`2` / Q28$total * 100
Q28$p3 <- Q28$`3` / Q28$total * 100
Q28$p4 <- Q28$`4` / Q28$total * 100
Q28$p5 <- Q28$`5` / Q28$total * 100
Q28$p6 <- Q28$`6` / Q28$total * 100
Q28$daccord <- Q28$p1 + Q28$p2
Q28$pasdaccord <- Q28$p4 + Q28$p5

Q28$res <- Q28$daccord / Q28$pasdaccord
res_borough <- merge(fondlondres, Q28, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q28$p1, Q28$p2, Q28$p3, Q28$p4, Q28$p5, Q28$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Positive")
mf_map(res_borough, "p2", type = "choro", leg_title = "Plutôt positive")
mf_map(res_borough, "p3", type = "choro", leg_title = "Plutôt négative")
mf_map(res_borough, "p4", type = "choro", leg_title = "Négative")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas assez de personnes")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "D'accord",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas d'accord",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (non)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "L'opinion sur Londres",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

