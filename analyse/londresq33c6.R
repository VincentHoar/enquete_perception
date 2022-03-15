#Q33C. To what extent are you worried or not about experiencing each of the following while living in Paris/votre commune? - Being physically attacked

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

#Création de la table de contingence : EPT & Q33C_6
Q33C_6 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q33C_6)
#Transformation de la table en dataframe
Q33C_6 = as.data.frame.matrix(Q33C_6)
Q33C_6$id <- row.names(Q33C_6)
Q33C_6$total <- Q33C_6$`1` + Q33C_6$`2` + Q33C_6$`3` + Q33C_6$`4` +Q33C_6$`5` +Q33C_6$`6`
Q33C_6$p1 <- Q33C_6$`1` / Q33C_6$total * 100
Q33C_6$p2 <- Q33C_6$`2` / Q33C_6$total * 100
Q33C_6$p3 <- Q33C_6$`3` / Q33C_6$total * 100
Q33C_6$p4 <- Q33C_6$`4` / Q33C_6$total * 100
Q33C_6$p5 <- Q33C_6$`5` / Q33C_6$total * 100
Q33C_6$p6 <- Q33C_6$`6` / Q33C_6$total * 100
Q33C_6$worried <- Q33C_6$p1 + Q33C_6$p2
Q33C_6$notworried <- Q33C_6$p3 + Q33C_6$p4

Q33C_6$res <- Q33C_6$worried / Q33C_6$notworried
res_borough <- merge(fondlondres, Q33C_6, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q33C_6$p1, Q33C_6$p2, Q33C_6$p3, Q33C_6$p4, Q33C_6$p5, Q33C_6$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très préoccupé")
mf_map(res_borough, "p2", type = "choro", leg_title = "Préoccupé")
mf_map(res_borough, "p3", type = "choro", leg_title = "Pas très préoccupé")
mf_map(res_borough, "p4", type = "choro", leg_title = "Très préoccupé")
mf_map(res_borough, "p5", type = "choro", leg_title = "Ne sait pas")
mf_map(res_borough, "p6", type = "choro", leg_title = "Préfère ne rien dire")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "worried", type = "choro",  leg_title = "En %")
mf_layout(
  title = "Préoccupé d'être attaqué physiquement",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "notworried", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas préoccupé d'être attaqué physiquement",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nPréoccupé / Pas préoccupé", leg_pos = "bottomleft2")
mf_layout(
  title = "Préoccupé ou non d'être attaqué physiquement ",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

