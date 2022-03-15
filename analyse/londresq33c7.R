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

#Création de la table de contingence : EPT & Q33c_7
Q33c_7 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q33C_7)
#Transformation de la table en dataframe
Q33c_7 = as.data.frame.matrix(Q33c_7)
Q33c_7$id <- row.names(Q33c_7)
Q33c_7$total <- Q33c_7$`1` + Q33c_7$`2` + Q33c_7$`3` + Q33c_7$`4` +Q33c_7$`5` +Q33c_7$`6`
Q33c_7$p1 <- Q33c_7$`1` / Q33c_7$total * 100
Q33c_7$p2 <- Q33c_7$`2` / Q33c_7$total * 100
Q33c_7$p3 <- Q33c_7$`3` / Q33c_7$total * 100
Q33c_7$p4 <- Q33c_7$`4` / Q33c_7$total * 100
Q33c_7$p5 <- Q33c_7$`5` / Q33c_7$total * 100
Q33c_7$p6 <- Q33c_7$`6` / Q33c_7$total * 100
Q33c_7$worried <- Q33c_7$p1 + Q33c_7$p2
Q33c_7$notworried <- Q33c_7$p3 + Q33c_7$p4

Q33c_7$res <- Q33c_7$worried / Q33c_7$notworried
res_borough <- merge(fondlondres, Q33c_7, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q33c_7$p1, Q33c_7$p2, Q33c_7$p3, Q33c_7$p4, Q33c_7$p5, Q33c_7$p6)

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
mf_map(res_borough, "worried", type = "choro", pal = "Reds", leg_title = "En %")
mf_layout(
  title = "Préoccupé",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "notworried", type = "choro",pal = "Greens", leg_title = "En %",)
mf_layout(
  title = "Pas préoccupé",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte 
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nPréoccupé / Pas préoccupé", leg_pos = "bottomleft2")
mf_layout(
  title = "Préoccupé ou pas d'être forcé physiquement à avoir des rapports sexuels ",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

