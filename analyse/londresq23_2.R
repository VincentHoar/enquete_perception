#Q23. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? 
#-  Encouraging people not to use a car to travel to work or school

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

#Création de la table de contingence : EPT & Q23_2
Q23_2 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q23_2)
#Transformation de la table en dataframe
Q23_2 = as.data.frame.matrix(Q23_2)
Q23_2$id <- row.names(Q23_2)
Q23_2$total <- Q23_2$`1` + Q23_2$`2` + Q23_2$`3` + Q23_2$`4` +Q23_2$`5` +Q23_2$`6`
Q23_2$p1 <- Q23_2$`1` / Q23_2$total * 100
Q23_2$p2 <- Q23_2$`2` / Q23_2$total * 100
Q23_2$p3 <- Q23_2$`3` / Q23_2$total * 100
Q23_2$p4 <- Q23_2$`4` / Q23_2$total * 100
Q23_2$p5 <- Q23_2$`5` / Q23_2$total * 100
Q23_2$p6 <- Q23_2$`6` / Q23_2$total * 100
Q23_2$daccord <- Q23_2$p1 + Q23_2$p2
Q23_2$pasdaccord <- Q23_2$p4 + Q23_2$p5
Q23_2$res <- Q23_2$daccord / Q23_2$pasdaccord
res_borough <- merge(fondlondres, Q23_2, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q23_2$p1, Q23_2$p2, Q23_2$p3, Q23_2$p4, Q23_2$p5, Q23_2$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Soutient fortement")
mf_map(res_borough, "p2", type = "choro", leg_title = "Soutient")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Ne soutient pas")
mf_map(res_borough, "p5", type = "choro", leg_title = "Ne soutient pas du tout")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "daccord", type = "choro",  leg_title = "En %")
mf_layout(
  title = "En faveur",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "En opposition",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Support d'une sensibilisation à ne pas utiliser la voiture pour aller à l'école ou au travail",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

