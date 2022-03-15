#Q23. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? 
#- Introducing more car-free days in inner-London, where cars are not allowed to drive into the centre of the city

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

#Création de la table de contingence : EPT & Q23_5
Q23_5 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q23_5)
#Transformation de la table en dataframe
Q23_5 = as.data.frame.matrix(Q23_5)
Q23_5$id <- row.names(Q23_5)
Q23_5$total <- Q23_5$`1` + Q23_5$`2` + Q23_5$`3` + Q23_5$`4` +Q23_5$`5` +Q23_5$`6`
Q23_5$p1 <- Q23_5$`1` / Q23_5$total * 100
Q23_5$p2 <- Q23_5$`2` / Q23_5$total * 100
Q23_5$p3 <- Q23_5$`3` / Q23_5$total * 100
Q23_5$p4 <- Q23_5$`4` / Q23_5$total * 100
Q23_5$p5 <- Q23_5$`5` / Q23_5$total * 100
Q23_5$p6 <- Q23_5$`6` / Q23_5$total * 100
Q23_5$daccord <- Q23_5$p1 + Q23_5$p2
Q23_5$pasdaccord <- Q23_5$p4 + Q23_5$p5
Q23_5$res <- Q23_5$daccord / Q23_5$pasdaccord
res_borough <- merge(fondlondres, Q23_5, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q23_5$p1, Q23_5$p2, Q23_5$p3, Q23_5$p4, Q23_5$p5, Q23_5$p6)

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
mf_map(res_borough, "daccord", type = "choro",  leg_title = "Pour")
mf_layout(
  title = "En faveur",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro",pal = "Reds", leg_title = "Contre",)
mf_layout(
  title = "En opposition",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (rapport)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Support à l'instauration de journées sans voitures dans Paris intra-muros",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

