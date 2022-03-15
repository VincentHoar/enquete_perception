#Q21. How concerned, if at all, are you about each of the following? - Loos of green spaced and urban biodiversity

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

#Création de la table de contingence : EPT & Q21_3
Q21_3 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q21_3)
#Transformation de la table en dataframe
Q21_3 = as.data.frame.matrix(Q21_3)
Q21_3$id <- row.names(Q21_3)
Q21_3$total <- Q21_3$`1` + Q21_3$`2` + Q21_3$`3` + Q21_3$`4` +Q21_3$`5`
Q21_3$p1 <- Q21_3$`1` / Q21_3$total * 100
Q21_3$p2 <- Q21_3$`2` / Q21_3$total * 100
Q21_3$p3 <- Q21_3$`3` / Q21_3$total * 100
Q21_3$p4 <- Q21_3$`4` / Q21_3$total * 100
Q21_3$p5 <- Q21_3$`5` / Q21_3$total * 100
Q21_3$concerned <- Q21_3$p1 + Q21_3$p2
Q21_3$notconcerned <- Q21_3$p3 + Q21_3$p4
Q21_3$res <- Q21_3$concerned - Q21_3$notconcerned
res_borough <- merge(fondlondres, Q21_3, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q21_3$p1, Q21_3$p2, Q21_3$p3, Q21_3$p4, Q21_3$p5, Q21_3$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très concerné")
mf_map(res_borough, "p2", type = "choro", leg_title = "Pas concerné")
mf_map(res_borough, "p3", type = "choro", leg_title = "Pas très concerné")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas concerné du tout")
mf_map(res_borough, "p5", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "concerned", type = "choro", leg_title = "En %")
mf_layout(
  title = "Concerné",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "notconcerned", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Pas ou peu concerné",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (Différence)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Différence\nConcerné / Pas concerné", leg_pos = "bottomleft2")
mf_layout(
  title = "Concerné ou non par la perte d'espaces verts et de biodiversité urbaine",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  

