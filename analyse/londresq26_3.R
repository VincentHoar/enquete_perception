#Q26. Thinking now about immigration to Paris/votre commune, to what extent do you agree or disagree with the following statements? 
#- I think Paris/Ma commune needs more immigration than the present level

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

#Création de la table de contingence : EPT & Q26_3
Q26_3 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q26_3)
#Transformation de la table en dataframe
Q26_3 = as.data.frame.matrix(Q26_3)
Q26_3$id <- row.names(Q26_3)
Q26_3$total <- Q26_3$`1` + Q26_3$`2` + Q26_3$`3` + Q26_3$`4` +Q26_3$`5` +Q26_3$`6`
Q26_3$p1 <- Q26_3$`1` / Q26_3$total * 100
Q26_3$p2 <- Q26_3$`2` / Q26_3$total * 100
Q26_3$p3 <- Q26_3$`3` / Q26_3$total * 100
Q26_3$p4 <- Q26_3$`4` / Q26_3$total * 100
Q26_3$p5 <- Q26_3$`5` / Q26_3$total * 100
Q26_3$p6 <- Q26_3$`6` / Q26_3$total * 100
Q26_3$daccord <- Q26_3$p1 + Q26_3$p2
Q26_3$pasdaccord <- Q26_3$p4 + Q26_3$p5

Q26_3$res <- Q26_3$daccord / Q26_3$pasdaccord
res_borough <- merge(fondlondres, Q26_3, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q26_3$p1, Q26_3$p2, Q26_3$p3, Q26_3$p4, Q26_3$p5, Q26_3$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "D'accord")
mf_map(res_borough, "p2", type = "choro", leg_title = "Plutôt d'accord")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas d'accord")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas du tout d'accord")
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
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Ma ville a besoin de plus d'immigration",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

