#Q14A. How likely, if at all, is it that you will move out of Paris/votre commune in the next five years?

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

#Création de la table de contingence : EPT & Q14A
Q14A = table(ipsoslondres$BOROUGH, ipsoslondres$Q14A)
#Transformation de la table en dataframe
Q14A = as.data.frame.matrix(Q14A)
Q14A$id <- row.names(Q14A)
Q14A$total <- Q14A$`1` + Q14A$`2` + Q14A$`3` + Q14A$`4` +Q14A$`5` +Q14A$`6`+ Q14A$`7`
Q14A$p1 <- Q14A$`1` / Q14A$total * 100
Q14A$p2 <- Q14A$`2` / Q14A$total * 100
Q14A$p3 <- Q14A$`3` / Q14A$total * 100
Q14A$p4 <- Q14A$`4` / Q14A$total * 100
Q14A$p5 <- Q14A$`5` / Q14A$total * 100
Q14A$p6 <- Q14A$`6` / Q14A$total * 100
Q14A$p7 <- Q14A$`7` / Q14A$total * 100
Q14A$daccord <- Q14A$p1 + Q14A$p2
Q14A$pasdaccord <- Q14A$p4 + Q14A$p5
Q14A$res <- Q14A$daccord - Q14A$pasdaccord
res_borough <- merge(fondlondres, Q14A, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q14A$p1, Q14A$p2, Q14A$p3, Q14A$p4, Q14A$p5, Q14A$p6)

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

