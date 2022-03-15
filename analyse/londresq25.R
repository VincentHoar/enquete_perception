#Q25. To what extent do you agree or disagree that your local area is a place where people from different social, ethnic or religious backgrounds get on well together?

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

#Création de la table de contingence : EPT & Q25
Q25 = table(ipsoslondres$BOROUGH, ipsoslondres$Q25)
#Transformation de la table en dataframe
Q25 = as.data.frame.matrix(Q25)
Q25$id <- row.names(Q25)
Q25$total <- Q25$`1` + Q25$`2` + Q25$`3` + Q25$`4` +Q25$`5` +Q25$`6` +Q25$`7`
Q25$p1 <- Q25$`1` / Q25$total * 100
Q25$p2 <- Q25$`2` / Q25$total * 100
Q25$p3 <- Q25$`3` / Q25$total * 100
Q25$p4 <- Q25$`4` / Q25$total * 100
Q25$p5 <- Q25$`5` / Q25$total * 100
Q25$p6 <- Q25$`6` / Q25$total * 100
Q25$p7 <- Q25$`7` / Q25$total * 100
Q25$daccord <- Q25$p1 + Q25$p2
Q25$pasdaccord <- Q25$p3 + Q25$p4
Q25$autre <- Q25$p5 + Q25$p6 + Q25$p7
Q25$res <- Q25$daccord / Q25$pasdaccord
res_borough <- merge(fondlondres, Q25, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q25$p1, Q25$p2, Q25$p3, Q25$p4, Q25$p5, Q25$p6, Q25$p7)

# Cartes par réponses
par(mfrow = c(4,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "D'accord")
mf_map(res_borough, "p2", type = "choro", leg_title = "Plutôt d'accord")
mf_map(res_borough, "p3", type = "choro", leg_title = "Plutôt pas d'accord")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas du tout d'accord")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas assez de personnes")
mf_map(res_borough, "p6", type = "choro", leg_title = "Tous le même milieu")
mf_map(res_borough, "p7", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,3))
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
mf_map(res_borough, "autre", type = "choro",pal = "Blues", leg_title = "En %",)
mf_layout(
  title = "Autre (même milieu, NSP ou peu de personnes)",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (nn)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nSoutien / Opposition", leg_pos = "bottomleft2")
mf_layout(
  title = "Your local area is a place where people from different backgrounds get on well together?",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  

