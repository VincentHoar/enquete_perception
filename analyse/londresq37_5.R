#Q37. Thinking about London in the next few years after the coronavirus pandemic, do you think each of the following will get better, get worse, or stay the same? -  Hospitality (pubs, bars, restaurants and cafés)
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

#Création de la table de contingence : EPT & Q37_5
Q37_5 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q37_NEW_5)
#Transformation de la table en dataframe
Q37_5 = as.data.frame.matrix(Q37_5)
Q37_5$id <- row.names(Q37_5)
Q37_5$total <- Q37_5$`1` + Q37_5$`2` + Q37_5$`3` + Q37_5$`4` +Q37_5$`5`
Q37_5$p1 <- Q37_5$`1` / Q37_5$total * 100
Q37_5$p2 <- Q37_5$`2` / Q37_5$total * 100
Q37_5$p3 <- Q37_5$`3` / Q37_5$total * 100
Q37_5$p4 <- Q37_5$`4` / Q37_5$total * 100
Q37_5$p5 <- Q37_5$`5` / Q37_5$total * 100


Q37_5$res <- Q37_5$p1 / Q37_5$p2
res_borough <- merge(fondlondres, Q37_5, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q37_5$p1, Q37_5$p2, Q37_5$p3, Q37_5$p4, Q37_5$p5)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Va s'améliorer")
mf_map(res_borough, "p2", type = "choro", leg_title = "Va se détériorer")
mf_map(res_borough, "p3", type = "choro", leg_title = "Va rester identique")
mf_map(res_borough, "p4", type = "choro", leg_title = "Ne sait pas")
mf_map(res_borough, "p5", type = "choro", leg_title = "Préfère ne rien dire")


# Résumé + - 
par(mfrow = c(1,3))
mf_map(res_borough, "p1", type = "choro",  leg_title = "En %")
mf_layout(
  title = "L'hospitalité de la restauration va s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "p2", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "L'hospitalité de la restauration va se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
mf_map(res_borough, "p3", type = "choro",pal = "Blues", leg_title = "En %",)
mf_layout(
  title = "L'hospitalité de la restauration va rester identique",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
