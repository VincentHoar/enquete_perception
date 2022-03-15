#Q2.The things people can buy and do — their housing, furniture, food, cars, 
#recreation and travel — make up their standard of living. 
#How satisfied or dissatisfied do you feel about your standard of living at present?

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


#Création de la table de contingence : Boroughs & Q2
Q2 = table(ipsoslondres$BOROUGH, ipsoslondres$Q2)
#Transformation de la table en dataframe
Q2 = as.data.frame.matrix(Q2)
Q2$id <- row.names(Q2)
Q2$total <- Q2$`1` + Q2$`2` + Q2$`3` + Q2$`4` +Q2$`5` +Q2$`6`
Q2$p1 <- Q2$`1` / Q2$total * 100
Q2$p2 <- Q2$`2` / Q2$total * 100
Q2$p3 <- Q2$`3` / Q2$total * 100
Q2$p4 <- Q2$`4` / Q2$total * 100
Q2$p5 <- Q2$`5` / Q2$total * 100
Q2$p6 <- Q2$`6` / Q2$total * 100
Q2$satisfait <- Q2$p1 + Q2$p2
Q2$passatisfait <- Q2$p4 + Q2$p5
Q2$res <- Q2$satisfait / Q2$passatisfait
res_borough <- merge(fondlondres, Q2, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q2$p1, Q2$p2, Q2$p3, Q2$p4, Q2$p5, Q2$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très satisfait")
mf_map(res_borough, "p2", type = "choro", leg_title = "Satisfait")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas satisfait")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas du tout atisfait")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "satisfait", type = "choro", leg_title = "En %")
mf_layout(
  title = "Satisfait de leur niveau de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "passatisfait", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Non satisfait de leur niveau de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (NE FONCTIONNE PAS)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\nsatisfait / n. insatisfait")
mf_layout(
  title = "Satisfaction vis à vis du niveau de vie",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
