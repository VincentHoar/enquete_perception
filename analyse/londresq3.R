##Q3. And do you expect that your standard of living will improve, get worse, or stay the same, over the next 5 years?

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


#Création de la table de contingence : Boroughs & Q3
Q3 = table(ipsoslondres$BOROUGH, ipsoslondres$Q3)
#Transformation de la table en dataframe
Q3 = as.data.frame.matrix(Q3)
Q3$id <- row.names(Q3)
Q3$total <- Q3$`1` + Q3$`2` + Q3$`3` + Q3$`4` +Q3$`5` +Q3$`6`
Q3$p1 <- Q3$`1` / Q3$total * 100
Q3$p2 <- Q3$`2` / Q3$total * 100
Q3$p3 <- Q3$`3` / Q3$total * 100
Q3$p4 <- Q3$`4` / Q3$total * 100
Q3$p5 <- Q3$`5` / Q3$total * 100
Q3$p6 <- Q3$`6` / Q3$total * 100
Q3$optimiste <- Q3$p1 + Q3$p2
Q3$pessimiste <- Q3$p4 + Q3$p5
Q3$res <- Q3$optimiste / Q3$pessimiste
res_borough <- merge(fondlondres, Q3, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q3$p1, Q3$p2, Q3$p3, Q3$p4, Q3$p5, Q3$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Très optimiste")
mf_map(res_borough, "p2", type = "choro", leg_title = "Optimiste")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pessimiste")
mf_map(res_borough, "p5", type = "choro", leg_title = "Très pessimiste")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "optimiste", type = "choro", leg_title = "En %")
mf_layout(
  title = "Vision optimiste de l'avenir",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pessimiste", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Vision pessimiste de l'avenir",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\noptimiste / n. inoptimiste")
mf_layout(
  title = "Vision plutôt optimiste ou pessimiste ?",
  credits = "IPSOS, Hoareau 2022",
  scale = 4, 2, 1.2
)  
