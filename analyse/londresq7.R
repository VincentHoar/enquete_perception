##Q7. And do you expect that the quality of local services such as schools, transport and police will improve, get worse, or stay the same over the next five years?

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


#Création de la table de contingence : Boroughs & Q7
Q7 = table(ipsoslondres$BOROUGH, ipsoslondres$Q7)
#Transformation de la table en dataframe
Q7 = as.data.frame.matrix(Q7)
Q7$id <- row.names(Q7)
Q7$total <- Q7$`1` + Q7$`2` + Q7$`3` + Q7$`4` +Q7$`5` +Q7$`6`
Q7$p1 <- Q7$`1` / Q7$total * 100
Q7$p2 <- Q7$`2` / Q7$total * 100
Q7$p3 <- Q7$`3` / Q7$total * 100
Q7$p4 <- Q7$`4` / Q7$total * 100
Q7$p5 <- Q7$`5` / Q7$total * 100
Q7$p6 <- Q7$`6` / Q7$total * 100
Q7$optimiste <- Q7$p1 + Q7$p2
Q7$pessimiste <- Q7$p4 + Q7$p5
Q7$res <- Q7$optimiste / Q7$pessimiste
res_borough <- merge(fondlondres, Q7, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q7$p1, Q7$p2, Q7$p3, Q7$p4, Q7$p5, Q7$p6)

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
  title = "Les services publics vont s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pessimiste", type = "choro", pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Les services publics vont se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Rapport\noptimiste / pessimiste", leg_pos = "bottomleft2")
mf_layout(
  title = "Quelle évolution pour la qualité des services publics?",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  