#Q37. Thinking about London in the next few years after the coronavirus pandemic, do you think each of the following will get better, get worse, or stay the same? - Culture (theatres and museums)
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

#Création de la table de contingence : EPT & Q37_6
Q37_6 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q37_NEW_6)
#Transformation de la table en dataframe
Q37_6 = as.data.frame.matrix(Q37_6)
Q37_6$id <- row.names(Q37_6)
Q37_6$total <- Q37_6$`1` + Q37_6$`2` + Q37_6$`3` + Q37_6$`4` +Q37_6$`5`
Q37_6$p1 <- Q37_6$`1` / Q37_6$total * 100
Q37_6$p2 <- Q37_6$`2` / Q37_6$total * 100
Q37_6$p3 <- Q37_6$`3` / Q37_6$total * 100
Q37_6$p4 <- Q37_6$`4` / Q37_6$total * 100
Q37_6$p5 <- Q37_6$`5` / Q37_6$total * 100


Q37_6$res <- Q37_6$p1 / Q37_6$p2
res_borough <- merge(fondlondres, Q37_6, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q37_6$p1, Q37_6$p2, Q37_6$p3, Q37_6$p4, Q37_6$p5)

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
  title = "La culture va s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "p2", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "La culture va se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
mf_map(res_borough, "p3", type = "choro",pal = "Blues", leg_title = "En %",)
mf_layout(
  title = "La culture va rester identique",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
