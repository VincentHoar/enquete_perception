### Q11B. Thinking now about living in Paris/votre commune, to what extent do you agree or disagree with the following statements? 
#Paris/ma commune is a good place for people like me to start a career

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

#Création de la table de contingence : EPT & Q11_1
Q11_1 = table(ipsoslondres$BOROUGH, ipsoslondres$GRID_Q11B_5)
#Transformation de la table en dataframe
Q11_1 = as.data.frame.matrix(Q11_1)
Q11_1$id <- row.names(Q11_1)
Q11_1$total <- Q11_1$`1` + Q11_1$`2` + Q11_1$`3` + Q11_1$`4` +Q11_1$`5` +Q11_1$`6`
Q11_1$p1 <- Q11_1$`1` / Q11_1$total * 100
Q11_1$p2 <- Q11_1$`2` / Q11_1$total * 100
Q11_1$p3 <- Q11_1$`3` / Q11_1$total * 100
Q11_1$p4 <- Q11_1$`4` / Q11_1$total * 100
Q11_1$p5 <- Q11_1$`5` / Q11_1$total * 100
Q11_1$p6 <- Q11_1$`6` / Q11_1$total * 100
Q11_1$daccord <- Q11_1$p1 + Q11_1$p2
Q11_1$pasdaccord <- Q11_1$p4 + Q11_1$p5
#Soustraction au lieu d'un rapport comme les autres questions
Q11_1$res <- Q11_1$daccord - Q11_1$pasdaccord
res_borough <- merge(fondlondres, Q11_1, by.x = "BOROUGH", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q11_1$p1, Q11_1$p2, Q11_1$p3, Q11_1$p4, Q11_1$p5, Q11_1$p6)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_borough, "p1", type = "choro", leg_title = "Tout à fait d'accord")
mf_map(res_borough, "p2", type = "choro", leg_title = "D'accord")
mf_map(res_borough, "p3", type = "choro", leg_title = "Neutre")
mf_map(res_borough, "p4", type = "choro", leg_title = "Pas d'accord")
mf_map(res_borough, "p5", type = "choro", leg_title = "Pas du tout d'accord")
mf_map(res_borough, "p6", type = "choro", leg_title = "Ne sait pas")

# Résumé + - 
par(mfrow = c(1,2))
mf_map(res_borough, "daccord", type = "choro", pal = "Reds", leg_title = "En %")
mf_layout(
  title = "Ma commune est un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_borough, "pasdaccord", type = "choro", leg_title = "En %")
mf_layout(
  title = "Ma commune n'est pas un bon endroit",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
# résumé 1 carte (NE FONCTIONNE PAS)
par(mfrow =c(1,1))
mf_map(res_borough, "res", type = "choro", pal = "Geyser", breaks = "quantile", nbreak=4, leg_title = "Différence\nd'accord / pas d'accord", leg_pos = "bottomleft2")
mf_layout(
  title = "Différence entre le pourcentage de 'd'accord' et le pourcentage de 'pas d'accord' ",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  

