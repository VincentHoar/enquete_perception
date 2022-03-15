#Q37. Thinking about Paris/Paris et sa région in the next few years after the coronavirus pandemic, do you think each of the following will get better, get worse, or stay the same? - Affordability of housing (to buy or rent)


library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier IPSOS
IPSOSPARIS = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")

#Import du fond de carte
dep <- st_read('data/fondparis.gpkg', layer = "dep")
ept <- st_read('data/fondparis.gpkg', layer = "ept_sans")


#Import du fichier csv des communes
com_ept = read.csv("data/com_ept1.csv", sep = ";")

#Changement de nom dans le fichier IPSOS pour faciliter le merge
IPSOSPARIS = IPSOSPARIS %>%
  rename(
    code_insee = QMktSize_1_1
  )

#Jointure des deux fichiers précédents
mergeparisipsos = merge(IPSOSPARIS, com_ept, by = "code_insee")



#Création de la table de contingence : EPT & Q37_2
Q37_2 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q37_NEW_2)
#Transformation de la table en dataframe
Q37_2 = as.data.frame.matrix(Q37_2)
Q37_2$id <- row.names(Q37_2)
Q37_2$total <- Q37_2$`1` + Q37_2$`2` + Q37_2$`3` + Q37_2$`4` +Q37_2$`5`
Q37_2$p1 <- Q37_2$`1` / Q37_2$total * 100
Q37_2$p2 <- Q37_2$`2` / Q37_2$total * 100
Q37_2$p3 <- Q37_2$`3` / Q37_2$total * 100
Q37_2$p4 <- Q37_2$`4` / Q37_2$total * 100
Q37_2$p5 <- Q37_2$`5` / Q37_2$total * 100


Q37_2$res <- Q37_2$p1 / Q37_2$p2
res_ept <- merge(ept, Q37_2, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q37_2$p1, Q37_2$p2, Q37_2$p3, Q37_2$p4, Q37_2$p5)

# Cartes par réponses
par(mfrow = c(3,2))
mf_map(res_ept, "p1", type = "choro", leg_title = "Va s'améliorer")
mf_map(res_ept, "p2", type = "choro", leg_title = "Va se détériorer")
mf_map(res_ept, "p3", type = "choro", leg_title = "Va rester identique")
mf_map(res_ept, "p4", type = "choro", leg_title = "Ne sait pas")
mf_map(res_ept, "p5", type = "choro", leg_title = "Préfère ne rien dire")


# Résumé + - 
par(mfrow = c(1,3))
mf_map(res_ept, "p1", type = "choro",  leg_title = "En %")
mf_layout(
  title = "L'affordabilité des logements va s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "p2", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "L'affordabilité des logements va se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
mf_map(res_ept, "p3", type = "choro",pal = "Blues", leg_title = "En %",)
mf_layout(
  title = "L'affordabilité des logements va rester identique",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  

