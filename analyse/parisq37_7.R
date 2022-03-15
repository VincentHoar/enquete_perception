#Q37. Thinking about Paris/Paris et sa région in the next few years after the coronavirus pandemic, do you think each of the following will get better, get worse, or stay the same? - Public services (schools, hospitals, etc)


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



#Création de la table de contingence : EPT & Q37_7
Q37_7 = table(mergeparisipsos$ID_EPT, mergeparisipsos$GRID_Q37_NEW_7)
#Transformation de la table en dataframe
Q37_7 = as.data.frame.matrix(Q37_7)
Q37_7$id <- row.names(Q37_7)
Q37_7$total <- Q37_7$`1` + Q37_7$`2` + Q37_7$`3` + Q37_7$`4` +Q37_7$`5`
Q37_7$p1 <- Q37_7$`1` / Q37_7$total * 100
Q37_7$p2 <- Q37_7$`2` / Q37_7$total * 100
Q37_7$p3 <- Q37_7$`3` / Q37_7$total * 100
Q37_7$p4 <- Q37_7$`4` / Q37_7$total * 100
Q37_7$p5 <- Q37_7$`5` / Q37_7$total * 100


Q37_7$res <- Q37_7$p1 / Q37_7$p2
res_ept <- merge(ept, Q37_7, by.x = "ID_EPT", by.y = "id", all.x =TRUE)



# repartition
boxplot(Q37_7$p1, Q37_7$p2, Q37_7$p3, Q37_7$p4, Q37_7$p5)

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
  title = "Les services publics vont s'améliorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)
mf_map(res_ept, "p2", type = "choro",pal = "Reds", leg_title = "En %",)
mf_layout(
  title = "Les services publics vont se détériorer",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  
mf_map(res_ept, "p3", type = "choro",pal = "Blues", leg_title = "En %",)
mf_layout(
  title = "Les services publics vont rester identiques",
  credits = "IPSOS, Hoareau 2022",
  scale = 5, 2, 1.2
)  

