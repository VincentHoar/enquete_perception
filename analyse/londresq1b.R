#Q1. What would you say is the most important issue facing Paris/votre commune today?

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

#Jointure
ipsl = merge(ipsoslondres, fondlondres, by = "BOROUGH")

# obtenir les labels de la question 1a dans un vecteur (mêmes pour toutes les thématiques de la Q1B)
labs <- levels(as_factor(ipsl$Q1B_1))
##Q1B_1
# pivoter la table & agréger les poids selons les réponse
rb1 <- tapply(ipsl$weight, list(ipsl$Q1B_1, ipsl$BOROUGH), sum)
# transformer la table
rb1 <- t(rb1)
# Part de chacune des réponses
rb1 <- 100 * rb1 / rowSums(rb1, na.rm = T)

b1 = as.data.frame(rb1)
b1$id = row.names(b1)
#merge
mrg <- merge(fondlondres, b1, by.x = "BOROUGH", by.y = "id", add.x = T)

#carto
mf_map(mrg, "1", "choro", leg_title = "Oui c'est un problème\nen %")
mf_title("Other important issue : The economy / employments opportunities")
