#Chargement des packages
library(sf)
library(dplyr)
library(sp)
library(mapsf)
library(haven)

#Import du fichier IPSOS
IPSOSPARIS = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")

#Import du fond de carte
dep <- st_read('data/fondparis.gpkg', layer = "dep")
ept_raw <- st_read('data/fondparis.gpkg', layer = "ept_sans")



#Import du fichier csv des communes
com_ept = read.csv("data/com_ept1.csv", sep = ";")

#Changement de nom dans le fichier IPSOS pour faciliter le merge
IPSOSPARIS = IPSOSPARIS %>%
  rename(
    code_insee = QMktSize_1_1
  )

#Jointure des deux fichiers précédents
ips = merge(IPSOSPARIS, com_ept, by = "code_insee")


# obtenir les labels de la question 1a dans un vecteur
labs <- levels(as_factor(ips$Q1A))


# pivoter la table & agréger les poids selons les réponse
r <- tapply(ips$weight, list(ips$Q1A, ips$ID_EPT), sum)
# transformer la table
r <- t(r)
# Part de chacune des réponses
r <- 100 * r / rowSums(r, na.rm = T)

# Quelle est la première réponse
prem <- apply(X = r, MARGIN = 1, FUN = which.max)
labs[prem]
# Sans surprise c'est bien le covid

# deuxième réponse (sans covid, other & don't know)
sec <- (apply(r[,-c(20, 22, 23)], 1, FUN = which.max))
labs[-c(20, 22, 23)][sec]

# ajouter une colonne avex la 2eme modalité la plus fréquente 
rt <- as.data.frame(r)
rt$top <- labs[-c(20, 22, 23)][sec]

# Ajouter une colonne avec la valeur correspondante
a <- matrix(c(1:nrow(r), sec), nrow = nrow(r), ncol = 2)
rt$lab <- round(r[,-c(20, 22, 23)][a], 1)



# joindre à l'objet sf
rt$id <- row.names(rt)
ept <- merge(ept_raw, rt, by.x = "ID_EPT", by.y = "id", add.x = T)


# carto
mf_export(expandBB = c(.45,0,0,0),ept, filename = "paris_issue2.svg")
mf_map(ept, "top", "typo", pal  ='Dynamic',)
mf_title("Most important issue (2nd)")
mf_label(ept, "lab")
dev.off()

