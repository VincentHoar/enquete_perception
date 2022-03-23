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

# obtenir les labels de la question 1a dans un vecteur
labsl <- levels(as_factor(ipsl$Q1A))


# pivoter la table & agréger les poids selons les réponse
r <- tapply(ipsl$weight, list(ipsl$Q1A, ipsl$BOROUGH), sum)
# transformer la table
r <- t(r)
# Part de chacune des réponses
r <- 100 * r / rowSums(r, na.rm = T)

# Quelle est la première réponse
prem <- apply(X = r, MARGIN = 1, FUN = which.max)
labsl[prem]
# Il y a 4 "Cost of living" à la première place et 1 "Crime and policing", le reste est la pandémie de Covid

#Création du dataframe pour réprésenter les principaux enjeux
ld1 = as.data.frame(prem)
ld1$id = row.names(ld1)
ld1$top = labsl[prem]

#Jointure avec le fond de carte
londres1 = merge(fondlondres, ld1, by.x = "BOROUGH", by.y = "id", add.x = T)

#Création de la carte
mf_map(londres1, "top", "typo", pal = "Dynamic")
mf_title("Most important issue (1st)")

# deuxième réponse (sans covid, other & don't know)
sec <- (apply(r[,-c(20, 22, 23)], 1, FUN = which.max))
labsl[-c(20, 22, 23)][sec]

# ajouter une colonne avex la 2eme modalité la plus fréquente 
rt <- as.data.frame(r)
rt$top <- labsl[-c(20, 22, 23)][sec]

# Ajouter une colonne avec la valeur correspondante
a <- matrix(c(1:nrow(r), sec), nrow = nrow(r), ncol = 2)
rt$lab <- round(r[,-c(20, 22, 23)][a], 1)



# joindre à l'objet sf
rt$id <- row.names(rt)
borough <- merge(fondlondres, rt, by.x = "BOROUGH", by.y = "id", add.x = T)

#Le cout de la vie s'étend un peu partout + le "crime and policing". La city avec l'économie semble logique.

library(leaflet)

#Pour most important issue 

londres4326 = st_transform(londres1, 4326)

pallondres <- colorFactor(
  palette = 'Set3',
  domain = londres4326$top,
)

leaflet(londres4326) %>%
  addPolygons(opacity = 100, 
              color = "black", 
              weight = 0.25,
              options = list(clickable = FALSE), 
              fill = T, fillColor = ~pallondres(londres4326$top), 
              fillOpacity = 0.8,
              popup = londres4326$BOROUGH)%>%
  addLegend("bottomleft", pal = pallondres, values = londres4326$top, title = "2nd most important issue",
  )


#Pour 2nd most important issue

borough4326 = st_transform(borough,4326)

palborough <- colorFactor(
  palette = 'Set3',
  domain = borough4326$top,
)

leaflet(borough4326) %>%
  addPolygons(opacity = 100, 
              color = "black", 
              weight = 0.25,
              options = list(clickable = FALSE), 
              fill = T, fillColor = ~palborough(borough4326$top), 
              fillOpacity = 0.8,
              popup = borough4326$BOROUGH)%>%
  addLegend("bottomleft", pal = palborough, values = borough4326$top, title = "2nd most important issue",
  )
