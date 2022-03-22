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



library(leaflet)
ept4326 <- st_transform(ept, 4326)

leaflet() %>%
  addPolygons(data = ept4326, popup = ept4326$ID_EPT, fillColor = topo.colors(9), stroke = FALSE)

paltop <- colorFactor(
  palette = 'Set3',
  domain = ept4326$top,
)
?setview


leaflet(ept4326) %>%
  addPolygons(opacity = 100, 
              color = "black", 
              weight = 0.25,
              options = list(clickable = FALSE), 
              fill = T, fillColor = ~paltop(ept4326$top), 
              fillOpacity = 0.8,
              popup = ept4326$ID_EPT)%>%
    addLegend("bottomleft", pal = paltop, values = ept4326$top, title = "2nd most important issue",
            )