# 2022-02-14

- Mettre le shapefile des communes dans data-raw OK

- Mettre une table de passage 'com_ept.csv' com->EPT dans data (séparateur virgule, ID_EPT, NOM_EPT, ID_COM, NOM_COM). Rassembler les communes hors GP dans une seule zone. OK

- créer un script analyse/creation_fond_de_carte_paris.R ou on créé le fond de carte des EPT grace au fond de carte des communes et au fichier com_ept.csv. 
    - importer fond de carte des communes (st_read()) OK
    - importer le fichier com_ept.csv (read.csv) OK
    - joindre le fond de carte des communes et le fichier com_ept.csv (merge()) OK
    - agreger les communes selon le code EPT (agregate())
    - sauver le résultat dans data (paris.gpkg, couches = EPT , com)


Dans le fichier analyse/paris.R
- importer le fichier ipsos
- importer le fichier com_ept.csv
- joindre les deux fichiers (merge)
- agreger selon le code EPT pour obtenir le nombre d'enquétés par zone (table EPT, nb enquétés)
- Créer la carte du nombre d'enquétés par EPT (utiliser la fonction mf_export())



Faire la même chose pour Londres. 
- shapefile des boroughs dans data-raw
Dans le fichier analyse/londres.R
- importer le fichier ipsos
- agreger selon le le code boroughs pour obtenir le nombre d'enquétés par zone (table bor, nb enquétés)
- Créer la carte du nombre d'enquétés par boroughs (utiliser la fonction mf_export())


# 2022-02-18
- Mettre le shapefile des communes dans data-raw OK
=> Un shp avec toutes les communes dont Paris OK
- Mettre une table de passage 'com_ept.csv' com->EPT dans data (séparateur virgule, ID_EPT, NOM_EPT, ID_COM, NOM_COM). Rassembler les communes hors GP dans une seule zone. OK
=> rajouter Paris et mettre tous les EPT d'IDF (pourquoi ???)
- Mettre un shp des départements d'IDF (dans le dossier data-raw) OK

- créer un script analyse/creation_fond_de_carte_paris.R ou on créé le fond de carte des EPT grace au fond de carte des communes et au fichier com_ept.csv. 
    - importer fond de carte des communes (st_read()) OK
    - importer le fichier com_ept.csv (read.csv) OK
    - joindre le fond de carte des communes et le fichier com_ept.csv (merge()) OK
    - agreger les communes selon le code EPT (agregate()) OK
    - sauver le résultat dans data (paris.gpkg, couches = EPT , com) (comment faire pour avoir un gpkg a plusieurs couches ??? == CF MAIL ("st_write"))
=> sauvegarder le resultat

Dans le fichier analyse/paris.R
- importer le fichier ipsos OK
- importer le fichier com_ept.csv OK
- joindre les deux fichiers (merge) OK
- agreger selon le le code EPT pour obtenir le nombre d'enquétés par zone (table EPT, nb enquétés) OK
- Créer la carte du nombre d'enquétés par EPT (utiliser la fonction mf_export()) 


Faire la même chose pour Londres. 
- shapefile des boroughs dans data-raw OK
Dans le fichier analyse/londres.R
- importer le fichier ipsos OK
- agreger selon le le code boroughs pour obtenir le nombre d'enquétés par zone (table bor, nb enquétés) OK
- Créer la carte du nombre d'enquétés par boroughs (utiliser la fonction mf_export()) OK

=> Utiliser mf_export() avec format svg (plus de problemes sur l'apsect des cercles) (PQ NE MARCHE PAS ?)


- Installation GIT
=> on verra à la rentrée, d'ici là tu continues à bosser en local dans le même dossier. 


- Rapport de stage
    - contexte de travail
    - contexte du stage (le contenu)
    - description du travail effectué
(TG) à la rentrée, te donner des exemplaires de mémoires des années précédentes. 


- Prendre une question de ton choix
    - proposer des représentations carto. ok
    Q11 : faire des typologies d'EPT/boroughs en fonction des critères d'emménagement, par exemple : 
    quartier pas cher et bcp d'espaces verts / quartiers accessibles et avec du travail
    - Des cartes en cercles proportionnels, des choroplèthes, typo, une carte original
  essayer de différencier les types de carte pour éviter au lecteur de devoir lire les mêmes types de cartes
  + permet de toucher un peu à tout grâce aux différentes manipulations R
    - Réfléchir à des traitements stats (ACP?, CAH?) ok
  Un mélange des 2, voire traitements plus simples (Q23 : % de gens inquiets pour les espaces verts par exemple)

Pour inspiration : https://larmarange.github.io/analyse-R/


=> Mettre cette reflexion dans Rmarkdown.



## 2022-02-28

A partir du code de parisQ4.R 
Refaire des séries de cartes pour toutes les questions utilisant un codage lickert (1-2-3...)

Rajouter toute la documentation dans le dossier **docs**






