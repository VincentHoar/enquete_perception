library(sf)
library(mapsf)
library(haven)




prep = function(ville = "Paris"){
  if (ville == "Paris"){
    survey = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - Paris FR SPSS V1_240621_Client Use Only.sav")
    hab = st_read('data/paris.gpkg', layer = "hab", quiet = TRUE)
    basemap = st_read('data/paris.gpkg', layer = "basemap", quiet = TRUE)
    dico = read.csv("data/com_ept1.csv", sep = ";")
    dico = dico[, c(1,3)]
    names(dico)[2] = "ID_GEO"
    survey = merge(survey, dico, by.x = "QMktSize_1_1", by.y = "code_insee")
    return(list(survey = survey, hab = hab, basemap = basemap))
  }
  if (ville == "London"){
    survey = read_sav("data-raw/21-032532-01 Kings UdP London Paris 2021 Survey - London UK SPSS V1_240621_Client Use Only.sav")
    hab = st_read('data/london.gpkg', layer = "hab",quiet = TRUE)
    basemap = st_read('data/london.gpkg', layer = "basemap", quiet = TRUE)
    survey$ID_GEO = survey$QMktSize_2_1
    return(list(survey = as.data.frame(survey), hab = hab, basemap = basemap))
  }
}

# names(survey)
# 
# 
# x = prep("London")
# 
# question = "Q6"
# mod_pos = c(1, 2)
# mod_neg = c(4, 5)
# mod_oth = c(3, 6)

geoagg = function(x, question, mod_pos, mod_neg, mod_oth){
  i_pos = x$survey[[question]] %in% mod_pos
  i_neg = x$survey[[question]] %in% mod_neg
  i_oth = x$survey[[question]] %in% mod_oth
  
  pos = aggregate(list(pos = x$survey[i_pos, "weight", drop = TRUE]), by = list(ID_GEO = x$survey[i_pos, "ID_GEO"]), sum)
  neg = aggregate(list(neg = x$survey[i_neg, "weight", drop = TRUE]), by = list(ID_GEO = x$survey[i_neg, "ID_GEO"]), sum)
  oth = aggregate(list(oth = x$survey[i_oth, "weight", drop = TRUE]), by = list(ID_GEO = x$survey[i_oth, "ID_GEO"]), sum)
  
  bm = x$basemap
  
  bm = merge(bm, pos, by = "ID_GEO", all.x = TRUE)
  bm = merge(bm, neg, by = "ID_GEO", all.x = TRUE)
  bm = merge(bm, oth, by = "ID_GEO", all.x = TRUE)
  
  bm[is.na(bm[['pos']]), "pos"] = 0
  bm[is.na(bm[['neg']]), "neg"] = 0
  bm[is.na(bm[['oth']]), "oth"] = 0
  
  bm$tot = bm$pos + bm$neg + bm$oth
  # bm$tot = apply(st_set_geometry(bm[,3:6], NULL), 1, sum, na.rm= T)
  
  bm$spos = bm$pos / bm$tot * 100
  bm$sneg = bm$neg / bm$tot * 100
  bm$soth = bm$oth / bm$tot * 100
  return(bm)
}


