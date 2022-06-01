source("gen_satisfy.R")

# import survey
x = prep("London")
y = prep("Paris")



# keep only relevant vars
tab1 <- x$survey
ind <- (substr(colnames(tab1), 1,4) %in% c("Q14B", "ID_G", "weig"))
tab1 <- tab1[, ind]
tab2 <- y$survey
ind <- (substr(colnames(tab2), 1,4) %in% c("Q14B", "ID_G", "weig"))
tab2 <- tab2[, ind]




# merge london and paris
tab <- rbind(tab1, tab2)



# use informative colnames
names(tab) <- c("qual_of_life", 
                "cost_of_living",
                "better_phys_env", 
                "buy_property", 
                "more_open_space", 
                "more_home_area",
                "closer_to_friends", 
                "raise_a_family", 
                "poor_qual_housing", 
                "personnal_safety", 
                "better_education", 
                "new_job_abroad", 
                "new_job_domestic", 
                "health_reasons", 
                "worries_pandemics", 
                "relocating_job", 
                "risk_of_eviction", 
                "other", 
                "dont_know", 
                "weight", "ID_GEO"
)




tabna <- tab[is.na(tab$qual_of_life), ]
nana <- aggregate(tabna$weight,  list(ID_GEO = tabna$ID_GEO),sum)

# keep only individuals who want to move responses
tab <- tab[!is.na(tab$qual_of_life), ]
nnana <- aggregate(tab$weight,  list(ID_GEO = tab$ID_GEO),sum)

nnana$ID_GEO==nana$ID_GEO
nana

nnana$w <- nnana$x / (nana$x + nnana$x)



nnana


# aggregate and compute shares
x <- aggregate(tab[,-21], by = list(ID_GEO = tab$ID_GEO), sum)
a <- apply(X=x[, -c(1, 21)], MARGIN = 2, FUN = function(YY){100 * YY / x$weight})


df <- cbind(ID_GEO = x$ID_GEO, as.data.frame(a))
row.names(df) <- df$ID_GEO

barplot((nnana$w))

library(FactoMineR)
library(factoextra)
res.pca = PCA(df[,-1], scale.unit=TRUE, ncp=5, quanti.sup = c(18,19), ind.sup = c(21,12,10), row.w = nnana$w[-c(21,12,10)]) 
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)


