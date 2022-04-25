source("gen_satisfy.R")

x = prep("London")
y = prep("Paris")

#Q22A. How important, if at all, do you think it is that people living in Paris/votre commune have access to all of the local facilities they need within their local area, that is the area within 15-20 minutes’ walking distance from their home?
question = "Q22A"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(3, 4), mod_oth = c(5))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)
res$indicep = res$spos / p

#Création séquence pour légende
bks = (c(0, .5, .75, 1, 1.25, 1.5, 10))

#Négatif
n <- 100*sum(res$neg) / sum (res$tot)
res$indicen = res$sneg / n

#Autre
o <- 100*sum(res$oth) / sum (res$tot)
res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(p, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Blue-Red 3", pos = "topleft" )
mf_layout(
  title = "Important que les habitants de ma commune ait accès à tout en 15-20min",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicen", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Pas important que les habitants de ma commune ait accès à tout en 15-20min",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indiceo", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "NSP¨: Importance de l'hyper proximité",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

#Pré-traitements pour résumé Londres
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(3, 4), mod_oth = c(5))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)
res$indicep = res$spos / p

#Création séquence pour légende
bks = (c(0, .5, .75, 1, 1.25, 1.5, 10))
#Négatif
n <- 100*sum(res$neg) / sum (res$tot)
res$indicen = res$sneg / n

#Other
o <- 100*sum(res$oth) / sum (res$tot)
res$indiceo = res$soth / o

# Résumé LONDRES
mf_export(res, filename = paste0("fig/Londres_", question, "_pos.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(p, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Blue-Red 3", pos = "bottomleft1" )
mf_layout(
  title = "Important que les habitants de ma commune ait accès à tout en 15-20min",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicen", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Pas important que les habitants de ma commune ait accès à tout en 15-20min",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indiceo", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "NSP : importance de l'hyper proximité",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()