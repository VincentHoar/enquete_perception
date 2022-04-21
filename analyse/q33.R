source("gen_satisfy.R")

x = prep("London")
y = prep("Paris")

#Q33_1. How safe do you feel walking alone in your local area… - ...during the day time?
question = "GRID_Q33_1"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1,2), mod_neg = c(3,4), mod_oth = c(5,6,7))

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
  title = "Se sentir en sécurité la journée dans son quartier",
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
  title = "Ne pas se sentir en sécurité la journée dans son quartier",
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
  title = "Autre",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

#Pré-traitements pour résumé Londres
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Se sentir en sécurité la journée dans son quartier",
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
  title = "Ne pas se sentir en sécurité la journée dans son quartier",
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
  title = "Autre",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q33_2. How safe do you feel walking alone in your local area… - ...after dark?
question = "GRID_Q33_2"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1,2), mod_neg = c(3,4), mod_oth = c(5,6,7))

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
  title = "Se sentir en sécurité la nuit dans son quartier",
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
  title = "Ne pas se sentir en sécurité la nuit dans son quartier",
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
  title = "Autre",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

#Pré-traitements pour résumé Londres
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Se sentir en sécurité la nuit dans son quartier",
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
  title = "Ne pas se sentir en sécurité la nuit dans son quartier",
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
  title = "Autre",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()