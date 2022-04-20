source("gen_satisfy.R")

x = prep("London")
y = prep("Paris")

#Q23_1. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Introducing a ‘congestion charge’ to reduce the number of cars driving into the centre of Paris on weekdays
question = "GRID_Q23_1"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP vis à vis du péage urbain en centre ville",
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
  title = "Pour l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP vis à vis du péage urbain en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_2. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Encouraging people not to use a car to travel to work or school
question = "GRID_Q23_2"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
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
  title = "Pour encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : encourager à ne pas utiliser sa voiture pour aller au travail ou à l'école",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_3. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Funding more cycle routes/lanes and facilities
question = "GRID_Q23_3"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour financer plus d'infrastructures pour vélos (routes, installations...)",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre financer plus d'infrastructures pour vélos (routes, installations...)",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : financer des infrastructures pour vélos (routes, installations...)",
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
  title = "Pour financer des infrastructures pour vélos (routes, installations...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre financer des infrastructures pour vélos (routes, installations...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : financer des infrastructures pour vélos (routes, installations...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_4. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Funding more pavements, footpaths and pedestrian areas
question = "GRID_Q23_4"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour financer plus de zones piétonnes (sentiers, trottoirs...)",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre financer plus de zones piétonnes (sentiers, trottoirs...)",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : financer plus de zones piétonnes (sentiers, trottoirs...)",
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
  title = "Pour financer plus de zones piétonnes (sentiers, trottoirs...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre financer plus de zones piétonnes (sentiers, trottoirs...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : financer plus de zones piétonnes (sentiers, trottoirs...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_5. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Introducing more car-free days in inner-Paris, where cars are not allowed to drive into the centre of the city
question = "GRID_Q23_5"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour des journées sans voitures à Paris intra-muros",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre des journées sans voitures à Paris intra-muros",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : des journées sans voitures à Paris intra-muros",
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
  title = "Pour des journées sans voitures à Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre des journées sans voitures à Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : des journées sans voitures à Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_6.  In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Only allowing access to the centre of Paris to electric or hybrid cars or vans on some days
question = "GRID_Q23_6"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour n'autoriser que les véhicules 'propres' certains jours dans Paris",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre n'autoriser que les véhicules 'propres' certains jours dans Paris",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : n'autoriser que les véhicules 'propres' certains jours dans Paris",
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
  title = "Pour n'autoriser que les véhicules 'propres' certains jours dans Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre n'autoriser que les véhicules 'propres' certains jours dans Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : n'autoriser que les véhicules 'propres' certains jours dans Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_7. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Reallocating space away from motor vehicles to cyclists and walkers
question = "GRID_Q23_7"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
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
  title = "Pour réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : réallouer de l'espace réservé aux véhicules aux cyclistes et piétons",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Q23_8. In principle, to what extent would you support or oppose introducing the following policies in region parisienne? - Reducing traffic speeds in denser built-up areas
question = "GRID_Q23_8"

#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

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
  title = "Pour réduire la vitesse limite dans les zones denses",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "topleft" )
mf_layout(
  title = "Contre réduire la vitesse limite dans les zones denses",
  credits = "IPSOS - Hoareau, 2022", arrow = FALSE
)
mf_arrow("topright")
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"), 
          expandBB = c(0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la valeur", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "topleft" )
mf_layout(
  title = "Neutre ou NSP : réduire la vitesse limite dans les zones denses",
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
  title = "Pour réduire la vitesse limite dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"), 
          expandBB = c(.0,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(n, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "ArmyRose", pos = "bottomleft1" )
mf_layout(
  title = "Contre réduire la vitesse limite dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"), 
          expandBB = c(.00,0,0,0))
mf_map(res, "indicep", type = "choro", breaks = bks, 
       leg_val_rnd = 2,  pal = "Earth", 
       leg_title = "En %", leg_pos = NA, add = TRUE)
mf_legend(title = paste0("Ecart à la", "\n","moyenne (",round(o, 0),"%)"),
          type = "choro", 
          val = c( "0", "x 0.5", "x 0.75",
                   "moyenne",
                   "x 1.25", "x 1.5", "> x 2")
          ,pal = "Earth", pos = "bottomleft1" )
mf_layout(
  title = "Neutre ou NSP : réduire la vitesse limite dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
