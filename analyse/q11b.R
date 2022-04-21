source("gen_satisfy.R")

x = prep("London")
y = prep("Paris")

#Q11B2. Thinking now about living in Paris/votre commune, to what extent do you agree or disagree with the following statements? - Housing in Paris/Ma commune is affordable for people like me
question = "GRID_Q11B_2"

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
  title = "Se loger dans ma commune est abordable pour les gens comme moi",
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
  title = "Se loger dans ma commune n'est pas abordable pour les gens comme moi",
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
  title = "Neutre ou NSP : se loger dans ma commune est abordable pour les gens comme moi",
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
  title = "Se loger dans ma commune est abordable pour les gens comme moi",
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
  title = "Se loger dans ma commune n'est pas abordable pour les gens comme moi",
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
  title = "Neutre ou NSP : se loger dans ma commune est abordable pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()


#Q11B3. Thinking now about living in Paris/votre commune, to what extent do you agree or disagree with the following statements? - It is too expensive for me to buy a property in Paris/Ma commune
question = "GRID_Q11B_3"

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
  title = "L'accès à la propriété est trop cher pour les gens comme moi",
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
  title = "L'accès à la propriété n'est pas trop cher pour les gens comme moi",
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
  title = "Neutre ou NSP : l'accès à la propriété est trop cher pour les gens comme moi",
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
  title = "L'accès à la propriété est trop cher pour les gens comme moi",
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
  title = "L'accès à la propriété n'est pas trop cher pour les gens comme moi",
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
  title = "Neutre ou NSP : l'accès à la propriété est trop cher pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()