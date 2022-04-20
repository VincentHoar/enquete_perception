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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Les prix de l'immobilier sont abordables pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Les prix de l'immobilier ne sont pas abordables pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
  credits = "\nIPSOS - Hoareau, 2022",
)
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
mf_export(res, filename = paste0("fig/Londres_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Les prix de l'immobilier sont abordables pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Les prix de l'immobilier ne sont pas abordables pour les gens comme moi",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "Minimum 2x supérieur"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Trop cher pour eux d'accéder à la propriété",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Pas trop cher pour eux d'accéder à la propriété",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x1.75"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
  credits = "\nIPSOS - Hoareau, 2022",
)
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
mf_export(res, filename = paste0("fig/Londres_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Trop cher pour eux d'accéder à la propriété",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Pas trop cher pour eux d'accéder à la propriété",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
