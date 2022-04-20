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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre l'introduction de péage urbain pour réduire le trafic en centre ville",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour encourager les gens à ne pas prendre leur voiture pour leurs trajets quotidiens",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre encourager les gens à ne pas prendre leur voiture pour leurs trajets quotidiens",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour encourager les gens à ne pas prendre leur voiture pour leurs trajets quotidiens",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre encourager les gens à ne pas prendre leur voiture pour leurs trajets quotidiens",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour financer davantage d'infrastructures pour vélos",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre financer davantage d'infrastructures pour vélos",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour financer davantage d'infrastructures pour vélos",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre financer davantage d'infrastructures pour vélos",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour financer davantage d'espaces piétons (trottoirs, sentiers...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre financer davantage d'espaces piétons (trottoirs, sentiers...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour financer davantage d'espaces piétons (trottoirs, sentiers...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre financer davantage d'espaces piétons (trottoirs, sentiers...)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour introduire plus de journées sans voitures dans Paris intra-muros",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre introduire plus de journées sans voitures dans Paris intra-muros",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour introduire plus de journées sans voitures à Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Pour introduire plus de journées sans voitures à Londres",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour n'autoriser que les véhicules hybrides ou électriques dans Paris certains jours",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre n'autoriser que les véhicules hybrides ou électriques dans Paris certains jours",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour n'autoriser que les véhicules hybrides ou électriques à Londres certains jours",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre n'autoriser que les véhicules hybrides ou électriques à Londres certains jours",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour transférer de l'espace pour les voitures aux piétons et cyclistes",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre transférer de l'espace pour les voitures aux piétons et cyclistes",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour transférer de l'espace pour les voitures aux piétons et cyclistes",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre transférer de l'espace pour les voitures aux piétons et cyclistes",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
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

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour réduire la vitesse de circulation dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre réduire la vitesse de circulation dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

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
mf_map(res, "indicep", type = "choro", breaks = bks2, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Pour réduire la vitesse de circulation dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Contre réduire la vitesse de circulation dans les zones denses",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks2, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x >2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Neutre ou NSP",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
