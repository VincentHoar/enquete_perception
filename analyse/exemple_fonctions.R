source("gen_satisfy.R")


x = prep("London")
y = prep("Paris")


#Q2.The things people can buy and do — their housing, furniture, food, cars, recreation and travel — make up their standard of living. How satisfied or dissatisfied do you feel about your standard of living at present?
question = "Q2"
#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)

res$indicep = res$spos / p

#Création séquence pour légende
bks = seq(0, 2, .25)

#Négatif
n <- 100*sum(res$neg) / sum (res$tot)

res$indicen = res$sneg / n

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x1.75"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x1.75"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x1.75"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Pré-traitements pour résumé Londres
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)
res$indicep = res$spos / p

#Création séquence pour légende
bks = seq(0, 2, .25)

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
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()


#Q3. And do you expect that your standard of living will improve, get worse, or stay the same, over the next 5 years?
question = "Q3"


#Pré-traitements pour résumé Paris
res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)

res$indicep = res$spos / p

#Création séquence pour légende
bks = seq(0, 2, .25)

#Négatif
n <- 100*sum(res$neg) / sum (res$tot)

res$indicen = res$sneg / n

o <- 100*sum(res$oth) / sum (res$tot)

res$indiceo = res$soth / o

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = "Blue-Red 3", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x1.75"),pal = "Blue-Red 3", pos = "bottomleft2" )
            
mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x1.75"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Paris_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0.25", "x 0.5", "x .75", paste0("x1 = ", o), "x1.25", "x1.5", "x1.75"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

#Pré-traitements pour résumé Londres
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

#Positif
p <- 100* sum(res$pos) / sum(res$tot)
res$indicep = res$spos / p

#Création séquence pour légende
bks = seq(0, 2, .25)

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
mf_legend(title = "Indice en fonction de\nla valeur moyenne",   type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "Blue-Red 3", pos = "bottomleft2" )

mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "ArmyRose", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "ArmyRose", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

mf_export(res, filename = paste0("fig/Londres_", question, "_oth.svg"))
mf_map(res, "indiceo", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = "Earth", 
       leg_title = "En %", leg_pos = NA)
mf_legend(title = "Indice en fonction de\nla valeur moyenne", type = "choro", val = c("x.0", "x 0.5", "x .75", paste0("x1 = ", p), "x1.25", "x1.5", "x2"),pal = "Earth", pos = "bottomleft2")

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
