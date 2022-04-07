source("gen_satisfy.R")


x = prep("London")
y = prep("Paris")


#Q2.The things people can buy and do — their housing, furniture, food, cars, recreation and travel — make up their standard of living. How satisfied or dissatisfied do you feel about your standard of living at present?
question = "Q2"
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé LONDRES
mf_export(res, filename = paste0("fig/London_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout(
  title = "Satisfait de leur présent niveau de vie",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/London_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout(
  title = "Non-satisfait de leur présent niveau de vie",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout(
  title = "Satisfait de leur présent niveau de vie",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout(
  title = "Non-satisfait de leur présent niveau de vie",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()


#Q3. And do you expect that your standard of living will improve, get worse, or stay the same, over the next 5 years?
question = "Q3"
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé LONDRES
mf_export(res, filename = paste0("fig/London_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/London_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/London_",question, "_oth.svg"))
mf_map(res, "soth", type = "choro", breaks = "quantile", nbreaks = 4, pal = "YlOrBr", leg_title = "En %")
mf_layout(
  title = "Leur niveau de vie va rester identique (ou NSP)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))

var = c(res$spos, res$sneg)


p <- 100* sum(res$pos) / sum(res$tot)

res$indicep = res$spos / p
bks = seq(.5, 2, .3)

n <- 100*sum(res$neg) / sum (res$tot)

res$indicen = res$sneg / n

# Résumé PARIS
mf_export(res, filename = paste0("fig/Paris_", question, "_pos.svg"))
mf_map(res, "indicep", type = "choro", breaks = bks, leg_val_rnd = 2,  pal = c("indianred4", "indianred1", "darkgoldenrod1", "cadetblue"), 
       leg_title = "En %", leg_pos = NA)
mf_legend(type = "choro", val = c("x 0.5", "x .75", paste0("valeur ville = ", p), "x1.75", "x 2"),pal = c("indianred4","indianred1", "darkgoldenrod1", "cadetblue") )
            
mf_layout(
  title = "Leur niveau de vie va s'améliorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/Paris_", question, "_neg.svg"))
mf_map(res, "indicen", type = "choro", breaks = bks, leg_val_rnd = 0, , pal = c("sienna4","sienna1", "navajowhite1", "royalblue3"), leg_title = "En %", leg_pos = NA)
mf_legend(type = "choro", val = c("x 0.5", "x .75", paste0("valeur ville = ", n),"x1.75", "x 2"),pal = c("sienna4","sienna1", "navajowhite1", "royalblue3") )

mf_layout(
  title = "Leur niveau de vie va se détériorer",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()
mf_export(res, filename = paste0("fig/Paris_",question, "_oth.svg"))
mf_map(res, "soth", type = "choro", breaks = "quantile", nbreaks = 4, pal = "YlOrBr", leg_title = "En %")
mf_layout(
  title = "Leur niveau de vie va rester identique (ou NSP)",
  credits = "\nIPSOS - Hoareau, 2022",
)
dev.off()

