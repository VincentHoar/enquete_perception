source("gen_satisfy.R")


x = prep("London")
y = prep("Paris")

question = "Q6"
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé + - 
mf_export(res, filename = paste0("London_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout()
dev.off()
mf_export(res, filename = paste0("London_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout()
dev.off()

res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé + - 
mf_export(res, filename = paste0("Paris_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout()
dev.off()
mf_export(res, filename = paste0("Paris_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout()
dev.off()



question = "Q7"
res = geoagg(x, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé + - 
mf_export(res, filename = paste0("London_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout()
dev.off()
mf_export(res, filename = paste0("London_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout()
dev.off()

res = geoagg(y, question, mod_pos = c(1, 2), mod_neg = c(4, 5), mod_oth = c(3, 6))
# Résumé + - 
mf_export(res, filename = paste0("Paris_", question, "_pos.svg"))
mf_map(res, "spos", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Burg", leg_title = "En %")
mf_layout()
dev.off()
mf_export(res, filename = paste0("Paris_", question, "_neg.svg"))
mf_map(res, "sneg", type = "choro", breaks = "quantile", nbreaks = 4, pal = "Teal", leg_title = "En %")
mf_layout()
dev.off()