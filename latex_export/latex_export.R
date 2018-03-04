# latex_export

library(stargazer)

datatable = read.csv(file.path("C:", "Users", "Jamie", "Dropbox", "Thesis", "jmv", "jmv_formatted.csv"), check.names = FALSE)
datatable$`Trees/ha` = round(datatable$`Trees/ha`)

## spatial index table

spatialindexrows = datatable[,c("Contagion", "DBH Differentiation", "Mingling", "QMD", "Nearest Neighbor", "Trees/ha")]

stargazer(spatialindexrows, summary = FALSE, header = FALSE, colnames = FALSE)


## species abundance

abundancerows = datatable[,c("ABICON", "ABIGRA", "ABILAS", "ACCI", "JUNOCC", "LARILAR", "LAROCC", "PICENG", "PINCON", "PINPON", "POPTRE", "PSEMEN")]

stargazer(abundancerows, summary = FALSE, header = FALSE, colnames = FALSE)


# diversity table

diversity = datatable[,c("Shannon Diversity", "Simpson Diversity", 'Richness')]

stargazer(diversity, summary = FALSE, header = FALSE, colnames = FALSE)
