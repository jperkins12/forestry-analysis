# jmv analysis
library(jmv)
library(Hmisc)
library(stargazer)

datatable = read.csv(file.path("C:", "Users", "Jamie", "Dropbox", "Thesis", "jmv", "jmv_formatted.csv"), row.names = 1, check.names = FALSE)


myplot = jmv::corrMatrix(
    data = datatable,
    vars = c(
        "Nearest Neighbor",
        "Mingling",
        "Contagion",
        "DBH Differentiation",
        "Trees/ha",
        "QMD",
        "Basal Area/ha",
        "Biomass/ha",
        "Shannon Diversity"),
    flag = TRUE)

myplot

flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut],
        adjust = p.adjust(pmat[ut], method="bonferroni")
    )
}

rmat = rcorr(as.matrix(datatable[,c("Nearest Neighbor","Mingling","Contagion","DBH Differentiation","Trees/ha","QMD","Basal Area/ha","Biomass/ha","Shannon Diversity")]))

flattend = flattenCorrMatrix(rmat$r, rmat$P)

stargazer(flattend[flattend$adjust < .05,], summary = FALSE, rownames = FALSE, digits=3, digits.extra = 0)
