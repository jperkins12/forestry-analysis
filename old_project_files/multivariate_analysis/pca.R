library(BiodiversityR)
library(vegan)
library(ggplot2)

savepng = function(filename) {
    
    path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\graphics'
    save_path = file.path(path, filename)
    ggsave(save_path, width = 7.5, height = 5, dpi = 100)
    
}

# principle_components_analysis

# load multivariate data
datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\forestdata.csv', row.names = 1)

speciesframe = datatable[,c("ABICON", "ABIGRA", "ABILAS", "ACCI", "JUNOCC", "LARILAR", "LAROCC", "PICENG", "PINCON", "PINPON", "POPTRE", "PSEMEN")]

# lapply(speciesframe[colnames(speciesframe)], x/sum)

# relativize species abundance by PLOT
speciesframe = sweep(speciesframe, 1, rowSums(speciesframe), '/')

speciesframe = cbind(speciesframe, datatable[,c("Richness", "Shannon", "Simpson", "tpha", "baha", "qmd", "bioha")])
# relativize tpha and bioha by column MAX
speciesframe$tpha = speciesframe$tpha/max(speciesframe$tpha)
speciesframe$bioha = speciesframe$bioha/max(speciesframe$bioha)
speciesframe$baha = speciesframe$baha/max(speciesframe$baha)
speciesframe$qmd = speciesframe$qmd/max(speciesframe$qmd)
speciesframe$Richness = speciesframe$Richness/max(speciesframe$Richness)

enviroframe = datatable[,c("nearest", "contagion", "mingling", "dbhdiff")]
enviroframe$nearest = enviroframe$nearest/max(enviroframe$nearest)

ordiframe = as.matrix(t(speciesframe)) %*% as.matrix(enviroframe)

#BiodiversityRGUI()

analysis.pca = rda(ordiframe)

analysis.pca
scrs <- scores(analysis.pca, display = c("sites", "species"), scaling = 3)
xlim <- with(scrs, range(species[,1], sites[,1]))
ylim <- with(scrs, range(species[,2], sites[,2]))

pca.plot = biplot(analysis.pca, type = c('points', 'points'), main = 'PCA of relative species abundances against spatial indices', xlim=xlim, ylim=ylim)
ordipointlabel(pca.plot, add = TRUE)
savepng('updatedpca.png')

set.seed(1)
analysis.nmds = metaMDS(ordiframe)
analysis.nmds

ndms.plot = ordiplot(analysis.nmds, main = 'NMDS of relative species abundances against spatial indices')
ordipointlabel(ndms.plot, add = TRUE)
savepng('updatedndms.png')
