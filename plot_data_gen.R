#########################################
#
# Main script to generate data
#
#########################################

# Generates all per plot data in single script
library(vegan)
library(plyr)

source(file.path("core", "r", "data_functions.R"))
source(file.path("core", "r", "forestry_functions.R"))
source(file.path("core", "r", "spatial_functions.R"))

main = function() {

    # Load stem location data
    dirPath = file.path("F:", "Box Sync", "OR_Data", "geo_files")
    flist = list.files(dirPath, pattern = ".*Geo.*")
    
    loadData = do.call("rbind", lapply(file.path(dirPath, flist), geoCsv))
    
    # fix errors in species raw data
    loadData$Species = sub("SUBALP", "ABILAS",sub("VINE MAPLE", "ACCI", sub("LORACC", "LAROCC", toupper(loadData$Species))))
    
    # grab plots and tree species
    plots = sort(unique(toupper(loadData$id)))
    species = sort(unique(toupper(loadData$Species[trimws(loadData$Species)!=""])))
    
    ##################################################
    # Species abundance, richness, and diversity
    ##################################################
    
    # build tree abundance table
    sp.abundance = data.frame(matrix(nrow = length(plots), ncol = length(species)), row.names = plots)
    colnames(sp.abundance) = species
    
    treefreq = count(loadData, c("Species", "id"))
    
    for (i in 1:nrow(treefreq)) {
        
        spc = treefreq$Species[i]
        plt = treefreq$id[i]
        cnt = treefreq$freq[i]
        
        if (spc %in% species) {
            sp.abundance[c(plt), c(spc)] = cnt
        }
    }
    
    # replace na vals with 0
    sp.abundance[is.na(sp.abundance)] = 0
    
    # set main dataframe
    maindata = data.frame(sp.abundance)
    
    # get species richness
    maindata$Richness = specnumber(sp.abundance)
    
    # get shannon and simpson index
    maindata$Shannon = diversity(sp.abundance)
    maindata$Simpson = diversity(sp.abundance, index = "simpson")
    
    # biometrics vals
    maindata$meanDBH = sapply(plots, function(x) {mean(loadData$DBH[loadData$id == x], na.rm = TRUE)})
    maindata$tpha = sapply(plots, function(x) {tpha(nrow(loadData[loadData$id == x,]))})
    maindata$baha = sapply(plots, function(x) {basalha(basal(loadData$DBH[loadData$id == x]))})
    maindata$qmd = qmd(maindata$baha, maindata$tpha)
    
    ################################################
    #
    # Spatial indexes
    #
    ################################################
    
    # initialise results
    results = 0
    
    spatialdata = as.data.frame(do.call("rbind", lapply(plots, function(x) {plotSpatial(loadData[loadData$id == x,])})))
    colnames(spatialdata) = c("nearest", "contagion", "mingling", "dbhdiff")
    row.names(spatialdata) = plots
    
    maindata = cbind(maindata, spatialdata)
    row.names(maindata) = maindata$Row.names
    
    return(maindata)

}

maindata = main()
