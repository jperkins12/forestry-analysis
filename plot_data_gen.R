#########################################
#
# Main script to generate data
#
#########################################

# Generates all per plot data in single script
source(file.path("core", "r", "data_functions.R"))
source(file.path("core", "r", "forestry_functions.R"))
source(file.path("core", "r", "spatial_functions.R"))

library(vegan)
#library(plyr)
library(rgdal)
library(tidyverse)

main = function(table = 'main') {

    # Load stem location data
    dirPath = file.path("F:", "Box Sync", "OR_Data", "geo_files")
    flist = list.files(dirPath, pattern = ".*Geo.*")
    
    loadData = do.call("rbind", lapply(file.path(dirPath, flist), geoCsv))
    
    # fix errors in species raw data
    loadData$Species = sub("JUNCON", "JUNOCC",sub("SUBALP", "ABILAS",sub("VINE MAPLE", "ACCI", sub("LORACC", "LAROCC", toupper(loadData$Species)))))
    loadData$Cell = toupper(loadData$Cell)
    
    # grab plots and tree species
    plots = sort(unique(toupper(loadData$Plot)))
    species = sort(unique(toupper(loadData$Species[trimws(loadData$Species)!=""])))
    
    ##################################################
    # Sample tree data
    ##################################################
    
    sample_dirpath = file.path("F:", "Box Sync", "OR_Data", "SampleTree_files")
    sample_flist = list.files(sample_dirpath, pattern = ".*sampleTrees.*")
    
    #load height estimates
    estDir = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment'
    est_flist = list.files(estDir, pattern = '.*sampleStats.*')
    
    sampleData = do.call("rbind", lapply(file.path(sample_dirpath, sample_flist), sampleCsv))
    
    estData = do.call("rbind", lapply(file.path(estDir, est_flist), estSampleCsv))
    
    sampleData$Cell = toupper(sampleData$Cell)
    sampleData$TreeID = as.numeric(sub("[a-zA-z]", "", sampleData$Tree..))
    
    sampleData = sampleData[sampleData$Total.Height < 100,]
    
    sampleData = merge.data.frame(sampleData, estData[,c("TreeID", "Actual.Height", "Plot", "Est.Height", "Est.Radius", "Actual.Radius")], by.x = c("TreeID", "Total.Height", "Plot"), by.y = c("TreeID", "Actual.Height", 'Plot'), all.x = TRUE)
    sampleData = sampleData[, c("TreeID", "Plot", "Tree..", "Page", "Cell", "Total.Height", "Est.Height", "Crown.Bottom", "Radius.1", "Radius.2", "Radius.3", "Radius.4", "Actual.Radius", "Est.Radius", "Centroid.Dist", "Centroid.Azi", "Percent.Crown.Visible")]
    sampleData = sampleData %>% dplyr::mutate(Actual.Radius = (Radius.1 + Radius.2 + Radius.3 + Radius.4)/4)
    
    sampleData = merge.data.frame(sampleData, loadData, by.x = c("TreeID", "Cell", "Plot"), by.y = c("TreeID", "Cell", "Plot"))
    sampleData = sampleData[sampleData$Species != '',]
    
    ##################################################
    # Species abundance, richness, and diversity
    ##################################################
    
    # build tree abundance table
    sp.abundance = data.frame(matrix(nrow = length(plots), ncol = length(species)), row.names = plots)
    colnames(sp.abundance) = species
    
    treefreq = count(loadData, c("Species", "Plot"))
    
    for (i in 1:nrow(treefreq)) {
        
        spc = treefreq$Species[i]
        plt = treefreq$Plot[i]
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
    maindata$meanDBH = sapply(plots, function(x) {mean(loadData$DBH[loadData$Plot == x], na.rm = TRUE)})
    maindata$tpha = sapply(plots, function(x) {tpha(nrow(loadData[loadData$Plot == x,]))})
    maindata$baha = sapply(plots, function(x) {basalha(basal(loadData$DBH[loadData$Plot == x]))})
    maindata$qmd = qmd(maindata$baha, maindata$tpha)
    
    # biomass
    biomassgroups = read.csv(file.path("src", "csv", "species_groups.csv"))
    loadData$speciesgroup = biomassgroups$species_group[match(loadData$Species, biomassgroups$species)]
    loadData$biomass = treeMass(loadData$speciesgroup, loadData$DBH)
    maindata$bioha = sapply(plots, function(x) {(sum(loadData$biomass[loadData$Plot == x], na.rm = TRUE)/1600)*10000})
    
    ################################################
    #
    # Spatial indexes
    #
    ################################################
    
    # initialise results
    results = 0
    
    spatialdata = as.data.frame(do.call("rbind", lapply(plots, function(x) {plotSpatial(loadData[loadData$Plot == x,])})))
    colnames(spatialdata) = c("nearest", "contagion", "mingling", "dbhdiff")
    row.names(spatialdata) = plots
    
    maindata = cbind(maindata, spatialdata)
    
    # add public land layer
    metadata = read.csv(file.path("C:", "Users", "Jamie", "Dropbox", "Thesis", "R", "tables", "metadata.csv"), row.names = 1)
    public = merge(maindata, metadata, by="row.names")$Public.Land
    maindata = cbind(maindata, public)
    
    if (table == 'main') {
        return(maindata)
    }
    else if (table == 'sample') {
        return(sampleData)
    }
    else if (table == 'trees') {
        return(loadData)
    }

}

getHeightEstimates = function() {
    
    modelDir = file.path("K:", "OR_Perkins", "OR_Imagery_2015", "*", "*", "Model", "temp", "*_treeCrowns_stats.shp")
    shpPaths = Sys.glob(modelDir)
    shpData = do.call("rbind", lapply(shpPaths, readshpData))

    treeData = merge.data.frame(treeData, shpData[, c("TreeID", "EstHeight", "Plot")], by = c("TreeID", "Plot"), all.x = TRUE)
    setorder(treeData, "Plot", "TreeID")
    return(treeData)
}

maindata = main()
sampledata = main(table = 'sample')
treeData = main(table = 'trees')

treeData = getHeightEstimates()


# write.csv(maindata, file.path("C:", "Users", "Jamie", "Dropbox", "Thesis", "R", "tables", "forestdata.csv"))
