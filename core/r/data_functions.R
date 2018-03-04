# data loading
#library(tidyverse)

geoCsv = function(csv) {
    
    datatable = read.csv(csv, stringsAsFactors = FALSE)
    Plot = sub("_Geo.csv", "", basename(csv))
    datatable$Plot = Plot
    
    return(datatable)
}

sampleCsv = function(csv) {
    
    datatable = read.csv(csv, skip = 1, stringsAsFactors = FALSE)
    datatable = datatable[,2:14]
    #print(basename(csv))
    #print(colnames(datatable))
    Plot = sub("_sampleTrees.csv", "", basename(csv))
    datatable$Plot = Plot
    datatable$Percent.Crown.Visible = as.numeric(sub("%", "", as.character(datatable$Percent.Crown.Visible)))
    datatable$Crown.Bottom = as.numeric(datatable$Crown.Bottom)
    
    return(datatable)
}

estSampleCsv = function(csv) {
    datatable = read.csv(csv, stringsAsFactors = FALSE)
    datatable = datatable[datatable$X != 'Mean', ]
    Plot = sub("_sampleStats.csv", "", basename(csv))
    datatable$Plot = Plot
    
    return(datatable)
}

readshpData = function(shp) {
    
    shpData = as.data.frame(readOGR(shp))
    Plot = sub("_treeCrowns_stats.shp", "", basename(shp))
    shpData = shpData[,1:4]
    colnames(shpData) = c("TreeID", "Area", "Radius", "EstHeight")
    # shpData$EstHeight = round(shpData$EstHeight)
    shpData$Plot = Plot
    return(shpData)
}

# export to png
savepng = function(filename) {
    
    path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\graphics'
    save_path = file.path(path, filename)
    ggsave(save_path, width = 7.5, height = 5, dpi = 100)
    
}