require(ggplot2)

#assemble all sample tree data and add DBH values

#loads data from crown rad and height tables
sampleDir = 'F:\\Box Sync\\OR_Data\\SampleTree_files'
sampleList = list.files(path = sampleDir, pattern = '_sampleTrees.csv')

#loads data from stem location tables, inc DBH
geoDir = 'F:\\Box Sync\\OR_Data\\geo_files'
#geoList = list.files(path = geoDir, pattern = '_Geo.csv')

#load height estimates
estDir = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment'

#initialize dataframe table
outTable = NULL

#iterate through all samplefiles
for (f in 1:length(sampleList)) {
  
  baseName = sampleList[f]
  #plot Code
  code = substr(baseName, 1, 3)
  print(code)
  #construct full path name
  fullPath = file.path(sampleDir, baseName)
  #construct 'geo' path
  geoBase = paste(code, '_Geo.csv', sep = '')
  geoPath = file.path(geoDir, geoBase)
  #construct 'est' path
  estBase = paste(code, '_sampleStats.csv', sep = '')
  estPath = file.path(estDir, estBase)
  
  #read data
  sampleTable = read.csv(fullPath, skip = 1)
  sampleTable = sampleTable[,2:length(sampleTable)]
  #add new plot ID to table
  sampleTable$Plot = code
  
  #open geo file
  if (file.exists(geoPath)) {
    
    geoTable = read.csv(geoPath)
  
  } else {
    
    next()
    
  }
  
  #change capitalization
  sampleTable$Cell = toupper(as.character(sampleTable$Cell))
  geoTable$Species = toupper(as.character(geoTable$Species))
  
  #add new columns to sample table
  sampleTable$DBH = geoTable$DBH[match(paste(sampleTable$Tree.., sampleTable$Cell), paste(geoTable$TreeID, geoTable$Cell))]
  sampleTable$Species = geoTable$Species[match(paste(sampleTable$Tree.., sampleTable$Cell), paste(geoTable$TreeID, geoTable$Cell))]
  #calc mean radius
  meanTable = sampleTable[c('Radius.1', 'Radius.2', 'Radius.3', 'Radius.4')]
  #need to fix this issue
  #meanTable[(is.character(meanTable))] = NA
  
  sampleTable$MeanRad = rowMeans(meanTable, na.rm = TRUE)
  
  sampleTable = sampleTable[c('Tree..', 'Cell', 'Plot', 'Species', 'DBH', 'Total.Height', 'MeanRad')]
  names(sampleTable)[names(sampleTable)=='Tree..'] = 'TreeID'
  
  #open est file
  if (file.exists(estPath)) {
    
    estTable = read.csv(estPath)
    estTable = estTable[1:nrow(estTable)-1,]
    estTable$Actual.Height = round(as.numeric(estTable$Actual.Height), digits = 1)
    estTable = estTable[c('TreeID', 'Actual.Radius', 'Est.Radius', 'Actual.Height', 'Est.Height')]
    sampleTable = merge(sampleTable, estTable, by.x = c('TreeID', 'Total.Height'), by.y = c('TreeID', 'Actual.Height'), all = TRUE)
    
    print(code)
    print(estTable$Actual.Height)
    
  } else {
    
    sampleTable$Est.Height = NA
    sampleTable$Est.Radius = NA
    sampleTable$Actual.Radius = NA
  
  }
  
  outTable = rbind(outTable, sampleTable)
  
}

#change incorrect values to NA
outTable[outTable==''] = NA
outTable$DBH[outTable$DBH > 300] = NA
outTable[outTable=='NaN'] = NA
outTable$Total.Height[outTable$Total.Height > 100] = NA

#add new log tables
outTable$LnDBH = log(outTable$DBH)
outTable$LnHT = log(outTable$Total.Height)

# these classes are arbitrary and useless
# outTable$SpeciesClass = ifelse(outTable$Species == 'PINPON' | outTable$Species == 'PINCON' | outTable$Species == '', 'Pine', 
#                                ifelse(outTable$Species == 'ABICON' | outTable$Species == 'ABIGRA' | outTable$Species == 'ABILAS' | outTable$Species == 'PSEMEN', 'Fir', 'Other'))


# #equations for diameter-height relationships
# calcDBH = function(ht, class) {
#   
#   lnht = log(ht)
#   
#   if (class == 'Fir') {
#     
#     x = 0.7918
#     y = 0.92108
#     
#   } else if (class == 'Pine') {
#     
#     x = 0.9283
#     y = 0.90855
#     
#   } else {
#     
#     next()
#     
#   }
#   
#   d = exp(x + y * lnht)
#   d = round(d, digits = 2)
#   
#   return(d)
#   
# }
# 
# outTable$DBHEst = ifelse(outTable$SpeciesClass == 'Pine', calcDBH(outTable$Est.Height, 'Pine'),
#                          ifelse(outTable$SpeciesClass == 'Fir', calcDBH(outTable$Est.Height, 'Fir'), NA))

# #calc mean height underest
# outTable$heightAdj = outTable$Est.Height + mean(outTable$Total.Height-outTable$Est.Height, na.rm = TRUE)
# 
# outTable$DBHEstAdj = ifelse(outTable$SpeciesClass == 'Pine', calcDBH(outTable$heightAdj, 'Pine'),
#                          ifelse(outTable$SpeciesClass == 'Fir', calcDBH(outTable$heightAdj, 'Fir'), NA))

# outPath = 'F:\\Box Sync\\OR_Data\\SampleAggregated.csv'
# 
# write.csv(outTable, file = outPath, na = '')

# put plot together
tree_plot = ggplot(outTable, aes(x=Total.Height, y=DBH))
tree_plot = tree_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
tree_plot = tree_plot + labs(title = 'Comparision between Height and Diameter', x = 'Height (m)', y = 'DBH (cm)')
tree_plot = tree_plot + geom_point() + geom_smooth(method=lm)

tree_plot
