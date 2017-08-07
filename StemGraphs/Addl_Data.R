#build metadata file
#Load additional data
addlPath = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment/Additional_Data.csv'
addl = read.csv(addlPath)

bioDir = 'F:\\Box Sync\\OR_Data\\biomass_estimates'

#fileList = list.files(path = bioDir, pattern = ' _biomass_est.csv')

for (i in 1:length(addl$Plot)) {
  
  plotCode = as.character(addl$Plot[i])
  
  #construct pathname
  fileName = paste(plotCode, ' _biomass_est.csv', sep = '')
  bioCSV = file.path(bioDir, fileName)
  
  #open bio csv
  bioData = read.csv(bioCSV)
  
  #find values
  treeCount = max(bioData$Tree.., na.rm = TRUE)
  basalArea = sum(bioData$Basal.Area..m.2., na.rm = TRUE)
  meanDBH = mean(bioData$DBH..cm., na.rm = TRUE)
  
  #record values
  addl$Stem.Density[i] = treeCount
  addl$Basal.Area[i] = basalArea
  addl$MeanDBH[i] = meanDBH
  
}

write.csv(addl, addlPath)