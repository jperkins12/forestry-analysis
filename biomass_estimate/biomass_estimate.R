#Create list of all directories
require(XLConnect)

searchPath = 'E:\\OR_Perkins\\OR_PlotData_2015'
savePath = 'E:\\OR_Perkins\\OR_PlotData_2015\\biomass_estimates'

coordsPath = 'E:\\OR_Perkins\\OR_2015_Extra\\OR_Data_2015\\OR_Coords_AllPlots.csv'

coordsFrame = read.csv(coordsPath)

rawDataList = list.files(path = searchPath, pattern = '*[!~]???_raw.xlsx', recursive = TRUE)
rawDataList = rawDataList[grepl('~', rawDataList) == FALSE]

#create table of biomass values
codes = c('PICENG', 'PSEMEN', 'ABIGRA', 'PINCON', 'PINPON', 'POPTRE', 'ABILAS', 'LARLYA', 'LARILAR', 'LAROCC', 'ABICON', 'PINALB')
bZero = c(-2.0773, -2.2304, -2.5384, -2.5356, -2.5356, -2.2094, -2.5384, -2.0336, -2.0336, -2.0336, -2.5384, -2.5356)
bOne = c(2.3323, 2.4435, 2.4814, 2.4349, 2.4349, 2.3867, 2.4814, 2.2592, 2.2592, 2.2592, 2.4814, 2.4349)
codesNames = c('code', 'b0', 'b1')

bioCodes = data.frame(codes, bZero, bOne)
colnames(bioCodes) = codesNames

for (i in 1:length(rawDataList)) {
  
  rdFile = rawDataList[i]
  rdPath = file.path(searchPath, rdFile)
  plotCode = substr(basename(rdFile),1,3)
  
  
  print(plotCode)
  
  #read excel file
  rawDataWB = loadWorkbook(rdPath)
  rawSheet = readWorksheet(rawDataWB, sheet = 'Stem Map')
  
  #find header row
  for (r in 1:nrow(rawSheet)) {
    
    celVal = rawSheet[r, 1]
    
    if (grepl('Tree', celVal, ignore.case = TRUE) == TRUE) {
      
      hdrIndex = r
      
      break
    
    }
      
  }
  
  bioFrame = rawSheet[(hdrIndex+1):nrow(rawSheet),]
  colnames(bioFrame) = rawSheet[hdrIndex,]
  
  newFrame = bioFrame[c('Tree #', 'Species', 'DBH (cm)')]
  newFrame$b0 = bioCodes[match(toupper(newFrame$Species), bioCodes$code), 2]
  newFrame$b1 = bioCodes[match(toupper(newFrame$Species), bioCodes$code), 3]
  newFrame$`biomass (kg)` = exp(newFrame$b0 + newFrame$b1 * log(as.numeric(newFrame$`DBH (cm)`)))
  
  newFrame$`Basal Area (m^2)` = (as.numeric(newFrame$`DBH (cm)`))^2 *0.00007854
  
  totmass = sum(newFrame$`biomass (kg)`, na.rm = TRUE)
  totBA = sum(newFrame$`Basal Area (m^2)`, na.rm = TRUE)
  treeNum = nrow(bioFrame)
  
  totals = c(NaN, 'Total Biomass', NaN, NaN, NaN, totmass, NaN)
  newFrame = rbind(newFrame, totals)
  
  writePath = file.path(savePath, paste(plotCode, '_biomass_est.csv'))
  write.csv(newFrame, file = writePath)
  
  totRow = c(plotCode, totmass, totBA, treeNum)
  
  #add totals
  if (i != 1) {
    
    totFrame = rbind(totFrame, totRow)
    
  } else {
    
    totFrame = data.frame(totRow[1], totRow[2], totRow[3], totRow[4], stringsAsFactors = FALSE)
    colnames(totFrame) = c('Plot', 'Biomass (kg)', 'Basal Area (m^2)', '# Tress')
    
  }
  
}

totFrame$`Biomass (kg/m^2)` = as.numeric(totFrame$`Biomass (kg)`)/40
totFrame$`Basal Area (m^2/ha)` = (as.numeric(totFrame$`Basal Area (m^2)`)/40)*100
totFrame$Lat = coordsFrame[match(paste(totFrame$Plot, 'CENTER'), paste(coordsFrame$Plot, coordsFrame$Point)), 13]
totFrame$Long = coordsFrame[match(paste(totFrame$Plot, 'CENTER'), paste(coordsFrame$Plot, coordsFrame$Point)), 14]

totPath = 'E:\\OR_Perkins\\OR_PlotData_2015\\biomass_estimates\\All_Plots.csv'
write.csv(totFrame, file = totPath)

print('done')