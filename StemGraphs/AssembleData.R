#Load additional data
addl = read.csv('F:\\Box Sync\\OR_Data\\Accuracy_Assessment/Additional_Data.csv')

#choose accuracy assessment dir
statsDir = choose.dir()
fileList = list.files(path = statsDir, pattern = 'stemStats.csv')

#names for rows
rnames = c('1m', '2m', '3m', '4m', '5m', 'All', 'Type', 'Elevation', 'County', 'Stem Density' , 'Basal Area', 'Mean DBH')
rowCount = length(rnames)

#determine number of plots
plotCount = length(fileList)

#initiate dataframe
statsStoreAcc = data.frame(matrix(ncol = plotCount, nrow = rowCount), row.names = rnames)
statsStoreCom = data.frame(matrix(ncol = plotCount, nrow = rowCount), row.names = rnames)
#initiate column names vector
cols = c()

#fetch data for each plot
for (i in 1:plotCount) {
  
  fileName = fileList[i]
  code = substr(fileName, 1, 3)
  filePath = file.path(statsDir, fileName)
  stats = read.csv(filePath)
  percentAccurate = as.numeric(stats$X..Accurate.1)
  percentCom = as.numeric(stats$X..Commission.Error)
  statsStoreAcc[1:6, i] = percentAccurate
  statsStoreCom[1:6, i] = percentCom
  cols[i] = code
  ftype = addl[match(code, addl$Plot), 3]
  county = as.character(addl[match(code, addl$Plot), 2])
  elevation = addl[match(code, addl$Plot), 4]
  stemDensity = addl[match(code, addl$Plot), 5]
  basal = addl[match(code, addl$Plot), 6]
  meanDBH = addl[match(code, addl$Plot), 7]
  statsStoreAcc[7, i] = ftype
  statsStoreCom[7, i] = ftype
  statsStoreAcc[8, i] = elevation
  statsStoreCom[8, i] = elevation
  statsStoreAcc[9, i] = county
  statsStoreCom[9, i] = county
  statsStoreAcc[10, i] = stemDensity
  statsStoreCom[10, i] = stemDensity
  statsStoreAcc[11, i] = basal
  statsStoreCom[11, i] = basal
  statsStoreAcc[12, i] = meanDBH
  statsStoreCom[12, i] = meanDBH
}

#add plot (column) names
colnames(statsStoreAcc) = cols
colnames(statsStoreCom) = cols

#plot layout
yrange = c(0,1)
xrange = c(1,5)
plot(xrange, yrange, type = 'n', xlab = 'Distance (meters)', ylab = 'Percent')

#Original color set
#colors = rainbow(plotCount)

#alt color set
colors = c('red', 'blue')

linetype = 1:plotCount
plotchar = seq(18,18+plotCount,1)

#add data for each plot
for (i in 1:plotCount) {
  
  plot = statsStoreAcc[1:6,i]
  colorVal = statsStoreAcc[7,i]
  lines(plot, type = 'b', lwd = 1.5, lty = linetype[1], col = colors[colorVal], pch = plotchar[i])
  
}

legend(1,1,cols, col = colors, pch = plotchar)

#reorganize for jmp
newColnames = c('Plot', 'Distance', 'Percent Accurate', 'Commission Error', 'Type', 'Elevation', 'County', 'Stem Density' , 'Basal Area', 'Mean DBH' )

jmpData = data.frame(matrix(ncol = length(newColnames), nrow = 6*plotCount))
colnames(jmpData) = newColnames

for (i in 1:plotCount) {
  
  plotName = cols[i]
  distNames = c(1:6)
  distVals = statsStoreAcc[1:6,i]
  comVals = statsStoreCom[1:6, i]
  countyVal = statsStoreAcc[9,i]
  typeVal = statsStoreAcc[7,i]
  eleVal = statsStoreAcc[8,i]
  stemDens = statsStoreAcc[10, i]
  basalA = statsStoreAcc[11, i]
  mDBH = statsStoreAcc[12, i]
  
  rowLoc = 1+(i-1)*6
  rowEnd = rowLoc+5
  
  jmpData$Plot[rowLoc:rowEnd] = plotName
  jmpData$Distance[rowLoc:rowEnd] = distNames
  jmpData$`Percent Accurate`[rowLoc:rowEnd] = distVals
  jmpData$Type[rowLoc:rowEnd] = typeVal
  jmpData$Elevation[rowLoc:rowEnd] = eleVal
  jmpData$County[rowLoc:rowEnd] = countyVal
  jmpData$`Stem Density`[rowLoc:rowEnd] = stemDens
  jmpData$`Basal Area`[rowLoc:rowEnd] = basalA
  jmpData$`Mean DBH`[rowLoc:rowEnd] = mDBH
  jmpData$`Commission Error`[rowLoc:rowEnd] = comVals
  
}

#transpose data
accTrans = t(statsStoreAcc)
comTrans = t(statsStoreCom)

#merge tables
tableOut = cbind(accTrans[,1:6], comTrans[,1:ncol(comTrans)])

write.csv(tableOut,file = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment/dataTable.csv')
write.csv(jmpData, file = 'F:\\Box Sync\\OR_Data\\Additional_Data/jmpstats.csv')
write.csv(accTrans, file = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment/Aggregated_StemStats_Acc.csv')
write.csv(comTrans, file = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment/Aggregated_StemStats_Com.csv')