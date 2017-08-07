#assemble all forest biometric data

#Load additional data
addl = read.csv('F:\\Box Sync\\OR_Data\\Accuracy_Assessment/Additional_Data.csv')

basePath = 'F:\\Box Sync\\OR_Data\\Accuracy_Assessment'
fileList = list.files(path = basePath, pattern = 'sampleStats.csv')
outpath = file.path(basePath, 'biometric_est.csv')

#determine number of plots
plotCount = length(fileList)

#initialize new dataframe
columnNames = c('Plot', '%Diff Height', '%Diff Rad', 'Density', 'Type')
newFrame = data.frame(matrix(ncol = length(columnNames), nrow = plotCount))
colnames(newFrame) = columnNames

#fetch data for each plot
for (i in 1:plotCount) {
  
  fileName = fileList[i]
  code = substr(fileName, 1, 3)
  filePath = file.path(basePath, fileName)
  csvData = read.csv(filePath)
  lastRow = nrow(csvData)
  diffHeight = csvData$X..Diff.Height[lastRow]
  diffRad = csvData$X..Diff.Radius[lastRow]
  ftype = addl[match(code, addl$Plot), 3]
  stemDensity = addl[match(code, addl$Plot), 5]
  
  info = c(code, diffHeight, diffRad, stemDensity, ftype)
  
  #add new data row
  newFrame[i,] = info
}

write.csv(newFrame, file = outpath)