#Aggregate all GEO files together
#Runs on HOME COMPUTER

searchPath = 'F:\\Box Sync\\OR_Data\\geo_files'
savePath = 'F:\\Box Sync\\OR_Data\\statsOutputs\\individual_tree_data.csv'

dataList = list.files(path = searchPath, pattern = '[!~]???_Geo.csv', full.names = TRUE)

loopLen = length(dataList)

#initialize dataframe
outFrame = data.frame()

for (i in 1:loopLen) {
  
  #gather metadata
  plotPath = dataList[i]
  geoPlot = substr(basename(plotPath),1,3)
  
  plotData = read.csv(plotPath)[,1:8]
  
  #add new plot column
  plotData['Plot'] = geoPlot
  
  #reorder columns
  plotData = plotData[c(9,1,2,3,4,5,6,7,8)]
  
  outFrame = rbind(outFrame, plotData)
  
}

#time to get sample tree data
sampleDataPath = 'F:\\Box Sync\\OR_Data\\SampleAggregated.csv'

sampleData = read.csv(sampleDataPath)

newFrame = merge(outFrame, sampleData[,2:8], by.x = c('Plot', 'TreeID', 'Cell', 'Species', 'DBH'), by.y = c('Plot', 'TreeID', 'Cell', 'Species', 'DBH'), all.x = TRUE)

#load sample tree radii
radData = read.csv('F:\\Box Sync\\OR_Data\\statsOutputs\\sample_tree_data.csv')

newFrame = merge(newFrame, radData, by.x = c('Plot', 'TreeID', 'Cell', 'Total.Height'), by.y = c('Plot', 'Tree..', 'Cell', 'Total.Height'), all.x = TRUE)

write.csv(newFrame[c(1,2,3,4,13,14,15,16,17,6,5,7,8,9,10)], file = savePath)