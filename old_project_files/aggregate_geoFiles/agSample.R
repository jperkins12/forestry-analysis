#aggregate sample tree data
searchPath = 'F:\\Box Sync\\OR_Data\\SampleTree_files'
savePath = 'F:\\Box Sync\\OR_Data\\statsOutputs\\sample_tree_data.csv'

dataList = list.files(path = searchPath, pattern = '[!~]???_sampleTrees.csv', full.names = TRUE)

loopLen = length(dataList)

#initialize dataframe
outFrame = data.frame()

for (i in 1:loopLen) {
  
  #gather metadata
  plotPath = dataList[i]
  geoPlot = substr(basename(plotPath),1,3)
  
  plotData = read.csv(plotPath, skip = 1)[,c('Tree..','Cell','Total.Height','Crown.Bottom', 'Radius.1', 'Radius.2', 'Radius.3', 'Radius.4')]
  
  #add new plot column
  plotData['Plot'] = geoPlot
  
  #reorder columns
  plotData = plotData[c(9,1,2,3,4,5,6,7,8)]
  
  outFrame = rbind(outFrame, plotData)
  
}

write.csv(outFrame, savePath)