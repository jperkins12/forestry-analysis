# data loader

loadData <- function() {
  # load all data
  
  path = file.path('..', '..', 'R', 'tables', 'all_data.csv')
  
  datatable = read.csv(path, row.names = 1)
  
  colnames(datatable) = c("Dominant Canopy Type", "Contagion", "DBH Differentiation", "Species Mingling", "Mean DBH", "Quadratic Mean Diameter", "Mean Nearest Neighbor", "Stem Count", "Public Land", "abicon", "ABICON", "abigra", "ABIGRA", "junocc", "JUNOCC", "larocc", "LAROCC", "piceng", "PICENG", "pincon", "PINCON", "pinpon", "PINPON", "psemen", "PSEMEN", "firs", "FIRS", "Species Richness", "Simpson Index", "Shannon Index", "Biomass (kg)", "Basal Area (m2)", "Biomass (kg/m2)", "Basal Area (m2/ha)")
  
  return(datatable)
  
}

spatialStats = function() {
  # load sptatial stats
  
  datatable = loadData()
  
  spatial_cols = c("Contagion", "DBH Differentiation", "Species Mingling", "Mean DBH", "Quadratic Mean Diameter", "Mean Nearest Neighbor", "Stem Count")
  
  return(datatable[,spatial_cols])
  
}

spatialStats()