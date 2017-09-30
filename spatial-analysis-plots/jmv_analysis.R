# jmv analysis
require(jmv)

datatable = read.csv('..\\..\\Thesis\\R\\tables\\all_data.csv', row.names = 1)

colnames(datatable) = c("Dominant Canopy Type", "Contagion", "DBH Differentiation", "Species Mingling", "Mean DBH", "Quadratic Mean Diameter", "Mean Nearest Neighbor", "Stem Count", "Public Land", "abicon", "ABICON", "abigra", "ABIGRA", "junocc", "JUNOCC", "larocc", "LAROCC", "piceng", "PICENG", "pincon", "PINCON", "pinpon", "PINPON", "psemen", "PSEMEN", "firs", "FIRS", "Species Richness", "Simpson Index", "Shannon Index")

jmv::linReg(
  data = datatable,
  dep = "Species Mingling",
  blocks = list(
    "Shannon Index"),
  fitMeasures = c("r", "r2", "r2Adj"))
