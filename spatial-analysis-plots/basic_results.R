# Basic Data Table

require(stargazer)

# spatial data graphs

datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\JMP_spatial_data.csv')

cols = c('Dominant.Canopy', 'Contagion', 'DBH.Differentiation', 'Mingling', 'Quadratic.Mean.Diameter', 'Mean.Nearest.Neighbor', 'Total.Stems', 'Public.Land')
cols_formatted = c('Dominant Canopy', 'Contagion', 'DBH Differentiation', 'Species Mingling', 'Quadratic Mean Diameter', 'Mean Nearest Neighbor', 'Stem Count', 'Public Land')

spatial_stats = datatable[,cols]
colnames(spatial_stats) = cols_formatted

acols = c('ABICON', 'ABIGRA', 'JUNOCC', 'LAROCC', 'PICENG', 'PINCON', 'PINPON', 'PSEMEN')

abundance_stats = datatable[,acols]

diversity_table = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\diversity.csv')

# print data
stargazer(spatial_stats, summary = FALSE)

stargazer(abundance_stats, summary = FALSE)

stargazer(diversity_table[,2:4], summary = FALSE)