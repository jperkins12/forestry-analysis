# biodiversity_measures

require(vegan)
require(Hotelling)

species_counts = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\species_counts.csv', row.names = 1)

# get species count data

names(species_counts)

richness = specnumber(species_counts)
shannon = diversity(species_counts)
simpson = diversity(species_counts, "simpson")

diversity_table = data.frame(cbind(simpson, shannon, richness))

#######
datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\jmp_spatial_data.csv', row.names = 1)
diversity = cbind(datatable, diversity_table)


write.csv(diversity_table, 'C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\diversity.csv')
