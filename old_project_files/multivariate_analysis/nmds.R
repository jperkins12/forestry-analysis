# NMDS

require("vegan")
require("ggplot2")

# load in data tables
main_datapath = 'F:\\Box Sync\\OR_Data_Extra\\PCORD_Analysis\\main_mat.csv'
supp_datapath = 'F:\\Box Sync\\OR_Data_Extra\\PCORD_Analysis\\sec_mat.csv'

# load data with first col as row names
main_data = read.csv(main_datapath, row.names = 1)
supp_data = read.csv(supp_datapath, row.names = 1)
# species X environment matrix = A'E
community = t(main_data) %% supp_data