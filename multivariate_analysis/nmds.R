# NMDS

require("vegan")
require("ggplot2")

# load in data tables
main_datapath = 'F:\\Box Sync\\OR_Data_Extra\\PCORD_Analysis\\main_mat.csv'
supp_datapath = 'F:\\Box Sync\\OR_Data_Extra\\PCORD_Analysis\\sec_mat.csv'

# load data with first col as row names
main_data = read.csv(main_datapath, row.names = 'X')
supp_data = read.csv(supp_datapath, row.names = 'X')

data.rda = rda(main_data[,c(2:ncol(main_data))], supp_data[,2:ncol(supp_data)-1])

plot(data.rda)

ordipointlabel(data.rda, display = "species", col = c(1), add = TRUE)
ordipointlabel(data.rda, display = "sites", col = c(2), add = FALSE)

orditorp(data.rda, display = "sites", priority = supp_data$totalStems, air = 25)

ordihull(data.rda, group=supp_data$PublicLand, col = c(2,3))

