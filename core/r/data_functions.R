# data loading

geoCsv = function(csv) {
    
    datatable = read.csv(csv)
    id = sub("_Geo.csv", "", basename(csv))
    datatable$id = id
    
    return(datatable)
}