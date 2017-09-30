###########################################
#Stand_spatial_analysis_main
#
#Use this script to compute spatial distribution indicies of
#stems in a stand, given (X,Y) stem coordinates
#
#Author: Jamie Perkins
#Email: jp2081@wildcats.unh.edu
###########################################

dirPath = file.path("F:", "Box Sync", "OR_Data", "geo_files")
flist = list.files(dirPath, pattern = ".*Geo.*")
pcodes = sub("_Geo.csv", "", flist)

fileIter = length(flist)

# initialise results
results = 0

############################################
#functions

qmd = function( ba, tpa, unittype="metric" ) {

    # Function to calculate the quadratic mean diameter from basal area and tree per acre
    # by David R. Larsen, Copyright October 9, 2012
    # Creative Commons http://creativecommons.org/licenses/by-nc/3.0/us/
    
    if (unittype == "imperial"){
        qmd = sqrt((ba / tpa) / 0.005454154)
    }else if (unitype == "metric"){
        qmd = sqrt((ba / tpa) / 0.00007854)
    }else{
        qmd = 0
    }
    qmd
}

basal = function(dbh, unittype="metric") {
    
    # calculate basal area on a sigle tree
    if (unittype == "imperial") {
        basal = 0.005454 * dbh^2
    } else if (unittype == "metric") {
        basal = 0.00007854 * dbh^2
    } else {
        basal = 0
    }
    basal
}


###########################################
for (q in 1:fileIter) {
    
    # generate full path
    fullpath = file.path(dirPath, flist[q])
    pcode = pcodes[q]
    
    # pull info on stems to table
    stemInfo = read.csv(fullpath)
    
    meanDBH = mean(stemInfo$DBH, na.rm = TRUE)
    
    # calc ba/ha with plot size of 1600 m^2
    basalarea = (sum(basal(stemInfo$DBH))/1600)*40
}


