# forestry biometrics functions

tpha = function(treecount, plotarea=1600) {
    
    # return trees/ha
    tpha = (treecount/plotarea)*10000
    return(tpha)
}

qmd = function(ba, tpa, unittype="metric" ) {
    
    # Function to calculate the quadratic mean diameter from basal area and tree per acre
    # by David R. Larsen, Copyright October 9, 2012
    # Creative Commons http://creativecommons.org/licenses/by-nc/3.0/us/
    
    if (unittype == "imperial"){
        qmd = sqrt((ba / tpa) / 0.005454154)
    }else if (unittype == "metric"){
        qmd = sqrt((ba / tpa) / 0.00007854)
    }else{
        qmd = 0
    }
    return(qmd)
}

basal = function(dbh, unittype="metric") {
    
    # calculate basal area on a sigle tree
    if (unittype == "imperial") {
        # output in ft2
        basal = 0.005454 * dbh^2
    } else if (unittype == "metric") {
        # output m2
        basal = 0.00007854 * dbh^2
    } else {
        basal = 0
    }
    return(basal)
}

basalha = function(basalvector, plotarea=1600, unittype="metric") {
    
    totalbasal = sum(basalvector, na.rm = TRUE)
    
    # calculate basal area per ha
    if (unittype == "metric") {
        basalha = (totalbasal/plotarea) * 10000
    }
    else {
        basalha = 0
    }
    return(basalha)
}