# spatial functions
library(iemisc)

spatialstats = function(stemID, plotdf) {
    
    refx = plotdf$X[plotdf$TreeID == stemID]
    refy = plotdf$Y[plotdf$TreeID == stemID]
    spc = plotdf$Species[plotdf$TreeID == stemID]
    dbh = plotdf$DBH[plotdf$TreeID == stemID]
    
    plotdf = plotdf[plotdf$TreeID != stemID,]
    
    plotdf$dist = sqrt((plotdf$X-refx)^2 + (plotdf$Y-refy)^2)
    plotdf$dbht = sapply(plotdf$DBH, function(x) {1 - (min(dbh, x)/max(dbh, x))})
    plotdf$mingling = ifelse(plotdf$Species == spc, 0 , 1)
    plotdf$azi = atan2d((plotdf$X-refx), (plotdf$Y-refy))
    plotdf$azi = ifelse(plotdf$azi < 0, plotdf$azi + 360, plotdf$azi)
    
    # sort by distance to point
    plotdf = plotdf[order(plotdf$dist),]
    
    # return contagion
    wvals = head(plotdf, 4)
    wvals = wvals[order(wvals$azi),]
    wvals$angle = wvals$azi-shift(wvals$azi)
    wvals$angle[is.na(wvals$angle)] = 360 - sum(wvals$angle, na.rm = TRUE)
    wvals$cval = ifelse(wvals$angle > 90, 0, 1)
    
    # return nearest-neighbor, contagion, species-mingling, and dbh-diff
    contagion = mean(wvals$cval)
    dbhdiff = mean(head(plotdf$dbht, 3))
    mingling = mean(head(plotdf$mingling, 3))
    nearest = head(plotdf$dist, 1)
    
    spatialidx = c(nearest, contagion, mingling, dbhdiff)
    
    return(spatialidx)
}

plotSpatial = function(df) {
    
    stems = df$TreeID
    
    spatialdf = as.data.frame(do.call("rbind", lapply(stems, spatialstats, df)))

    colnames(spatialdf) = c("nearest", "contagion", "mingling", "dbhdiff")
    
    plotnearest = mean(spatialdf$nearest, na.rm = TRUE)
    plotcontagion = mean(spatialdf$contagion, na.rm = TRUE)
    plotmingling = mean(spatialdf$mingling, na.rm = TRUE)
    plotdbhdiff = mean(spatialdf$dbhdiff, na.rm = TRUE)
    
    return(c(plotnearest, plotcontagion, plotmingling, plotdbhdiff))
    
}