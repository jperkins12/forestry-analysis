library(tidyverse)
library(stargazer)
library(directlabels)


distMatdir = 'F:\\Box Sync\\OR_Data\\DistMats'


fileList = list.files(path = distMatdir, pattern = '_join.csv', full.names = TRUE)

for (filename in fileList) {

    fplot = str_sub(basename(filename),1,3)
    
    distmat = read_csv(filename) %>%
        select(InputID, TargetID, Distance) %>%
        arrange(Distance) %>%
        mutate(InputID = as.character(InputID)) %>%
        mutate(TargetID = as.character(TargetID))
    
    distmat$actualCount = as.integer(count(distinct(distmat,InputID)))
    distmat$estCount = as.integer(count(distinct(distmat,TargetID)))
    distmat$plot = fplot
    distmat$cs = NA
    
    if (!exists("uniquepairs")) {
        uniquepairs = distmat[0,]
    }
    
    plotunique = distmat[0,]
    
    for (row in 1:nrow(distmat)) {
        
        actualid = distmat$InputID[row]
        estid = distmat$TargetID[row]
    
        if ((actualid %in% plotunique$InputID) | (estid %in% plotunique$TargetID)) {
            # pass
        } else {
            plotunique = bind_rows(plotunique, distmat[row,])
        }
    
    }
    
    plotunique$cs = cumsum(1/(plotunique$actualCount))
    
    uniquepairs = bind_rows(uniquepairs, plotunique)
    
}

uniquepairs = uniquepairs %>%
    left_join(plotCode_ref, by = c("plot" = "value")) %>%
    mutate(ID = as.character(ID)) %>%
    rename(`Plot ID` = ID)

uniquepairs = uniquepairs %>%
    left_join(as.tibble(maindata, rownames = "plot") %>%
                  select(plot, tpha, baha, qmd),
              by = c("plot" = "plot"))

uniquepairs = uniquepairs %>%
    left_join(metadata %>%
                  select(Plot, County, Elevation, Forest.Type), by = c("plot" = "Plot"))

summary_stats = uniquepairs %>%
    mutate(`Percent Accurate` = as.integer(round((estCount/actualCount)*100))) %>%
    mutate(`Percent Error` = as.integer(round(((estCount-actualCount)/actualCount)*100))) %>%
    group_by(`Plot ID`, actualCount, estCount, `Percent Accurate`, `Percent Error`) %>%
    summarise(`Mean Distance (m)` = round(mean(Distance), digits = 2)) %>%
    rename(`Actual Count` = actualCount, `Estimated Count` = estCount) %>%
    arrange(as.integer(`Plot ID`))

stargazer(uniquepairs %>%
              mutate(Elevation = as.integer(Elevation)) %>%
              mutate(baha = round(baha, digits = 2)) %>%
              mutate(qmd = round(qmd, digits = 2)) %>%
              group_by(`Plot ID`, County, Elevation, Forest.Type, tpha, baha, qmd) %>%
              summarise() %>%
              arrange(as.integer(`Plot ID`)) %>%
              rename(`Forest Type` = Forest.Type, `Trees/ha` = tpha, `Basal Area (m^2/ha)` = baha, `Quadratic Mean Diameter` = qmd),
          rownames = FALSE, summary = FALSE)

stargazer(summary_stats,
          rownames = FALSE,
          summary = FALSE)

sd(summary_stats$`Percent Accurate`)/sqrt(length(summary_stats$`Percent Accurate`))

sd(summary_stats$`Mean Distance (m)`)/sqrt(length(summary_stats$`Mean Distance (m)`))

####################################
savepng = function(filename) {
    
    path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\graphics'
    save_path = file.path(path, filename)
    ggsave(save_path, width = 7.5, height = 5, dpi = 100)
    
}

## Percent id'd over distance
stem_plot = ggplot(uniquepairs, aes(x=Distance, y=cs, colour=`Plot ID`, group=`Plot ID`, order = as.integer(`Plot ID`))) + geom_line()
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent of total stems identified by distance.', x = 'Distance (m)', y = 'Stems identified (percent)')
stem_plot = stem_plot + scale_x_continuous(limits = c(0, 20))
stem_plot = stem_plot + geom_dl(aes(label = `Plot ID`), method = list(dl.combine("last.points"), cex = 1))
stem_plot
filename = 'pctacc_v_dist.png'
savepng(filename)

# percent id'd by stem density
stem_plot = ggplot(uniquepairs, aes(x=Distance, y=cs, colour=tpha, group=`Plot ID`)) + geom_line()
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent of total stems identified by distance and stem density.', x = 'Distance (m)', y = 'Stems identified (percent)')
stem_plot = stem_plot + scale_x_continuous(limits = c(0, 20))
stem_plot = stem_plot + labs(color = "Trees/ha")
stem_plot
stem_plot
savepng('stemAccbyDens.png')

# percent id'd by basal area
stem_plot = ggplot(uniquepairs, aes(x=Distance, y=cs, colour=baha, group=`Plot ID`)) + geom_line()
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent of total stems identified by distance and basal area.', x = 'Distance (m)', y = 'Stems identified (percent)')
stem_plot = stem_plot + scale_x_continuous(limits = c(0, 20))
stem_plot = stem_plot + labs(color = "Basal Area (m^2/ha)")
stem_plot
stem_plot
savepng('stemAccbybasal.png')


stem_plot = ggplot(uniquepairs %>% group_by(`Plot ID`, estCount, actualCount, tpha, baha) %>% summarise() %>% mutate(`Percent Identified` = estCount/actualCount), aes(x=tpha, y=`Percent Identified`)) + geom_point() + geom_smooth(se = FALSE)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent of total stems identified by stem density.', x = 'Trees / ha', y = 'Stems identified (percent of total)')
stem_plot = stem_plot + scale_y_continuous(limits = c(0, 2))
stem_plot
stem_plot
savepng('stemidbyDens.png')

