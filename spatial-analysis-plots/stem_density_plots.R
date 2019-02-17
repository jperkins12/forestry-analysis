# Stem accuracy charts
# export with 750x500px

#################################################
rm(list=ls()) 
#################################################
library(ggplot2)

stem_data_path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\stats\\Stem_Distance_Accuracy_Table.csv'
stem_data = read.csv(stem_data_path)

stem_data = as.tibble(left_join(stem_data, plotCode_ref, by = c("Plot" = "value"))) %>%
    mutate(ID = as.character(ID)) %>%
    rename(`Plot ID` = ID)

stem_data$Percent.Accurate = as.numeric(sub('%','',stem_data$Percent.Accurate))/100
stem_data$Commission = as.numeric(sub('%','',stem_data$Commission))/100

# Convert to per ha
stem_data$Stem.Density = stem_data$Stem.Density * 6.25
stem_data$Basal.Area = stem_data$Basal.Area * 6.25

toPercent = function(x) {
  
  return(paste0(as.character(x*100), '%'))
  
}

savepng = function(filename) {
  
  path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\graphics'
  save_path = file.path(path, filename)
  ggsave(save_path, width = 7.5, height = 5, dpi = 100)
  
}

# Percent Accuracy Plot
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Percent.Accurate, colour = `Plot ID`, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent accuracy at varying distance thresholds.', x = 'Distance (m)', y = 'Stem count accuracy (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)

stem_plot
filename = 'pctacc_v_dist.png'
savepng(filename)
######################

# Percent Accuracy Plot by Forest Type
# stemAccbyType
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Percent.Accurate, colour = Forest.Type, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent accuracy at varying distance thresholds by forest type.', x = 'Distance (m)', y = 'Stem count accuracy (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)
stem_plot = stem_plot + labs(colour = 'Forest Type')

stem_plot
filename = 'stemAccbyType.png'
savepng(filename)
######################

# Percent Accuracy Plot by Stem Density
# stemAccbyDens
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Percent.Accurate, colour = Stem.Density, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Percent accuracy at varying distance thresholds by stem density.', x = 'Distance (m)', y = 'Stem count accuracy (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)
stem_plot = stem_plot + labs(colour = 'Stem Density\n(stems*ha)')

stem_plot
savepng('stemAccbyDens.png')
######################

# Commission Error
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Commission, colour = `Plot ID`, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Commission error at varying distance thresholds.', x = 'Distance (m)', y = 'Commission Error (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)

stem_plot
savepng('comErrorPlot.png')
######################

# Commission Error by Forest Type
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Commission, colour = Forest.Type, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Commission error at varying distance thresholds by forest type.', x = 'Distance (m)', y = 'Commission Error (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)
stem_plot = stem_plot + labs(colour = 'Forest Type')

stem_plot
savepng('comErrorType.png')
######################

# Commission Error by Stem Density
######################
stem_plot = ggplot(stem_data, aes(x = Distance, y = Commission, colour = Stem.Density, group = `Plot ID`))
stem_plot = stem_plot + scale_y_continuous(labels = toPercent)
stem_plot = stem_plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
stem_plot = stem_plot + labs(title = 'Commission error at varying distance thresholds by stem density.', x = 'Distance (m)', y = 'Commission Error (percent)')
stem_plot = stem_plot + geom_line() + stat_summary(aes(group = 1), geom = 'line', fun.y = mean, size = 1.5)
stem_plot = stem_plot + labs(colour = 'Stem Density\n(stems*ha)')

stem_plot
savepng('comErrorDens.png')
######################