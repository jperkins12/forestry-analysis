
#
# --- Run below every time
#

# package check
loadPackages = function(packages) {
  new.packages = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for (i in packages) {
    library(i, character.only = TRUE)
  }
}

required.packages = c('ggplot2', 'GGally')

loadPackages(required.packages)

# spatial data graphs

# load data
# source("dataLoader.R")
# datatable = loadData()
 datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\all_data.csv', row.names = 1)
# with groupings
publictable = datatable[datatable$`Public.Land`=='Y',]
privatetable = datatable[datatable$`Public.Land`=='N',]

savepng = function(filename) {
  
  path = 'C:\\Users\\Jamie\\Dropbox\\Thesis\\graphics'
  save_path = file.path(path, filename)
  ggsave(save_path, width = 7.5, height = 5, dpi = 100)
  
}

# adds linear equation and r-squared to plot
lm_eqn <- function(df){
  y <- df[,1]
  x <- df[,2]
  m <- lm(y ~ poly(x,2), df);
  eq <- substitute(italic(y) == a + b %.% italic(x) + c %.% italic(x)^2*","~~italic(r)^2~"="~r2, 
    list(a = format(coef(m)[1], digits = 2), 
         b = format(coef(m)[2], digits = 2),
         c = format(coef(m)[3], digits = 2),
         r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


# generate single plot

plotvars <- function(datatable, xcol, ycol, xlab, ylab, title, eqx, eqy, grouping=FALSE, groupvar=NULL, groupdata=NULL, groupdata2=NULL, eqx2=NULL, eqy2=NULL, xlim=NULL, ylim=NULL){
  
  plot = ggplot(datatable, aes_string(x=xcol, y=ycol, color=groupvar))
  
  # add limits
  if (!is.null(xlim)) {
    
    plot = plot + xlim(1,xlim)
    
  }
  
  if (!is.null(ylim)) {
    
    plot = plot + ylim(0,ylim)
    
  }
  
  plot = plot + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
  plot = plot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
  plot = plot + labs(title=title, x=xlab, y=ylab)
  
  if (grouping == FALSE) {
    
    plot = plot + geom_text(x=eqx, y=eqy, label=lm_eqn(data.frame(datatable[[ycol]],datatable[[xcol]])), parse = TRUE)
    
  } else {
    
    plot = plot + geom_text(x=eqx, y=eqy, label=lm_eqn(data.frame(groupdata[[ycol]], groupdata[[xcol]])), parse = TRUE, aes(color="N", fill='white'))
    plot = plot + geom_text(x=eqx2, y=eqy2, label=lm_eqn(data.frame(groupdata2[[ycol]], groupdata2[[xcol]])), parse = TRUE, aes(color="Y", fill='white'))
    
  }
  
  
  plot
  
}

### ----------
###########################################
#mingling vs Shannon graph
###########################################
xvar = 'shannon'
xlab = 'Shannon Index'
yvar = 'Mingling'
ylab = 'Species Mingling'
titletext = 'Species mingling as a function of species diversity'
eqx = 0.3
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('minglingvsrich.png')

groupvar = 'Public.Land'
eqx = 0.4
eqy = 0.65
eqx2 = 0.4
eqy2 = 0.60
titletext = 'Species mingling as a function of species diversity,\ngrouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('minglinggroup.png')

###########################################
# mingling vs PINPON graph
###########################################
xvar = 'PINPON'
xlab = 'Relative PINPON Abundance'
yvar = 'Mingling'
ylab = 'Species Mingling'
titletext = 'Species mingling as a function of relative PINPON abundance'
eqx = 0.8
eqy = 0.8

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, ylim = 1)

savepng('minglingpinpon.png')

# with group
groupvar = 'Public.Land'
eqx = 0.8
eqy = 0.9
eqx2 = 0.8
eqy2 = 0.84
titletext = 'Species mingling as a function of relative PINPON abundance, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2, ylim = 1)

savepng('pinpongroup.png')

###########################################
# contagion graph
# apply scale of management
###########################################
xvar = 'PINPON'
xlab = 'Relative PINPON Abundance'
yvar = 'Contagion'
ylab = 'Stand Contagion'
titletext = 'Stand contagion as a function of species diversity'
eqx = 0.7
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

# by diversity

xvar = 'shannon'
xlab = 'Shannon Index'
yvar = 'Contagion'
ylab = 'Stand Contagion'
titletext = 'Stand contagion as a function of species diversity'
eqx = 1.4
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('contagionvsrich.png')

# with group
groupvar = 'Public.Land'
eqx = 1.3
eqy = 0.69
eqx2 = 1.3
eqy2 = 0.68
titletext = 'Stand contagion as a function of species diversity, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('contagiongroup.png')

###########################################
# DBH vs QMD graph
###########################################
xvar = 'Quadratic.Mean.Diameter'
xlab = 'Quadratic Mean Diameter'
yvar = 'DBH.Differentiation'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of QMD'
eqx = 62
eqy = 0.55

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiff.png')

# with group

groupvar = 'Public.Land'
eqx = 62
eqy = 0.55
eqx2 = 62
eqy2 = 0.52
titletext = 'DBH Differentiation as a function of QMD, grouped by land ownership'

###########################################
# DBH vs Basal Area graph
###########################################
xvar = 'Basal.Area..m.2.ha.'
xlab = 'Basal Area (m2/ha)'
yvar = 'DBH.Differentiation'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of Basal Area'
eqx = 62
eqy = 0.55

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiffbasal.png')

# with group

groupvar = 'Public.Land'
eqx = 62
eqy = 0.55
eqx2 = 62
eqy2 = 0.52
titletext = 'DBH Differentiation as a function of Basal Area, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('dbhgroupbasal.png')

###########################################
# DBH vs Biomass Area graph
###########################################
xvar = 'Biomass..kg.m.2.'
xlab = 'Biomass (kg/m2)'
yvar = 'DBH.Differentiation'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of Basal Area'
eqx = 1200
eqy = 0.51

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiffbasal.png')

# with group

groupvar = 'Public.Land'
eqx = 1200
eqy = 0.51
eqx2 = 1200
eqy2 = 0.53
titletext = 'DBH Differentiation as a function of Biomass, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('dbhgroupbasal.png')


# t test
compared.means = ggplot(datatable, aes(x=Public.Land, y=DBH.Differentiation)) + geom_point()
compared.means = compared.means + labs(title='Comparison of means for DBH Differentiation values by Land Ownership', x='Land Ownership', y='DBH Differentiation') + geom_boxplot(color='blue') + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted')) + scale_x_discrete(labels = c('Private', 'Public'))
compared.means

savepng('dbhmeans.png')
# make plot matrix
ggpairs(datatable, horInd = seq(12,28, by=2), verInd = 2:9)