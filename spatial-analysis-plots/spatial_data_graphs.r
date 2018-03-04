
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
datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\forestdata.csv', row.names = 1)
# make biomass more readable
datatable$biok = round(datatable$bioha/1000)
# with groupings
publictable = datatable[datatable$public=='Y',]
privatetable = datatable[datatable$public=='N',]

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
    plot = plot + labs(color="Public Land")
  }
  
  
  plot
  
}

### ----------
###########################################
# nearest neighbor graph
###########################################
xvar = 'tpha'
xlab = 'Trees/ha'
yvar = 'nearest'
ylab = 'Nearest Neighbor (m)'
titletext = 'Mean Nearset Neighbor as a function of trees per hectare'
eqx = 900
eqy = 5.15

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('nearvstph.png')

xvar = 'qmd'
xlab = 'Quadratic Mean Diameter (cm)'
yvar = 'nearest'
ylab = 'Nearest Neighbor (m)'
titletext = 'Mean Nearset Neighbor as a function of quadratic mean diameter'
eqx = 20
eqy = 6.5

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('nearvsqmd.png')

###########################################
#mingling vs Shannon graph
###########################################
xvar = 'Shannon'
xlab = 'Shannon Index'
yvar = 'mingling'
ylab = 'Species Mingling'
titletext = 'Species mingling as a function of species diversity'
eqx = 0.3
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('minglingvsrich.png')

groupvar = 'public'
eqx = 0.4
eqy = 0.65
eqx2 = 0.4
eqy2 = 0.60
titletext = 'Species mingling as a function of species diversity,\ngrouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('minglinggroup.png')

###########################################
# mingling vs biomass graph
###########################################
xvar = 'biok'
xlab = 'Above Ground Biomass ((10^3)kg/ha)'
yvar = 'mingling'
ylab = 'Species Mingling'
titletext = 'Species mingling as a function of above ground biomass'
eqx = 285
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('minglingbiomass.png')

###########################################
# contagion graph
# apply scale of management
###########################################
# by diversity

xvar = 'Shannon'
xlab = 'Shannon Index'
yvar = 'contagion'
ylab = 'Stand Contagion'
titletext = 'Stand contagion as a function of species diversity'
eqx = 1.45
eqy = 0.655

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('contagionvsdiv.png')

# with group
groupvar = 'public'
eqx = 1.3
eqy = 0.72
eqx2 = 1.3
eqy2 = 0.71
titletext = 'Stand contagion as a function of species diversity, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('contagiongroup.png')

# by biomass

xvar = 'biok'
xlab = 'Above Ground Biomass ((10^3)kg/ha)'
yvar = 'contagion'
ylab = 'Stand Contagion'
titletext = 'Stand contagion as a function of quadratic mean diameter'
eqx = 40
eqy = 0.655

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('contagionvsqmd.png')

# with group
groupvar = 'public'
eqx = 40
eqy = 0.71
eqx2 = 40
eqy2 = 0.69
titletext = 'Stand contagion as a function of quadratic mean diameter, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('contagionqmdgroup.png')

###########################################
# DBH vs QMD graph
###########################################
xvar = 'qmd'
xlab = 'Quadratic Mean Diameter'
yvar = 'dbhdiff'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of QMD'
eqx = 25
eqy = 0.51

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiffqmd.png')

# with group

groupvar = 'public'
eqx = 62
eqy = 0.55
eqx2 = 62
eqy2 = 0.52
titletext = 'DBH Differentiation as a function of QMD, grouped by land ownership'

###########################################
# DBH vs Basal Area graph
###########################################
xvar = 'baha'
xlab = 'Basal Area (m2/ha)'
yvar = 'dbhdiff'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of Basal Area'
eqx = 50
eqy = 0.52

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiffbasal.png')

# with group

groupvar = 'public'
eqx = 40
eqy = 0.53
eqx2 = 40
eqy2 = 0.51
titletext = 'DBH Differentiation as a function of Basal Area, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('dbhgroupbasal.png')

####

xvar = 'tpha'
xlab = 'Trees per hectare'
yvar = 'dbhdiff'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of Basal Area'
eqx = 400
eqy = 0.52

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)
savepng('dbhdifftpa.png')
###########################################
# DBH vs Biomass Area graph
###########################################
xvar = 'biok'
xlab = 'Biomass ((10^3)kg/m2)'
yvar = 'dbhdiff'
ylab = 'DBH Differentiation'
titletext = 'DBH differentiation as a function of above ground biomass'
eqx = 300
eqy = 0.52

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('dbhdiffbio.png')

# with group

groupvar = 'public'
eqx = 280
eqy = 0.52
eqx2 = 280
eqy2 = 0.54
titletext = 'DBH Differentiation as a function of above ground biomass, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('dbhgroupbio.png')


# t test
compared.means = ggplot(datatable, aes(x=public, y=dbhdiff)) + geom_point()
compared.means = compared.means + labs(title='Comparison of means for DBH Differentiation values by Land Ownership', x='Land Ownership', y='DBH Differentiation') + geom_boxplot(color='blue') + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted')) + scale_x_discrete(labels = c('Private', 'Public'))
compared.means

savepng('dbhmeans.png')
# make plot matrix
ggpairs(datatable, horInd = seq(12,28, by=2), verInd = 2:9)