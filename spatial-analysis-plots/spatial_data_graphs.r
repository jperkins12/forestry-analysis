
#
# --- Run below every time
#

# spatial data graphs
require(ggplot2)

datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\JMP_spatial_data.csv')
# with groupings
publictable = datatable[datatable$Public.Land=='Y',]
privatetable = datatable[datatable$Public.Land=='N',]

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
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
    list(a = format(coef(m)[1], digits = 2), 
         b = format(coef(m)[2], digits = 2), 
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

# mingling graph
xvar = 'Species.Richness'
xlab = 'Species Richness'
yvar = 'Mingling'
ylab = 'Species Mingling'
titletext = 'Species mingling as a function of species richness'
eqx = 3
eqy = 0.75

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy)

savepng('minglingvsrich.png')

groupvar = 'Public.Land'
eqx2 = 3
eqy2 = 0.83
titletext = 'Species mingling as a function of species richness,\ngrouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('minglinggroup.png')


# mingling vs PINPON graph

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


# contagion graph

xvar = 'Species.Richness'
xlab = 'Species Richness'
yvar = 'Contagion'
ylab = 'Stand Contagion'
titletext = 'Stand contagion as a function of species richness'
eqx = 5.1
eqy = 0.65

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, xlim = 7)

savepng('contagionvsrich.png')

# with group
groupvar = 'Public.Land'
eqx = 5.2
eqy = 0.69
eqx2 = 5.2
eqy2 = 0.68
titletext = 'Stand contagion as a function of species richness, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('contagiongroup.png')


# DBH graph

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

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)

savepng('dbhgroup.png')