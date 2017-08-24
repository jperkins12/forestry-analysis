
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

plotvars <- function(datatable, xcol, ycol, xlab, ylab, title, eqx, eqy, grouping=FALSE, groupvar=NULL, groupdata=NULL, groupdata2=NULL, eqx2=NULL, eqy2=NULL){
  
  plot = ggplot(datatable, aes_string(x=xcol, y=ycol, color=groupvar))
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

#savepng('minglingvsrich.png')

groupvar = 'Public.Land'
eqx2 = 3
eqy2 = 0.83
titletext = 'Species mingling as a function of species richness, grouped by land ownership'

plotvars(datatable, xcol = xvar, ycol = yvar, xlab = xlab, ylab = ylab, title = titletext, eqx = eqx, eqy = eqy, grouping = TRUE, groupvar = groupvar, groupdata = privatetable, groupdata2 = publictable, eqx2 = eqx2, eqy2 = eqy2)


# with group
minglinggroup = ggplot(datatable, aes(x=Species.Richness, y=Mingling, color=Public.Land))
minglinggroup = minglinggroup + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
minglinggroup = minglinggroup + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
minglinggroup = minglinggroup + labs(title='Species mingling as a function of species richness', x='Species Richness', y='Species Mingling')
minglinggroup = minglinggroup + geom_text(x=3, y=0.75, label=lm_eqn(data.frame(privatetable$Mingling,privatetable$Species.Richness)), parse = TRUE, aes(color="N", fill='white'))
minglinggroup = minglinggroup + geom_text(x=3, y=0.83, label=lm_eqn(data.frame(publictable$Mingling,publictable$Species.Richness)), parse = TRUE, aes(color="Y", fill='white'))

minglinggroup

savepng('minglinggroup.png')


# mingling vs PINPON graph
minglingpinpon = ggplot(datatable, aes(x=PINPON, y=Mingling)) + ylim(0,1)
minglingpinpon = minglingpinpon + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
minglingpinpon = minglingpinpon + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
minglingpinpon = minglingpinpon + labs(title='Species mingling as a function of relative PINPON abundance', x='Relative PINPON Abundance', y='Species Mingling')
minglingpinpon = minglingpinpon + geom_text(x=0.8, y=0.8, label=lm_eqn(data.frame(datatable$Mingling,datatable$Species.Richness)), parse = TRUE)

minglingpinpon

savepng('minglingpinpon.png')

# with group
pinpongroup = ggplot(datatable, aes(x=PINPON, y=Mingling, color=Public.Land)) + ylim(0,1)
pinpongroup = pinpongroup + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
pinpongroup = pinpongroup + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
pinpongroup = pinpongroup + labs(title='Species mingling as a function of relative PINPON abundance', x='Relative PINPON Abundance', y='Species Mingling')
pinpongroup = pinpongroup + geom_text(x=0.8, y=0.9, label=lm_eqn(data.frame(privatetable$Mingling,privatetable$Species.Richness)), parse = TRUE, aes(color="N", fill='white'))
pinpongroup = pinpongroup + geom_text(x=0.8, y=0.84, label=lm_eqn(data.frame(publictable$Mingling,publictable$Species.Richness)), parse = TRUE, aes(color="Y", fill='white'))

pinpongroup

savepng('pinpongroup.png')

# contagion graph
contagionplot = ggplot(datatable, aes(x=Species.Richness, y=Contagion)) + xlim(1,7)
contagionplot = contagionplot + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
contagionplot = contagionplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
contagionplot = contagionplot + labs(title='Stand contagion as a function of species richness', x='Species Richness', y='Stand Contagion')
contagionplot = contagionplot + geom_text(x=5.1, y=0.65, label=lm_eqn(data.frame(datatable$Contagion,datatable$Species.Richness)), parse = TRUE)

contagionplot

savepng('contagionvsrich.png')

# with group
contagiongroup = ggplot(datatable, aes(x=Species.Richness, y=Contagion, color=Public.Land))
contagiongroup = contagiongroup + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
contagiongroup = contagiongroup + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
contagiongroup = contagiongroup + labs(title='Stand contagion as a function of species richness', x='Species Richness', y='Stand Contagion')
contagiongroup = contagiongroup + geom_text(x=5.2, y=0.69, label=lm_eqn(data.frame(privatetable$Contagion,privatetable$Species.Richness)), parse = TRUE, aes(color="N", fill='white'))
contagiongroup = contagiongroup + geom_text(x=5.2, y=0.68, label=lm_eqn(data.frame(publictable$Contagion,publictable$Species.Richness)), parse = TRUE, aes(color="Y", fill='white'))

contagiongroup

savepng('contagiongroup.png')


# DBH graph
dbhplot = ggplot(datatable, aes(x=Quadratic.Mean.Diameter, y=DBH.Differentiation))
dbhplot = dbhplot + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
dbhplot = dbhplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
dbhplot = dbhplot + labs(title='DBH differentiation as a function of QMD', x='Quadratic Mean Diameter', y='DBH Differentiation')
dbhplot = dbhplot + geom_text(x=62, y=0.55, label=lm_eqn(data.frame(datatable$DBH.Differentiation,datatable$Quadratic.Mean.Diameter)), parse = TRUE)

dbhplot

savepng('dbhdiff.png')

# with group
dbhgroup = ggplot(datatable, aes(x=Quadratic.Mean.Diameter, y=DBH.Differentiation, color=Public.Land))
dbhgroup = dbhgroup + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
dbhgroup = dbhgroup + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
dbhgroup = dbhgroup + labs(title='DBH Differentiation as a function of QMD', x='Species Richness', y='Stand dbh')
dbhgroup = dbhgroup + geom_text(x=62, y=0.55, label=lm_eqn(data.frame(privatetable$Quadratic.Mean.Diameter,privatetable$DBH.Differentiation)), parse = TRUE, aes(color="N", fill='white'))
dbhgroup = dbhgroup + geom_text(x=62, y=0.52, label=lm_eqn(data.frame(publictable$Quadratic.Mean.Diameter,publictable$DBH.Differentiation)), parse = TRUE, aes(color="Y", fill='white'))

dbhgroup

savepng('dbhgroup.png')