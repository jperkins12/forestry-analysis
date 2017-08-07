# spatial data graphs
require(ggplot2)

datatable = read.csv('C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\JMP_spatial_data.csv')

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

# mingling graph
minglingplot = ggplot(datatable, aes(x=Species.Richness, y=Mingling))
minglingplot = minglingplot + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
minglingplot = minglingplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
minglingplot = minglingplot + labs(title='Species mingling as a function of species richness', x='Species Richness', y='Species Mingling')
minglingplot = minglingplot + geom_text(x=3, y=0.75, label=lm_eqn(data.frame(datatable$Mingling,datatable$Species.Richness)), parse = TRUE)

minglingplot

# with groupings
publictable = datatable[datatable$Public.Land=='Y',]
privatetable = datatable[datatable$Public.Land=='N',]

minglinggroup = ggplot(datatable, aes(x=Species.Richness, y=Mingling, color=Public.Land))
minglinggroup = minglinggroup + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x,2))
minglinggroup = minglinggroup + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
minglinggroup = minglinggroup + labs(title='Species mingling as a function of species richness', x='Species Richness', y='Species Mingling')
minglinggroup = minglinggroup + geom_text(x=3, y=0.75, label=lm_eqn(data.frame(privatetable$Mingling,privatetable$Species.Richness)), parse = TRUE, aes(color="N", fill='white'))
minglinggroup = minglinggroup + geom_text(x=3, y=0.83, label=lm_eqn(data.frame(publictable$Mingling,publictable$Species.Richness)), parse = TRUE, aes(color="Y", fill='white'))


minglinggroup