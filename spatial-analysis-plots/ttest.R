# t-test results

datatable = read.csv('..\\..\\Thesis\\R\\tables\\all_data.csv', row.names = 1)

plot(datatable$Mingling, datatable$shannon)

equation.lm = lm(Mingling ~ poly(shannon, 2, raw = TRUE), data = datatable)

summary(equation.lm)
