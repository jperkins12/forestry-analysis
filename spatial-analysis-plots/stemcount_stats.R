library(tidyverse)
library(stargazer)

stemCounts = read_csv(file.path("C:", "Users", "Jamie", "Dropbox", "Thesis", "R", "tables", "stemCountEst.csv"))

analyseCounts = stemCounts %>%
    inner_join(plotCode_ref, by = c('Plot' = 'value')) %>%
    mutate(Diff = `Estimated Stem Count`-`Actual Stem Count`) %>%
    mutate(Error = Diff/`Actual Stem Count`)
    

ftest = var.test(analyseCounts$`Actual Stem Count`, analyseCounts$`Estimated Stem Count`)
ttest = t.test(analyseCounts$`Actual Stem Count`, analyseCounts$`Estimated Stem Count`, paired = TRUE)
meaddiff - mean(analyseCounts$Diff)


tablecols = c("Test", "Statistic", "P-Value", "DF")

f.row = c(
    "F-Test",
    ftest$statistic,
    ftest$p.value,
    NA
)

t.row = c(
    "T-Test",
    ttest$statistic,
    ttest$p.value,
    ttest$parameter
)

statsTable = as.tibble(rbind(t.row, f.row))
colnames(statsTable) = tablecols

statsTable = statsTable %>%
    type_convert() %>%
    mutate(`P-Value` = signif(`P-Value`, 3)) %>%
    mutate(Statistic = signif(Statistic, 2))

stargazer(statsTable, rownames = FALSE, summary = FALSE, digits = 2)
