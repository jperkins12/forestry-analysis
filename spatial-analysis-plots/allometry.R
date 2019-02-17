# allometric eqns
library(ggplot2)
library(stargazer)
library(broom)
library(tidyverse)
library(scales)
library(Metrics)
source('plot_data_gen.R')

# load sample data
sampleTreeData = as_tibble(sampledata[,c('Plot', 'Species', 'DBH', 'Total.Height', 'Est.Height', 'Actual.Radius', 'Est.Radius')])
sampleTreeData$lnDBH = log(sampleTreeData$DBH)
sampleTreeData$lnHeight = log(sampleTreeData$Total.Height)


vars_calc = function(spc) {
    # calculate intercept and coefficient for each species
    # return relevent stats and vars in vector
    
    species_data = sampleTreeData[sampleTreeData$Species == spc,]
    eqn = lm(species_data$lnDBH ~ species_data$Total.Height)
    eqnsummary = summary(eqn)
    values = c(
        spc,
        as.numeric(eqnsummary$coefficients[1]),
        eqnsummary$coefficients[3],
        eqnsummary$coefficients[2],
        eqnsummary$coefficients[4],
        eqnsummary$r.squared,
        eqnsummary$df[2]
        )
}

speciesList = unique(sampleTreeData$Species)

vars_frame = as_tibble(
    as.data.frame(do.call('rbind', lapply(speciesList, vars_calc)), stringsAsFactors = FALSE)
    )

colnames(vars_frame) = c('Species', 'Intercept', 'Std. Error (I)', 'Coefficient', 'Std. Error (C)', 'R-squared', 'DF')
vars_frame = type_convert(vars_frame)

# assign each row to 1 of 2 sample groups
# sampleTreeData$sampleGroup = as.numeric(
#     as.character(
#         lapply(sampleTreeData$Est.Height, function(x) {ifelse(!is.na(x), sample(1:2, 1), x)})
#         )
#     )

# number of observations for trees where height & radius were measured
height.observations =
    sampleTreeData %>%
    filter(!is.na(Est.Height)) %>%
    nrow()

radius.observations =     sampleTreeData %>%
    filter(!is.na(Est.Radius)) %>%
    nrow()

# mean % difference in height and radius estimates
mean.height.diff.all = as.numeric(
    sampleTreeData %>%
    filter(!is.na(Est.Height)) %>%
    mutate(height_diff = Total.Height - Est.Height) %>%
    dplyr::summarize(mean_diff = mean(height_diff))
    )
    
mean.radius.diff = as.numeric(
    sampleTreeData %>%
    filter(!is.na(Est.Height)) %>%
    mutate(height_diff = Actual.Radius - Est.Radius) %>%
    dplyr::summarize(mean_diff = mean(height_diff))
    )



estDBH = function(spc, height) {
    # use parameters calculated above to estimate dbh for full set of trees
    
    if (spc %in% vars_frame$Species == TRUE) {
        lndbh = vars_frame$Intercept[vars_frame$Species == spc] + vars_frame$Coefficient[vars_frame$Species == spc] * height
        dbh = exp(lndbh)
    } else {
        dbh = NA
    }
    
    return(dbh)
    
}

sampleTreeData$estDBH = mapply(estDBH, sampleTreeData$Species, sampleTreeData$Total.Height)

#write_csv(sampleTreeData, "C:\\Users\\Jamie\\Dropbox\\Thesis\\R\\tables\\sampleEstimates.csv")

treeData$estDBH = mapply(estDBH, treeData$Species, treeData$EstHeight)

plotmeans = sampleTreeData %>%
    group_by(Plot) %>%
    summarise(`Mean Radius` = mean(Actual.Radius, na.rm = TRUE),
              `Mean Estimate` = mean(Est.Radius, na.rm = TRUE),
              `Mean Difference` = mean(Est.Radius - Actual.Radius, na.rm = TRUE),
              RMSE = rmse(Actual.Radius, Est.Radius)) %>%
    left_join(metadata %>%
                  select(Plot, Forest.Type), by = c("Plot" = "Plot")) %>%
    left_join(plotCode_ref, by = c('Plot' = 'value')) %>%
    rename(`Forest Type` = Forest.Type, `Plot ID` = ID) %>%
    select(`Plot ID`, `Forest Type`, `Mean Radius`, `Mean Estimate`, `Mean Difference`)

stargazer(plotmeans %>%
              filter(!is.na(`Mean Estimate`)) %>%
              mutate(`Mean Radius` = round(`Mean Radius`, digits = 3),
                     `Mean Estimate` = round(`Mean Estimate`, digits = 3),
                     `Mean Difference` = round(`Mean Difference` , digits = 3)),
          rownames = FALSE, summary = FALSE, digits = 3)

stargazer(plotmeans %>%
    filter(!is.na(`Mean Estimate`)) %>%
    group_by(`Forest Type`) %>%
    summarise(`Mean Radius` = mean(`Mean Radius`),
              `Mean Estimate` = mean(`Mean Estimate`),
              `Mean Difference` = mean(`Mean Difference`),
              RMSE = rmse(`Mean Radius`, `Mean Estimate`)),
    rownames = FALSE,
    summary = FALSE)


# return latex table of regression vars
stargazer(
    vars_frame %>%
        arrange(Species) %>% 
        mutate_if(is.double, funs(round(., 3))) %>%
        left_join(sampleTreeData %>%
                      filter(!is.na(DBH), !is.na(Species), !is.na(estDBH)) %>%
                      group_by(Species) %>%
                      summarize(RMSE = round(rmse(DBH, estDBH), digits=3)), by = c('Species' = 'Species')),
    digits = 3, summary = FALSE, rownames = FALSE
)



rmse(sampleTreeData$DBH[sampleTreeData$Species =='PINPON' & !is.na(sampleTreeData$DBH)], sampleTreeData$estDBH[sampleTreeData$Species=='PINPON' & !is.na(sampleTreeData$DBH)])

treeData %>%
    as_tibble() %>%
    dplyr::filter(!is.na(estDBH)) %>%
    dplyr::mutate(error = (estDBH-DBH)/DBH) %>%
    dplyr::group_by(`Species`) %>%
    dplyr::summarise(`Mean Error` = mean(error, na.rm = TRUE))

## summary of ground truth data by species

stargazer(sampleTreeData %>%
    filter(!is.na(Est.Height)) %>%
    group_by(Species) %>%
    dplyr::summarize(Observations = n()), summary = FALSE, rownames = FALSE)

## summary of dbh est by species
stargazer(treeData %>%
    as_tibble() %>%
    filter(!is.na(estDBH)) %>%
    mutate(error = (estDBH-DBH)/DBH) %>%
    group_by(Species) %>%
    dplyr::summarise(`Mean Error` = percent(mean(error, na.rm = TRUE)), counts = n()), summary = FALSE, rownames = FALSE)

## statistical analysis
testdata.height =
    sampleTreeData %>%
    filter(!is.na(sampleTreeData$Est.Height)) %>%
    select(Total.Height, Est.Height)

height.ftest = var.test(testdata.height$Total.Height, testdata.height$Est.Height)
height.ttest = t.test(testdata.height$Total.Height, testdata.height$Est.Height, paired = TRUE)
height.eqn = lm(testdata.height$Total.Height ~ testdata.height$Est.Height)
height.summary = summary(height.eqn)

height.row = c(
    height.ftest$statistic,
    height.ftest$p.value,
    height.ttest$statistic,
    height.ttest$p.value,
    height.summary$coefficients[1],
    height.summary$coefficients[3],
    height.summary$coefficients[2],
    height.summary$coefficients[4],
    height.summary$r.squared,
    height.ttest$parameter,
    mean(testdata.height$Est.Height-testdata.height$Total.Height),
    sd(testdata.height$Est.Height-testdata.height$Total.Height)/sqrt(length(testdata.height$Est.Height-testdata.height$Total.Height)),
    rmse(testdata.height$Total.Height, testdata.height$Est.Height)
)

testdata.crown =
    sampleTreeData %>%
    filter(!is.na(sampleTreeData$Est.Height)) %>%
    select(Actual.Radius, Est.Radius)
    
crown.ftest = var.test(testdata.crown$Actual.Radius, testdata.crown$Est.Radius)
crown.ttest = t.test(testdata.crown$Actual.Radius, testdata.crown$Est.Radius, paired = TRUE)
crown.eqn = lm(testdata.crown$Actual.Radius ~ testdata.crown$Est.Radius)
crown.summary = summary(crown.eqn)

crown.row = c(
    crown.ftest$statistic,
    crown.ftest$p.value,
    crown.ttest$statistic,
    crown.ttest$p.value,
    crown.summary$coefficients[1],
    crown.summary$coefficients[3],
    crown.summary$coefficients[2],
    crown.summary$coefficients[4],
    crown.summary$r.squared,
    crown.ttest$parameter,
    mean(testdata.crown$Est.Radius - testdata.crown$Actual.Radius),
    sd(testdata.crown$Est.Radius - testdata.crown$Actual.Radius)/sqrt(length(testdata.crown$Est.Radius - testdata.crown$Actual.Radius)),
    rmse(testdata.crown$Actual.Radius, testdata.crown$Est.Radius)
)

dbh.ftest = var.test(treeData$DBH[!is.na(treeData$estDBH)], treeData$estDBH[!is.na(treeData$estDBH)])
dbh.ttest = t.test(treeData$DBH, treeData$estDBH, paired = TRUE)
dbh.eqn = lm(treeData$DBH ~ treeData$estDBH)
dbh.summary = summary(dbh.eqn)

dbh.row = c(
    dbh.ftest$statistic,
    dbh.ftest$p.value,
    dbh.ttest$statistic,
    dbh.ttest$p.value,
    dbh.summary$coefficients[1],
    dbh.summary$coefficients[3],
    dbh.summary$coefficients[2],
    dbh.summary$coefficients[4],
    dbh.summary$r.squared,
    dbh.ttest$parameter,
    mean(treeData$estDBH[!is.na(treeData$estDBH)] - treeData$DBH[!is.na(treeData$estDBH)], na.rm = TRUE),
    sd(treeData$estDBH[!is.na(treeData$estDBH)] - treeData$DBH[!is.na(treeData$DBH)], na.rm = TRUE)/sqrt(length(na.omit(treeData$estDBH[!is.na(treeData$estDBH)] - treeData$DBH[!is.na(treeData$estDBH)]))),
    rmse(treeData$DBH[!is.na(treeData$DBH)], treeData$estDBH[!is.na(treeData$estDBH)])
)

cols = c(
    "F-Statistic",
    "p-value (F)",
    "T-Statistic",
    "p-value (T)",
    "Intercept",
    "Std Error (I)",
    "Coefficient",
    "Std Error (C)",
    "R-Squared",
    "DF",
    "Mean Difference",
    "Std Error",
    "RMSE"
)
rowlabels = c("Height", "Crown Radius", "DBH")

stats.data = rbind.data.frame(height.row, crown.row, dbh.row)
colnames(stats.data) = cols
rownames(stats.data) = rowlabels
stats.data = signif(stats.data, 3)

stargazer(stats.data[,c("F-Statistic", "p-value (F)", "T-Statistic", "p-value (T)", "DF")], summary = FALSE)
stargazer(stats.data[,c("Intercept", "Std Error (I)", "Coefficient", "Std Error (C)", "R-Squared")], summary = FALSE)
stargazer(stats.data[,c("Mean Difference", "Std Error", "RMSE")], summary = FALSE)

## add plots

## dbh comparision
dbhplot = ggplot(treeData[!is.na(treeData$estDBH),], aes(x=DBH, y=estDBH, color = Species)) + geom_point()
dbhplot = dbhplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
dbhplot = dbhplot + labs(title="Accuracy of Estimated DBH Values", x='Diameter at Breast Height (cm)', y="Estimated Diameter at Breast Height (cm)")
dbhplot = dbhplot + geom_abline(slope = 1, color = 'blue', size = 1, linetype = 'longdash') + expand_limits(y=0, x=0)
dbhplot

# savepng('dbhaccuracy.png')

var.test(sampleTreeData$DBH, sampleTreeData$estDBH)
t.test(sampleTreeData$DBH, sampleTreeData$estDBH, paired = TRUE)

# height comparsion
heightplot = ggplot(sampleTreeData[!is.na(sampleTreeData$Est.Height),], aes(x=Total.Height, y=Est.Height, color = Species)) + geom_point()
heightplot = heightplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
heightplot = heightplot + labs(title="Accuracy of Estimated Height Values", x='Height (m)', y="Estimated Height (m)")
heightplot = heightplot + geom_abline(slope = 1, color = 'blue', size = 1, linetype = 'longdash') + expand_limits(y=0, x=0)
heightplot


summary(lm(sampleTreeData$Total.Height ~ sampleTreeData$Est.Height))

# crown radius
radiusplot = ggplot(sampleTreeData[!is.na(sampleTreeData$Est.Radius),], aes(x=Actual.Radius, y=Est.Radius, color = Species)) + geom_point()
radiusplot = radiusplot + theme(panel.background =  element_rect(fill = 'white', colour = 'red'), panel.grid.major = element_line(colour = 'black', linetype = 'dotted'))
radiusplot = radiusplot + labs(title="Accuracy of Estimated Crown Radius Values", x='Crown Radius (m)', y="Estimated Crown Radius (m)")
radiusplot = radiusplot + geom_abline(slope = 1, color = 'blue', size = 1, linetype = 'longdash') + expand_limits(y=0, x=0)
radiusplot

var.test(sampleTreeData$Actual.Radius[!is.na(sampleTreeData$Est.Radius)], sampleTreeData$Est.Radius[!is.na(sampleTreeData$Est.Radius)])
t.test(sampleTreeData$Actual.Radius[!is.na(sampleTreeData$Est.Radius)], sampleTreeData$Est.Radius[!is.na(sampleTreeData$Est.Radius)], paired = TRUE)

heighteqn = lm(sampleTreeData$adjustedEstHeight ~ sampleTreeData$Total.Height)

heighteqn

hsum = summary(heighteqn)
hsum$r.squared
