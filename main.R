# notes for testers and developers:
# if you need a new package, put the name in "list.of.packages" variable

################################################### init
## list of packages needed
list.of.packages <- c("ggplot2", "Rcpp", "plotrix", 
                      "gridExtra", "ggpubr", "gganimate", 
                      "here", "rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
  library(new.packages)
}
## place all functions into memory

## enable local path (tested on Windows 10) 
path <- getSourceEditorContext()$path
mypath <- str_remove(path, "main.R")
setwd(mypath)
loadTables(getwd())

source(paste(mypath, "main.R", sep=''))

## creates and tidy tables
cleanModifyTables() # clean current tables
oneDataFrame() # generates 3 tables (just to play around)
sumDataFrame() # generates one table

##############################################


############################################# render plots individually or all

scatterPlotFacetCountries()
scatterPlotLinearPlotCorrelation()
scatterPlotLinearPlotCorrelationColors()
correlationTable()
barPlotBikes()
barPlotTurnoverByCountry()
linearPlotPopulationOverYearCountries()
linearPlotBikesOverYearCountries() 
linearPlotPopYearAllCountries()
linearPlotBikesYearAllCountries()
linerPlotCountryPopBikesTurnover("FRANCE") # country smallest pop
linerPlotCountryPopBikes("FRANCE") # country smallest pop 
linerPlotCountryPopBikesTurnover("SWITZERLAND") ## country largest pop
linerPlotCountryPopBikes("SWITZERLAND") ## country largest pop
pieChartPopulation()
pieChartBikes()
# pieChartTurnOver() removed. Turnover not represented very well





