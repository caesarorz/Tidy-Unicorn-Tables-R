# notes for testers and developers:
# if you need a new package, put the name in "list.of.packages" variable



# importing rstudioapi package
library("rstudioapi") 
path <- getSourceEditorContext()$path

path
mypath <- str_remove(path, "main.R")

setwd(mypath)



loadTables(mypath)


## list of packages needed
list.of.packages <- c("ggplot2", "Rcpp", "plotrix", "gridExtra", "ggpubr", "gganimate", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

source("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/data.R")

if(length(new.packages)){
  install.packages(new.packages)
  library(new.packages)
}

loadLibraries() 
loadTables() # load tables
cleanModifyTables() # clean current tables
oneDataFrame() # generates 3 tables (just to play around)
sumDataFrame() # generates one table


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
linerPlotCountryPopBikesTurnover("SWITZERLAND") ## country largest pop
linerPlotCountryPopBikes("FRANCE") # country smallest pop 
linerPlotCountryPopBikes("SWITZERLAND") ## country largest pop
pieChartBikes()
pieChartPopulation()
pieChartTurnOver()





