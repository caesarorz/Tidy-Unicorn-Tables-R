# #workdir <- setwd("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/experiment")
# 
# source("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/experiment/data.R")
# 
# if("plyr" %in% (.packages())){
#   print("TRUE") 
# } else {
#   print("FALSE")
# }
# 
# 
# 
# #workdir
# # Uniform color
# barplot(height=data$value, names=data$name, col=rgb(0.2,0.4,0.6,0.6) )
# 
# # Specific color for each bar? Use a well known palette
# library(RColorBrewer)
# coul <- brewer.pal(5, "Set2") 
# barplot(height=data$value, names=data$name, col=coul )
# 
# # Change border color
# barplot(height=data$value, names=data$name, border="#69b3a2", col="white" )


source("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/experiment/data.R")



#installPackages() # takes some time
loadLibraries() 
loadTables()
cleanModifyTables()
oneDataFrame()
sumDataFrame()
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
pieChartTurnOver()   ### new vs country


