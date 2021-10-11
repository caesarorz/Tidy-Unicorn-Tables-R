



installPackages <- function(){
  # install packages and load libraries 
  install.packages("plotrix")
  install.packages("gridExtra")
  ## install libraries and load packages for tables
  install.packages("plotrix")
  install.packages("gridExtra")
  install.packages("ggpubr")
}

loadLibraries <- function() {
  library(gridExtra)
  library(grid)
  library("readxl")
  library(ggplot2)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(ggpubr)
}

# if("gridExtra" %in% (.packages())){
#   print("TRUE") 
# } else {
#   print("FALSE")
# }
# list.of.packages <- c("ggplot2", "Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)


loadTables <- function(){
  observations <<- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/observations.xlsx")
  sales <<- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/sales.xlsx")
}

# filters, cleaning and modify variables in tables
cleanModifyTables <- function(){
  obs <<- observations %>%
    mutate(name_of_country = countryname,
           name_of_country = toupper(name_of_country),
           year=as.integer(year),
           pop = as.integer(pop)
    ) %>%
    select(name_of_country, year, pop)
  ### create bike_sales and bike_turnover tables out of sales table (check above)
  bike_sales <<- sales %>%
    select(1:3) %>%
    rename(name_of_country = "name_of_country...1", year = "year...2") %>%
    mutate(
      year = as.integer(year),
      bikes = as.integer(bikes)
    )
  bike_turnover <<- sales %>%
    select(6:8) %>%
    rename(name_of_country = "name_of_country...6", year = "year...7") %>%
    mutate(
      year = as.integer(year)
    )
}

oneDataFrame <- function(){
  ## combine the two df into one, cleared, tidied. 
  sales2 <- sales %>%
    rename(name_of_country = "name_of_country...1",
           year = "year...2") %>%
    select(name_of_country, year, bikes, total_turnover) %>%
    mutate(year=as.integer(year),
           bikes = as.integer(bikes),
           total_turnover = as.integer(total_turnover)) %>%
    group_by(name_of_country, year) 
  obs2 <- obs %>%
    mutate(year=as.integer(year),
           population = as.integer(pop),
           name_of_country = toupper(name_of_country)) %>%
    select(name_of_country, year, population) %>%
    group_by(name_of_country, year)
  df_obs_sales <<- sales2 %>%
    left_join(obs2)
  return(df_obs_sales)
}

sumDataFrame <- function(){
  df_sum <<- df_obs_sales %>% 
    group_by(name_of_country) %>% 
    summarize(total_turnover = sum(total_turnover),
              bikes = sum(bikes ),
              population = sum(population))
}

scatterPlotFacetCountries <- function(){
  #### scatterplot  all bikes and pop by country  facet_wrap   ++++++++++++++ with colors and change beauty
  obs_sales_contries <<- obs %>%
    left_join(bike_sales)
  obs_sales_contries %>%
    ggplot(.) + 
    geom_point(mapping = aes(x = pop, y = bikes)) + 
    facet_wrap(~ name_of_country, nrow = 2) +
    theme_bw()
}

scatterPlotLinearPlotCorrelation <- function(){
  # scatterplot bikes vs population linear plot correlation
  #  trend line for all countries:   unicorn population vs number of bikes
  pop_bikes_turn <<- obs %>% # create pop_bikes_turn table with joins
    left_join(bike_sales) %>%
    left_join(bike_turnover)
  pop_bikes_turn %>%
    ggplot(.) + 
    geom_point(mapping = aes(x = pop, y = bikes)) +
    geom_smooth(mapping = aes(x = pop, y = bikes)) +
    theme_bw()
}

scatterPlotLinearPlotCorrelationColors <- function(){
  # scatterplot with trend line for all countries and with clustering colors: 
  #unicorn population vs number of bikes
  pop_bikes_turn %>%
    ggplot(., mapping = aes(x = pop, y = bikes)) + 
    geom_point(mapping = aes(color = name_of_country)) + 
    geom_smooth(data = filter(pop_bikes_turn)) +
    theme_bw()
}

correlationTable <- function(){
  ############### correlation pop and bikes
  # table with countries, correlation and total_turnover
  cor_turnover_countries <- obs_sales_contries %>% 
    group_by(name_of_country) %>% 
    summarize(correlation = cor(pop, bikes),
              total_population = sum(pop),
              total_bikes = sum(bikes)) %>%
    grid.table(.) 
}

barPlotBikes <- function(){ ###################################
  ############### bar plot sales per  country and   
  sum_pop_bikes_country <<- obs_sales_contries %>% 
    group_by(name_of_country) %>% 
    summarize(bikes = sum(bikes),
              total_population = sum(pop),
              total_bikes = sum(bikes)
    )
  ggplot(data = sum_pop_bikes_country) +
    geom_bar(mapping = aes(x = name_of_country, y = total_bikes, fill=name_of_country), stat = "identity") +
    theme_minimal()
}



barPlotTurnoverByCountry <- function(){
  theme_set(theme_pubr())
  
  ggplot(data=df_sum, aes(x = total_turnover, y = name_of_country )) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = c(total_turnover)), vjust = -0.3) + 
    theme_pubclean()
}


## swiss, adn france

linerPlotCountryPopBikesTurnover <- function(country){
  df <-  df_obs_sales %>% 
    filter(name_of_country == country)
  ggplot() +
    geom_line(data=df, mapping = aes(x=year, y=total_turnover), color="blue") +
    geom_point(data=df, mapping = aes(x=year, y=total_turnover), color="blue") +
    geom_line(data=df, mapping = aes(x=year, y=population), color="orange") +
    geom_point(data=df, mapping = aes(x=year, y=population), color="orange") +
    geom_line(data=df, mapping = aes(x=year, y=bikes), color="green") +
    geom_point(data=df, mapping = aes(x=year, y=bikes), color="green") +
    labs(y = "Turnover, population and bikes", 
         x = "Year",
         title = "Turnover",
         subtitle = country,
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles") +
    theme_bw()
} 

linerPlotCountryPopBikes <- function(country){
  df <-  df_obs_sales %>% 
    filter(name_of_country == country)
  ggplot() +
    geom_line(data=df, mapping = aes(x=year, y=population), color="orange") +
    geom_point(data=df, mapping = aes(x=year, y=population), color="orange") +
    geom_line(data=df, mapping = aes(x=year, y=bikes), color="green") +
    geom_point(data=df, mapping = aes(x=year, y=bikes), color="green") +
    labs(y = "population and bikes", 
         x = "Year",
         title = "Turnover",
         subtitle = country,
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles") +
    theme_bw()
}

pieChartPopulation <- function(){
  ###  generates tables with y position for pie chart
  sum_pop_bikes_country <<- df_obs_sales %>% 
    group_by(name_of_country) %>% 
    summarize(bikes = sum(bikes),
              total_population = sum(population),
              total_bikes = sum(bikes)
    )  
  sum_pop_bikes_country %>% 
    arrange(desc(name_of_country)) %>%
    mutate(prop = ceiling(total_population / sum(sum_pop_bikes_country$total_population) *100)) %>%
    mutate(yposition = cumsum(total_population)- 0.5*total_population ) %>%
    ggplot(., aes(x="", y=total_population, fill=name_of_country)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    #theme(legend.position="none") +
    geom_text(aes(y = yposition, label = total_population), color = "white", size=6) +
    scale_fill_brewer(palette="Set1") +
    ggtitle("Population by country") +
    labs(y = "Population", 
         x = "Year",
         title = "Population",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}

pieChartBikes <- function(){
  ###  generates tables with y position for pie chart
  sum_pop_bikes_country %>% 
    arrange(desc(name_of_country)) %>%
    mutate(total_population = ceiling(total_population / sum(sum_pop_bikes_country$total_bikes) *100)) %>%
    mutate(yposition = cumsum(total_bikes)- 0.5*total_bikes ) %>%
    ggplot(., aes(x="", y=total_bikes, fill=name_of_country)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    #theme(legend.position="none") +
    geom_text(aes(y = yposition, label = total_bikes), color = "white", size=6) +
    scale_fill_brewer(palette="Set1") +
    ggtitle("Number of Bikes by country")  +
    labs(y = "Bikes", 
         x = "Year",
         title = "Bikes",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}

pieChartTurnOver <- function(){
  ###  generates tables with y position for pie chart
  df_sum %>% 
    mutate(total_turnover = ceiling(total_turnover / sum(df_sum$total_turnover) *100)) %>%
    mutate(yposition = cumsum(total_turnover)- 0.5*total_turnover ) %>%
    ggplot(., aes(x="", y=total_turnover, fill=name_of_country)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    #theme(legend.position="none") +
    scale_fill_brewer(palette="Set1") +
    ggtitle("Turnover by Country") +
    labs(y = "Population", 
         x = "Year",
         title = "Turnover",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}

linearPlotPopulationOverYearCountries <- function(){
  ###############   linear plot: population over years (sum of 5 countries)
  year_pop <<- obs_sales_contries %>% 
    group_by(year) %>% 
    summarize(population = sum(pop))
  
  # with better lines etc
  ggplot(year_pop, 
         aes(x = year, 
             y = population)) +
    geom_line(size = 1.5, 
              color = "lightgrey") +
    geom_point(size = 3, 
               color = "steelblue") +
    labs(y = "Population", 
         x = "Year",
         title = "Population over time",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles") +
    theme_bw()
}

linearPlotBikesOverYearCountries <- function() {
  ###############   linear plot: bikes over years (sum of 5 countries)
  year_bikes <<- obs_sales_contries %>% 
    group_by(year) %>% 
    summarize(bikes = sum(bikes))
  
  # with better lines etc
  ggplot(year_bikes, 
         aes(x = year, 
             y = bikes)) +
    geom_line(size = 1.5, 
              color = "lightgrey") +
    geom_point(size = 3, 
               color = "steelblue") +
    labs(y = "Number bikes", 
         x = "Year",
         title = "Number bikes over time",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles") +
    theme_bw()
}


linearPlotPopYearAllCountries <- function(){
  ## for all countries population over year
  ggplot(data=obs_sales_contries, aes(x=year, y=pop, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    labs(y = "Population", 
         x = "Year",
         title = "Population over time",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}

linearPlotBikesYearAllCountries <- function() {
  ## for all countries bikes over year
  ggplot(data=obs_sales_contries, aes(x=year, y=bikes, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    labs(y = "Population", 
         x = "Year",
         title = "Population over time",
         subtitle = "Austria, Switchland, Germany, Netherlands, ",
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}











