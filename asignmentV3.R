installPackages <- function(){
  # install packages and load libraries 
  install.packages("plotrix")
  install.packages("gridExtra")
  ## install libraries and load packages for tables
  install.packages("plotrix")
  install.packages("gridExtra")
}

loadLibraries <- function() {
  library(gridExtra)
  library(grid)
  library("readxl")
  library(ggplot2)
  library(tidyverse)
  library(gridExtra)
  library(grid)
}

loadTables <- function(){
  observations <<- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/observations.xlsx")
  sales <<- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/sales.xlsx")
}

# filters, cleaning and modify variables in tables
cleanModifyTables <- function(){
  observations <<- observations %>%
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

scatterPlotFacetCountries <- function(){
  #### scatterplot  all bikes and pop by country  facet_wrap   ++++++++++++++ with colors and change beauty
  obs_sales_contries <<- observations %>%
    left_join(bike_sales)
  obs_sales_contries %>%
    ggplot(.) + 
    geom_point(mapping = aes(x = pop, y = bikes)) + 
    facet_wrap(~ name_of_country, nrow = 2)  
}

scatterPlotLinearPlotCorrelation <- function(){
  # scatterplot bikes vs population linear plot correlation
  #  trend line for all countries:   unicorn population vs number of bikes
  pop_bikes_turn <<- observations %>% # create pop_bikes_turn table with joins
    left_join(bike_sales) %>%
    left_join(bike_turnover)
  pop_bikes_turn %>%
    ggplot(.) + 
    geom_point(mapping = aes(x = pop, y = bikes)) +
    geom_smooth(mapping = aes(x = pop, y = bikes))
}

scatterPlotLinearPlotCorrelationColors <- function(){
  # scatterplot with trend line for all countries and with clustering colors: 
  #unicorn population vs number of bikes
  pop_bikes_turn %>%
    ggplot(., mapping = aes(x = pop, y = bikes)) + 
    geom_point(mapping = aes(color = name_of_country)) + 
    geom_smooth(data = filter(pop_bikes_turn)) 
}

correlationTable <- function(){
  ############### correlation pop and bikes
  # table with countries, correlation and total_turnover
  cor_turnover_countries <- obs_sales_contries %>% 
    group_by(name_of_country) %>% 
    summarize(correlation = cor(pop, bikes),
              sum = sum(pop)) %>%
    grid.table(.) 
}

barPlotBikesYear1 <- function(){ ####################################++++++++ in progress
  ############### bar plot sales per  country and   
  sum_pop_bikes_country <<- obs_sales_contries %>% 
    group_by(name_of_country) %>% 
    summarize(bikes = sum(bikes),
              total_population = sum(pop),
              total_bikes = sum(bikes)
    )
  ggplot(data = sum_pop_bikes_country) +
    geom_bar(mapping = aes(x = name_of_country, y = total_bikes), stat = "identity")
}

barPlotBikesYear2 <- function(){ ####################################++++++++ in progress
  ############### bar plot sales per  country and   
  sum_pop_bikes_country <<- obs_sales_contries %>% 
    group_by(name_of_country) %>% 
    summarize(bikes = sum(bikes),
              total_population = sum(pop),
              total_bikes = sum(bikes)
    )
  ggplot(data=obs_sales_contries, aes(x=year, y=bikes)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=name_of_country), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()
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
         caption = "Source: https://github.com/RMHogervorst/unicorns_on_unicycles")
}

linearPlotPopYearAllCountries <- function(){
  ## for all countries population over year
  ggplot(data=obs_sales_contries, aes(x=year, y=pop, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
}

linearPlotBikesYearAllCountries <- function() {
  ## for all countries bikes over year
  ggplot(data=obs_sales_contries, aes(x=year, y=bikes, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
}


installPackages() # takes some time
loadLibraries() 
loadTables()
cleanModifyTables()
scatterPlotFacetCountries()
scatterPlotLinearPlotCorrelation()
scatterPlotLinearPlotCorrelationColors()
correlationTable()
barPlotBikesYear1()
barPlotBikesYear2()
linearPlotPopulationOverYearCountries()
linearPlotPopYearAllCountries()
linearPlotBikesYearAllCountries()








  


############### pie charts                       ++++++++ in progress

# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

data

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#####################################################

















