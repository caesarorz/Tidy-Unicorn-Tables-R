## Collaborators:

- Cesar Orozco Zamora. 
- Megan Madrigal Amador. 
- Johan Bastos Zamora. 



# R project. Unicorn and monocycles

based on: https://github.com/RMHogervorst/unicorns_on_unicycles

## Description

The main purpose is to tidy sales and observations tables, get insights and create a story to answer questions.

The tables are about the population of unicorns and the sales of monocycles (yes I know!). As the original author described:

> His work contains multiple tables, carefully written down, documenting the population of unicorns over time in multiple places and related to that the sales and numbers of unicycles in those countries.

and also:

> This 'raw' data gives us a nice example of typical dirty data you would find in the wild. The goal is to combine the sales data of unicycles and the populations of unicorns into a single 'tidy' dataframe.

Tables are:
- Sales
- Observations

Everything is from 1600's century, taking place in five countries: Austria, Germany, France, Switzerland , Netherlands.

# Questions

1. Is there any correlation between sales of monocycles and the presence of unicorns?
2. How is the trend of bikes vs the number of unicorns. Growing, slowing down?
3. What is the country with more sales given presence of unicorns? Bigger number of bikes and turnover? With less sales?
4. What is the population and bikes evolution? The evolution of turnover.  
5. Which country has the largest and smallest population/bikes relation? 

# Analisys

* All code comes from asignmentV3.R file.


1. Is there any correlation between sales of monocycles and the presence of unicorns?

```
scatterPlotFacetCountries <- function(){
  #### scatterplot  all bikes and pop by country  facet_wrap   ++++++++++++++ with colors and change beauty
  obs_sales_contries <<- observations %>%
    left_join(bike_sales)
  obs_sales_contries %>%
    ggplot(.) + 
    geom_point(mapping = aes(x = pop, y = bikes)) + 
    facet_wrap(~ name_of_country, nrow = 2)  
}
```

![alt text](./plots/1_facet_countries_pop_bikes_correlation.png)

The facet above pictures the correlation for 5 countries in terms of population vs number of bikes. The evolution is positive.

- - -
```
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
```

![alt text](./plots/2_linear_correlation_bikes_pop.png)

This plot shows the overoll behavior for the countries, even with some projection (shadow around the line). 
The slope is positive.

- - -
```
scatterPlotLinearPlotCorrelationColors <- function(){
  # scatterplot with trend line for all countries and with clustering colors: 
  #unicorn population vs number of bikes
  pop_bikes_turn %>%
    ggplot(., mapping = aes(x = pop, y = bikes)) + 
    geom_point(mapping = aes(color = name_of_country)) + 
    geom_smooth(data = filter(pop_bikes_turn)) 
}
```

![alt text](./plots/3_correlation_bikes_pop_scatterplot.png)
- - -
This one gives us how the countries tight up to the correlation, Some has highest correlation than others.


```
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
```

![alt text](./plots/4_table_correlation_per_country.png)

To finish the conversion on correlation, this table summarize it. As we can see, correlation is almost 1, which means all countries have a strong bond in regards to population and the number of monocycles.

- - -
```
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
```

![alt text](./plots/5_1_total_bikes_country.png)

- - -
```
barPlotTurnoverByCountry <- function(){
  theme_set(theme_pubr())

  ggplot(data=df_sum, aes(x = total_turnover, y = name_of_country )) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = c(total_turnover)), vjust = -0.3) + 
    theme_pubclean()
}
```

![alt text](./plots/5_2_total_bikes_country.png)

- - -


```
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
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}
```
![alt text](./plots/6_1_pop_over_time_forAllCountries.png)

- - -

```
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
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles")
}

```
![alt text](./plots/6_2_bikes_over_time_forAllCountries.png)

- - -

```
linearPlotPopYearAllCountries <- function(){
  ## for all countries population over year
  ggplot(data=obs_sales_contries, aes(x=year, y=pop, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
}
```
![alt text](./plots/7_1_pop_over_year_5_countries_detail.png)



- - -


```
linearPlotBikesYearAllCountries <- function() {
  ## for all countries bikes over year
  ggplot(data=obs_sales_contries, aes(x=year, y=bikes, group=name_of_country, color=name_of_country)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
}
```
![alt text](./plots/7_2_bikes_over_year_5_countries_detail.png)


- - -

```
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
    labs(y = "Population", 
         x = "Year",
         title = "Turnover",
         subtitle = country,
         caption = "Source:https://github.com/RMHogervorst/unicorns_on_unicycles") +
    theme_bw()
} 



linerPlotCountryPopBikesTurnover("FRANCE")
linerPlotCountryPopBikesTurnover("SWITZERLAND")
```
![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)
France

![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)
Switzerland
- - - 


```
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

linerPlotCountryPopBikes("FRANCE") # country smallest pop 
linerPlotCountryPopBikes("SWITZERLAND") ## country largest pop

```
![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)
France

![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)
Switzerland

- - -

```
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
```
![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)

- - - 


```
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
```
![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)

- - - 

```
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
```
![alt text](./plots/8_1_linear_plot_bikes_pop_turnover_country.png)


