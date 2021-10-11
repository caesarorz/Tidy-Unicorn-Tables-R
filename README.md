Collaborators:
* Cesar Orozco
* Megan 
* Johan Bastos


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
The slope is positve.

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
This one gives us how the countries tight up to the correlation, Some has highest correlation than others. ....


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
```

![alt text](./plots/5_1_total_bikes_country.png)

- - -
```
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
         caption = "Source: http://www.gapminder.org/data/")
}
```
![alt text](./plots/6_1_pop_over_time_forAllCountries.png)

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

