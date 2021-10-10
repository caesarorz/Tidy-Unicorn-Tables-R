# install packages and load libraries 
install.packages("plotrix")
install.packages("gridExtra")
## install libraries and load packages for tables
install.packages("plotrix")
install.packages("gridExtra")
library(gridExtra)
library(grid)
library("readxl")
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)



## load the files (excel)
##   Note: read_excel(...)  must be changed to your Windows path
observations <- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/observations.xlsx")
sales <- read_excel("C:/Users/50687/Desktop/dojo/bootcamp/week5/weekend/sales.xlsx")


############# cleaning

## Create observations table and modify it, make country upper case
observations <- observations %>%
  mutate(name_of_country = countryname,
         name_of_country = toupper(name_of_country),
         year=as.integer(year),
         pop = as.integer(pop)
         ) %>%
  select(name_of_country, year, pop)
observations


### create bike_sales and bike_turnover tables out of sales table (check above)
bike_sales <- sales %>%
  select(1:3) %>%
  rename(name_of_country = "name_of_country...1", year = "year...2") %>%
  mutate(
    year = as.integer(year),
    bikes = as.integer(bikes)
  )

bike_turnover <- sales %>%
  select(6:8) %>%
  rename(name_of_country = "name_of_country...6", year = "year...7") %>%
  mutate(
    year = as.integer(year)
  )

View(bike_turnover)
View(observations)
View(bike_sales)


#### sales and observations tables (just for austria)  ++++++++ in progress

austria_bikes <- bike_sales %>%
  filter(name_of_country == 'AUSTRIA')
austria_observations <- observations %>%
  filter(name_of_country == 'AUSTRIA')

austria_bikes
austria_observations







###################### plots

#### scatterplot  all bikes and pop by country  facet_wrap   ++++++++++++++ with colors and change beauty
obs_sales_contries <- observations %>%
  left_join(bike_sales)

obs_sales_contries %>%
ggplot(.) + 
  geom_point(mapping = aes(x = pop, y = bikes)) + 
  facet_wrap(~ name_of_country, nrow = 2)


# scatterplot bikes vs population linear plot correlation
#  trend line for all countries:   unicorn population vs number of bikes
pop_bikes_turn <- observations %>% # create pop_bikes_turn table with joins
  left_join(bike_sales) %>%
  left_join(bike_turnover)

pop_bikes_turn %>%
ggplot(.) + 
  geom_point(mapping = aes(x = pop, y = bikes)) +
  geom_smooth(mapping = aes(x = pop, y = bikes))


# scatterplot with trend line for all countries and with clustering colors: 
#unicorn population vs number of bikes
pop_bikes_turn %>%
ggplot(., mapping = aes(x = pop, y = bikes)) + 
  geom_point(mapping = aes(color = name_of_country)) + 
  geom_smooth(data = filter(pop_bikes_turn))

############### correlation pop and bikes

# table with countries, correlation and total_turnover
cor_turnover_countries <- obs_sales_contries %>% 
  group_by(name_of_country) %>% 
  summarize(correlation = cor(pop, bikes),
            sum = sum(pop)) %>%
  grid.table(.)  


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




############### bar plot sales per  country and   ++++++++ in progress

sum_pop_bikes_country <- obs_sales_contries %>% 
  group_by(name_of_country) %>% 
  summarize(bikes = sum(bikes),
            total_population = sum(pop),
            total_bikes = sum(bikes)
  )
sum_pop_bikes_country

ggplot(data = sum_pop_bikes_country) +
  geom_bar(mapping = aes(x = name_of_country, y = total_population), stat = "identity")

ggplot(data = sum_pop_bikes_country) +
  geom_bar(mapping = aes(x = name_of_country, y = total_bikes), stat = "identity")


ggplot(data=obs_sales_contries, aes(x=year, y=bikes)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=name_of_country), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

head(df2)
head(obs_sales_contries)

country_year_pop <- obs_sales_contries %>%
  select(name_of_country,year,pop)
ggplot(data = country_year_pop) + 
  geom_bar(mapping = aes(x = year, fill = name_of_country))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))


# 




ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()



###############   linear plot: population over years (sum of 5 countries)
year_pop <- obs_sales_contries %>% 
  group_by(year) %>% 
  summarize(population = sum(pop))

# simple
ggplot(year_pop, 
       aes(x = year, 
           y = population)) +
  geom_line()

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


# # for all countries
obs_sales_contries
ggplot(data=obs_sales_contries, aes(x=year, y=pop, group=name_of_country, color=name_of_country)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()

## linear plot: bikes over years
year_bikes <- obs_sales_contries %>% 
  group_by(year) %>% 
  summarize(sum_bikes = sum(bikes))

# simple
ggplot(year_bikes, 
       aes(x = year, 
           y = sum_bikes)) +
  geom_line()

# better
ggplot(year_bikes, 
       aes(x = year, 
           y = sum_bikes)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Number of bikes", 
       x = "Year",
       title = "Number of bikes over time",
       subtitle = "Austria, Switchland, Germany, Netherlands, ",
       caption = "Source: http://www.gapminder.org/data/")

# for all countries
obs_sales_contries
ggplot(data=obs_sales_contries, aes(x=year, y=bikes, group=name_of_country, color=name_of_country)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()







##############################################



df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

ggplot(data=df2, aes(x=dose, y=len, group=supp, color=supp)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()


obs_sales_contries
ggplot(data=obs_sales_contries, aes(x=year, y=pop, group=name_of_country, color=name_of_country)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()

economics


df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)
library(plyr)
# Sort by dose and supp
df_sorted <- arrange(df2, dose, supp)
head(df_sorted)
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))
head(df_cumsum)

ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()



