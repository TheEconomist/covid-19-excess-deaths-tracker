# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
options(scipen=999)

# Step 2: create CSV for the table ----------------------------------------

# read every csv
table_data <- bind_rows(read_csv('output-data/excess-deaths/all_weekly_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/all_monthly_excess_deaths.csv'))

# we include some cities for countries that lack nationwide figures
cities <- c('Istanbul', 'Jakarta')

# generate table csv
table <- table_data %>% 
  filter(country == region | region %in% cities) %>% 
  group_by(region) %>% 
  mutate(cumulative_covid_deaths = cumsum(covid_deaths)) %>% 
  # we want to count since the first 50 deaths
  filter(cumulative_covid_deaths >= 50 | (region %in% c("Taiwan","New Zealand","Iceland") & start_date >= as.Date("2020-02-01"))) %>%
  summarise(
    covid_deaths = round(sum(covid_deaths, na.rm=T),-1),
    excess_deaths_per_100k = round(sum(excess_deaths_per_100k, na.rm=T)),
    non_covid_deaths = round(sum(non_covid_deaths, na.rm=T),-1),
    excess_deaths = round(sum(excess_deaths, na.rm=T),-1),
    start_date = min(start_date),
    end_date = max(end_date),
  ) %>% 
  arrange(-excess_deaths) %>% 
  select(region, covid_deaths, excess_deaths, excess_deaths_per_100k, start_date, end_date) %>% 
  write_csv('./output-data/interactive/interactive_table.csv')

timestamp <- tibble(timestamp = Sys.time()) %>% 
  write_csv('./output-data/interactive/timestamp.csv')
