# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
options(scipen=999)

# Step 2: create CSV for the table ----------------------------------------

# we only want these places
table_places <- c('Britain',
                  'Spain',
                  'France',
                  'Italy',
                  'New York City',
                  'Netherlands',
                  'Belgium',
                  'Istanbul',
                  'Sweden',
                  'Jakarta',
                  'Austria')

# read every csv
table_data <- bind_rows(read_csv('output-data/excess-deaths/britain_excess_deaths.csv') %>% 
                          select(-region_code),
                        read_csv('output-data/excess-deaths/spain_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/france_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/italy_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/united_states_excess_deaths.csv') %>% 
                          select(-region_code),
                        read_csv('output-data/excess-deaths/netherlands_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/belgium_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/turkey_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/sweden_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/indonesia_excess_deaths.csv'),
                        read_csv('output-data/excess-deaths/austria_excess_deaths.csv'))

# generate table csv
table <- table_data %>% 
  ungroup() %>% 
  filter(region %in% table_places) %>% 
  group_by(region) %>% 
  mutate(cumulative_covid_deaths = cumsum(covid_deaths)) %>% 
  # we want to count since the first 50 deaths
  filter(cumulative_covid_deaths >= 50) %>%
  summarise(
    covid_deaths = sum(covid_deaths, na.rm=T),
    non_covid_deaths = sum(non_covid_deaths, na.rm=T),
    excess_deaths = sum(excess_deaths, na.rm=T),
    start_date = min(start_date),
    end_date = max(end_date),
  ) %>% 
  arrange(-excess_deaths) %>% 
  mutate(
    pct = (covid_deaths / excess_deaths) * 100,
    timestamp = Sys.time()
  ) %>% 
  select(region, covid_deaths, excess_deaths, pct, start_date, end_date, timestamp) %>% 
  write_csv('./output-data/interactive/interactive_table.csv')


# Step 3: create CSV for the main small multiple --------------------------

# we only want these places
small_multiple_places <- c('France',
                           'Spain',
                           'Britain',
                           'Netherlands',
                           'New York City',
                           'Italy',
                           'Belgium',
                           'Sweden',
                           'Istanbul',
                           'Austria')

# read every csv
small_multiple_data <- bind_rows(read_csv('output-data/excess-deaths/france_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/spain_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/britain_excess_deaths.csv') %>% 
                                   select(-region_code),
                                 read_csv('output-data/excess-deaths/netherlands_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/united_states_excess_deaths.csv') %>% 
                                   select(-region_code),
                                 read_csv('output-data/excess-deaths/italy_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/belgium_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/sweden_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/turkey_excess_deaths.csv'),
                                 read_csv('output-data/excess-deaths/austria_excess_deaths.csv'))

# create small multiple csv
small_multiple <- small_multiple_data %>% 
  ungroup() %>% 
  filter(region %in% small_multiple_places) %>% 
  mutate(timestamp = Sys.time()) %>% 
  write_csv('./output-data/interactive/all_regions.csv')
