# This script can be used to inspect expected deaths estimates of The Economist for a given country:

# Load packages
library(tidyverse)
library(data.table)
library(readr)
library(ggplot2)
options(scipen=999)

inspect = F

# This defines a function to inspect total and expected mortality for a given country:
inspect_estimates <- function(country = 'Sweden'){
  # Select country:
  country <- tolower(country)

  if(country == 'united states'){
    country <- 'united_states'
  }
    
  # Load historical deaths data:
  hist <- read_csv(sort(paste0('output-data/historical-deaths/', dir('output-data/historical-deaths/')[grep(country, dir('output-data/historical-deaths/'))]))[1])

  if(country == 'united_states'){
    hist <- hist[hist$region_code == 'USA', ]
    }
  
  # Load expected deaths data:
  expected <- read_csv(sort(paste0('output-data/excess-deaths/', dir('output-data/excess-deaths/')[grep(country, dir('output-data/excess-deaths/'))]))[1])

  # Combine the two:
  hist$excess_deaths <- NA 
  hist <- hist[, ]
  expected <- expected[, ]
  df <- rbind(expected[, colnames(hist)], hist)
  df$expected_deaths <- as.numeric(df$expected_deaths)
  df <- df[!duplicated(df$start_date), ]
  
  # If weekly, convert to daily data set (fixes weeks split between years):
  if('week' %in% colnames(df)){
    df <- rbind(df, df, df, df, df, df, df)
    df <- df[order(df$start_date), ]
    df$date <- ave(df$start_date, df$start_date, FUN = function(x){x + 0:6})
    for(i in c('total_deaths', 'covid_deaths', 'expected_deaths', 'excess_deaths')){
      df[, i] <- df[, i]/7
    }
    
    # Check that it worked
    if(sum(df$date < df$start_date | df$date > df$end_date) > 0){
      stop('Transfer from weekly dataset had an error, please inspect')
    }
    
    df$year <- year(df$date)
    df <- df[df$year >= 2015, ]
  }
  
  df$expected_deaths_annual <- ave(df$expected_deaths, df$year, FUN = sum)
  df$expected_deaths_annual[df$year < 2020] <- NA
  df$total_deaths_annual <- ave(df$total_deaths, df$year, FUN = sum)

  # Plot by week:
  a <- ggplot(df, aes(x=start_date, y=total_deaths, color = 'total deaths'))+geom_line()+geom_line(aes(y=expected_deaths, col='expected_deaths'))
  
  # Plot by year (excluding 2022):
  b <- ggplot(df[df$year < 2022 & !duplicated(df$year), ], aes(x=start_date, y=total_deaths_annual, color = 'total deaths, annual'))+geom_line()+geom_point()+geom_line(aes(y=expected_deaths_annual, col='expected deaths, annual'))+geom_point(aes(y=expected_deaths_annual, col='expected deaths, annual'))+geom_vline(aes(xintercept = as.Date('2019-07-01')), col = 'black')+theme_minimal()
  
  export <- df[!duplicated(df$year) & df$year < 2022, c('country', 'year', 'expected_deaths_annual', 'total_deaths_annual', 'excess_deaths_annual')]
  return(list(a, b, export))}

# To inspect a given country, use e.g.:
sweden <- inspect_estimates('Sweden')
sweden[[1]]
sweden[[2]]
sweden[[3]]


# Export of selected countries:
export <- data.frame()
countries <- c('Denmark', 'Sweden', 'Finland', 'Norway', 'Iceland', 'Belgium', 'Germany', 'Portugal', 'Spain', 'Japan', 'Ireland', 'United_States')
for(i in countries){
  export <- rbind(export, inspect_estimates(country = i)[[3]])
}
write_csv(export, 'output-data/annual_data_for_selected_countries.csv')

if(inspect){
ggplot(export, aes(x=year, y=total_deaths_annual, col='total deaths'))+
  geom_line()+
  geom_point()+
  geom_line(newdata = export[export$year >= 2020, ], aes(x=year, y=expected_deaths_annual, col = 'expected deaths'))+
  facet_wrap(country~.)
}



