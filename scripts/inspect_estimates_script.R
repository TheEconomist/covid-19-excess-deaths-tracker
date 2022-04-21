# This script can be used to inspect expected deaths estimates of The Economist for a given country:

# Load packages
library(tidyverse)
library(data.table)
library(readr)
library(ggplot2)
options(scipen=999)

inspect = F

# Part 1: Inspect total and expected mortality for a given country ------------------------------------

# This defines a function, which takes a country name as the input and then provides 2 plots and a summary data frame in return
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
  a <- ggplot(df, aes(x=start_date, y=total_deaths, color = 'total deaths'))+geom_line()+geom_line(aes(y=expected_deaths, col='expected_deaths'))+ggtitle(country)
  
  # Plot by year (excluding 2022):
  b <- ggplot(df[df$year < 2022 & !duplicated(df$year), ], aes(x=start_date, y=total_deaths_annual, color = 'total deaths, annual'))+geom_line()+geom_point()+geom_line(aes(y=expected_deaths_annual, col='expected deaths, annual'))+geom_point(aes(y=expected_deaths_annual, col='expected deaths, annual'))+geom_vline(aes(xintercept = as.Date('2019-07-01')), col = 'black')+theme_minimal()+ggtitle(country)
  
  export <- df[!duplicated(df$year) & df$year < 2022, c('country', 'year', 'expected_deaths_annual', 'total_deaths_annual')]
  return(list(a, b, export))}

# To inspect a given country, use e.g.:
if(inspect){
  sweden <- inspect_estimates('Sweden')
  sweden[[1]]
  sweden[[2]]
  sweden[[3]]
}

# This code chunk exports a few selected countries:
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


# Part 2: Replication for a given country ------------------------------------
# This can be useful to see how we come up with our expected deaths estimates, and the details of our models

# Load historical deaths data:
country <- 'sweden'
hist <- read_csv(sort(paste0('output-data/historical-deaths/', dir('output-data/historical-deaths/')[grep(country, dir('output-data/historical-deaths/'))]))[1])

if(country == 'united_states'){
  hist <- hist[hist$region_code == 'USA', ]
}

# Load expected deaths data:
expected <- read_csv(sort(paste0('output-data/excess-deaths/', dir('output-data/excess-deaths/')[grep(country, dir('output-data/excess-deaths/'))]))[1])

# Fit model:
lm_fit <- lm(total_deaths ~ as.factor(week) + year, data = hist[hist$end_date < as.Date('2020-03-01'), ])
expected$replicated_expected <- predict(lm_fit, newdata = expected)

# We also provide an alternative here, which does not include the first two months of 2020 in the training data
lm_fit <- lm(total_deaths ~ as.factor(week) + year, data = hist[hist$end_date < as.Date('2020-01-01'), ])
expected$replicated_expected_alternative <- predict(lm_fit, newdata = expected)

# This is what the two look like for 2020+2021 totals (not adjusting for leap year or split weeks effects, which the above does):
sum(expected$total_deaths[expected$start_date <= as.Date('2022-01-01')] -
      expected$replicated_expected[expected$start_date <= as.Date('2022-01-01')])
sum(expected$total_deaths[expected$start_date <= as.Date('2022-01-01')] -
      expected$replicated_expected_alternative[expected$start_date <= as.Date('2022-01-01')])

library(ggplot2)
ggplot(expected, aes(x=start_date, y=total_deaths))+
  geom_line()+
  geom_line(aes(y=expected_deaths, col='expected (economist data)'))+
  geom_line(aes(y=replicated_expected, col='expected (replicated via above model)'))+
  geom_line(aes(y=replicated_expected_alternative, col='expected (using smaller training data)'))+
  ggtitle('Note: leap year correction makes feb figure differ')



