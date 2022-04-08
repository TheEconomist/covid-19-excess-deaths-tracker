# This script can be used to inspect expected deaths estimates of The Economist for a given country:

# Load packages
library(tidyverse)
library(data.table)
library(readr)
library(ggplot2)
options(scipen=999)

# Select country:
country <- 'Sweden'

# Load historical deaths data:
hist <- read_csv(paste0('output-data/historical-deaths/', tolower(country), '_weekly_deaths.csv'))
# hist <- read_csv(paste0('output-data/historical-deaths/', tolower(country), '_monthly_deaths.csv'))
# hist <- read_csv(paste0('output-data/historical-deaths/', tolower(country), '_quarterly_deaths.csv'))

# Load expected deaths data:
expected <- read_csv(paste0('output-data/excess-deaths/', tolower(country), '_excess_deaths.csv'))

# Combine the two:
hist$excess_deaths <- NA 
hist <- hist[hist$end_date < as.Date('2020-01-01'), ]
expected <- expected[expected$start_date > as.Date('2020-01-01'), ]
df <- rbind(hist, expected[, colnames(hist)])
df$expected_deaths <- as.numeric(df$expected_deaths)

df$expected_deaths_annual <- ave(df$expected_deaths, df$year, FUN = sum)
df$total_deaths_annual <- ave(df$total_deaths, df$year, FUN = sum)

# Plot by week:
ggplot(df, aes(x=start_date, y=total_deaths, color = 'total deaths'))+geom_line()+geom_line(aes(y=expected_deaths, col='expected_deaths'))

# Plot by year (excluding 2022):
ggplot(df[df$year < 2022 & !duplicated(df$year), ], aes(x=start_date, y=total_deaths_annual, color = 'total deaths, annual'))+geom_line()+geom_point()+geom_line(aes(y=expected_deaths_annual, col='expected deaths, annual'))


