# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
options(scipen=999)

# Import data
austria_weekly_deaths <- fread("output-data/historical-deaths/austria_weekly_deaths.csv")
belgium_weekly_deaths <- fread("output-data/historical-deaths/belgium_weekly_deaths.csv")
britain_weekly_deaths <- fread("output-data/historical-deaths/britain_weekly_deaths.csv")
denmark_weekly_deaths <- fread("output-data/historical-deaths/denmark_weekly_deaths.csv")
ecuador_monthly_deaths <- fread("output-data/historical-deaths/ecuador_monthly_deaths.csv")
france_weekly_deaths <- fread("output-data/historical-deaths/france_weekly_deaths.csv")
germany_weekly_deaths <- fread("output-data/historical-deaths/germany_weekly_deaths.csv")
indonesia_monthly_deaths <- fread("output-data/historical-deaths/indonesia_monthly_deaths.csv")
italy_weekly_deaths <- fread("output-data/historical-deaths/italy_weekly_deaths.csv")
netherlands_weekly_deaths <- fread("output-data/historical-deaths/netherlands_weekly_deaths.csv")
norway_weekly_deaths <- fread("output-data/historical-deaths/norway_weekly_deaths.csv")
portugal_weekly_deaths <- fread("output-data/historical-deaths/portugal_weekly_deaths.csv")
russia_monthly_deaths <- fread("output-data/historical-deaths/russia_monthly_deaths.csv")
spain_weekly_deaths <- fread("output-data/historical-deaths/spain_weekly_deaths.csv")
sweden_weekly_deaths <- fread("output-data/historical-deaths/sweden_weekly_deaths.csv")
switzerland_weekly_deaths <- fread("output-data/historical-deaths/switzerland_weekly_deaths.csv")
turkey_weekly_deaths <- fread("output-data/historical-deaths/turkey_weekly_deaths.csv")
united_states_weekly_deaths <- fread("output-data/historical-deaths/united_states_weekly_deaths.csv")

# Step 2: define function that calculates excess deaths, and apply to weekly deaths ---------------------------------------

# Define function that calculates excess deaths
get_excess_deaths <- function(df,frequency="weekly",calculate=TRUE){
  
  if(frequency == "weekly" & calculate == TRUE) {
    
    # Calculate expected deaths for weekly time series
    expected_deaths <- df %>%
      dplyr::select(-expected_deaths) %>%
      filter(year == 2020) %>%
      left_join(df %>%
                  filter(year >= 2015,year <= 2019) %>%
                  group_by(region,week) %>%
                  summarise(expected_deaths = mean(total_deaths,na.rm=T)))
      
  } else if(frequency == "monthly" & calculate == TRUE) {
    
    # Calculate expected deaths for monthly time series
    expected_deaths <- df %>%
      dplyr::select(-expected_deaths) %>%
      filter(year == 2020) %>%
      left_join(df %>%
                  filter(year >= 2015,year <= 2019) %>%
                  group_by(region,month) %>%
                  summarise(expected_deaths = mean(total_deaths,na.rm=T)))
    
  } else { expected_deaths <- df %>% filter(year == 2020)}
  
  # Calculate excess deaths
  excess_deaths <- expected_deaths %>%
    mutate(excess_deaths = total_deaths - expected_deaths,
           non_covid_deaths = total_deaths - covid_deaths)
  
}

# Export Austria
write.csv(get_excess_deaths(austria_weekly_deaths,calculate=FALSE),
       "output-data/excess-deaths/austria_excess_deaths.csv",
       fileEncoding = "UTF-8",row.names=FALSE)

# Export Belgium
write.csv(get_excess_deaths(belgium_weekly_deaths,calculate=FALSE),
          "output-data/excess-deaths/belgium_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Britain
write.csv(get_excess_deaths(britain_weekly_deaths),
          "output-data/excess-deaths/britain_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Denmark
write.csv(get_excess_deaths(denmark_weekly_deaths),
          "output-data/excess-deaths/denmark_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Ecuador
write.csv(get_excess_deaths(ecuador_monthly_deaths,frequency="monthly"),
          "output-data/excess-deaths/ecuador_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export France
write.csv(get_excess_deaths(france_weekly_deaths),
          "output-data/excess-deaths/france_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Germany
write.csv(get_excess_deaths(germany_weekly_deaths),
          "output-data/excess-deaths/germany_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Indonesia
write.csv(get_excess_deaths(indonesia_monthly_deaths,frequency="monthly"),
          "output-data/excess-deaths/indonesia_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Italy
write.csv(get_excess_deaths(italy_weekly_deaths),
          "output-data/excess-deaths/italy_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export the Netherlands
write.csv(get_excess_deaths(netherlands_weekly_deaths),
          "output-data/excess-deaths/netherlands_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Norway
write.csv(get_excess_deaths(norway_weekly_deaths),
          "output-data/excess-deaths/norway_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Portugal
write.csv(get_excess_deaths(portugal_weekly_deaths),
          "output-data/excess-deaths/portugal_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Russia
write.csv(get_excess_deaths(russia_monthly_deaths,frequency="monthly"),
          "output-data/excess-deaths/russia_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Spain
write.csv(get_excess_deaths(spain_weekly_deaths,calculate=FALSE),
          "output-data/excess-deaths/spain_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Sweden
write.csv(get_excess_deaths(sweden_weekly_deaths),
          "output-data/excess-deaths/sweden_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Switzerland
write.csv(get_excess_deaths(switzerland_weekly_deaths),
          "output-data/excess-deaths/switzerland_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Turkey
write.csv(get_excess_deaths(turkey_weekly_deaths),
          "output-data/excess-deaths/turkey_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export the United States
write.csv(get_excess_deaths(united_states_weekly_deaths),
          "output-data/excess-deaths/united_states_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)