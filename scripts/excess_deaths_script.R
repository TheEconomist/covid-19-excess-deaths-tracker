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
brazil_monthly_deaths <- fread("output-data/historical-deaths/brazil_monthly_deaths.csv")
britain_weekly_deaths <- fread("output-data/historical-deaths/britain_weekly_deaths.csv")
chile_weekly_deaths <- fread("output-data/historical-deaths/chile_weekly_deaths.csv")
denmark_weekly_deaths <- fread("output-data/historical-deaths/denmark_weekly_deaths.csv")
ecuador_monthly_deaths <- fread("output-data/historical-deaths/ecuador_monthly_deaths.csv")
france_weekly_deaths <- fread("output-data/historical-deaths/france_weekly_deaths.csv")
germany_weekly_deaths <- fread("output-data/historical-deaths/germany_weekly_deaths.csv")
indonesia_monthly_deaths <- fread("output-data/historical-deaths/indonesia_monthly_deaths.csv")
italy_weekly_deaths <- fread("output-data/historical-deaths/italy_weekly_deaths.csv")
mexico_weekly_deaths <- fread("output-data/historical-deaths/mexico_weekly_deaths.csv")
netherlands_weekly_deaths <- fread("output-data/historical-deaths/netherlands_weekly_deaths.csv")
norway_weekly_deaths <- fread("output-data/historical-deaths/norway_weekly_deaths.csv")
peru_monthly_deaths <- fread("output-data/historical-deaths/peru_monthly_deaths.csv")
portugal_weekly_deaths <- fread("output-data/historical-deaths/portugal_weekly_deaths.csv")
russia_monthly_deaths <- fread("output-data/historical-deaths/russia_monthly_deaths.csv")
south_africa_weekly_deaths <- fread("output-data/historical-deaths/south_africa_weekly_deaths.csv")
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
           non_covid_deaths = total_deaths - covid_deaths,
           region_code = as.character(region_code)) %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)
  
  # Calculate weekly rates for monthly data
  if(frequency == "monthly") {
    
    excess_deaths <- excess_deaths %>%
      mutate(month_days = as.numeric(difftime(end_date,start_date,units=c("days"))) + 1,
             total_deaths_per_7_days = total_deaths / month_days * 7,
             covid_deaths_per_7_days = covid_deaths / month_days * 7,
             expected_deaths_per_7_days = expected_deaths / month_days * 7,
             excess_deaths_per_7_days = excess_deaths / month_days * 7,
             non_covid_deaths_per_7_days = non_covid_deaths / month_days * 7,
             covid_deaths_per_100k_per_7_days = covid_deaths_per_100k / month_days * 7,
             excess_deaths_per_100k_per_7_days = excess_deaths_per_100k / month_days * 7) %>%
      dplyr::select(-month_days)
    
  }
  
  excess_deaths
  
}

# Export Austria
austria_excess_deaths <- get_excess_deaths(austria_weekly_deaths)
write.csv(austria_excess_deaths,"output-data/excess-deaths/austria_excess_deaths.csv",
       fileEncoding = "UTF-8",row.names=FALSE)

# Export Belgium
belgium_excess_deaths <- get_excess_deaths(belgium_weekly_deaths,calculate=FALSE)
write.csv(belgium_excess_deaths,"output-data/excess-deaths/belgium_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Brazil
brazil_excess_deaths <- get_excess_deaths(brazil_monthly_deaths,frequency="monthly")
write.csv(brazil_excess_deaths,"output-data/excess-deaths/brazil_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Britain
britain_excess_deaths <- get_excess_deaths(britain_weekly_deaths)
write.csv(britain_excess_deaths,"output-data/excess-deaths/britain_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Chile
chile_excess_deaths <- get_excess_deaths(chile_weekly_deaths)
write.csv(chile_excess_deaths,"output-data/excess-deaths/chile_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Denmark
denmark_excess_deaths <- get_excess_deaths(denmark_weekly_deaths)
write.csv(denmark_excess_deaths,"output-data/excess-deaths/denmark_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Ecuador
ecuador_excess_deaths <- get_excess_deaths(ecuador_monthly_deaths,frequency="monthly")
write.csv(ecuador_excess_deaths,"output-data/excess-deaths/ecuador_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export France
france_excess_deaths <- get_excess_deaths(france_weekly_deaths)
write.csv(france_excess_deaths,"output-data/excess-deaths/france_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Germany
germany_excess_deaths <- get_excess_deaths(germany_weekly_deaths)
write.csv(germany_excess_deaths,"output-data/excess-deaths/germany_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Indonesia
indonesia_excess_deaths <- get_excess_deaths(indonesia_monthly_deaths,frequency="monthly")
write.csv(indonesia_excess_deaths,"output-data/excess-deaths/indonesia_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Italy
italy_excess_deaths <- get_excess_deaths(italy_weekly_deaths)
write.csv(italy_excess_deaths,"output-data/excess-deaths/italy_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Mexico
mexico_excess_deaths <- get_excess_deaths(mexico_weekly_deaths)
write.csv(mexico_excess_deaths,"output-data/excess-deaths/mexico_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export the Netherlands
netherlands_excess_deaths <- get_excess_deaths(netherlands_weekly_deaths)
write.csv(netherlands_excess_deaths,"output-data/excess-deaths/netherlands_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Norway
norway_excess_deaths <- get_excess_deaths(norway_weekly_deaths)
write.csv(norway_excess_deaths,"output-data/excess-deaths/norway_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Peru
peru_excess_deaths <- get_excess_deaths(peru_monthly_deaths,frequency="monthly")
write.csv(peru_excess_deaths,"output-data/excess-deaths/peru_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Portugal
portugal_excess_deaths <- get_excess_deaths(portugal_weekly_deaths)
write.csv(portugal_excess_deaths,"output-data/excess-deaths/portugal_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Russia
russia_excess_deaths <- get_excess_deaths(russia_monthly_deaths,frequency="monthly")
write.csv(russia_excess_deaths,"output-data/excess-deaths/russia_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export South Africa
south_africa_excess_deaths <- get_excess_deaths(south_africa_weekly_deaths,calculate=FALSE)
write.csv(south_africa_excess_deaths,"output-data/excess-deaths/south_africa_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Spain
spain_excess_deaths <- get_excess_deaths(spain_weekly_deaths,calculate=FALSE)
write.csv(spain_excess_deaths,"output-data/excess-deaths/spain_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Sweden
sweden_excess_deaths <- get_excess_deaths(sweden_weekly_deaths)
write.csv(sweden_excess_deaths,"output-data/excess-deaths/sweden_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Switzerland
switzerland_excess_deaths <- get_excess_deaths(switzerland_weekly_deaths)
write.csv(switzerland_excess_deaths,"output-data/excess-deaths/switzerland_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export Turkey
turkey_excess_deaths <- get_excess_deaths(turkey_weekly_deaths)
write.csv(turkey_excess_deaths,"output-data/excess-deaths/turkey_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Export the United States
united_states_excess_deaths <- get_excess_deaths(united_states_weekly_deaths)
write.csv(united_states_excess_deaths,"output-data/excess-deaths/united_states_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Step 3: combine weekly and monthly deaths together, and calculate deaths per 100,000 people and percentage change ---------------------------------------

# Combine weekly deaths and calculate per 100,000 people and percentage change
all_weekly_excess_deaths <- bind_rows(austria_excess_deaths,
                                      belgium_excess_deaths,
                                      britain_excess_deaths,
                                      chile_excess_deaths,
                                      denmark_excess_deaths,
                                      france_excess_deaths,
                                      germany_excess_deaths,
                                      italy_excess_deaths,
                                      mexico_excess_deaths,
                                      netherlands_excess_deaths,
                                      norway_excess_deaths,
                                      portugal_excess_deaths,
                                      south_africa_excess_deaths,
                                      spain_excess_deaths,
                                      sweden_excess_deaths,
                                      switzerland_excess_deaths,
                                      turkey_excess_deaths,
                                      united_states_excess_deaths) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)

# Export weekly deaths
write.csv(all_weekly_excess_deaths,"output-data/excess-deaths/all_weekly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Combine monthly deaths and calculate per 100,000 people and percentage change
all_monthly_excess_deaths <- bind_rows(brazil_excess_deaths,
                                       ecuador_excess_deaths,
                                       indonesia_excess_deaths,
                                       peru_excess_deaths,
                                       russia_excess_deaths) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)

# Export monthly deaths
write.csv(all_monthly_excess_deaths,"output-data/excess-deaths/all_monthly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)
