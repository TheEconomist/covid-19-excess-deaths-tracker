# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(aweek)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

options(scipen=999)

# Import global JHU data from Our World In Data
global_covid_source_latest <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv") %>%
  add_row(date=seq(as.Date("2019-12-31"), as.Date("2020-01-21"), by="days")) %>% arrange(date)

# Adding in a few countries without covid-19 deaths data from Our World in Data:
missing_countries <- c("Aruba", "Bermuda", "Faroe Islands",
                       "French Polynesia", "Gibraltar",
                       "Greenland", "Macao", "Hong Kong")

for(missing_country in setdiff(missing_countries, colnames(global_covid_source_latest))){
  temp <- tibble(country = rep(NA, nrow(global_covid_source_latest)))
  colnames(temp) <- missing_country
  global_covid_source_latest <- cbind(
    global_covid_source_latest, temp)  
}

# Import population data from Our World In Data
country_population_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>% mutate(iso3c = iso_code) %>% select(population, iso3c, location) %>% unique()
country_population_data <- rbind(country_population_data,
                                 data.frame("population" = 49085,
                                            "iso3c" = "FRO",
                                            "location" = "Faroe Islands"),
                                 # https://www.insee.fr/fr/statistiques/1893198
                                 # https://www.insee.fr/fr/statistiques/fichier/1893198/estim-pop-dep-sexe-gca-1975-2021.xlsx
                                 data.frame("population" = 856858,
                                            "iso3c" = "REU",
                                            "location" = "Réunion"))

# Import global mortality data from World Mortality Dataset
world_mortality_dataset <- fread("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv", encoding="UTF-8")

# We also harmonize the name for Bosnia:
world_mortality_dataset$country_name[world_mortality_dataset$country_name == "Bosnia"] <- "Bosnia and Herzegovina"
world_mortality_dataset$country_name[world_mortality_dataset$country_name == "Cabo Verde"] <- "Cape Verde"

# Step 2: Define function to clean data ---------------------------------------

# This combines covid-19 deaths, excess deaths, and population for a country, any name changes and week correspondences, and writes a panel data frame of the following form:

# country = country name
# region = country name) 
# region_code = 0 
# start_date = date of beginning of interval 
# end_date = date of end of interval 
# days = number of days in interval
# week/month/quarter = 1-52, 1-12, or 1-4, as applicable 
# population = country population
# total_deaths = total deaths in interval 
# covid_deaths = total offical covid-19 deaths in interval

cleaning_to_csv <- function(country = "Albania",
                            start_year = 2015,
                            covid_data = global_covid_source_latest,
                            mortality_data = world_mortality_dataset,
                            population_data = country_population_data,
                            replace_names = 
                              data.frame(name = c("Czechia", "United Kingdom"),
                                         replacement = c("Czech Republic", "Britain")),
                            weeks_iso = T,
                            week_starts_on = NA, 
                            output_folder = "output-data/historical-deaths/"){
  
  # Get data frequency:
  frequency <- unique(mortality_data[mortality_data$country_name == country, "time_unit"])
  if(length(frequency) != 1 | !frequency %in% c("weekly", "monthly", "quarterly")){
    stop("Frequency of data not in recognized format.")
  }
  
  # Get country population
  country_population <- population_data$population[population_data$location == country]
  if(length(country_population) != 1){stop(paste0("Missing population data for ", country))}
  
  # This chunk cleans data provided weekly:
  if(frequency == "weekly"){
    
    # If all weeks should follow ISO standard for when they start and end
    if(weeks_iso){
      
      country_weekly_total_deaths <- world_mortality_dataset %>%
        filter(country_name == country, year >= 2015) %>%
        mutate(country = country_name, region = country_name, region_code = 0, 
               population = country_population, 
               week = time, total_deaths = deaths,
               start_date = aweek::get_date(week=week,year=year),
               end_date = start_date + 6) %>%
        mutate(days = end_date - start_date + 1) %>%
        dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)
      
      global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
      country_weekly_covid_deaths <- global_covid_source_latest %>%
        filter(date >= as.Date("2020-01-01")) %>%
        mutate(week_date = date,
               week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
               year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4))) %>%
        dplyr::select(date,year,week,covid_deaths) %>%
        group_by(year,week) %>%
        summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
        drop_na() 
    } else {
      
      country_weekly_total_deaths <- world_mortality_dataset %>%
        filter(country_name == country, year >= 2015) %>%
        mutate(country = country_name, region = country_name, region_code = 0, 
               population = country_population, 
               week = time, total_deaths = deaths,
               start_date = aweek::get_date(week=week,year=year) + ifelse(country %in% week_starts_on[["C"]], -1, 0) + ifelse(country %in% week_starts_on[["D"]], -2, 0),
               end_date = start_date + 6) %>%
        mutate(days = end_date - start_date + 1) %>%
        dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)
      
    
    # Group covid deaths by week
    if(country %in% c(week_starts_on[["B"]])){
      global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
      country_weekly_covid_deaths <- global_covid_source_latest %>%
        filter(date >= as.Date("2020-01-01")) %>%
        mutate(week_date = date,
               week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
               year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4))) %>%
        dplyr::select(date,year,week,covid_deaths) %>%
        group_by(year,week) %>%
        summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
        drop_na() 
      
    } else if(country %in% c(week_starts_on[["C"]])){
      global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
      country_weekly_covid_deaths <- global_covid_source_latest %>%
        filter(date >= as.Date("2020-01-01")) %>%
        mutate(week_date = date,
               week = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),7,8)),
               year = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),1,4))) %>%
        dplyr::select(date,year,week,covid_deaths) %>%
        group_by(year,week) %>%
        summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
        drop_na() 
      
    } else if(country %in% c(week_starts_on[["D"]])){
      global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
      country_weekly_covid_deaths <- global_covid_source_latest %>%
        filter(date >= as.Date("2020-01-01")) %>%
        mutate(week_date = date,
               week = as.numeric(str_sub(aweek::date2week(date+2,week_start=1),7,8)),
               year = as.numeric(str_sub(aweek::date2week(date+2,week_start=1),1,4))) %>%
        dplyr::select(date,year,week,covid_deaths) %>%
        group_by(year,week) %>%
        summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
        drop_na() 
      
    } else { 
      # If no week given
      global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
      country_weekly_covid_deaths <- global_covid_source_latest %>%
        filter(date >= as.Date("2020-01-01")) %>%
        mutate(week_date = date,
               week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
               year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4))) %>%
        dplyr::select(date,year,week,covid_deaths) %>%
        group_by(year,week) %>%
        summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
        drop_na() 
    }
    }
    
    # Join weekly total deaths and weekly covid deaths together
    country_deaths <- country_weekly_total_deaths %>%
      left_join(country_weekly_covid_deaths) %>% 
      mutate(covid_deaths = replace_na(covid_deaths,0),
             expected_deaths = "TBC") %>% # To be calculated
      ungroup() %>%
      dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                    population,total_deaths,covid_deaths,expected_deaths) %>%
      drop_na()
  }
  
  # This chunk cleans data provided monthly:
  if(frequency == "monthly"){
    # Import and group country's total deaths by month
    country_monthly_total_deaths <- world_mortality_dataset %>%
      filter(country_name == country, year >= start_year) %>%
      mutate(country = country_name, region = country_name, region_code = 0, 
             population = country_population, 
             month = time, total_deaths = deaths,
             start_date = as.Date(ISOdate(year,month,1)),
             end_date = ceiling_date(start_date,unit="month")-1) %>%
      mutate(days = end_date - start_date + 1) %>%
      dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)
    
    # Group covid deaths by month
    global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
    country_monthly_covid_deaths <- global_covid_source_latest %>%
      filter(date >= as.Date("2020-01-01")) %>%
      mutate(month = month(date),
             year = year(date)) %>%
      dplyr::select(date,year,month,covid_deaths) %>%
      group_by(year,month) %>%
      summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
      drop_na()
    
    # Join monthly total deaths and monthly covid deaths together
    country_deaths <- country_monthly_total_deaths %>%
      left_join(country_monthly_covid_deaths) %>% 
      mutate(covid_deaths = replace_na(covid_deaths,0),
             expected_deaths = "TBC") %>% # To be calculated
      ungroup() %>%
      dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                    population,total_deaths,covid_deaths,expected_deaths) %>%
      drop_na()
  }
  
  # This chunk cleans data provided quarterly:
  if(frequency == "quarterly"){
    # Import and group country's total deaths by quarter
    country_quarterly_total_deaths <- world_mortality_dataset %>%
      filter(country_name == country, year >= 2015) %>%
      mutate(country = country_name, region = country_name, region_code = 0, 
             population = country_population, 
             quarter = time, total_deaths = deaths,
             start_date = as.Date(ISOdate(year,(quarter*3)-2,1)), 
             end_date = ceiling_date(start_date,unit="quarter")-1) %>%
      mutate(start_date = start_date - 11, # Use Solar Hijri dates
             end_date = end_date - 11) %>%
      mutate(days = end_date - start_date + 1) %>%
      dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,population,total_deaths)
    
    # Group covid deaths by quarter
    global_covid_source_latest$covid_deaths <- global_covid_source_latest[, country]
    country_quarterly_covid_deaths <- global_covid_source_latest %>%
      filter(date >= as.Date("2020-01-01")) %>%
      mutate(solar_hijri_date = date + 11,
             quarter = quarter(solar_hijri_date),
             year = year(solar_hijri_date)) %>%
      dplyr::select(date,year,quarter,covid_deaths) %>%
      group_by(year,quarter) %>%
      summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
      drop_na()
    
    # Join monthly total deaths and monthly covid deaths together
    country_deaths <- country_quarterly_total_deaths %>%
      left_join(country_quarterly_covid_deaths) %>% 
      mutate(covid_deaths = replace_na(covid_deaths,0),
             expected_deaths = "TBC") %>% # To be calculated
      ungroup() %>%
      dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,
                    population,total_deaths,covid_deaths,expected_deaths) %>%
      drop_na()
    
  }
  
  # Replace names for output csv:
  if(country %in% replace_names$name){
    country <- replace_names$replacement[replace_names$name == country]
    country_deaths$country <- country
    country_deaths$region <- country
  }
  
  # Move spaces to "_" make lower case for file names:
  country <- tolower(unlist(gsub(" ", "_", country)))
  
  # Convert from data table to data frame:
  country_deaths <- data.frame(country_deaths)
  
  # Ensure no duplicated country-time-unit observations
  country_deaths <- country_deaths[!duplicated(paste0(country_deaths$year, "_", country_deaths[, c("week", "month", "quarter")[c("week", "month", "quarter") %in% colnames(country_deaths)]])), ]
  
  # Export as CSV
  write.csv(country_deaths %>%
              mutate(start_date = format(start_date, "%Y-%m-%d"),
                     end_date = format(end_date, "%Y-%m-%d")),
            paste0(output_folder, tolower(country), "_", frequency, "_deaths.csv"),
            fileEncoding = "UTF-8",
            row.names=FALSE)
}

# Step 3: Clean data for countries (excepting non-sovereign entities and the United States) ---------------------------------------

# The following areas in the mortality dataset are skipped as they are overseas French departments or non-sovereign countries:
skip <- c("French Guiana", "Guadeloupe", "Martinique", "Mayotte", "Réunion", "Transnistria", "New Caledonia", "Puerto Rico", "Guernsey", "Jersey")

# Cycle through countries:
for(i in setdiff(unique(world_mortality_dataset$country_name), skip)){
  cleaning_to_csv(country = i)
}

# Generate legacy dataset exports:
# 1. Map correspondences for weekly data

# This list of correspondences maps excess deaths reporting week starts (Monday, Tuesday, etc.) to countries. This follows the mappings in the original excess deaths tracker script exactly. Please open an issue if any of them have since changed so we can adjust (such errors would slightly shifts deaths between weeks, but not their total number).
week_start <- list(
  "A" = NA,
  "B" = c("Austria", "Australia", "Belgium", "Bulgaria",       
          "Chile",          "Colombia",       "Croatia",       
          "Cyprus",         "Czechia",        "Denmark",        "Ecuador",       
          "Estonia",        "Finland",        "France",         "Germany",       
          "Greece",         "Guatemala",      "Hungary",        "Iceland",       
          "Israel",         "Italy",          "Latvia",         "Lithuania",   
          "Luxembourg",     "Malta",          "Martinique",     "Mayotte",       
          "Mexico",         "Montenegro",     "Netherlands",    "New Zealand",   
          "Norway",         "Peru",           "Poland",         "Portugal",      
          "Réunion",       "Romania",        "Slovakia",       "Slovenia",
          "South Korea",    "Spain",          "Sweden",        
          "Switzerland",    "Tunisia"),
  "C" = c("Canada", "South Africa"),
  "D" = c("United Kingdom"))

# 2. Cycle through countries and export to separate folder:
for(i in setdiff(unique(world_mortality_dataset$country_name), skip)){
  cleaning_to_csv(country = i,  output_folder = "output-data/alternative-exports-by-non-iso-week/historical-deaths/",
                  weeks_iso = F,
                  week_starts_on = week_start)
}

# Step 4: import and clean the United States' data (this enables results by state) ---------------------------------------

# Import the United States' data
united_states_states <- fread("source-data/united-states/united_states_states.csv")
united_states_covid_source_latest <- fread("https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv")
united_states_total_source_latest <- fread("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv")

# Load world mortality data to find threshold for completeness:
most_recent <- world_mortality_dataset[world_mortality_dataset$country == 'United States', ]
if(most_recent$time_unit == 'weekly'){
  most_recent <- c(max(most_recent$year, na.rm = T), max(most_recent$time, na.rm = T))
} else {
  stop('United States data is no longer weekly. Please inspect manually to ensure new observations are not affected by reporting lags.')
}

# Group US states' total and expected deaths by week
united_states_weekly_total_deaths <- united_states_total_source_latest %>%
  filter(Type == "Predicted (weighted)", Outcome == "All causes") %>%
  mutate(country = "United States",
         region = case_when(State == "New York City" ~ "New York", TRUE ~ State),
         end_date = ymd(`Week Ending Date`),
         start_date = end_date - 6,
         days = 7,
         week = as.numeric(str_sub(aweek::date2week(start_date,week_start=7),7,8)),
         year = as.numeric(str_sub(aweek::date2week(start_date,week_start=7),1,4)),
         total_deaths = `Observed Number`,
         expected_deaths = `Average Expected Count`) %>%
  left_join(united_states_states) %>%
  group_by(country,region,region_code,start_date,end_date,days,year,week,population) %>%
  summarise(total_deaths = sum(total_deaths),
            expected_deaths = sum(expected_deaths)) %>%
  drop_na() %>%
  ungroup()

# This line excludes observations deemed to suffer from plausible reporting lags by the World Mortality project:
united_states_weekly_total_deaths <- united_states_weekly_total_deaths[united_states_weekly_total_deaths$end_date < max(united_states_weekly_total_deaths$end_date[united_states_weekly_total_deaths$week <= most_recent[2] & united_states_weekly_total_deaths$year <= most_recent[1]], na.rm = T), ]

# Group US states' covid deaths by week
united_states_weekly_covid_deaths <- united_states_covid_source_latest %>%
  gather("date","cumulative_deaths",-c(countyFIPS,`County Name`,State,StateFIPS)) %>%
  mutate(state = State,
         cumulative_deaths = as.numeric(cumulative_deaths)) %>%
  group_by(state,date) %>%
  summarise(cumulative_deaths = sum(cumulative_deaths,na.rm=T)) %>%
  ungroup() %>%
  mutate(date = ymd(date)) %>%
  bind_rows(expand.grid(state = unique(united_states_covid_source_latest$State), # Bind on rows before January 21st
                        date = seq(as.Date("2015-01-01"), as.Date("2020-01-21"), by="days"),
                        cumulative_deaths = 0)) %>%
  arrange(state,date) %>%
  group_by(state) %>% # Create a lag, to calculate daily deaths from cumulative ones
  mutate(region_code = state,
         week = as.numeric(str_sub(aweek::date2week(date,week_start=7),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=7),1,4)),
         previous_day_deaths = lag(cumulative_deaths, n = 1, default = NA),
         covid_deaths = case_when(!is.na(cumulative_deaths) & !is.na(previous_day_deaths) ~ cumulative_deaths - previous_day_deaths,
                                  !is.na(cumulative_deaths) ~ cumulative_deaths)) %>%
  group_by(region_code,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths)) 

# Join weekly total deaths and weekly covid deaths together
united_states_weekly_deaths <- united_states_weekly_total_deaths %>%
  left_join(united_states_weekly_covid_deaths) %>% 
  ungroup() %>%
  drop_na() %>%
  bind_rows(united_states_weekly_total_deaths %>%
              left_join(united_states_weekly_covid_deaths) %>% 
              drop_na() %>%
              group_by(country,start_date,end_date,days,year,week) %>%
              summarise(population = sum(population),
                        total_deaths = sum(total_deaths),
                        covid_deaths = sum(covid_deaths),
                        expected_deaths = sum(expected_deaths)) %>%
              mutate(region = "United States",region_code = "USA") %>%
              ungroup()) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths)

# Ensure no duplicate observations
united_states_weekly_deaths <- united_states_weekly_deaths[!duplicated(paste0(united_states_weekly_deaths$year, "_", united_states_weekly_deaths$week, "_", united_states_weekly_deaths$region)), ]

# Remove most recent week for country-level data and two most recent weeks for state-level data (as these observations frequently have notes about reporting lag):
united_states_weekly_deaths <- united_states_weekly_deaths[united_states_weekly_deaths$region == 'United States' | !united_states_weekly_deaths$end_date %in% sort(unique(united_states_weekly_deaths$end_date), decreasing = T)[1:2], ]
united_states_weekly_deaths <- united_states_weekly_deaths[!united_states_weekly_deaths$end_date %in% sort(unique(united_states_weekly_deaths$end_date), decreasing = T)[1], ]

# Export as CSV
write.csv(united_states_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/united_states_by_state_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Export for alternative directory (follows old convention)
write.csv(united_states_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/alternative-exports-by-non-iso-week/historical-deaths/united_states_by_state_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

