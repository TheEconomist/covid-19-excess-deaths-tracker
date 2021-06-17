# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(aweek)
options(scipen=999)

# Import global JHU data from Our World In Data
global_covid_source_latest <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv") %>%
  add_row(date=seq(as.Date("2019-12-31"), as.Date("2020-01-21"), by="days")) %>% arrange(date)
global_covid_source_cumulative <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths.csv") %>%
  add_row(date=seq(as.Date("2019-12-31"), as.Date("2020-01-21"), by="days")) %>% arrange(date)

# Import global mortality data from World Mortality Dataset
world_mortality_dataset <- fread("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")

# Step 2: import and clean Albania's data ---------------------------------------

# Import and group Albania's total deaths by month
albania_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Albania", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2845955, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
albania_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Albania) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
albania_monthly_deaths <- albania_monthly_total_deaths %>%
  left_join(albania_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(albania_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/albania_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 3: import and clean Armenia's data ---------------------------------------

# Import and group Armenia's total deaths by month
armenia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Armenia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2956900, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
armenia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Armenia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
armenia_monthly_deaths <- armenia_monthly_total_deaths %>%
  left_join(armenia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(armenia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/armenia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 4: import and clean Australia's data ---------------------------------------

# Import and group Australia's total deaths by week
australia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Australia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 25814400, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
australia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week_date = date,
         week = week(week_date),
         year = year(week_date),
         covid_deaths = Australia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
australia_weekly_deaths <- australia_weekly_total_deaths %>%
  left_join(australia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(australia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/australia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 5: import and clean Austria's data ---------------------------------------

# Import and group Austria's total deaths by week
austria_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Austria", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 8902600, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
austria_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Austria) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
austria_weekly_deaths <- austria_weekly_total_deaths %>%
  left_join(austria_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(austria_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/austria_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 6: import and clean Azerbaijan's data ---------------------------------------

# Import and group Azerbaijan's total deaths by month
azerbaijan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Azerbaijan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 10127874, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
azerbaijan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Azerbaijan) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
azerbaijan_monthly_deaths <- azerbaijan_monthly_total_deaths %>%
  left_join(azerbaijan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(azerbaijan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/azerbaijan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 7: import and clean Belarus's data ---------------------------------------

# Import and group Belarus's total deaths by month
belarus_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Belarus", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 9408400, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
belarus_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Belarus) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
belarus_monthly_deaths <- belarus_monthly_total_deaths %>%
  left_join(belarus_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(belarus_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/belarus_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 8: import and clean Belgium's data ---------------------------------------

# Import and group Belgium's total deaths by week
belgium_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Belgium", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 11431406, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
belgium_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Belgium) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
belgium_weekly_deaths <- belgium_weekly_total_deaths %>%
  left_join(belgium_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(belgium_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/belgium_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 9: import and clean Bolivia's data ---------------------------------------

# Import and group Bolivia's total deaths by month
bolivia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Bolivia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 11428245, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
bolivia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Bolivia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
bolivia_monthly_deaths <- bolivia_monthly_total_deaths %>%
  left_join(bolivia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(bolivia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/bolivia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 10: import and clean Bosnia and Herzegovina's data ---------------------------------------

# Import and group Bosnia and Herzegovina's total deaths by month
bosnia_and_herzegovina_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Bosnia", year >= 2015) %>%
  mutate(country_name = "Bosnia and Herzegovina",country = country_name, region = country_name, 
         region_code = 0, population = 3301000, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
bosnia_and_herzegovina_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Bosnia and Herzegovina`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
bosnia_and_herzegovina_monthly_deaths <- bosnia_and_herzegovina_monthly_total_deaths %>%
  left_join(bosnia_and_herzegovina_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(bosnia_and_herzegovina_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/bosnia_and_herzegovina_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 11: import and clean Brazil's data ---------------------------------------

# Import and group Brazil's total deaths by month
brazil_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Brazil", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 210147125, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
brazil_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Brazil) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
brazil_monthly_deaths <- brazil_monthly_total_deaths %>%
  left_join(brazil_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(brazil_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/brazil_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 12: import and clean Britain's data ---------------------------------------

# Import and group Britain's total deaths by week
britain_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "United Kingdom", year >= 2015) %>%
  mutate(country = "Britain", region = "Britain", region_code = 0, population = 67886004, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year)-2,
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
britain_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date+2,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date+2,week_start=1),1,4)),
         covid_deaths = `United Kingdom`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
britain_weekly_deaths <- britain_weekly_total_deaths %>%
  left_join(britain_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(britain_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/britain_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 13: import and clean Bulgaria's data ---------------------------------------

# Import and group Bulgaria's total deaths by week
bulgaria_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Bulgaria", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 6951482, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
bulgaria_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Bulgaria) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
bulgaria_weekly_deaths <- bulgaria_weekly_total_deaths %>%
  left_join(bulgaria_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(bulgaria_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/bulgaria_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 14: import and clean Canada's data ---------------------------------------

# Import and group Canada's total deaths by week
canada_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Canada", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 38008005, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year)-1,
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
canada_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),1,4)),
         covid_deaths = Canada) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
canada_weekly_deaths <- canada_weekly_total_deaths %>%
  left_join(canada_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(canada_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/canada_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 15: import and clean Chile's data ---------------------------------------

# Import and group Chile's total deaths by week
chile_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Chile", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 17574003, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
chile_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Chile) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
chile_weekly_deaths <- chile_weekly_total_deaths %>%
  left_join(chile_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(chile_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/chile_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 16: import and clean Colombia's data ---------------------------------------

# Import and group Colombia's total deaths by week
colombia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Colombia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 50372424, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
colombia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Colombia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
colombia_weekly_deaths <- colombia_weekly_total_deaths %>%
  left_join(colombia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(colombia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/colombia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 17: import and clean Costa Rica's data ---------------------------------------

# Import and group Costa Rica's total deaths by month
costa_rica_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Costa Rica", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 4999441, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
costa_rica_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Costa Rica`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
costa_rica_monthly_deaths <- costa_rica_monthly_total_deaths %>%
  left_join(costa_rica_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(costa_rica_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/costa_rica_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 18: import and clean Croatia's data ---------------------------------------

# Import and group Croatia's total deaths by week
croatia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Croatia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 4058165, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
croatia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Croatia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
croatia_weekly_deaths <- croatia_weekly_total_deaths %>%
  left_join(croatia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(croatia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/croatia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 19: import and clean Cyprus's data ---------------------------------------

# Import and group Cyprus's total deaths by week
cyprus_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Cyprus", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 1189265, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
cyprus_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Cyprus) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
cyprus_weekly_deaths <- cyprus_weekly_total_deaths %>%
  left_join(cyprus_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(cyprus_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/cyprus_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 20: import and clean the Czech Republic's data ---------------------------------------

# Import and group the Czech Republic's total deaths by week
czech_republic_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Czechia", year >= 2015) %>%
  mutate(country = "Czech Republic", region = "Czech Republic", region_code = 0, population = 10693939, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
czech_republic_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Czechia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
czech_republic_weekly_deaths <- czech_republic_weekly_total_deaths %>%
  left_join(czech_republic_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(czech_republic_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/czech_republic_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 21: import and clean Denmark's data ---------------------------------------

# Import and group Denmark's total deaths by week
denmark_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Denmark", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5837213, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
denmark_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Denmark) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
denmark_weekly_deaths <- denmark_weekly_total_deaths %>%
  left_join(denmark_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(denmark_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/denmark_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 22: import and clean Ecuador's data ---------------------------------------

# Import and group Ecuador's total deaths by week
ecuador_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Ecuador", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 17684536, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
ecuador_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Ecuador) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
ecuador_weekly_deaths <- ecuador_weekly_total_deaths %>%
  left_join(ecuador_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(ecuador_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/ecuador_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 23: import and clean Egypt's data ---------------------------------------

# Import and group Egypt's total deaths by month
egypt_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Egypt", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 100075480, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
egypt_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Egypt) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
egypt_monthly_deaths <- egypt_monthly_total_deaths %>%
  left_join(egypt_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(egypt_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/egypt_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 24: import and clean El Salvador's data ---------------------------------------

# Import and group El Salvador's total deaths by month
el_salvador_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "El Salvador", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 6420746, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
el_salvador_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `El Salvador`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
el_salvador_monthly_deaths <- el_salvador_monthly_total_deaths %>%
  left_join(el_salvador_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(el_salvador_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/el_salvador_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 25: import and clean Estonia's data ---------------------------------------

# Import and group Estonia's total deaths by week
estonia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Estonia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 1329460, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
estonia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Estonia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
estonia_weekly_deaths <- estonia_weekly_total_deaths %>%
  left_join(estonia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(estonia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/estonia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 26: import and clean Finland's data ---------------------------------------

# Import and group Finland's total deaths by week
finland_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Finland", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5536146, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
finland_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Finland) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
finland_weekly_deaths <- finland_weekly_total_deaths %>%
  left_join(finland_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(finland_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/finland_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 27: import and clean France's data ---------------------------------------

# Import and group France's total deaths by week
france_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "France", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 67413000, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
france_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = France) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
france_weekly_deaths <- france_weekly_total_deaths %>%
  left_join(france_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(france_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/france_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 28: import and clean Georgia's data ---------------------------------------

# Import and group Georgia's total deaths by month
georgia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Georgia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 3716858, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
georgia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Georgia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
georgia_monthly_deaths <- georgia_monthly_total_deaths %>%
  left_join(georgia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(georgia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/georgia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 29: import and clean Germany's data ---------------------------------------

# Import and group Germany's total deaths by week
germany_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Germany", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 83166711, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
germany_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Germany) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
germany_weekly_deaths <- germany_weekly_total_deaths %>%
  left_join(germany_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(germany_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/germany_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 30: import and clean Greece's data ---------------------------------------

# Import and group Greece's total deaths by week
greece_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Greece", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 10724599, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
greece_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Greece) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
greece_weekly_deaths <- greece_weekly_total_deaths %>%
  left_join(greece_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(greece_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/greece_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 31: import and clean Hungary's data ---------------------------------------

# Import and group Hungary's total deaths by week
hungary_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Hungary", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 9769526, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
hungary_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Hungary) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
hungary_weekly_deaths <- hungary_weekly_total_deaths %>%
  left_join(hungary_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(hungary_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/hungary_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 32: import and clean Iceland's data ---------------------------------------

# Import and group Iceland's total deaths by week
iceland_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Iceland", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 364134, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
iceland_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Iceland) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
iceland_weekly_deaths <- iceland_weekly_total_deaths %>%
  left_join(iceland_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(iceland_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/iceland_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 33: import and clean Indonesia's data ---------------------------------------

# Import Indonesia's data
indonesia_total_source_latest <- fread("source-data/indonesia/indonesia_total_source_latest.csv") 

# Join weekly total deaths and weekly covid deaths together
indonesia_monthly_deaths <- indonesia_total_source_latest %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         days = end_date - start_date + 1, 
         region_code = 0,
         expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(indonesia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/indonesia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 34: import and clean Iran's data ---------------------------------------

# Import and group Iran's total deaths by quarter
iran_quarterly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Iran", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 83183741, 
         quarter = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,(quarter*3)-2,1)), 
         end_date = ceiling_date(start_date,unit="quarter")-1) %>%
  mutate(start_date = start_date - 11, # Use Solar Hijri dates
         end_date = end_date - 11) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,population,total_deaths)

# Group covid deaths by quarter
iran_quarterly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(solar_hijri_date = date + 11,
         quarter = quarter(solar_hijri_date),
         year = year(solar_hijri_date),
         covid_deaths = `Iran`) %>%
  dplyr::select(date,year,quarter,covid_deaths) %>%
  group_by(year,quarter) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
iran_quarterly_deaths <- iran_quarterly_total_deaths %>%
  left_join(iran_quarterly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(iran_quarterly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/iran_quarterly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 35: import and clean Israel's data ---------------------------------------

# Import and group Israel's total deaths by week
israel_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Israel", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 9312200, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
israel_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Israel) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
israel_weekly_deaths <- israel_weekly_total_deaths %>%
  left_join(israel_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(israel_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/israel_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 36: import and clean Italy's data ---------------------------------------

# Import and group Italy's total deaths by week
italy_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Italy", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 60317116, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
italy_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Italy) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
italy_weekly_deaths <- italy_weekly_total_deaths %>%
  left_join(italy_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(italy_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/italy_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 37: import and clean Jamaica's data ---------------------------------------

# Import and group Jamaica's total deaths by month
jamaica_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Jamaica", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2726667, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
jamaica_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Jamaica`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
jamaica_monthly_deaths <- jamaica_monthly_total_deaths %>%
  left_join(jamaica_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(jamaica_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/jamaica_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 38: import and clean Japan's data ---------------------------------------

# Import and group Japan's total deaths by month
japan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Japan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 125570000, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
japan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Japan) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
japan_monthly_deaths <- japan_monthly_total_deaths %>%
  left_join(japan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(japan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/japan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 39: import and clean Kazakhstan's data ---------------------------------------

# Import and group Kazakhstan's total deaths by month
kazakhstan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Kazakhstan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 18711200, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
kazakhstan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Kazakhstan`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
kazakhstan_monthly_deaths <- kazakhstan_monthly_total_deaths %>%
  left_join(kazakhstan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(kazakhstan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/kazakhstan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 40: import and clean Kosovo's data ---------------------------------------

# Import and group Kosovo's total deaths by month
kosovo_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Kosovo", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 1873160, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
kosovo_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Kosovo) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
kosovo_monthly_deaths <- kosovo_monthly_total_deaths %>%
  left_join(kosovo_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(kosovo_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/kosovo_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 41: import and clean Kyrgyzstan's data ---------------------------------------

# Import and group Kyrgyzstan's total deaths by month
kyrgyzstan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Kyrgyzstan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 6586600, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
kyrgyzstan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Kyrgyzstan) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
kyrgyzstan_monthly_deaths <- kyrgyzstan_monthly_total_deaths %>%
  left_join(kyrgyzstan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(kyrgyzstan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/kyrgyzstan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 42: import and clean Latvia's data ---------------------------------------

# Import and group Latvia's total deaths by week
latvia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Latvia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 1907675, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
latvia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Latvia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
latvia_weekly_deaths <- latvia_weekly_total_deaths %>%
  left_join(latvia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(latvia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/latvia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 43: import and clean Lithuania's data ---------------------------------------

# Import and group Lithuania's total deaths by week
lithuania_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Lithuania", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2793694, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
lithuania_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Lithuania) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
lithuania_weekly_deaths <- lithuania_weekly_total_deaths %>%
  left_join(lithuania_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(lithuania_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/lithuania_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 44: import and clean Luxembourg's data ---------------------------------------

# Import and group Luxembourg's total deaths by week
luxembourg_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Luxembourg", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 626108, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
luxembourg_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Luxembourg) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
luxembourg_weekly_deaths <- luxembourg_weekly_total_deaths %>%
  left_join(luxembourg_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(luxembourg_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/luxembourg_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 45: import and clean Malaysia's data ---------------------------------------

# Import and group Malaysia's total deaths by quarter
malaysia_quarterly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Malaysia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 32730000, 
         quarter = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,(quarter*3)-2,1)), 
         end_date = ceiling_date(start_date,unit="quarter")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,population,total_deaths)

# Group covid deaths by quarter
malaysia_quarterly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(quarter = quarter(date),
         year = year(date),
         covid_deaths = `Malaysia`) %>%
  dplyr::select(date,year,quarter,covid_deaths) %>%
  group_by(year,quarter) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
malaysia_quarterly_deaths <- malaysia_quarterly_total_deaths %>%
  left_join(malaysia_quarterly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(malaysia_quarterly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/malaysia_quarterly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 46: import and clean Malta's data ---------------------------------------

# Import and group Malta's total deaths by week
malta_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Malta", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 514564, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
malta_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Malta) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
malta_weekly_deaths <- malta_weekly_total_deaths %>%
  left_join(malta_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(malta_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/malta_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 47: import and clean Mauritius's data ---------------------------------------

# Import and group Mauritius's total deaths by month
mauritius_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Mauritius", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 1265475, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
mauritius_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Mauritius`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
mauritius_monthly_deaths <- mauritius_monthly_total_deaths %>%
  left_join(mauritius_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(mauritius_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/mauritius_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 48: import and clean Mexico's data ---------------------------------------

# Import and group Mexico's total deaths by week
mexico_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Mexico", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 126014024, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
mexico_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Mexico) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
mexico_weekly_deaths <- mexico_weekly_total_deaths %>%
  left_join(mexico_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(mexico_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/mexico_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 49: import and clean Moldova's data ---------------------------------------

# Import and group Moldova's total deaths by month
moldova_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Moldova", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2640438, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
moldova_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Moldova) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
moldova_monthly_deaths <- moldova_monthly_total_deaths %>%
  left_join(moldova_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(moldova_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/moldova_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 50: import and clean Mongolia's data ---------------------------------------

# Import and group Mongolia's total deaths by month
mongolia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Mongolia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 3353470, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
mongolia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Mongolia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
mongolia_monthly_deaths <- mongolia_monthly_total_deaths %>%
  left_join(mongolia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(mongolia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/mongolia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 51: import and clean Montenegro's data ---------------------------------------

# Import and group Montenegro's total deaths by week
montenegro_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Montenegro", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 621873, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
montenegro_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Montenegro) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
montenegro_weekly_deaths <- montenegro_weekly_total_deaths %>%
  left_join(montenegro_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(montenegro_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/montenegro_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 52: import and clean the Netherlands' data ---------------------------------------

# Import and group the Netherlands' total deaths by week
netherlands_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Netherlands", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 17414806, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
netherlands_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Netherlands) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
netherlands_weekly_deaths <- netherlands_weekly_total_deaths %>%
  left_join(netherlands_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(netherlands_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/netherlands_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 53: import and clean New Zealand's data ---------------------------------------

# Import and group New Zealand's total deaths by week
new_zealand_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "New Zealand", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5110490, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
new_zealand_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = `New Zealand`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
new_zealand_weekly_deaths <- new_zealand_weekly_total_deaths %>%
  left_join(new_zealand_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(new_zealand_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/new_zealand_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 54: import and clean Nicaragua's data ---------------------------------------

# Import and group Nicaragua's total deaths by month
nicaragua_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Nicaragua", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 6486201, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
nicaragua_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Nicaragua`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
nicaragua_monthly_deaths <- nicaragua_monthly_total_deaths %>%
  left_join(nicaragua_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(nicaragua_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/nicaragua_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 55: import and clean North Macedonia's data ---------------------------------------

# Import and group North Macedonia's total deaths by month
north_macedonia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "North Macedonia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2077132, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
north_macedonia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `North Macedonia`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
north_macedonia_monthly_deaths <- north_macedonia_monthly_total_deaths %>%
  left_join(north_macedonia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(north_macedonia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/north_macedonia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 56: import and clean Norway's data ---------------------------------------

# Import and group Norway's total deaths by week
norway_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Norway", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5384576, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
norway_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Norway) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
norway_weekly_deaths <- norway_weekly_total_deaths %>%
  left_join(norway_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(norway_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/norway_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 57: import and clean Oman's data ---------------------------------------

# Import and group Oman's total deaths by month
oman_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Oman", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 4829473, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
oman_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Oman) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
oman_monthly_deaths <- oman_monthly_total_deaths %>%
  left_join(oman_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(oman_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/oman_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 58: import and clean Panama's data ---------------------------------------

# Import and group Panama's total deaths by month
panama_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Panama", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 4176869, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
panama_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Panama`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
panama_monthly_deaths <- panama_monthly_total_deaths %>%
  left_join(panama_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(panama_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/panama_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 59: import and clean Paraguay's data ---------------------------------------

# Import and group Paraguay's total deaths by month
paraguay_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Paraguay", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 4176869, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
paraguay_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = `Paraguay`) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
paraguay_monthly_deaths <- paraguay_monthly_total_deaths %>%
  left_join(paraguay_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(paraguay_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/paraguay_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 60: import and clean Peru's data ---------------------------------------

# Import and group Peru's total deaths by week
peru_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Peru", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 32824358, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
peru_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Peru) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
peru_weekly_deaths <- peru_weekly_total_deaths %>%
  left_join(peru_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(peru_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/peru_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 61: import and clean the Philippines' data ---------------------------------------

# Import and group the Philippines' total deaths by month
philippines_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Philippines", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 106651394, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
philippines_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Philippines) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
philippines_monthly_deaths <- philippines_monthly_total_deaths %>%
  left_join(philippines_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(philippines_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/philippines_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 62: import and clean Poland's data ---------------------------------------

# Import and group Poland's total deaths by week
poland_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Poland", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 38383000, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
poland_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Poland) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
poland_weekly_deaths <- poland_weekly_total_deaths %>%
  left_join(poland_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(poland_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/poland_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 63: import and clean Portugal's data ---------------------------------------

# Import and group Portugal's total deaths by week
portugal_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Portugal", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 10295909, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
portugal_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Portugal) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
portugal_weekly_deaths <- portugal_weekly_total_deaths %>%
  left_join(portugal_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(portugal_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/portugal_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 64: import and clean Qatar's data ---------------------------------------

# Import and group Qatar's total deaths by month
qatar_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Qatar", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2795484, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
qatar_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Qatar) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
qatar_monthly_deaths <- qatar_monthly_total_deaths %>%
  left_join(qatar_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(qatar_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/qatar_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 65: import and clean Romania's data ---------------------------------------

# Import and group Romania's total deaths by week
romania_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Romania", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 19317984, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
romania_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Romania) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
romania_weekly_deaths <- romania_weekly_total_deaths %>%
  left_join(romania_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(romania_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/romania_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 66: import and clean Russia's data ---------------------------------------

# Import and group Russia's total deaths by month
russia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Russia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 146238185, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
russia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Russia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
russia_monthly_deaths <- russia_monthly_total_deaths %>%
  left_join(russia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(russia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/russia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 67: import and clean Serbia's data ---------------------------------------

# Import and group Serbia's total deaths by month
serbia_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Serbia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 6926705, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
serbia_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Serbia) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
serbia_monthly_deaths <- serbia_monthly_total_deaths %>%
  left_join(serbia_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(serbia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/serbia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 68: import and clean Singapore's data ---------------------------------------

# Import and group Singapore's total deaths by month
singapore_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Singapore", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5703600, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
singapore_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Singapore) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
singapore_monthly_deaths <- singapore_monthly_total_deaths %>%
  left_join(singapore_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(singapore_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/singapore_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 69: import and clean Slovakia's data ---------------------------------------

# Import and group Slovakia's total deaths by week
slovakia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Slovakia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 5464060, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
slovakia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Slovakia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
slovakia_weekly_deaths <- slovakia_weekly_total_deaths %>%
  left_join(slovakia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(slovakia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/slovakia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 70: import and clean Slovenia's data ---------------------------------------

# Import and group Slovenia's total deaths by week
slovenia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Slovenia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 2100126, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
slovenia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Slovenia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
slovenia_weekly_deaths <- slovenia_weekly_total_deaths %>%
  left_join(slovenia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(slovenia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/slovenia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 71: import and clean South Africa's data ---------------------------------------

# Import and group South Africa's total deaths by week
south_africa_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "South Africa", year >= 2020) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 59622350, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year)-1,
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Import and group South Africa's expected deaths by week
south_africa_weekly_expected_deaths <- world_mortality_dataset %>%
  filter(country_name == "South Africa", year == 0) %>%
  mutate(country = country_name, week = time, expected_deaths = deaths) %>%
  dplyr::select(country,week,expected_deaths)

# Group covid deaths by week
south_africa_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),1,4)),
         covid_deaths = `South Africa`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
south_africa_weekly_deaths <- south_africa_weekly_total_deaths %>%
  left_join(south_africa_weekly_expected_deaths) %>% 
  left_join(south_africa_weekly_covid_deaths) %>% 
  mutate(region_code = 0) %>%
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(south_africa_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/south_africa_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 72: import and clean South Korea's data ---------------------------------------

# Import and group South Korea's total deaths by week
south_korea_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "South Korea", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 51709098, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
south_korea_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = `South Korea`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
south_korea_weekly_deaths <- south_korea_weekly_total_deaths %>%
  left_join(south_korea_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(south_korea_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/south_korea_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 73: import and clean Spain's data ---------------------------------------

# Import and group Spain's total deaths by week
spain_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Spain", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 47450795, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
spain_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Spain) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
spain_weekly_deaths <- spain_weekly_total_deaths %>%
  left_join(spain_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(spain_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/spain_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 74: import and clean Sweden's data ---------------------------------------

# Import and group Sweden's total deaths by week
sweden_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Sweden", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 10380245, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
sweden_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Sweden) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
sweden_weekly_deaths <- sweden_weekly_total_deaths %>%
  left_join(sweden_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(sweden_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/sweden_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 75: import and clean Switzerland's data ---------------------------------------

# Import and group Switzerland's total deaths by week
switzerland_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Switzerland", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 8570146, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
switzerland_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Switzerland) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
switzerland_weekly_deaths <- switzerland_weekly_total_deaths %>%
  left_join(switzerland_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(switzerland_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/switzerland_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 76: import and clean Taiwan's data ---------------------------------------

# Import and group Taiwan's total deaths by month
taiwan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Taiwan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 23568378, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
taiwan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Taiwan) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
taiwan_monthly_deaths <- taiwan_monthly_total_deaths %>%
  left_join(taiwan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(taiwan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/taiwan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 77: import and clean Tajikistan's data ---------------------------------------

# Import and group Tajikistan's total deaths by quarter
tajikistan_quarterly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Tajikistan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 9537645, 
         quarter = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,(quarter*3)-2,1)), 
         end_date = ceiling_date(start_date,unit="quarter")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,population,total_deaths)

# Group covid deaths by quarter
tajikistan_quarterly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(quarter = quarter(date),
         year = year(date),
         covid_deaths = `Tajikistan`) %>%
  dplyr::select(date,year,quarter,covid_deaths) %>%
  group_by(year,quarter) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
tajikistan_quarterly_deaths <- tajikistan_quarterly_total_deaths %>%
  left_join(tajikistan_quarterly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,quarter,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(tajikistan_quarterly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/tajikistan_quarterly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 78: import and clean Thailand's data ---------------------------------------

# Import and group Thailand's total deaths by month
thailand_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Thailand", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 66558935, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
thailand_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Thailand) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
thailand_monthly_deaths <- thailand_monthly_total_deaths %>%
  left_join(thailand_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(thailand_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/thailand_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 79: import and clean Tunisia's data ---------------------------------------

# Import and group Tunisia's total deaths by week
tunisia_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Tunisia", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 11708370, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year),
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
tunisia_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date,week_start=1),1,4)),
         covid_deaths = Tunisia) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
tunisia_weekly_deaths <- tunisia_weekly_total_deaths %>%
  left_join(tunisia_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(tunisia_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/tunisia_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 80: import and clean Turkey's data ---------------------------------------

# Import Turkey's data
turkey_total_source_latest <- fread("source-data/turkey/turkey_total_source_latest.csv") 

# Group total deaths by week
turkey_weekly_total_deaths <- turkey_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,year,week,population) %>%
  summarise(days = sum(!is.na(total_deaths)),
            total_deaths = sum(total_deaths, na.rm=T)) %>%
  ungroup() %>%
  filter(days != 0) %>%
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + days)

# Group covid deaths by week, assigning Istanbul half of Turkey's national deaths
turkey_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(year = year(date),
         week = week(date),
         covid_deaths = Turkey / 2) %>% # Assign Istanbul half of Turkey's national deaths
  dplyr::select(year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  ungroup() %>%
  bind_rows(expand.grid(year = seq(2017,2019), week = seq(1,52), covid_deaths = 0)) %>%
  arrange(year, week)

# Join weekly total deaths and weekly covid deaths together
turkey_weekly_deaths <- turkey_weekly_total_deaths %>%
  left_join(turkey_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) 

# Export as CSV
write.csv(turkey_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/turkey_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 81: import and clean Ukraine's data ---------------------------------------

# Import and group Ukraine's total deaths by month
ukraine_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Ukraine", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 41629926, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
ukraine_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Ukraine) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
ukraine_monthly_deaths <- ukraine_monthly_total_deaths %>%
  left_join(ukraine_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(ukraine_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/ukraine_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 82: import and clean the United States' data ---------------------------------------

# Import and group the United States' total deaths by week
united_states_weekly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "United States", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 331449281, 
         week = time, total_deaths = deaths,
         start_date = aweek::get_date(week=week,year=year)-1,
         end_date = start_date + 6) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,population,total_deaths)

# Group covid deaths by week
united_states_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),7,8)),
         year = as.numeric(str_sub(aweek::date2week(date+1,week_start=1),1,4)),
         covid_deaths = `United States`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
united_states_weekly_deaths <- united_states_weekly_total_deaths %>%
  left_join(united_states_weekly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(united_states_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/united_states_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 84: import and clean Uzbekistan's data ---------------------------------------

# Import and group Uzbekistan's total deaths by month
uzbekistan_monthly_total_deaths <- world_mortality_dataset %>%
  filter(country_name == "Uzbekistan", year >= 2015) %>%
  mutate(country = country_name, region = country_name, region_code = 0, population = 33570609, 
         month = time, total_deaths = deaths,
         start_date = as.Date(ISOdate(year,month,1)),
         end_date = ceiling_date(start_date,unit="month")-1) %>%
  mutate(days = end_date - start_date + 1) %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,population,total_deaths)

# Group covid deaths by month
uzbekistan_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Uzbekistan) %>%
  dplyr::select(date,year,month,covid_deaths) %>%
  group_by(year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
uzbekistan_monthly_deaths <- uzbekistan_monthly_total_deaths %>%
  left_join(uzbekistan_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,days,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(uzbekistan_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/uzbekistan_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)
