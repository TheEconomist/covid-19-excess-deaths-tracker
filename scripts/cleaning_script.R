# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
options(scipen=999)

# Import global ECDC data from Our World In Data
global_covid_source_latest <- read_csv("https://covid.ourworldindata.org/data/ecdc/new_deaths.csv") %>%
  mutate(date = date - 1) # Adjust for one-day lag, because ECDC reports at 10am CET the next day
global_covid_source_cumulative <- read_csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv") %>%
  mutate(date = date - 1) # Adjust for one-day lag, because ECDC reports at 10am CET the next day

# Step 2: import and clean Austria's data ---------------------------------------

# Import Austria's data
austria_total_source_latest <- fread("https://data.statistik.gv.at/data/OGD_gest_kalwo_GEST_KALWOCHE_100.csv")
austria_week_windows <- fread("source-data/austria/austria_week_windows.csv")

# Group total and expected deaths by week
austria_weekly_total_deaths <- austria_total_source_latest %>% 
  left_join(austria_week_windows) %>%
  mutate(country = "Austria",
         region = "Austria",
         population = 8902600,
         start_date = dmy(start_date),
         end_date = dmy(end_date)) %>%
  filter(year >= 2010) %>%
  group_by(country,region,population,start_date,end_date,year,week) %>%
  summarise(total_deaths = sum(`F-ANZ-1`, na.rm=T)) %>%
  ungroup()

# Group covid deaths by week
austria_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week_date = date - 5, # Use Austria's weekly windows
         week = week(week_date),
         year = year(week_date),
         covid_deaths = Austria) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
austria_weekly_deaths <- austria_weekly_total_deaths %>%
  left_join(austria_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(austria_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/austria_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 3: import and clean Belgium's data ---------------------------------------

# Import Belgium's data
belgium_total_source_latest <- fread("source-data/belgium/belgium_total_source_latest.csv") 
belgium_covid_source_latest <- fread("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv")

# Group total and expected deaths by week
belgium_weekly_total_deaths <- belgium_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,week,year,population) %>%
  summarise(total_deaths = sum(total_deaths)) %>%
  arrange(year,week) %>%
  drop_na()

# Group covid deaths by week
belgium_weekly_covid_deaths <- belgium_covid_source_latest %>%
  mutate(date = ymd(DATE)) %>%
  group_by(date) %>%
  summarise(covid_deaths = sum(DEATHS,na.rm=T)) %>%
  bind_rows(expand.grid(date = seq(as.Date("2020-01-01"), as.Date("2020-03-09"), by="days"),
                        covid_deaths = 0)) %>%
  mutate(country = "Belgium",
         week = week(date),
         year = year(date)) %>%
  dplyr::select(country,date,year,week,covid_deaths) %>%
  group_by(country,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na() %>%
  dplyr::select(country,year,week,covid_deaths)

# Join weekly total deaths and weekly covid deaths together
belgium_weekly_deaths <- belgium_weekly_total_deaths %>%
  left_join(belgium_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
       end_date <= as.Date("2020-10-04")) # Remove weeks with incomplete data

# Export as CSV
write.csv(belgium_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/belgium_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 4: import and clean Brazil's data ---------------------------------------

# Import Brazil's data
brazil_total_source_latest <- fread("source-data/brazil/brazil_total_source_latest.csv")

# Group total and expected deaths by week
brazil_weekly_total_deaths <- brazil_total_source_latest %>%
  mutate(week = nu_semana_epidemiologica) %>%
  group_by(week) %>%
  summarise(total_deaths = sum(qt_obitos_2020_corrigido,na.rm=T),
            expected_deaths = sum(qt_obitos_2020_esperado,na.rm=T)) %>%
  ungroup() %>%
  filter(total_deaths > 0) %>%
  mutate(country = "Brazil",
         region = "Brazil",
         region_code = 0,
         start_date = as.Date("2019-12-29") + (week-1)*7,
         end_date = start_date + 6,
         year = 2020,
         population = 210147125)

# Group covid deaths by week
brazil_weekly_covid_deaths <- global_covid_source_latest %>%
  mutate(week_date = date + 3, # Use Brazil's weekly windows
         year = year(week_date),
         week = week(week_date),
         covid_deaths = Brazil) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year, week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T))

# Join weekly total deaths and weekly covid deaths together
brazil_weekly_deaths <- brazil_weekly_total_deaths %>%
  left_join(brazil_weekly_covid_deaths) %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths)

# Export as CSV
write.csv(brazil_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/brazil_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 5: import and clean Britain's data ---------------------------------------

# Import Britain's data
britain_regions <- fread("source-data/britain/britain_regions.csv")
britain_total_source_latest <- read_excel("source-data/britain/britain_total_source_latest.xlsx") 
britain_covid_source_latest <- read_excel("source-data/britain/britain_covid_source_latest.xlsx") 

# Group total deaths by week and region
britain_regions_weekly_total_deaths <- gather(britain_total_source_latest,"region","total_deaths",
                                              -c(country,start_date,end_date,week)) %>%
  left_join(britain_regions %>% 
              dplyr::select(region,region_code,population)) %>%
  mutate(year = year(start_date),
         week = week(start_date))

# Group covid deaths by week and region
britain_regions_weekly_covid_deaths <- gather(britain_covid_source_latest,"region","covid_deaths",
                                              -c(country,start_date,end_date,week)) %>%
  left_join(britain_regions %>% 
              dplyr::select(region,region_code,population)) %>%
  mutate(year = year(start_date),
         week = week(start_date))

# Join weekly total deaths and weekly covid deaths together
britain_regions_weekly_deaths <- britain_regions_weekly_total_deaths %>%
  left_join(britain_regions_weekly_covid_deaths) %>%
  mutate(expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(britain_regions_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/britain_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 6: import and clean Chile's data ---------------------------------------

# Import Chile's data
chile_regions <- read_excel("source-data/chile/chile_regions.xlsx")
chile_total_source_2015_12_31 <- read_excel("source-data/chile/chile_total_source_2015_12_31.xlsx")
chile_total_source_2016_12_31 <- read_excel("source-data/chile/chile_total_source_2016_12_31.xlsx")
chile_total_source_2017_12_31 <- read_excel("source-data/chile/chile_total_source_2017_12_31.xlsx")
chile_total_source_2018_12_31 <- read_excel("source-data/chile/chile_total_source_2018_12_31.xlsx")
chile_total_source_2019_12_31 <- read_excel("source-data/chile/chile_total_source_2019_12_31.xlsx")
chile_total_source_latest <- read_excel("source-data/chile/chile_total_source_latest.xlsx")
chile_covid_source_latest <- read_csv("https://raw.githubusercontent.com/DataScienceResearchPeru/covid-19_latinoamerica/master/latam_covid_19_data/time_series/time_series_deaths.csv") %>%
  filter(Country == "Chile")

# Group total deaths by week and region
chile_regions_weekly_total_deaths <- bind_rows(chile_total_source_2015_12_31,chile_total_source_2016_12_31,
                                       chile_total_source_2017_12_31,chile_total_source_2018_12_31,
                                       chile_total_source_2019_12_31,chile_total_source_latest) %>%
  mutate(year = AÑO, month = MES, day = DIA, region_long_name = REGION) %>%
  left_join(chile_regions) %>%
  group_by(region_number,year,month,day) %>%
  summarise(total_deaths = sum(TOTAL,na.rm=T)) %>%
  ungroup() %>%
  mutate(date = as.Date(ISOdate(year, month, day)),
         week = week(date)) %>%
  group_by(region_number,year,week) %>%
  summarise(total_deaths = sum(total_deaths,na.rm=T))
  
# Group covid deaths by week and region
chile_regions_weekly_covid_deaths <- chile_covid_source_latest %>%
  mutate(region_code = `ISO 3166-2 Code`) %>%
  dplyr::select(-c(`ISO 3166-2 Code`,Country,Subdivision,`Last Update`)) %>%
  pivot_longer(cols = c(-region_code), names_to = "date", values_to = "cumulative_deaths") %>%
  # avoid double-counting if value is NA
  fill(cumulative_deaths, .direction = "down") %>% 
  mutate(date = ymd(date)) %>%
  bind_rows(expand.grid(region_code = unique(chile_regions$region_code),
                        date = seq(as.Date("2015-01-01"), as.Date("2020-02-24"), by="days"), # Bind on rows with 0 covid deaths before February 24th
                        cumulative_deaths = 0)) %>%
  filter(region_code != "CL") %>%
  arrange(region_code,date) %>%
  group_by(region_code) %>%
  mutate(previous_day_deaths = lag(cumulative_deaths, n = 1, default = NA), # Create a lag, to calculate daily deaths from cumulative ones 
         covid_deaths = case_when(!is.na(cumulative_deaths) & !is.na(previous_day_deaths) ~ cumulative_deaths - previous_day_deaths,
                                  !is.na(cumulative_deaths) ~ cumulative_deaths)) %>%
  mutate(week = week(date), year = year(date)) %>%
  left_join(chile_regions) %>%
  group_by(region_number, year, week) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm=T))
  
# Join weekly total deaths and weekly covid deaths together in each region
chile_regions_weekly_deaths <- chile_regions_weekly_covid_deaths %>%
  left_join(chile_regions_weekly_total_deaths) %>%
  left_join(chile_regions %>% filter(region_code != "CL-NB")) %>%
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = covid_deaths,
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
         end_date <= as.Date("2020-10-21")) # Remove weeks with incomplete data

# Aggregate at the national level
chile_national_weekly_deaths <- chile_regions_weekly_deaths %>%
  ungroup() %>%
  mutate(region = "Chile",
         region_code = "CL") %>%
  group_by(country,region,region_code,start_date,end_date,year,week) %>%
  summarise(population = sum(population,na.rm=T),
            total_deaths = sum(total_deaths,na.rm=T),
            covid_deaths = sum(covid_deaths,na.rm=T),
            expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 

# Export as CSV
write.csv(bind_rows(chile_regions_weekly_deaths,chile_national_weekly_deaths) %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/chile_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 7: import and clean Denmark's data ---------------------------------------

# Import Denmark's data
denmark_total_source_latest <- fread("source-data/denmark/denmark_total_source_latest.csv") 

# Group total deaths by week
denmark_weekly_total_deaths <- denmark_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,year,week,population) %>%
  summarise(total_deaths = sum(total_deaths)) 

# Group covid deaths by week
denmark_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = week(date),
         year = year(date),
         covid_deaths = Denmark) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
denmark_weekly_deaths <- denmark_weekly_total_deaths %>%
  left_join(denmark_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
         end_date <= as.Date("2020-10-13")) # Remove weeks with incomplete data

# Export as CSV
write.csv(denmark_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/denmark_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 8: import and clean Ecuador's data ---------------------------------------

# Import Ecuador's data
ecuador_total_source_latest <- read_excel("source-data/ecuador/ecuador_total_source_latest.xlsx")

# Group total deaths by month and region
ecuador_monthly_total_deaths <- ecuador_total_source_latest 

# Group national covid deaths by month
ecuador_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Ecuador,
         region_code = "EC") %>%
  dplyr::select(date,region_code,year,month,covid_deaths) %>%
  group_by(region_code,year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
ecuador_monthly_deaths <- ecuador_monthly_total_deaths %>%
  left_join(ecuador_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  arrange(region_code,year,month)

# Export as CSV
write.csv(ecuador_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/ecuador_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 9: import and clean France's data ---------------------------------------

# Import France's data
france_depts <- read_excel("source-data/france/france_depts.xlsx")
france_total_source_2015_12_31 <- fread("source-data/france/france_total_source_2015_12_31.csv") 
france_total_source_2016_12_31 <- fread("source-data/france/france_total_source_2016_12_31.csv") 
france_total_source_2017_12_31 <- fread("source-data/france/france_total_source_2017_12_31.csv") 
france_total_source_2018_12_31 <- fread("source-data/france/france_total_source_2018_12_31.csv") 
france_total_source_2019_12_31 <- fread("source-data/france/france_total_source_2019_12_31.csv") 
france_total_source_2020_01_31 <- fread("source-data/france/france_total_source_2020_01_31.csv") 
france_total_source_2020_02_29 <- fread("source-data/france/france_total_source_2020_02_29.csv")
france_total_source_2020_03_31 <- fread("source-data/france/france_total_source_2020_03_31.csv")
france_total_source_2020_04_30 <- fread("source-data/france/france_total_source_2020_04_30.csv")
france_total_source_latest <- fread("source-data/france/france_total_source_latest.csv") 
france_covid_source_latest <- fread("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7")

# Group France's departements into regions
france_regions <- france_depts %>%
  group_by(country,region,region_code) %>%
  summarise(population = sum(population,na.rm=T))

# Define function that extracts dept_code from France's historical deaths register
get_french_dept_code <- function(x) { dept_code <- substr(x,1,nchar(x)-3) }

# Using France's historical deaths register, calculate the number of daily total deaths in each region
france_regions_daily_total_deaths <- bind_rows(france_total_source_2015_12_31,france_total_source_2016_12_31,
                                               france_total_source_2017_12_31,france_total_source_2018_12_31,
                                               france_total_source_2019_12_31,france_total_source_2020_01_31,
                                               france_total_source_2020_02_29,france_total_source_2020_03_31,
                                               france_total_source_2020_04_30) %>%
  mutate(date = ymd(datedeces),
         dept_code = get_french_dept_code(lieudeces)) %>%
  left_join(france_depts %>%
              dplyr::select(dept_code,region, region_code)) %>%
  filter(date >= as.Date("2015-01-01"),
         date <= as.Date("2020-02-29"), # Calculate daily total deaths up until the end of February 2020
         !is.na(region)) %>%
  group_by(region,region_code,date) %>%
  summarise(total_deaths = n())

# Group total deaths by week and region, binding the historical register and latest data together
france_regions_weekly_total_deaths <- france_total_source_latest %>%
  mutate(date = dmy(Date_evenement)) %>%
  group_by(Zone) %>% # Create a lag, to calculate daily deaths from cumulative ones 
  mutate(zone = Zone,
         previous_day_deaths = lag(Total_deces_2020, n = 1, default = NA),
         total_deaths = case_when(!is.na(Total_deces_2020) & !is.na(previous_day_deaths) ~ Total_deces_2020 - previous_day_deaths,
                                  !is.na(Total_deces_2020) ~ Total_deces_2020)) %>%
  left_join(france_depts %>% # Join the region name and code
              dplyr::select(zone,region,region_code)) %>%
  ungroup() %>%
  dplyr::select(region,region_code,date,total_deaths) %>%
  bind_rows(france_regions_daily_total_deaths) %>% # Bind on rows from historical deaths register
  drop_na() %>%
  arrange(region_code,date) %>%
  mutate(week = week(date), # Group total deaths by week and departement
         year = year(date)) %>%
  left_join(france_regions) %>% # Join the region population, remove regions outside mainland France
  filter(population > 0) %>%
  group_by(country,region,region_code,year,week,population) %>%
  summarise(total_deaths = sum(total_deaths,na.rm=T)) %>%
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6)

# The next sections of code train a model to impute covid deaths in each departement before March 18th
# Calculate daily cumulative covid deaths in each departement after March 18th
france_depts_daily_covid_deaths <- france_covid_source_latest %>%
  filter(sexe == 0) %>%
  mutate(dept_code = case_when(str_detect(dep,"^0") ~ str_replace(dep,"^0",""), TRUE ~ as.character(dep)),
         date = ymd(jour),
         cumulative_deaths = dc) %>%
  dplyr::select(dept_code, date, cumulative_deaths) %>%
  left_join(france_depts %>%
              dplyr::select(dept_code,dept)) %>%
  arrange(dept_code,date) %>%
  drop_na()

# Generate dataframe to impute each departement's share of national covid deaths before March 18th
france_covid_model_df <- france_depts_daily_covid_deaths %>%
  left_join(france_depts_daily_covid_deaths %>% # Join on the daily sum of national covid deaths
              group_by(date) %>%
              summarise(national_deaths = sum(cumulative_deaths,na.rm=T))) %>%
  mutate(national_share = cumulative_deaths / national_deaths, # Calculate each departement's share of national covid deaths
         yday = yday(date))

# Train logistic model to impute each departement's share of national covid deaths
france_covid_model <- glm(national_share ~ yday + dept_code + dept_code:yday, # Use an interaction between each departement and day
                          data = france_covid_model_df, family="binomial")
summary(france_covid_model)

# Use model to make imputations of each departement's daily share of national covid deaths before March 18th
france_modelled_covid_shares <- expand.grid(date = seq(as.Date("2015-01-01"), Sys.Date(), by="days"), # Create empty grid
                                            dept_code = unique(france_covid_model_df$dept_code)) %>%
  left_join(france_covid_model_df %>%
              dplyr::select(dept_code, dept)) %>% # Join on departement name
  left_join(france_covid_model_df) %>%
  distinct() %>%
  mutate(date = ymd(date),
         yday = yday(date)) %>%
  mutate(imputed_national_share = predict(france_covid_model,.,type="response"))

# Group covid deaths by week and region, combining observed data with imputed values
france_regions_weekly_covid_deaths <- france_modelled_covid_shares %>%
  left_join(global_covid_source_cumulative %>% # Join the ECDC's daily cumulative covid deaths for the whole of France before March 18th
              mutate(ECDC_deaths = France) %>%
              dplyr::select(date,ECDC_deaths)) %>%
  mutate(modelled_deaths = case_when(is.na(cumulative_deaths) ~ imputed_national_share * ECDC_deaths, 
                                     TRUE ~ as.numeric(cumulative_deaths)), # Impute cumulative covid deaths in each dept from ECDC data
         week = week(date),
         year = year(date)) %>%
  group_by(dept_code) %>% # Create a lag, to calculate daily deaths from cumulative ones 
  mutate(previous_day_deaths = lag(modelled_deaths, n = 1, default = NA),
         covid_deaths = case_when(!is.na(modelled_deaths) & !is.na(previous_day_deaths) ~ modelled_deaths - previous_day_deaths,
                                  !is.na(modelled_deaths) ~ modelled_deaths,
                                  TRUE ~ 0),
         covid_deaths = case_when(covid_deaths < 0 ~ 0, TRUE ~ as.numeric(covid_deaths))) %>%
  left_join(france_depts) %>% # Join and group by region
  group_by(country,region,region_code,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm=T))

# Join weekly total deaths and weekly covid deaths together in each region
france_regions_weekly_deaths <- france_regions_weekly_total_deaths %>%
  left_join(france_regions_weekly_covid_deaths) %>%
  mutate(expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53)

# Aggregate at the national level, use ECDC covid data to include nursing homes
france_national_weekly_deaths <- france_regions_weekly_deaths %>%
  ungroup() %>%
  mutate(region = "France",
         region_code = 0) %>%
  group_by(country,region,region_code,start_date,end_date,year,week) %>%
  summarise(population = sum(population,na.rm=T),
            total_deaths = sum(total_deaths,na.rm=T),
            expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  left_join(global_covid_source_latest %>% # Join ECDC covid data
              dplyr::select(date,France) %>%
              mutate(covid_deaths = France,
                     date = ymd(date),
                     week = week(date),
                     year = year(date)) %>%
              group_by(year,week) %>%
              summarise(covid_deaths = sum(covid_deaths,na.rm=T))) %>%
  mutate(covid_deaths = replace_na(covid_deaths,0)) %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 

# Export as CSV
write.csv(bind_rows(france_regions_weekly_deaths,france_national_weekly_deaths) %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")) %>%
            filter(end_date <= as.Date("2020-09-29")), # Remove weeks with incomplete data
          "output-data/historical-deaths/france_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 10: import and clean Germany's data ---------------------------------------

# Import Germany's data
germany_total_source_latest <- fread("source-data/germany/germany_total_source_latest.csv") 

# Group total deaths by week
germany_weekly_total_deaths <- germany_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,year,week,population) %>%
  summarise(total_deaths = sum(total_deaths))

# Group covid deaths by week
germany_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = week(date),
         year = year(date),
         covid_deaths = Germany) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
germany_weekly_deaths <- germany_weekly_total_deaths %>%
  left_join(germany_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
         end_date <= as.Date("2020-09-20")) # Remove weeks with incomplete data

# Export as CSV
write.csv(germany_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/germany_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 11: import and clean Indonesia's data ---------------------------------------

# Import Indonesia's data
indonesia_total_source_latest <- fread("source-data/indonesia/indonesia_total_source_latest.csv") 

# Join weekly total deaths and weekly covid deaths together
indonesia_monthly_deaths <- indonesia_total_source_latest %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         region_code = 0,
         expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(indonesia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/indonesia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 12: import and clean Italy's data ---------------------------------------

# Import Italy's data
italy_comunes <- read_excel("source-data/italy/italy_comunes.xlsx")
italy_total_source_latest <- fread("source-data/italy/italy_total_source_2020_08_31.csv")
italy_covid_source_latest <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

# Create list of Italian comunes with reliable data
italy_comunes_reliable <- italy_total_source_latest %>%
  filter(GE <= 831, T_20 != "n.d.") %>% # Filter out any comunes missing data before June 30th
  dplyr::select(COD_PROVCOM) %>%
  distinct() %>%
  pull()

# Group Italian comunes into regions
italy_regions <- italy_comunes %>%
  filter(comune_code %in% italy_comunes_reliable) %>%
  group_by(country,region,region_code) %>%
  summarise(population = sum(population,na.rm=T))

# Group total deaths by week and region
italy_regions_weekly_total_deaths <- italy_total_source_latest %>%
  mutate(comune_code = COD_PROVCOM) %>%
  left_join(italy_comunes) %>% 
  filter(GE <= 831, T_20 != "n.d.") %>% # Remove any missing days
  group_by(region_code,GE) %>%
  summarise(total_deaths_2015 = sum(T_15,na.rm=T),
            total_deaths_2016 = sum(T_16,na.rm=T),
            total_deaths_2017 = sum(T_17,na.rm=T),
            total_deaths_2018 = sum(T_18,na.rm=T),
            total_deaths_2019 = sum(T_19,na.rm=T),
            total_deaths_2020 = sum(as.numeric(T_20),na.rm=T)) %>%
  gather("period","total_deaths",
         -c(region_code,GE)) %>%
  ungroup() %>%
  mutate(month = as.numeric(round(GE/100)), # Extract date from GE character variable
         day = as.numeric(GE-(month*100)),
         year = as.numeric(map_chr(period,substr,14,18)),
         date = as.Date(ISOdate(year, month, day)),
         week = week(date)) %>%
  group_by(region_code,year,week) %>%
  summarise(days = n(),
            total_deaths = sum(total_deaths)) %>%
  filter(days == 7) %>% # Remove any incomplete weeks
  left_join(italy_regions) %>%
  ungroup()
  
# Group covid deaths by week and region
italy_regions_weekly_covid_deaths <- italy_covid_source_latest %>%
  mutate(date = as.Date(data),
         region_code = as.numeric(codice_regione)) %>%
  group_by(date,region_code) %>% # Group Trentino and Sudtirol together
  summarise(cumulative_deaths = sum(deceduti)) %>%
  ungroup() %>%
  dplyr::select(date,region_code,cumulative_deaths) %>%
  bind_rows(expand.grid(date = seq(as.Date("2015-01-01"), as.Date("2020-02-23"), by="days"), # Bind on rows with 0 covid deaths before February 23rd
                        region_code = as.numeric(unique(italy_covid_source_latest$codice_regione)),
                        cumulative_deaths = 0)) %>%
  left_join(italy_regions) %>%
  arrange(region_code,date) %>%
  group_by(region_code) %>%
  mutate(previous_day_deaths = lag(cumulative_deaths, n = 1, default = NA), # Create a lag, to calculate daily deaths from cumulative ones 
         covid_deaths = case_when(!is.na(cumulative_deaths) & !is.na(previous_day_deaths) ~ cumulative_deaths - previous_day_deaths,
                                  !is.na(cumulative_deaths) ~ cumulative_deaths)) %>%
  ungroup() %>%
  mutate(year = year(date),
         week = week(date)) %>%
  group_by(country,region,region_code,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm=T)) %>%
  ungroup()
  
# Join weekly total deaths and weekly covid deaths together in each region
italy_regions_weekly_deaths <- italy_regions_weekly_total_deaths %>%
  left_join(italy_regions_weekly_covid_deaths %>%
              dplyr::select(region_code,year,week,covid_deaths)) %>%
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = covid_deaths,
         expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 

# Aggregate at the national level
italy_national_weekly_deaths <- italy_regions_weekly_deaths %>%
  ungroup() %>%
  mutate(region = "Italy",
         region_code = 0) %>%
  group_by(country,region,region_code,start_date,end_date,year,week) %>%
  summarise(population = sum(population,na.rm=T),
            total_deaths = sum(total_deaths,na.rm=T),
            covid_deaths = sum(covid_deaths,na.rm=T),
            expected_deaths = "TBC") %>% # To be calculated
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 

# Export as CSV
write.csv(bind_rows(italy_regions_weekly_deaths,italy_national_weekly_deaths) %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/italy_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 13: import and clean Mexico's data ---------------------------------------

# Import Mexico's data
mexico_total_source_latest <- fread("source-data/mexico/mexico_total_source_latest.csv")

# Group covid deaths by week
mexico_weekly_covid_deaths <- global_covid_source_latest %>%
  mutate(week_date = date + 3, # Use Mexico's weekly windows
         year = year(week_date),
         week = week(week_date),
         covid_deaths = Mexico) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year, week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T))

# Join weekly total deaths and weekly covid deaths together
mexico_weekly_deaths <- mexico_total_source_latest %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         week = week(end_date)) %>%
  left_join(mexico_weekly_covid_deaths) %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(mexico_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/mexico_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 14: import and clean the Netherlands' data ---------------------------------------

# Import the Netherlands' data
netherlands_total_source_latest <- fread("source-data/netherlands/netherlands_total_source_latest.csv") 

# Group total deaths by week
netherlands_weekly_total_deaths <- netherlands_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date)) 

# Group covid deaths by week
netherlands_weekly_covid_deaths <- global_covid_source_latest %>%
  mutate(week_date = date - 5, # Use the Netherlands' weekly windows
         year = year(week_date),
         week = week(week_date),
         covid_deaths = Netherlands) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year, week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T))

# Join weekly total deaths and weekly covid deaths together
netherlands_weekly_deaths <- netherlands_weekly_total_deaths %>%
  left_join(netherlands_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(netherlands_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/netherlands_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 15: import and clean Norway's data ---------------------------------------

# Import Norway's data
norway_total_source_latest <- fread("source-data/norway/norway_total_source_latest.csv") 

# Group total deaths by week
norway_weekly_total_deaths <- norway_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date))

# Group covid deaths by week
norway_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week = week(date),
         year = year(date),
         covid_deaths = Norway) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
norway_weekly_deaths <- norway_weekly_total_deaths %>%
  left_join(norway_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53)

# Export as CSV
write.csv(norway_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/norway_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 16: import and clean Peru's data ---------------------------------------

# Import Peru's data
peru_total_source_latest <- fread("source-data/peru/peru_total_source_latest.csv")

# Group total deaths by month and region
peru_monthly_total_deaths <- peru_total_source_latest %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date))

# Group national covid deaths by month
peru_monthly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(month = month(date),
         year = year(date),
         covid_deaths = Peru,
         region_code = "PE") %>%
  dplyr::select(date,region_code,year,month,covid_deaths) %>%
  group_by(region_code,year,month) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join monthly total deaths and monthly covid deaths together
peru_monthly_deaths <- peru_monthly_total_deaths %>%
  left_join(peru_monthly_covid_deaths) %>% 
  mutate(covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,month,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  arrange(region_code,year,month)

# Export as CSV
write.csv(peru_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/peru_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 17: import and clean Portugal's data ---------------------------------------

# Import Portugal's data
portugal_total_source_latest <- fread("source-data/portugal/portugal_total_source_latest.csv")

# Group total deaths by week
portugal_weekly_total_deaths <- portugal_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,year,week,population) %>%
  summarise(total_deaths = sum(total_deaths,na.rm=T))

# Group covid deaths by week
portugal_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(year = year(date),
         week = week(date),
         covid_deaths = Portugal) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
portugal_weekly_deaths <- portugal_weekly_total_deaths %>%
  left_join(portugal_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
         end_date <= as.Date("2020-10-22")) # Remove weeks with incomplete data

# Export as CSV
write.csv(portugal_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/portugal_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 18: import and clean Russia's data ---------------------------------------

# Import Russia's data
russia_total_source_latest <- fread("source-data/russia/russia_total_source_latest.csv") 

# Join monthly total deaths and monthly covid deaths together
russia_monthly_deaths <- russia_total_source_latest %>%
  left_join(global_covid_source_latest %>%
              filter(date >= as.Date("2020-01-01")) %>%
              mutate(month = month(date),
                     year = year(date),
                     covid_deaths = Russia) %>%
              group_by(year,month) %>%
              summarise(covid_deaths = sum(covid_deaths,na.rm=T)) %>% 
              dplyr::select(month,year,covid_deaths)) %>%
  ungroup() %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC")

# Export as CSV
write.csv(russia_monthly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/russia_monthly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 19: import and clean South Africa's data ---------------------------------------

# Import South Africa's data
south_africa_total_source_latest <- fread("source-data/south-africa/south_africa_total_source_latest.csv")

# Group total deaths by week
south_africa_weekly_total_deaths <- south_africa_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date))

# Group covid deaths by week
south_africa_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(year = year(date),
         week = week(date),
         covid_deaths = `South Africa`) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
south_africa_weekly_deaths <- south_africa_weekly_total_deaths %>%
  left_join(south_africa_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6) %>%
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na()

# Export as CSV
write.csv(south_africa_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/south_africa_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 20: import and clean Spain's data ---------------------------------------

# Import Spain's data
spain_regions <- read_excel("source-data/spain/spain_regions.xlsx")
spain_total_source_latest <- fread("source-data/spain/spain_total_source_latest.csv")
download.file("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Fallecidos_COVID19.xlsx", # Download covid data
               destfile="source-data/spain/spain_covid_source_latest.xlsx")
spain_covid_source_latest <- read_excel("source-data/spain/spain_covid_source_latest.xlsx")

# Group total and expected deaths by week and region
spain_regions_weekly_total_deaths <- spain_total_source_latest %>%
  filter(cod_sexo == "all", cod_gedad == "all") %>%
  mutate(date = dmy(fecha_defuncion), 
         year = year(date),
         week = week(date), 
         region_code = replace_na(cod_ine_ambito,0), 
         total_deaths = defunciones_observadas,
         expected_deaths = defunciones_esperadas) %>%
  dplyr::select(date,year,week,region_code,total_deaths,expected_deaths) %>%
  left_join(spain_regions) %>%
  group_by(country,region,region_code,year,week,population) %>%
  summarise(days = n(),
            total_deaths = sum(total_deaths, na.rm=T),
            expected_deaths = sum(expected_deaths, na.rm=T)) %>%
  ungroup() %>%
  filter(days == 7) %>% # Remove incomplete weeks
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6)
         
# Group covid deaths by week and region
spain_regions_weekly_covid_deaths <- spain_covid_source_latest %>%
  drop_na() %>%
  rename_all(~c("date","Andalusia","Aragón","Asturias","Balearics","Canary Islands","Cantabria","Castile-La Mancha",
                "Castile & León","Catalonia","Ceuta","Valencia","Extremadura","Galicia","Madrid","Melilla","Murcia",
                "Navarre","Basque Country","La Rioja","Spain")) %>%
  mutate(date = as.Date(as.numeric(date), origin="1899-12-30")) %>%
  pivot_longer(-date,names_to="region",values_to="covid_deaths") %>%
  bind_rows(expand.grid(date = seq(as.Date("2018-01-01"), as.Date("2020-02-12"), by="days"),
                        region = unique(spain_regions$region),
                        covid_deaths = 0)) %>%
  left_join(spain_regions) %>%
  arrange(region_code,date) %>%
  mutate(year = year(date),
         week = week(date)) %>%
  group_by(country,region,region_code,year,week,population) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  ungroup() 

# Join weekly total deaths and weekly covid deaths together
spain_regions_weekly_deaths <- spain_regions_weekly_total_deaths %>%
  left_join(spain_regions_weekly_covid_deaths) %>%
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  filter(end_date <= as.Date("2020-10-22")) # Remove weeks with incomplete data

# Export as CSV
write.csv(spain_regions_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/spain_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 21: import and clean Sweden's data ---------------------------------------

# Import Sweden's data
sweden_total_source_latest <- fread("source-data/sweden/sweden_total_source_latest.csv") 
download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data", # Download covid data
              destfile="source-data/sweden/sweden_covid_source_latest.xlsx")
sweden_covid_source_latest <- read_excel("source-data/sweden/sweden_covid_source_latest.xlsx",sheet=2) %>%
  filter(Datum_avliden != "Uppgift saknas") %>% # Remove deaths with an unknown date of occurrence
  mutate(date = as.Date(as.numeric(Datum_avliden), origin="1899-12-30"),
         covid_deaths = Antal_avlidna)

# Group total deaths by week
sweden_weekly_total_deaths <- sweden_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,week,year,population) %>%
  summarise(total_deaths = sum(total_deaths,na.rm=T)) %>%
  arrange(year,week)

# Group covid deaths by week
sweden_weekly_covid_deaths <- sweden_covid_source_latest %>%
  dplyr::select(date,covid_deaths) %>%
  bind_rows(expand.grid(date = seq(as.Date("2015-01-01"), as.Date("2020-03-10"), by="days"),
                        covid_deaths = 0)) %>%
  mutate(week = week(date),
         year = year(date)) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na()

# Join weekly total deaths and weekly covid deaths together
sweden_weekly_deaths <- sweden_weekly_total_deaths %>%
  left_join(sweden_weekly_covid_deaths) %>% 
  mutate(region_code = 0,
         start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() %>%
  filter(week != 53,
         end_date <= as.Date("2020-10-08")) # Remove weeks with incomplete data

# Export as CSV
write.csv(sweden_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/sweden_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 22: import and clean Switzerland's data ---------------------------------------

# Import Switzerland's data
switzerland_total_source_2019_12_31 <- fread("source-data/switzerland/switzerland_total_source_2019_12_31.csv")
switzerland_total_source_latest <- fread("source-data/switzerland/switzerland_total_source_latest.csv") 

# Group total deaths by week
switzerland_weekly_total_deaths <- switzerland_total_source_2019_12_31 %>% # Group deaths from 2010-19
  mutate(end_date = dmy(Ending),
         start_date = end_date - 6,
         year = year(start_date),
         week = week(start_date),
         total_deaths = NumberOfDeaths) %>%
  dplyr::select(start_date,end_date,year,week,total_deaths) %>%
  group_by(start_date,end_date,year,week) %>%
  summarise(total_deaths = sum(total_deaths)) %>%
  bind_rows(switzerland_total_source_latest %>% # Bind on deaths from 2020
              mutate(end_date = dmy(Ending),
                     start_date = end_date - 6,
                     year = year(start_date),
                     week = week(start_date),
                     total_deaths = NoDec_EP) %>%
              dplyr::select(start_date,end_date,year,week,total_deaths) %>%
              group_by(start_date,end_date,year,week) %>%
              summarise(total_deaths = sum(total_deaths))) %>%
  drop_na()

# Group covid deaths by week
switzerland_weekly_covid_deaths <- global_covid_source_latest %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(week_date = date - 5, # Use Switzerland's weekly windows
         week = week(week_date),
         year = year(week_date),
         covid_deaths = Switzerland) %>%
  dplyr::select(date,year,week,covid_deaths) %>%
  group_by(year,week) %>%
  summarise(covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  drop_na() %>%
  ungroup()

# Join weekly total deaths and weekly covid deaths together
switzerland_weekly_deaths <- switzerland_weekly_total_deaths %>%
  left_join(switzerland_weekly_covid_deaths) %>% 
  mutate(country = "Switzerland",
         region = "Switzerland",
         region_code = 0,
         population = 8570146,
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) 

# Export as CSV
write.csv(switzerland_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/switzerland_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 23: import and clean Turkey's data ---------------------------------------

# Import Turkey's data
turkey_total_source_latest <- fread("source-data/turkey/turkey_total_source_latest.csv") 

# Group total deaths by week
turkey_weekly_total_deaths <- turkey_total_source_latest %>% 
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date),
         year = year(start_date),
         week = week(start_date)) %>%
  group_by(country,region,year,week,population) %>%
  summarise(days = sum(total_deaths > 0),
            total_deaths = sum(total_deaths, na.rm=T)) %>%
  ungroup() %>%
  filter(days == 7, total_deaths > 0) %>% # Remove incomplete weeks
  mutate(start_date = as.Date(ISOdate(year-1, 12, 31)) + (week*7) - 6,
         end_date = start_date + 6)

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
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) 

# Export as CSV
write.csv(turkey_weekly_deaths %>%
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/turkey_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)

# Step 24: import and clean the United States' data ---------------------------------------

# Import the United States' data
united_states_states <- fread("source-data/united-states/united_states_states.csv")
united_states_week_windows <- fread("source-data/united-states/united_states_week_windows.csv")
united_states_covid_source_latest <- fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
united_states_total_source_latest <- fread("source-data/united-states/united_states_total_source_latest.csv")
united_states_total_source_2020 <- fread("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv")
new_york_city_covid_source_latest <- fread("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/deaths/probable-confirmed-dod.csv") 

# Group total deaths by week and state
united_states_weekly_total_deaths <- united_states_total_source_latest %>%
  mutate(year = case_when(WEEK >= 40 ~ as.numeric(substr(SEASON,1,4)), TRUE ~ as.numeric(substr(SEASON,1,4)) + 1), # Sort US flu seasons into years
         week = WEEK, state = `SUB AREA`,
         total_deaths = case_when(str_detect(`TOTAL DEATHS`,",") ~ as.numeric(str_replace(`TOTAL DEATHS`,",","")), # Convert strings to integers
                                  TRUE ~ as.numeric(`TOTAL DEATHS`))) %>%
  dplyr::select(state,year,week,total_deaths) %>%
  filter(year >= 2015) %>%
  left_join(united_states_states %>%
              mutate(state = state_name) %>%
              dplyr::select(-state_name)) %>%
  arrange(state,year,week) %>%
  left_join(united_states_total_source_2020 %>% # Join on corrected 2020 data from the CDC
              mutate(state = State,
                     year = year(`Week Ending Date`),
                     week = week(`Week Ending Date`),
                     total_corrected_deaths = `Observed Number`) %>%
              filter(year == 2020,
                     Type == "Predicted (weighted)",
                     Outcome == "All causes") %>%
              dplyr::select(state,year,week,total_corrected_deaths)) %>%
  mutate(total_deaths = case_when(is.na(total_corrected_deaths) ~ total_deaths, TRUE ~ as.numeric(total_corrected_deaths))) %>%
  dplyr::select(-total_corrected_deaths)

# Group together total deaths for New York state and New York City
new_york_state_weekly_total_deaths <- united_states_weekly_total_deaths %>%
  filter(state == "New York") %>%
  left_join(united_states_weekly_total_deaths %>%
              filter(state == "New York City") %>%
              mutate(NYC_deaths = total_deaths) %>%
              dplyr::select(year,week,NYC_deaths)) %>%
  mutate(total_deaths = total_deaths + NYC_deaths) %>%
  dplyr::select(-NYC_deaths)

# Group NYC covid deaths by week
new_york_city_weekly_covid_deaths <- new_york_city_covid_source_latest %>%
  mutate(date = mdy(DATE_OF_DEATH),
         PROBABLE_COUNT = case_when(is.na(PROBABLE_COUNT) ~ 0, TRUE ~ as.numeric(PROBABLE_COUNT)),
         covid_deaths = CONFIRMED_COUNT + PROBABLE_COUNT) %>% # Add confirmed and probable deaths together
  dplyr::select(date, covid_deaths) %>%
  bind_rows(expand.grid(date = seq(as.Date("2020-01-01"), as.Date("2020-03-10"), by="days"), # Bind on rows before March 11th
                        covid_deaths = 0)) %>%
  mutate(state_code = "NYC",
         week_date = date + 3, # Use the US's weekly windows
         week = week(week_date),
         year = year(week_date)) %>%
  group_by(state_code,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm=T)) %>%
  ungroup()

# Group US states' covid deaths by week
united_states_weekly_covid_deaths <- united_states_covid_source_latest %>%
  gather("date","cumulative_deaths",-c(countyFIPS,`County Name`,State,stateFIPS)) %>%
  mutate(state = State,
         cumulative_deaths = as.numeric(cumulative_deaths)) %>%
  group_by(state,date) %>%
  summarise(cumulative_deaths = sum(cumulative_deaths,na.rm=T)) %>%
  ungroup() %>%
  mutate(date = mdy(date)) %>%
  bind_rows(expand.grid(state = unique(united_states_covid_source_latest$State), # Bind on rows before January 21st
                        date = seq(as.Date("2015-01-01"), as.Date("2020-01-21"), by="days"),
                        cumulative_deaths = 0)) %>%
  arrange(state,date) %>%
  group_by(state) %>% # Create a lag, to calculate daily deaths from cumulative ones 
  mutate(state_code = state,
         week_date = date + 3, # Use the US's weekly windows
         week = week(week_date),
         year = year(week_date),
         previous_day_deaths = lag(cumulative_deaths, n = 1, default = NA), 
         covid_deaths = case_when(!is.na(cumulative_deaths) & !is.na(previous_day_deaths) ~ cumulative_deaths - previous_day_deaths,
                                  !is.na(cumulative_deaths) ~ cumulative_deaths)) %>%
  group_by(state_code,year,week) %>%
  summarise(covid_deaths = sum(covid_deaths)) %>%
  ungroup() %>%
  bind_rows(new_york_city_weekly_covid_deaths) 

# Join weekly total deaths and weekly covid deaths together
united_states_weekly_deaths <- united_states_weekly_total_deaths %>%
  filter(state != "New York") %>%
  bind_rows(new_york_state_weekly_total_deaths) %>%
  left_join(united_states_weekly_covid_deaths) %>%
  left_join(united_states_week_windows) %>%
  mutate(country = "United States",
         region = state,
         region_code = state_code,
         start_date = dmy(start_date),
         end_date = dmy(end_date),
         covid_deaths = replace_na(covid_deaths,0),
         expected_deaths = "TBC") %>% # To be calculated
  ungroup() %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 

# Aggregate at the national level
united_states_national_weekly_deaths <- united_states_weekly_deaths %>%
  filter(region != "New York City") %>%
  group_by(country,start_date,end_date,year,week) %>%
  summarise(population = sum(population, na.rm=T),
            total_deaths = sum(total_deaths, na.rm=T),
            covid_deaths = sum(covid_deaths, na.rm=T)) %>%
  ungroup() %>%
  mutate(region = "United States",
         region_code = "USA",
         expected_deaths = "TBC") %>%
  dplyr::select(country,region,region_code,start_date,end_date,year,week,
                population,total_deaths,covid_deaths,expected_deaths) %>%
  drop_na() 
  
# Export as CSV
write.csv(united_states_weekly_deaths %>%
            bind_rows(united_states_national_weekly_deaths) %>%
            filter(end_date <= as.Date("2020-10-03")) %>% # Remove weeks with incomplete data
            mutate(start_date = format(start_date, "%Y-%m-%d"),
                   end_date = format(end_date, "%Y-%m-%d")),
          "output-data/historical-deaths/united_states_weekly_deaths.csv",
          fileEncoding = "UTF-8",
          row.names=FALSE)
