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
brazil_weekly_deaths <- fread("output-data/historical-deaths/brazil_weekly_deaths.csv")
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

# Import models
austria_expected_deaths_model <- readRDS("output-data/expected-deaths-models/austria_expected_deaths_model.RDS")
belgium_expected_deaths_model <- readRDS("output-data/expected-deaths-models/belgium_expected_deaths_model.RDS")
britain_expected_deaths_model <- readRDS("output-data/expected-deaths-models/britain_expected_deaths_model.RDS")
chile_expected_deaths_model <- readRDS("output-data/expected-deaths-models/chile_expected_deaths_model.RDS")
denmark_expected_deaths_model <- readRDS("output-data/expected-deaths-models/denmark_expected_deaths_model.RDS")
ecuador_expected_deaths_model <- readRDS("output-data/expected-deaths-models/ecuador_expected_deaths_model.RDS")
france_expected_deaths_model <- readRDS("output-data/expected-deaths-models/france_expected_deaths_model.RDS")
germany_expected_deaths_model <- readRDS("output-data/expected-deaths-models/germany_expected_deaths_model.RDS")
indonesia_expected_deaths_model <- readRDS("output-data/expected-deaths-models/indonesia_expected_deaths_model.RDS")
italy_expected_deaths_model <- readRDS("output-data/expected-deaths-models/italy_expected_deaths_model.RDS")
netherlands_expected_deaths_model <- readRDS("output-data/expected-deaths-models/netherlands_expected_deaths_model.RDS")
norway_expected_deaths_model <- readRDS("output-data/expected-deaths-models/norway_expected_deaths_model.RDS")
peru_expected_deaths_model <- readRDS("output-data/expected-deaths-models/peru_expected_deaths_model.RDS")
portugal_expected_deaths_model <- readRDS("output-data/expected-deaths-models/portugal_expected_deaths_model.RDS")
russia_expected_deaths_model <- readRDS("output-data/expected-deaths-models/russia_expected_deaths_model.RDS")
sweden_expected_deaths_model <- readRDS("output-data/expected-deaths-models/sweden_expected_deaths_model.RDS")
switzerland_expected_deaths_model <- readRDS("output-data/expected-deaths-models/switzerland_expected_deaths_model.RDS")
turkey_expected_deaths_model <- readRDS("output-data/expected-deaths-models/turkey_expected_deaths_model.RDS")
united_states_expected_deaths_model <- readRDS("output-data/expected-deaths-models/united_states_expected_deaths_model.RDS")

# Step 2: define function that calculates excess deaths ---------------------------------------

# Define function that calculates excess deaths
get_excess_deaths <- function(df,expected_deaths_model,frequency="weekly",calculate=TRUE,train_model=TRUE){
  
  # Define formulas and count number of regions
  weekly_formula <- as.formula(total_deaths ~ year + week)
  weekly_regional_formula <- as.formula(total_deaths ~ year + week + region + region:year + region:week)
  monthly_formula <- as.formula(total_deaths ~ year + month)
  monthly_regional_formula <- as.formula(total_deaths ~ year + month + region + region:year + region:month)
  df_regions <- length(unique(df$region))
  
  # Convert weeks and months into fixed effects
  if(frequency == "weekly") {
    df <- df %>% mutate(week = as.factor(week))
  } else {
    df <- df %>% mutate(month = as.factor(month))
  }
  
  # Identify the correct formula for the dataframe
  if(frequency == "weekly" & df_regions == 1) {
    expected_deaths_formula <- weekly_formula
  } else if (frequency == "weekly" & df_regions > 1) {
    expected_deaths_formula <- weekly_regional_formula
  } else if (df_regions == 1) {
    expected_deaths_formula <- monthly_formula
  } else {
    expected_deaths_formula <- monthly_regional_formula
  }

  # Calculate expected deaths
  if(calculate == FALSE) {
    
    # Use pre-existing official model
    expected_deaths <- df %>% filter(year == 2020)
    
  } else if(train_model == FALSE) {
    
    # Use previously trained Economist model
    expected_deaths <- df %>% filter(year == 2020) %>%
      mutate(expected_deaths = predict(expected_deaths_model,.))
    
  } else {
    
    # Train an Economist model
    train_df <- df %>% filter(end_date < as.Date("2020-03-01"))
    expected_deaths_model <- lm(expected_deaths_formula,train_df)
    expected_deaths <- df %>% filter(year == 2020) %>%
      mutate(expected_deaths = predict(expected_deaths_model,.))
    
  }
  
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
  
  list(expected_deaths_model,excess_deaths)
  
}

# Step 3: calculate excess deaths for each country ---------------------------------------

# Export Austria
austria_results <- get_excess_deaths(austria_weekly_deaths,austria_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(austria_results[[1]],"output-data/expected-deaths-models/austria_expected_deaths_model.RDS")
write.csv(austria_results[[2]],"output-data/excess-deaths/austria_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Belgium
belgium_results <- get_excess_deaths(belgium_weekly_deaths,belgium_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(belgium_results[[1]],"output-data/expected-deaths-models/belgium_expected_deaths_model.RDS")
write.csv(belgium_results[[2]],"output-data/excess-deaths/belgium_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Brazil
brazil_results <- get_excess_deaths(brazil_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(brazil_results[[2]],"output-data/excess-deaths/brazil_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Britain
britain_results <- get_excess_deaths(britain_weekly_deaths,britain_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(britain_results[[1]],"output-data/expected-deaths-models/britain_expected_deaths_model.RDS")
write.csv(britain_results[[2]],"output-data/excess-deaths/britain_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Chile
chile_results <- get_excess_deaths(chile_weekly_deaths,chile_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
saveRDS(chile_results[[1]],"output-data/expected-deaths-models/chile_expected_deaths_model.RDS")
write.csv(chile_results[[2]],"output-data/excess-deaths/chile_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Denmark
denmark_results <- get_excess_deaths(denmark_weekly_deaths,denmark_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(denmark_results[[1]],"output-data/expected-deaths-models/denmark_expected_deaths_model.RDS")
write.csv(denmark_results[[2]],"output-data/excess-deaths/denmark_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Ecuador
ecuador_results <- get_excess_deaths(ecuador_monthly_deaths,ecuador_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(ecuador_results[[1]],"output-data/expected-deaths-models/ecuador_expected_deaths_model.RDS")
write.csv(ecuador_results[[2]],"output-data/excess-deaths/ecuador_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export France
france_results <- get_excess_deaths(france_weekly_deaths,france_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(france_results[[1]],"output-data/expected-deaths-models/france_expected_deaths_model.RDS")
write.csv(france_results[[2]],"output-data/excess-deaths/france_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Germany
germany_results <- get_excess_deaths(germany_weekly_deaths,germany_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(germany_results[[1]],"output-data/expected-deaths-models/germany_expected_deaths_model.RDS")
write.csv(germany_results[[2]],"output-data/excess-deaths/germany_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Indonesia
indonesia_results <- get_excess_deaths(indonesia_monthly_deaths,indonesia_expected_deaths_model,"monthly",calculate=TRUE,train_model=FALSE)
#saveRDS(indonesia_results[[1]],"output-data/expected-deaths-models/indonesia_expected_deaths_model.RDS")
write.csv(indonesia_results[[2]],"output-data/excess-deaths/indonesia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Italy
italy_results <- get_excess_deaths(italy_weekly_deaths,italy_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(italy_results[[1]],"output-data/expected-deaths-models/italy_expected_deaths_model.RDS")
write.csv(italy_results[[2]],"output-data/excess-deaths/italy_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Mexico
mexico_results <- get_excess_deaths(mexico_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(mexico_results[[2]],"output-data/excess-deaths/mexico_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the Netherlands
netherlands_results <- get_excess_deaths(netherlands_weekly_deaths,netherlands_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(netherlands_results[[1]],"output-data/expected-deaths-models/netherlands_expected_deaths_model.RDS")
write.csv(netherlands_results[[2]],"output-data/excess-deaths/netherlands_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Norway
norway_results <- get_excess_deaths(norway_weekly_deaths,norway_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(norway_results[[1]],"output-data/expected-deaths-models/norway_expected_deaths_model.RDS")
write.csv(norway_results[[2]],"output-data/excess-deaths/norway_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Peru
peru_results <- get_excess_deaths(peru_monthly_deaths,peru_expected_deaths_model,"monthly",calculate=TRUE,train_model=FALSE)
#saveRDS(peru_results[[1]],"output-data/expected-deaths-models/peru_expected_deaths_model.RDS")
write.csv(peru_results[[2]],"output-data/excess-deaths/peru_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Portugal
portugal_results <- get_excess_deaths(portugal_weekly_deaths,portugal_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(portugal_results[[1]],"output-data/expected-deaths-models/portugal_expected_deaths_model.RDS")
write.csv(portugal_results[[2]],"output-data/excess-deaths/portugal_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Russia
russia_results <- get_excess_deaths(russia_monthly_deaths,russia_expected_deaths_model,"monthly",calculate=TRUE,train_model=FALSE)
#saveRDS(russia_results[[1]],"output-data/expected-deaths-models/russia_expected_deaths_model.RDS")
write.csv(russia_results[[2]],"output-data/excess-deaths/russia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export South Africa
south_africa_results <- get_excess_deaths(south_africa_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(south_africa_results[[2]],"output-data/excess-deaths/south_africa_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Spain
spain_results <- get_excess_deaths(spain_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(spain_results[[2]],"output-data/excess-deaths/spain_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Sweden
sweden_results <- get_excess_deaths(sweden_weekly_deaths,sweden_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(sweden_results[[1]],"output-data/expected-deaths-models/sweden_expected_deaths_model.RDS")
write.csv(sweden_results[[2]],"output-data/excess-deaths/sweden_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Switzerland
switzerland_results <- get_excess_deaths(switzerland_weekly_deaths,switzerland_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(switzerland_results[[1]],"output-data/expected-deaths-models/switzerland_expected_deaths_model.RDS")
write.csv(switzerland_results[[2]],"output-data/excess-deaths/switzerland_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Turkey
turkey_results <- get_excess_deaths(turkey_weekly_deaths,turkey_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(turkey_results[[1]],"output-data/expected-deaths-models/turkey_expected_deaths_model.RDS")
write.csv(turkey_results[[2]],"output-data/excess-deaths/turkey_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the United States
united_states_results <- get_excess_deaths(united_states_weekly_deaths,united_states_expected_deaths_model,"weekly",calculate=TRUE,train_model=FALSE)
#saveRDS(united_states_results[[1]],"output-data/expected-deaths-models/united_states_expected_deaths_model.RDS")
write.csv(united_states_results[[2]],"output-data/excess-deaths/united_states_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Step 4: combine weekly and monthly deaths together, and calculate deaths per 100,000 people and percentage change ---------------------------------------

# Combine weekly deaths and calculate per 100,000 people and percentage change
all_weekly_excess_deaths <- bind_rows(austria_results[[2]],
                                      belgium_results[[2]],
                                      brazil_results[[2]],
                                      britain_results[[2]],
                                      chile_results[[2]],
                                      denmark_results[[2]],
                                      france_results[[2]],
                                      germany_results[[2]],
                                      italy_results[[2]],
                                      mexico_results[[2]],
                                      netherlands_results[[2]],
                                      norway_results[[2]],
                                      portugal_results[[2]],
                                      south_africa_results[[2]],
                                      spain_results[[2]],
                                      sweden_results[[2]],
                                      switzerland_results[[2]],
                                      turkey_results[[2]],
                                      united_states_results[[2]]) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)

# Export weekly deaths
write.csv(all_weekly_excess_deaths,"output-data/excess-deaths/all_weekly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Combine monthly deaths and calculate per 100,000 people and percentage change
all_monthly_excess_deaths <- bind_rows(ecuador_results[[2]],
                                       indonesia_results[[2]],
                                       peru_results[[2]],
                                       russia_results[[2]]) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)

# Export monthly deaths
write.csv(all_monthly_excess_deaths,"output-data/excess-deaths/all_monthly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)
