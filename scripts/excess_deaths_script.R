# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
options(scipen=999)

# Import data
historical_deaths <- dir('output-data/historical-deaths')

# Updated list of countries with data:
countries <- c("Albania", "Andorra", "Antigua and Barbuda", 
               "Argentina", "Armenia", "Aruba", 
               "Azerbaijan", "Belarus", "Belize", 
               "Bermuda", "Bolivia", "Bosnia and Herzegovina", 
               "Brazil", "Costa Rica", "Cuba", 
               "Egypt", "El Salvador", "Faroe Islands", 
               "French Polynesia", "Georgia", "Gibraltar", 
               "Greenland", "Hong Kong", "Indonesia", 
               "Ireland", "Jamaica", "Japan", 
               "Kazakhstan", "Kosovo", "Kyrgyzstan", 
               "Lebanon", "Liechtenstein", "Macao", 
               "Malaysia", "Mauritius", "Moldova", 
               "Monaco", "Mongolia", "Nicaragua", 
               "North Macedonia", "Oman", "Panama", 
               "Paraguay", "Philippines", "Qatar", 
               "Russia", "San Marino", "Serbia", 
               "Seychelles", "Singapore", "Taiwan", 
               "Thailand", "Ukraine", "Uruguay", 
               "Uzbekistan", "Australia", "Austria", "Belgium", "Britain", 
               "Bulgaria", 
               "Canada", "Chile", "Colombia", "Croatia", "Cyprus", 
               "Czech Republic", "Denmark", "Ecuador", "Estonia", "Finland", 
               "France", "Germany", "Greece", "Guatemala", "Hungary", 
               "Iceland", "Iran", "Israel", "Italy", "Latvia", 
               "Lithuania", "Luxembourg", "Malta", "Mexico", "Montenegro", 
               "Netherlands", "New Zealand", "Norway", "Peru", "Poland", 
               "Portugal", "Romania", "Slovakia", "Slovenia", "South Africa", 
               "South Korea", "Spain", "Sweden", "Switzerland", "Tunisia", 
               "Turkey", "United States", "Tajikistan", "Dominican Republic", "Kuwait", "Brunei", "Barbados", "Jordan", "Maldives", "Palestine",
               "Algeria", "Saint Kitts and Nevis", 'Suriname', 'Cape Verde', 'Saint Vincent and the Grenadines', 'Bahamas', 'Isle of Man', 'United Arab Emirates')

# Step 2: define function that calculates excess deaths ---------------------------------------

# Define function that calculates excess deaths
get_excess_deaths <- function(df,expected_deaths_model,frequency="weekly",calculate=TRUE,train_model=TRUE){
  
  # Define formulas and count number of regions
  weekly_formula <- as.formula(total_deaths_per_day ~ year + week)
  weekly_regional_formula <- as.formula(total_deaths_per_day ~ year + week + region + region:year + region:week)
  monthly_formula <- as.formula(total_deaths_per_day ~ year + month)
  monthly_regional_formula <- as.formula(total_deaths_per_day ~ year + month + region + region:year + region:month)
  quarterly_formula <- as.formula(total_deaths_per_day ~ year + quarter)
  quarterly_regional_formula <- as.formula(total_deaths_per_day ~ year + quarter + region + region:year + region:quarter)
  df_regions <- length(unique(df$region))
  
  # Convert weeks and months into fixed effects
  if(frequency == "weekly") {
    df <- df %>% mutate(week = as.character(week))
  } else if (frequency == "monthly") {
    df <- df %>% mutate(month = as.character(month))
  } else if (frequency == "quarterly") {
    df <- df %>% mutate(quarter = as.character(quarter))
  }
  
  # Identify the correct formula for the dataframe
  if(frequency == "weekly" & df_regions == 1) {
    expected_deaths_formula <- weekly_formula
  } else if (frequency == "weekly" & df_regions > 1) {
    expected_deaths_formula <- weekly_regional_formula
  } else if (frequency == "monthly" & df_regions == 1) {
    expected_deaths_formula <- monthly_formula
  } else if (frequency == "monthly" & df_regions > 1)  {
    expected_deaths_formula <- monthly_regional_formula
  } else if (frequency == "quarterly" & df_regions == 1) {
    expected_deaths_formula <- quarterly_formula
  } else if (frequency == "quarterly" & df_regions > 1)  {
    expected_deaths_formula <- quarterly_regional_formula
  }

  # Calculate expected deaths
  if(calculate == FALSE) {
    
    # Use pre-existing official model
    expected_deaths <- df %>% filter(year >= 2020)
    
  } else if(train_model == FALSE) {
    
    # Use previously trained Economist model
    expected_deaths <- df %>% filter(year >= 2020) %>%
      mutate(expected_deaths = predict(expected_deaths_model,.) * days)
    
  } else if(frequency == "weekly") {
    
    # Create dataframe of week 53, using week 52 and 53 in previous years
    week_53_df <- df %>%
      filter(week %in% c("52","53")) %>% mutate(week = "53", week_53 = 1)
    
    # Train an Economist weekly model
    train_df <- df %>% 
      filter(week != "53") %>%
      bind_rows(week_53_df) %>%
      filter(end_date < as.Date("2020-03-01")) %>%
      mutate(total_deaths_per_day = total_deaths / days)
    expected_deaths_model <- lm(expected_deaths_formula,train_df)
    expected_deaths <- df %>% filter(year >= 2020) %>%
      mutate(expected_deaths = predict(expected_deaths_model,newdata=.) * days)
    
  } else if(frequency %in% c("monthly","quarterly")) {
    
    # Train an Economist monthly or quarterly model
    train_df <- df %>% 
      filter(end_date < as.Date("2020-03-01")) %>%
      mutate(total_deaths_per_day = total_deaths / days)
    expected_deaths_model <- lm(expected_deaths_formula,train_df)
    expected_deaths <- df %>% filter(year >= 2020) %>%
      mutate(expected_deaths = predict(expected_deaths_model,newdata=.) * days)
    
  }
  
  # Set expected deaths to be non-negative (on implementation, this had no impact, but makes code more robust to the off chance that some country had extremely strong time effects and extremely strong downward trend in deaths over time):
  expected_deaths$expected_deaths[expected_deaths$expected_deaths < 0] <- 0
  
  # Calculate excess deaths
  excess_deaths <- expected_deaths %>%
    mutate(excess_deaths = total_deaths - expected_deaths,
           non_covid_deaths = total_deaths - covid_deaths,
           region_code = as.character(region_code)) %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = ((expected_deaths + excess_deaths) / expected_deaths) - 1)
  
  # Calculate weekly rates for monthly and quarterly data
  if(frequency %in% c("monthly","quarterly")) {
    
    excess_deaths <- excess_deaths %>%
      mutate(total_deaths_per_7_days = total_deaths / days * 7,
             covid_deaths_per_7_days = covid_deaths / days * 7,
             expected_deaths_per_7_days = expected_deaths / days * 7,
             excess_deaths_per_7_days = excess_deaths / days * 7,
             non_covid_deaths_per_7_days = non_covid_deaths / days * 7,
             covid_deaths_per_100k_per_7_days = covid_deaths_per_100k / days * 7,
             excess_deaths_per_100k_per_7_days = excess_deaths_per_100k / days * 7)
    
  }
  
  list(expected_deaths_model,excess_deaths)
  
}

# Step 3: calculate excess deaths for each country ---------------------------------------
frequencies_used <- c() # Records which frequencies we use in the data

# Cycle through countries
for(i in historical_deaths){
  
  # Load data
  dat <- fread(paste0('output-data/historical-deaths/', i))
  
  # Get data frequency
  dat_frequency <- paste0(colnames(dat)[8], "ly")
  frequencies_used <- c(frequencies_used, dat_frequency)

  # Get country
  country <- dat$country[1]
  
  # Check that country in list of countries with data not from new country - if so, insert break:
 
 if(!country %in% countries){
   stop(paste0(country, " is a new country, please inspect manually to ensure consistency."))
 }
  
  # If loading regional data, then do not use national estimate as well
  regional <- FALSE # Set default
  if(any(dat$country != dat$region) > 0){
    regional <- TRUE
    dat <- dat[dat$country != dat$region, ]
  }

  # Get excess deaths, training a new model (except for South Africa, where expected deaths are provided explicitly):
  res <- get_excess_deaths(dat,
                           expected_deaths_model = NULL,
                           frequency = dat_frequency)
  
  # Move spaces to "_" make lower case for file names:
  country <- tolower(unlist(gsub(" ", "_", country)))
  
  # Save the model 
  saveRDS(res[[1]], paste0("output-data/expected-deaths-models/", 
                           country, ifelse(regional, '_by_region', ''), "_expected_deaths_model.RDS"))
  
  # Save the results
  write.csv(res[[2]], 
            paste0("output-data/excess-deaths/", country, ifelse(regional, '_by_region', ''), "_excess_deaths.csv"),
            fileEncoding = "UTF-8", row.names=FALSE)
}

# We finally combine the US estimates into one file for convenience and consistency with past output
# Load files:
US_national <- read_csv("output-data/excess-deaths/united_states_excess_deaths.csv")
US_regional <- read_csv("output-data/excess-deaths/united_states_by_region_excess_deaths.csv")
US <- rbind(US_national, US_regional)

# Write common export:
write.csv(US,
          "output-data/excess-deaths/united_states_excess_deaths.csv",
          fileEncoding = "UTF-8", row.names=FALSE)

# Remove export now part of main file:
unlink("output-data/excess-deaths/united_states_by_region_excess_deaths.csv")

# Step 4: combine weekly, monthly and quarterly deaths together, and calculate deaths per 100,000 people and percentage change ---------------------------------------

# Combine weekly deaths and calculate per 100,000 people and percentage change
data <- lapply(setdiff(dir('output-data/excess-deaths/'), 
                       c("all_monthly_excess_deaths.csv",
                         "all_quarterly_excess_deaths.csv",
                         "all_weekly_excess_deaths.csv")), 
               FUN = function(i){
                 temp <- read_csv(paste0('output-data/excess-deaths/', i))
                 temp$region_code <- as.character(temp$region_code)
                 temp})

# Save weekly data
if("weekly" %in% frequencies_used){
all_weekly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
  colnames(data[[i]])[8] == "week"
}))])  %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1) 

# Deduplication
if(max(table(with(all_weekly_excess_deaths, paste0(country, "_", region, "_", year, "_", week)))) != 1){stop("Duplications in weekly data, please inspect")}
}

# Save monthly data
if("monthly" %in% frequencies_used){
all_monthly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
  colnames(data[[i]])[8] == "month"
}))])  %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

# Deduplication
if(max(table(with(all_monthly_excess_deaths, paste0(country, "_", region, "_", year, "_", month)))) != 1){stop("Duplications in monthly data, please inspect")}
}

# Save quarterly data
if("quarterly" %in% frequencies_used){
all_quarterly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
  colnames(data[[i]])[8] == "quarter"
}))])  %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

# Deduplication
if(max(table(with(all_quarterly_excess_deaths, paste0(country, "_", region, "_", year, "_", quarter)))) != 1){stop("Duplications in quarterly data, please inspect")}
}


# Check that values do not differ enormously from previous ones:
compare_weekly <- read.csv("output-data/excess-deaths/all_weekly_excess_deaths.csv")
compare_monthly <- read.csv("output-data/excess-deaths/all_monthly_excess_deaths.csv")
compare_quarterly <- read.csv("output-data/excess-deaths/all_quarterly_excess_deaths.csv")

# Define function to generate comparison with existing data:
gen_comparison <- function(data1 = all_weekly_excess_deaths,
                           data2 = compare_weekly,
                           frequency = "week"){
  data1 <- data.frame(data1)
  data2 <- data.frame(data2)
  
compare <-        merge(data1[, c("country", "region",
                                  "year", frequency,
                                  "excess_deaths_per_100k",
                                  "excess_deaths")],
                        data2[, c("country", "region",
                                  "year", frequency,
                                  "excess_deaths_per_100k",
                                  "excess_deaths")], 
                        by = c("country", "region", "year", frequency))

compare$diff <- abs(compare$excess_deaths.x - compare$excess_deaths.y)
compare$diff_per_100k <- abs(compare$excess_deaths_per_100k.x - compare$excess_deaths_per_100k.y)

return(compare_weekly)
}

# Prepare for comparison (this makes robust to any category eventually having no observations):
if(!exists('all_weekly_excess_deaths')){
  all_weekly_excess_deaths <- read_csv("output-data/excess-deaths/all_weekly_excess_deaths.csv")
}
if(!exists('all_monthly_excess_deaths')){
  all_monthly_excess_deaths <- read_csv("output-data/excess-deaths/all_monthly_excess_deaths.csv")
}
if(!exists('all_quarterly_excess_deaths')){
  all_quarterly_excess_deaths <- read_csv("output-data/excess-deaths/all_quarterly_excess_deaths.csv")
}

week_comparison <- gen_comparison(data1 = all_weekly_excess_deaths,
                    data2 = compare_weekly,
                    frequency = "week")
month_comparison <- gen_comparison(data1 = all_monthly_excess_deaths,
                                  data2 = compare_monthly,
                                  frequency = "month")

if(max(week_comparison$diff) > 1000 |
   max(month_comparison$diff) > 4000 |
   max(week_comparison$diff_per_100k) > 5 |
   max(month_comparison$diff_per_100k) > 20){
  stop("Differences with former data is very large, please inspect manually")
} else {

# Export weekly deaths
  if(exists("all_weekly_excess_deaths") & "weekly" %in% frequencies_used){
    write.csv(all_weekly_excess_deaths,
          file = "output-data/excess-deaths/all_weekly_excess_deaths.csv", 
          fileEncoding = "UTF-8", row.names=FALSE)
  }

# Export monthly deaths
  if(exists("all_monthly_excess_deaths") & "monthly" %in% frequencies_used){
    write.csv(all_monthly_excess_deaths,
          file = "output-data/excess-deaths/all_monthly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)
  }

# Export quarterly deaths
if(exists("all_quarterly_excess_deaths") & "quarterly" %in% frequencies_used){
  write.csv(all_quarterly_excess_deaths,
          file = "output-data/excess-deaths/all_quarterly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)
  }
}

# Check to ensure no country has fewer observations than in the immediately prior data update:
obs_matrix <- data.frame(table(c(all_weekly_excess_deaths$country[
  !is.na(all_weekly_excess_deaths$excess_deaths_per_100k) & 
    all_weekly_excess_deaths$country == all_weekly_excess_deaths$region], all_monthly_excess_deaths$country[
      !is.na(all_monthly_excess_deaths$excess_deaths_per_100k) &
        all_monthly_excess_deaths$country == all_monthly_excess_deaths$region], all_quarterly_excess_deaths$country[!is.na(all_quarterly_excess_deaths$excess_deaths_per_100k) & all_quarterly_excess_deaths$country == all_quarterly_excess_deaths$region])))

last_update_matrix <- read_csv("output-data/observations_per_country.csv")
for(i in 1:nrow(last_update_matrix)){
  if(last_update_matrix$Freq[i] > 1.1*obs_matrix$Freq[obs_matrix$Var1 == last_update_matrix$Var1[i]]){
    stop(paste0('Fewer observations than in latest update for ', last_update_matrix$Var1[i], " please inspect manually"))
  }
}
write_csv(obs_matrix, "output-data/observations_per_country.csv")


# Step 5: repeat process, using non-iso weeks ---------------------------------------
cat('\n\n Repeating process for legacy export (i.e. of non-iso week data) \n\n')

# Import data
historical_deaths <- dir('output-data/alternative-exports-by-non-iso-week/historical-deaths')

# Cycle through countries
for(i in historical_deaths){
  
  # Load data
  dat <- fread(paste0('output-data/alternative-exports-by-non-iso-week/historical-deaths/', i))
  
  # Get data frequency
  dat_frequency <- paste0(colnames(dat)[8], "ly")
  
  # Get country
  country <- dat$country[1]

  # Check that country in list of countries with data not from new country - if so, insert break:
 
  if(!country %in% countries){
    stop(paste0(country, " is a new country, please inspect manually to ensure consistency."))
  }
  
  # Get excess deaths, training a new model (except for South Africa, where expected deaths are provided explicitly):
  res <- get_excess_deaths(dat,
                           expected_deaths_model = NULL,
                           frequency = dat_frequency)
  
  # Move spaces to "_" make lower case for file names:
  country <- tolower(unlist(gsub(" ", "_", country)))
  
  # Save the model 
  saveRDS(res[[1]], paste0("output-data/alternative-exports-by-non-iso-week/expected-deaths-models/", 
                           country, "_expected_deaths_model.RDS"))
  
  # Save the results
  write.csv(res[[2]], 
            paste0("output-data/alternative-exports-by-non-iso-week/excess-deaths/", country, "_excess_deaths.csv"),
            fileEncoding = "UTF-8",row.names=FALSE)
}

# Combine weekly deaths and calculate per 100,000 people and percentage change
data <- lapply(setdiff(dir('output-data/alternative-exports-by-non-iso-week/excess-deaths/'), 
                       c("all_monthly_excess_deaths.csv",
                         "all_quarterly_excess_deaths.csv",
                         "all_weekly_excess_deaths.csv")), 
               FUN = function(i){
                 temp <- read_csv(paste0('output-data/alternative-exports-by-non-iso-week/excess-deaths/', i))
                 temp$region_code <- as.character(temp$region_code)
                 temp})

# Save weekly data
if("weekly" %in% frequencies_used){
  all_weekly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
    colnames(data[[i]])[8] == "week"
  }))])  %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = (total_deaths / expected_deaths) - 1) 
  
  # Deduplication
  if(max(table(with(all_weekly_excess_deaths, paste0(country, "_", region, "_", year, "_", week)))) != 1){stop("Duplications in weekly data, please inspect")}
}

# Save monthly data
if("monthly" %in% frequencies_used){
  all_monthly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
    colnames(data[[i]])[8] == "month"
  }))])  %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)
  
  # Deduplication
  if(max(table(with(all_monthly_excess_deaths, paste0(country, "_", region, "_", year, "_", month)))) != 1){stop("Duplications in monthly data, please inspect")}
}

# Save quarterly data
if("quarterly" %in% frequencies_used){
  all_quarterly_excess_deaths <- rbindlist(data[unlist(lapply(1:length(data), FUN = function(i){
    colnames(data[[i]])[8] == "quarter"
  }))])  %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)
  
  # Deduplication
  if(max(table(with(all_quarterly_excess_deaths, paste0(country, "_", region, "_", year, "_", quarter)))) != 1){stop("Duplications in quarterly data, please inspect")}
}
 
# Export weekly deaths
if(exists("all_weekly_excess_deaths") & "weekly" %in% frequencies_used){
  write.csv(all_weekly_excess_deaths,
            file = "output-data/excess-deaths/all_weekly_excess_deaths.csv", 
            fileEncoding = "UTF-8", row.names=FALSE)
}

# Export monthly deaths
if(exists("all_monthly_excess_deaths") & "monthly" %in% frequencies_used){
  write.csv(all_monthly_excess_deaths,
            file = "output-data/excess-deaths/all_monthly_excess_deaths.csv",
            fileEncoding = "UTF-8", row.names=FALSE)
}

# Export quarterly deaths
if(exists("all_quarterly_excess_deaths") & "quarterly" %in% frequencies_used){
  write.csv(all_quarterly_excess_deaths,
            file = "output-data/excess-deaths/all_quarterly_excess_deaths.csv",
            fileEncoding = "UTF-8", row.names=FALSE)
}

