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
               "Botswana", "Brazil", "Costa Rica", "Cuba", 
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
               "Algeria", "Saint Kitts and Nevis", 'Suriname', 'Cape Verde', 'Saint Vincent and the Grenadines', 'Bahamas', 'Isle of Man', 'United Arab Emirates', 'Bhutan', 'Namibia')

# Series with fewer than three full pre-pandemic years give an unreliable
# trend. Their trend is set from annual deaths in another source instead
# (level and seasonality still come from the series itself).
annual_deaths_for_trends <- read_csv("source-data/annual_deaths_for_trends.csv")

get_trend_growth <- function(dat){
  unit <- colnames(dat)[8]
  full_years <- dat %>%
    filter(year <= 2019) %>%
    group_by(year) %>%
    summarise(periods = n_distinct(.data[[unit]])) %>%
    filter(periods >= c(week = 52, month = 12, quarter = 4)[[unit]]) %>%
    nrow()
  annual <- annual_deaths_for_trends %>% filter(country == dat$country[1])
  if(nrow(annual) == 0){
    if(full_years < 3){
      stop(paste0(dat$country[1], " has fewer than three full pre-pandemic years; add its annual deaths to source-data/annual_deaths_for_trends.csv"))
    }
    return(NULL)
  }
  # Linear trend, as in the baseline model, relative to the mean
  coef(lm(deaths ~ year, annual))[["year"]] / mean(annual$deaths)
}

# Step 2: define function that calculates excess deaths ---------------------------------------

# Define function that calculates excess deaths
get_excess_deaths <- function(df,expected_deaths_model,frequency="weekly",calculate=TRUE,train_model=TRUE,trend_growth=NULL){
  
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

  # Fix the year trend at a given annual growth rate, applied to the
  # pre-pandemic level in deaths per day
  if(!is.null(trend_growth)){
    if(df_regions > 1) stop("A fixed trend is only set up for national series")
    pre_pandemic <- df$year <= 2019
    trend_slope <- trend_growth * mean(df$total_deaths[pre_pandemic] / df$days[pre_pandemic])
    expected_deaths_formula <- update(expected_deaths_formula, . ~ . - year + offset(trend_slope * year))
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
                           frequency = dat_frequency,
                           trend_growth = get_trend_growth(dat))
  
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

return(compare)
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

# Step 5: repeat process, using non-iso weeks ---------------------------------------
cat('\n\n Repeating process for legacy export (i.e. of non-iso week data) \n\n')

# The aggregate exports (all_*_excess_deaths.csv) are built from this non-ISO
# folder. source-data/published_series.csv lists the national series they hold
# and the frequency of each; other series are left out of the aggregates.
published_series <- read_csv("source-data/published_series.csv")
stopifnot(!anyDuplicated(published_series$country),
          all(published_series$frequency %in% c("weekly", "monthly", "quarterly")))

# Import data
historical_deaths <- dir('output-data/alternative-exports-by-non-iso-week/historical-deaths')
listed_inputs <- c()

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

  # Several input files write to the same country file, so the last one would
  # win. For listed countries, use only the national file at the listed frequency.
  listed_frequency <- published_series$frequency[published_series$country == country]
  if(length(listed_frequency) == 1){
    if(dat_frequency != listed_frequency | any(dat$country != dat$region)){
      cat("Skipping", i, "- not the listed series for", country, "\n")
      next
    }
    listed_inputs <- c(listed_inputs, country)
  }

  # Get excess deaths, training a new model (except for South Africa, where expected deaths are provided explicitly):
  res <- get_excess_deaths(dat,
                           expected_deaths_model = NULL,
                           frequency = dat_frequency,
                           trend_growth = get_trend_growth(dat))
  
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

if(anyDuplicated(listed_inputs)){
  stop(paste0("Several input files used for ", paste(unique(listed_inputs[duplicated(listed_inputs)]), collapse = ", ")))
}

# Every listed series must come from an input processed in this run, so an
# output file left over from an earlier run cannot stand in for it
not_processed <- setdiff(published_series$country, listed_inputs)
if(length(not_processed) > 0){
  stop(paste0("No input file at the listed frequency for: ", paste(not_processed, collapse = ", ")))
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

# Keep only the listed series, national rows only
left_out <- setdiff(unique(unlist(lapply(data, function(d) d$country))), published_series$country)
if(length(left_out) > 0){
  cat("Not in published_series.csv, left out of the aggregates:", paste(left_out, collapse = ", "), "\n")
}
data <- lapply(data, function(d){
  listed <- published_series$country[published_series$frequency == paste0(colnames(d)[8], "ly")]
  d[d$country == d$region & d$country %in% listed, ]
})
data <- data[sapply(data, nrow) > 0]

aggregated <- unique(bind_rows(lapply(data, function(d) tibble(country = d$country[1], frequency = paste0(colnames(d)[8], "ly")))))
missing <- anti_join(published_series, aggregated, by = c("country", "frequency"))
if(nrow(missing) > 0){
  stop(paste0("Listed series missing from the aggregates: ", paste(missing$country, missing$frequency, collapse = ", ")))
}

# Combine each frequency
aggregates <- list()
for(unit in c("week", "month", "quarter")){
  frames <- data[sapply(data, function(d) colnames(d)[8] == unit)]
  if(length(frames) == 0) next
  aggregate_excess_deaths <- rbindlist(frames) %>%
    mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
           excess_deaths_per_100k = excess_deaths / population * 100000,
           excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

  # Deduplication
  if(max(table(paste0(aggregate_excess_deaths$country, "_", aggregate_excess_deaths$region, "_",
                      aggregate_excess_deaths$year, "_", aggregate_excess_deaths[[unit]]))) != 1){
    stop(paste0("Duplications in ", unit, "ly data, please inspect"))
  }
  aggregates[[unit]] <- aggregate_excess_deaths
}

# Check that no published series has fewer observations than in the previous update:
obs_matrix <- data.frame(table(unlist(lapply(aggregates, function(d) d$country[!is.na(d$excess_deaths_per_100k)]))))
last_update_matrix <- read_csv("output-data/observations_per_country.csv") %>%
  filter(Var1 %in% published_series$country, Var1 != "Peru") %>%
  left_join(obs_matrix %>% mutate(Var1 = as.character(Var1)), by = "Var1", suffix = c("_last", "_now")) %>%
  mutate(Freq_now = replace_na(Freq_now, 0))
fewer <- last_update_matrix$Var1[last_update_matrix$Freq_last > 1.1 * last_update_matrix$Freq_now]
if(length(fewer) > 0){
  stop(paste0("Fewer observations than in latest update for ", paste(fewer, collapse = ", "), ", please inspect manually"))
}
write_csv(obs_matrix, "output-data/observations_per_country.csv")

# Export. A frequency with no listed series gets a header-only file, which
# downstream scripts expect.
for(unit in c("week", "month", "quarter")){
  path <- paste0("output-data/excess-deaths/all_", unit, "ly_excess_deaths.csv")
  if(is.null(aggregates[[unit]])){
    write.csv(read.csv(path)[0, ], file = path, fileEncoding = "UTF-8", row.names = FALSE)
    next
  }
  write.csv(aggregates[[unit]], file = path, fileEncoding = "UTF-8", row.names = FALSE)
}

