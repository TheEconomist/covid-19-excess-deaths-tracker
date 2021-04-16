# Step 1: import libraries and data ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
options(scipen=999)

# Import data
albania_weekly_deaths <- fread("output-data/historical-deaths/albania_weekly_deaths.csv")
armenia_weekly_deaths <- fread("output-data/historical-deaths/armenia_weekly_deaths.csv")
australia_weekly_deaths <- fread("output-data/historical-deaths/australia_weekly_deaths.csv")
austria_weekly_deaths <- fread("output-data/historical-deaths/austria_weekly_deaths.csv")
azerbaijan_monthly_deaths <- fread("output-data/historical-deaths/azerbaijan_monthly_deaths.csv")
belarus_monthly_deaths <- fread("output-data/historical-deaths/belarus_monthly_deaths.csv")
belgium_weekly_deaths <- fread("output-data/historical-deaths/belgium_weekly_deaths.csv")
bolivia_monthly_deaths <- fread("output-data/historical-deaths/bolivia_monthly_deaths.csv")
bosnia_and_herzegovina_monthly_deaths <- fread("output-data/historical-deaths/bosnia_and_herzegovina_monthly_deaths.csv")
brazil_monthly_deaths <- fread("output-data/historical-deaths/brazil_monthly_deaths.csv")
britain_weekly_deaths <- fread("output-data/historical-deaths/britain_weekly_deaths.csv")
bulgaria_weekly_deaths <- fread("output-data/historical-deaths/bulgaria_weekly_deaths.csv")
canada_weekly_deaths <- fread("output-data/historical-deaths/canada_weekly_deaths.csv")
chile_weekly_deaths <- fread("output-data/historical-deaths/chile_weekly_deaths.csv")
colombia_weekly_deaths <- fread("output-data/historical-deaths/colombia_weekly_deaths.csv")
costa_rica_monthly_deaths <- fread("output-data/historical-deaths/costa_rica_monthly_deaths.csv")
croatia_weekly_deaths <- fread("output-data/historical-deaths/croatia_weekly_deaths.csv")
cyprus_weekly_deaths <- fread("output-data/historical-deaths/cyprus_weekly_deaths.csv")
czech_republic_weekly_deaths <- fread("output-data/historical-deaths/czech_republic_weekly_deaths.csv")
denmark_weekly_deaths <- fread("output-data/historical-deaths/denmark_weekly_deaths.csv")
ecuador_monthly_deaths <- fread("output-data/historical-deaths/ecuador_monthly_deaths.csv")
egypt_monthly_deaths <- fread("output-data/historical-deaths/egypt_monthly_deaths.csv")
el_salvador_monthly_deaths <- fread("output-data/historical-deaths/el_salvador_monthly_deaths.csv")
estonia_weekly_deaths <- fread("output-data/historical-deaths/estonia_weekly_deaths.csv")
finland_weekly_deaths <- fread("output-data/historical-deaths/finland_weekly_deaths.csv")
france_weekly_deaths <- fread("output-data/historical-deaths/france_weekly_deaths.csv")
georgia_weekly_deaths <- fread("output-data/historical-deaths/georgia_weekly_deaths.csv")
germany_weekly_deaths <- fread("output-data/historical-deaths/germany_weekly_deaths.csv")
greece_weekly_deaths <- fread("output-data/historical-deaths/greece_weekly_deaths.csv")
hungary_weekly_deaths <- fread("output-data/historical-deaths/hungary_weekly_deaths.csv")
iceland_weekly_deaths <- fread("output-data/historical-deaths/iceland_weekly_deaths.csv")
indonesia_monthly_deaths <- fread("output-data/historical-deaths/indonesia_monthly_deaths.csv")
iran_quarterly_deaths <- fread("output-data/historical-deaths/iran_quarterly_deaths.csv")
israel_weekly_deaths <- fread("output-data/historical-deaths/israel_weekly_deaths.csv")
italy_weekly_deaths <- fread("output-data/historical-deaths/italy_weekly_deaths.csv")
jamaica_monthly_deaths <- fread("output-data/historical-deaths/jamaica_monthly_deaths.csv")
japan_monthly_deaths <- fread("output-data/historical-deaths/japan_monthly_deaths.csv")
kazakhstan_monthly_deaths <- fread("output-data/historical-deaths/kazakhstan_monthly_deaths.csv")
kosovo_monthly_deaths <- fread("output-data/historical-deaths/kosovo_monthly_deaths.csv")
kyrgyzstan_monthly_deaths <- fread("output-data/historical-deaths/kyrgyzstan_monthly_deaths.csv")
latvia_weekly_deaths <- fread("output-data/historical-deaths/latvia_weekly_deaths.csv")
lithuania_weekly_deaths <- fread("output-data/historical-deaths/lithuania_weekly_deaths.csv")
luxembourg_weekly_deaths <- fread("output-data/historical-deaths/luxembourg_weekly_deaths.csv")
malaysia_quarterly_deaths <- fread("output-data/historical-deaths/malaysia_quarterly_deaths.csv")
malta_weekly_deaths <- fread("output-data/historical-deaths/malta_weekly_deaths.csv")
mauritius_monthly_deaths <- fread("output-data/historical-deaths/mauritius_monthly_deaths.csv")
mexico_weekly_deaths <- fread("output-data/historical-deaths/mexico_weekly_deaths.csv")
moldova_monthly_deaths <- fread("output-data/historical-deaths/moldova_monthly_deaths.csv")
mongolia_monthly_deaths <- fread("output-data/historical-deaths/mongolia_monthly_deaths.csv")
montenegro_weekly_deaths <- fread("output-data/historical-deaths/montenegro_weekly_deaths.csv")
netherlands_weekly_deaths <- fread("output-data/historical-deaths/netherlands_weekly_deaths.csv")
new_zealand_weekly_deaths <- fread("output-data/historical-deaths/new_zealand_weekly_deaths.csv")
nicaragua_monthly_deaths <- fread("output-data/historical-deaths/nicaragua_monthly_deaths.csv")
north_macedonia_monthly_deaths <- fread("output-data/historical-deaths/north_macedonia_monthly_deaths.csv")
norway_weekly_deaths <- fread("output-data/historical-deaths/norway_weekly_deaths.csv")
oman_monthly_deaths <- fread("output-data/historical-deaths/oman_monthly_deaths.csv")
panama_monthly_deaths <- fread("output-data/historical-deaths/panama_monthly_deaths.csv")
paraguay_monthly_deaths <- fread("output-data/historical-deaths/paraguay_monthly_deaths.csv")
peru_monthly_deaths <- fread("output-data/historical-deaths/peru_monthly_deaths.csv")
philippines_monthly_deaths <- fread("output-data/historical-deaths/philippines_monthly_deaths.csv")
poland_weekly_deaths <- fread("output-data/historical-deaths/poland_weekly_deaths.csv")
portugal_weekly_deaths <- fread("output-data/historical-deaths/portugal_weekly_deaths.csv")
qatar_monthly_deaths <- fread("output-data/historical-deaths/qatar_monthly_deaths.csv")
romania_weekly_deaths <- fread("output-data/historical-deaths/romania_weekly_deaths.csv")
russia_monthly_deaths <- fread("output-data/historical-deaths/russia_monthly_deaths.csv")
serbia_monthly_deaths <- fread("output-data/historical-deaths/serbia_monthly_deaths.csv")
singapore_monthly_deaths <- fread("output-data/historical-deaths/singapore_monthly_deaths.csv")
slovakia_weekly_deaths <- fread("output-data/historical-deaths/slovakia_weekly_deaths.csv")
slovenia_weekly_deaths <- fread("output-data/historical-deaths/slovenia_weekly_deaths.csv")
south_africa_weekly_deaths <- fread("output-data/historical-deaths/south_africa_weekly_deaths.csv")
south_korea_weekly_deaths <- fread("output-data/historical-deaths/south_korea_weekly_deaths.csv")
spain_weekly_deaths <- fread("output-data/historical-deaths/spain_weekly_deaths.csv")
sweden_weekly_deaths <- fread("output-data/historical-deaths/sweden_weekly_deaths.csv")
switzerland_weekly_deaths <- fread("output-data/historical-deaths/switzerland_weekly_deaths.csv")
tajikistan_quarterly_deaths <- fread("output-data/historical-deaths/tajikistan_quarterly_deaths.csv")
taiwan_weekly_deaths <- fread("output-data/historical-deaths/taiwan_weekly_deaths.csv")
thailand_monthly_deaths <- fread("output-data/historical-deaths/thailand_monthly_deaths.csv")
tunisia_weekly_deaths <- fread("output-data/historical-deaths/tunisia_weekly_deaths.csv")
turkey_weekly_deaths <- fread("output-data/historical-deaths/turkey_weekly_deaths.csv")
ukraine_monthly_deaths <- fread("output-data/historical-deaths/ukraine_monthly_deaths.csv")
united_states_weekly_deaths <- fread("output-data/historical-deaths/united_states_weekly_deaths.csv")
uzbekistan_monthly_deaths <- fread("output-data/historical-deaths/uzbekistan_monthly_deaths.csv")

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

# Export Albania
albania_results <- get_excess_deaths(albania_weekly_deaths,austria_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(albania_results[[1]],"output-data/expected-deaths-models/albania_expected_deaths_model.RDS")
write.csv(albania_results[[2]],"output-data/excess-deaths/albania_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Armenia
armenia_results <- get_excess_deaths(armenia_weekly_deaths,austria_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(armenia_results[[1]],"output-data/expected-deaths-models/armenia_expected_deaths_model.RDS")
write.csv(armenia_results[[2]],"output-data/excess-deaths/armenia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Australia
australia_results <- get_excess_deaths(australia_weekly_deaths,australia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(australia_results[[1]],"output-data/expected-deaths-models/australia_expected_deaths_model.RDS")
write.csv(australia_results[[2]],"output-data/excess-deaths/australia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Austria
austria_results <- get_excess_deaths(austria_weekly_deaths,austria_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(austria_results[[1]],"output-data/expected-deaths-models/austria_expected_deaths_model.RDS")
write.csv(austria_results[[2]],"output-data/excess-deaths/austria_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Azerbaijan
azerbaijan_results <- get_excess_deaths(azerbaijan_monthly_deaths,azerbaijan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(azerbaijan_results[[1]],"output-data/expected-deaths-models/azerbaijan_expected_deaths_model.RDS")
write.csv(azerbaijan_results[[2]],"output-data/excess-deaths/azerbaijan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Belarus
belarus_results <- get_excess_deaths(belarus_monthly_deaths,belarus_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(belarus_results[[1]],"output-data/expected-deaths-models/belarus_expected_deaths_model.RDS")
write.csv(belarus_results[[2]],"output-data/excess-deaths/belarus_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Belgium
belgium_results <- get_excess_deaths(belgium_weekly_deaths,belgium_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(belgium_results[[1]],"output-data/expected-deaths-models/belgium_expected_deaths_model.RDS")
write.csv(belgium_results[[2]],"output-data/excess-deaths/belgium_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Bolivia
bolivia_results <- get_excess_deaths(bolivia_monthly_deaths,bolivia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(bolivia_results[[1]],"output-data/expected-deaths-models/bolivia_expected_deaths_model.RDS")
write.csv(bolivia_results[[2]],"output-data/excess-deaths/bolivia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Bosnia and Herzegovina
bosnia_and_herzegovina_results <- get_excess_deaths(bosnia_and_herzegovina_monthly_deaths,bosnia_and_herzegovina_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(bosnia_and_herzegovina_results[[1]],"output-data/expected-deaths-models/bosnia_and_herzegovina_expected_deaths_model.RDS")
write.csv(bosnia_and_herzegovina_results[[2]],"output-data/excess-deaths/bosnia_and_herzegovina_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Brazil
brazil_results <- get_excess_deaths(brazil_monthly_deaths,brazil_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(brazil_results[[1]],"output-data/expected-deaths-models/brazil_expected_deaths_model.RDS")
write.csv(brazil_results[[2]],"output-data/excess-deaths/brazil_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Britain
britain_results <- get_excess_deaths(britain_weekly_deaths,britain_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(britain_results[[1]],"output-data/expected-deaths-models/britain_expected_deaths_model.RDS")
write.csv(britain_results[[2]],"output-data/excess-deaths/britain_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Bulgaria
bulgaria_results <- get_excess_deaths(bulgaria_weekly_deaths,bulgaria_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(bulgaria_results[[1]],"output-data/expected-deaths-models/bulgaria_expected_deaths_model.RDS")
write.csv(bulgaria_results[[2]],"output-data/excess-deaths/bulgaria_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Canada
canada_results <- get_excess_deaths(canada_weekly_deaths,canada_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(canada_results[[1]],"output-data/expected-deaths-models/canada_expected_deaths_model.RDS")
write.csv(canada_results[[2]],"output-data/excess-deaths/canada_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Chile
chile_results <- get_excess_deaths(chile_weekly_deaths,chile_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(chile_results[[1]],"output-data/expected-deaths-models/chile_expected_deaths_model.RDS")
write.csv(chile_results[[2]],"output-data/excess-deaths/chile_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Colombia
colombia_results <- get_excess_deaths(colombia_weekly_deaths,colombia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(colombia_results[[1]],"output-data/expected-deaths-models/colombia_expected_deaths_model.RDS")
write.csv(colombia_results[[2]],"output-data/excess-deaths/colombia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Costa Rica
costa_rica_results <- get_excess_deaths(costa_rica_monthly_deaths,costa_rica_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(costa_rica_results[[1]],"output-data/expected-deaths-models/costa_rica_expected_deaths_model.RDS")
write.csv(costa_rica_results[[2]],"output-data/excess-deaths/costa_rica_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Croatia
croatia_results <- get_excess_deaths(croatia_weekly_deaths,croatia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(croatia_results[[1]],"output-data/expected-deaths-models/croatia_expected_deaths_model.RDS")
write.csv(croatia_results[[2]],"output-data/excess-deaths/croatia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Cyprus
cyprus_results <- get_excess_deaths(cyprus_weekly_deaths,cyprus_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(cyprus_results[[1]],"output-data/expected-deaths-models/cyprus_expected_deaths_model.RDS")
write.csv(cyprus_results[[2]],"output-data/excess-deaths/cyprus_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the Czech Republic
czech_republic_results <- get_excess_deaths(czech_republic_weekly_deaths,czech_republic_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(czech_republic_results[[1]],"output-data/expected-deaths-models/czech_republic_expected_deaths_model.RDS")
write.csv(czech_republic_results[[2]],"output-data/excess-deaths/czech_republic_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Denmark
denmark_results <- get_excess_deaths(denmark_weekly_deaths,denmark_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(denmark_results[[1]],"output-data/expected-deaths-models/denmark_expected_deaths_model.RDS")
write.csv(denmark_results[[2]],"output-data/excess-deaths/denmark_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Ecuador
ecuador_results <- get_excess_deaths(ecuador_monthly_deaths,ecuador_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(ecuador_results[[1]],"output-data/expected-deaths-models/ecuador_expected_deaths_model.RDS")
write.csv(ecuador_results[[2]],"output-data/excess-deaths/ecuador_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Egypt
egypt_results <- get_excess_deaths(egypt_monthly_deaths,egypt_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(egypt_results[[1]],"output-data/expected-deaths-models/egypt_expected_deaths_model.RDS")
write.csv(egypt_results[[2]],"output-data/excess-deaths/egypt_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export El Salvador
el_salvador_results <- get_excess_deaths(el_salvador_monthly_deaths,el_salvador_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(el_salvador_results[[1]],"output-data/expected-deaths-models/el_salvador_expected_deaths_model.RDS")
write.csv(el_salvador_results[[2]],"output-data/excess-deaths/el_salvador_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Estonia
estonia_results <- get_excess_deaths(estonia_weekly_deaths,estonia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(estonia_results[[1]],"output-data/expected-deaths-models/estonia_expected_deaths_model.RDS")
write.csv(estonia_results[[2]],"output-data/excess-deaths/estonia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Finland
finland_results <- get_excess_deaths(finland_weekly_deaths,finland_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(finland_results[[1]],"output-data/expected-deaths-models/finland_expected_deaths_model.RDS")
write.csv(finland_results[[2]],"output-data/excess-deaths/finland_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export France
france_results <- get_excess_deaths(france_weekly_deaths,france_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(france_results[[1]],"output-data/expected-deaths-models/france_expected_deaths_model.RDS")
write.csv(france_results[[2]],"output-data/excess-deaths/france_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Georgia
georgia_results <- get_excess_deaths(georgia_weekly_deaths,georgia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(georgia_results[[1]],"output-data/expected-deaths-models/georgia_expected_deaths_model.RDS")
write.csv(georgia_results[[2]],"output-data/excess-deaths/georgia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Germany
germany_results <- get_excess_deaths(germany_weekly_deaths,germany_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(germany_results[[1]],"output-data/expected-deaths-models/germany_expected_deaths_model.RDS")
write.csv(germany_results[[2]],"output-data/excess-deaths/germany_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Greece
greece_results <- get_excess_deaths(greece_weekly_deaths,greece_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(greece_results[[1]],"output-data/expected-deaths-models/greece_expected_deaths_model.RDS")
write.csv(greece_results[[2]],"output-data/excess-deaths/greece_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Hungary
hungary_results <- get_excess_deaths(hungary_weekly_deaths,hungary_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(hungary_results[[1]],"output-data/expected-deaths-models/hungary_expected_deaths_model.RDS")
write.csv(hungary_results[[2]],"output-data/excess-deaths/hungary_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Iceland
iceland_results <- get_excess_deaths(iceland_weekly_deaths,iceland_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(iceland_results[[1]],"output-data/expected-deaths-models/iceland_expected_deaths_model.RDS")
write.csv(iceland_results[[2]],"output-data/excess-deaths/iceland_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Indonesia
indonesia_results <- get_excess_deaths(indonesia_monthly_deaths,indonesia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(indonesia_results[[1]],"output-data/expected-deaths-models/indonesia_expected_deaths_model.RDS")
write.csv(indonesia_results[[2]],"output-data/excess-deaths/indonesia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Iran
iran_results <- get_excess_deaths(iran_quarterly_deaths,iran_expected_deaths_model,"quarterly",calculate=TRUE,train_model=TRUE)
saveRDS(iran_results[[1]],"output-data/expected-deaths-models/iran_expected_deaths_model.RDS")
write.csv(iran_results[[2]],"output-data/excess-deaths/iran_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Israel
israel_results <- get_excess_deaths(israel_weekly_deaths,israel_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(israel_results[[1]],"output-data/expected-deaths-models/israel_expected_deaths_model.RDS")
write.csv(israel_results[[2]],"output-data/excess-deaths/israel_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Italy
italy_results <- get_excess_deaths(italy_weekly_deaths,italy_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(italy_results[[1]],"output-data/expected-deaths-models/italy_expected_deaths_model.RDS")
write.csv(italy_results[[2]],"output-data/excess-deaths/italy_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Jamaica
jamaica_results <- get_excess_deaths(jamaica_monthly_deaths,japan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(jamaica_results[[1]],"output-data/expected-deaths-models/jamaica_expected_deaths_model.RDS")
write.csv(jamaica_results[[2]],"output-data/excess-deaths/jamaica_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Japan
japan_results <- get_excess_deaths(japan_monthly_deaths,japan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(japan_results[[1]],"output-data/expected-deaths-models/japan_expected_deaths_model.RDS")
write.csv(japan_results[[2]],"output-data/excess-deaths/japan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Kazakhstan
kazakhstan_results <- get_excess_deaths(kazakhstan_monthly_deaths,kazakhstan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(kazakhstan_results[[1]],"output-data/expected-deaths-models/kazakhstan_expected_deaths_model.RDS")
write.csv(kazakhstan_results[[2]],"output-data/excess-deaths/kazakhstan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Kosovo
kosovo_results <- get_excess_deaths(kosovo_monthly_deaths,kosovo_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(kosovo_results[[1]],"output-data/expected-deaths-models/kosovo_expected_deaths_model.RDS")
write.csv(kosovo_results[[2]],"output-data/excess-deaths/kosovo_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Kyrgyzstan
kyrgyzstan_results <- get_excess_deaths(kyrgyzstan_monthly_deaths,kyrgyzstan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(kyrgyzstan_results[[1]],"output-data/expected-deaths-models/kyrgyzstan_expected_deaths_model.RDS")
write.csv(kyrgyzstan_results[[2]],"output-data/excess-deaths/kyrgyzstan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Latvia
latvia_results <- get_excess_deaths(latvia_weekly_deaths,latvia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(latvia_results[[1]],"output-data/expected-deaths-models/latvia_expected_deaths_model.RDS")
write.csv(latvia_results[[2]],"output-data/excess-deaths/latvia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Lithuania
lithuania_results <- get_excess_deaths(lithuania_weekly_deaths,lithuania_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(lithuania_results[[1]],"output-data/expected-deaths-models/lithuania_expected_deaths_model.RDS")
write.csv(lithuania_results[[2]],"output-data/excess-deaths/lithuania_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Luxembourg
luxembourg_results <- get_excess_deaths(luxembourg_weekly_deaths,luxembourg_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(luxembourg_results[[1]],"output-data/expected-deaths-models/luxembourg_expected_deaths_model.RDS")
write.csv(luxembourg_results[[2]],"output-data/excess-deaths/luxembourg_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Malaysia
malaysia_results <- get_excess_deaths(malaysia_quarterly_deaths,malaysia_expected_deaths_model,"quarterly",calculate=TRUE,train_model=TRUE)
saveRDS(malaysia_results[[1]],"output-data/expected-deaths-models/malaysia_expected_deaths_model.RDS")
write.csv(malaysia_results[[2]],"output-data/excess-deaths/malaysia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Malta
malta_results <- get_excess_deaths(malta_weekly_deaths,malta_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(malta_results[[1]],"output-data/expected-deaths-models/malta_expected_deaths_model.RDS")
write.csv(malta_results[[2]],"output-data/excess-deaths/malta_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Mauritius
mauritius_results <- get_excess_deaths(mauritius_monthly_deaths,mauritius_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(mauritius_results[[1]],"output-data/expected-deaths-models/mauritius_expected_deaths_model.RDS")
write.csv(mauritius_results[[2]],"output-data/excess-deaths/mauritius_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Mexico
mexico_results <- get_excess_deaths(mexico_weekly_deaths,mexico_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(mexico_results[[1]],"output-data/expected-deaths-models/mexico_expected_deaths_model.RDS")
write.csv(mexico_results[[2]],"output-data/excess-deaths/mexico_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Moldova
moldova_results <- get_excess_deaths(moldova_monthly_deaths,moldova_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(moldova_results[[1]],"output-data/expected-deaths-models/moldova_expected_deaths_model.RDS")
write.csv(moldova_results[[2]],"output-data/excess-deaths/moldova_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Mongolia
mongolia_results <- get_excess_deaths(mongolia_monthly_deaths,mongolia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(mongolia_results[[1]],"output-data/expected-deaths-models/mongolia_expected_deaths_model.RDS")
write.csv(mongolia_results[[2]],"output-data/excess-deaths/mongolia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Montenegro
montenegro_results <- get_excess_deaths(montenegro_weekly_deaths,montenegro_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(montenegro_results[[1]],"output-data/expected-deaths-models/montenegro_expected_deaths_model.RDS")
write.csv(montenegro_results[[2]],"output-data/excess-deaths/montenegro_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the Netherlands
netherlands_results <- get_excess_deaths(netherlands_weekly_deaths,netherlands_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(netherlands_results[[1]],"output-data/expected-deaths-models/netherlands_expected_deaths_model.RDS")
write.csv(netherlands_results[[2]],"output-data/excess-deaths/netherlands_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export New Zealand
new_zealand_results <- get_excess_deaths(new_zealand_weekly_deaths,new_zealand_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(new_zealand_results[[1]],"output-data/expected-deaths-models/new_zealand_expected_deaths_model.RDS")
write.csv(new_zealand_results[[2]],"output-data/excess-deaths/new_zealand_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Nicaragua
nicaragua_results <- get_excess_deaths(nicaragua_monthly_deaths,nicaragua_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(nicaragua_results[[1]],"output-data/expected-deaths-models/nicaragua_expected_deaths_model.RDS")
write.csv(nicaragua_results[[2]],"output-data/excess-deaths/nicaragua_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export North Macedonia
north_macedonia_results <- get_excess_deaths(north_macedonia_monthly_deaths,north_macedonia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(north_macedonia_results[[1]],"output-data/expected-deaths-models/north_macedonia_expected_deaths_model.RDS")
write.csv(north_macedonia_results[[2]],"output-data/excess-deaths/north_macedonia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Norway
norway_results <- get_excess_deaths(norway_weekly_deaths,norway_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(norway_results[[1]],"output-data/expected-deaths-models/norway_expected_deaths_model.RDS")
write.csv(norway_results[[2]],"output-data/excess-deaths/norway_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Oman
oman_results <- get_excess_deaths(oman_monthly_deaths,oman_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(oman_results[[1]],"output-data/expected-deaths-models/oman_expected_deaths_model.RDS")
write.csv(oman_results[[2]],"output-data/excess-deaths/oman_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Panama
panama_results <- get_excess_deaths(panama_monthly_deaths,panama_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(panama_results[[1]],"output-data/expected-deaths-models/panama_expected_deaths_model.RDS")
write.csv(panama_results[[2]],"output-data/excess-deaths/panama_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Paraguay
paraguay_results <- get_excess_deaths(paraguay_monthly_deaths,paraguay_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(paraguay_results[[1]],"output-data/expected-deaths-models/paraguay_expected_deaths_model.RDS")
write.csv(paraguay_results[[2]],"output-data/excess-deaths/paraguay_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Peru
peru_results <- get_excess_deaths(peru_monthly_deaths,peru_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(peru_results[[1]],"output-data/expected-deaths-models/peru_expected_deaths_model.RDS")
write.csv(peru_results[[2]],"output-data/excess-deaths/peru_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the Philippines
philippines_results <- get_excess_deaths(philippines_monthly_deaths,philippines_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(philippines_results[[1]],"output-data/expected-deaths-models/philippines_expected_deaths_model.RDS")
write.csv(philippines_results[[2]],"output-data/excess-deaths/philippines_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Poland
poland_results <- get_excess_deaths(poland_weekly_deaths,poland_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(poland_results[[1]],"output-data/expected-deaths-models/poland_expected_deaths_model.RDS")
write.csv(poland_results[[2]],"output-data/excess-deaths/poland_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Portugal
portugal_results <- get_excess_deaths(portugal_weekly_deaths,portugal_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(portugal_results[[1]],"output-data/expected-deaths-models/portugal_expected_deaths_model.RDS")
write.csv(portugal_results[[2]],"output-data/excess-deaths/portugal_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Qatar
qatar_results <- get_excess_deaths(qatar_monthly_deaths,qatar_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(qatar_results[[1]],"output-data/expected-deaths-models/qatar_expected_deaths_model.RDS")
write.csv(qatar_results[[2]],"output-data/excess-deaths/qatar_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Romania
romania_results <- get_excess_deaths(romania_weekly_deaths,romania_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(romania_results[[1]],"output-data/expected-deaths-models/romania_expected_deaths_model.RDS")
write.csv(romania_results[[2]],"output-data/excess-deaths/romania_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Russia
russia_results <- get_excess_deaths(russia_monthly_deaths,russia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(russia_results[[1]],"output-data/expected-deaths-models/russia_expected_deaths_model.RDS")
write.csv(russia_results[[2]],"output-data/excess-deaths/russia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Serbia
serbia_results <- get_excess_deaths(serbia_monthly_deaths,serbia_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(serbia_results[[1]],"output-data/expected-deaths-models/serbia_expected_deaths_model.RDS")
write.csv(serbia_results[[2]],"output-data/excess-deaths/serbia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Singapore
singapore_results <- get_excess_deaths(singapore_monthly_deaths,singapore_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(singapore_results[[1]],"output-data/expected-deaths-models/singapore_expected_deaths_model.RDS")
write.csv(singapore_results[[2]],"output-data/excess-deaths/singapore_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Slovakia
slovakia_results <- get_excess_deaths(slovakia_weekly_deaths,slovakia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(slovakia_results[[1]],"output-data/expected-deaths-models/slovakia_expected_deaths_model.RDS")
write.csv(slovakia_results[[2]],"output-data/excess-deaths/slovakia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Slovenia
slovenia_results <- get_excess_deaths(slovenia_weekly_deaths,slovenia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(slovenia_results[[1]],"output-data/expected-deaths-models/slovenia_expected_deaths_model.RDS")
write.csv(slovenia_results[[2]],"output-data/excess-deaths/slovenia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export South Africa
south_africa_results <- get_excess_deaths(south_africa_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(south_africa_results[[2]],"output-data/excess-deaths/south_africa_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export South Korea
south_korea_results <- get_excess_deaths(south_korea_weekly_deaths,south_korea_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(south_korea_results[[1]],"output-data/expected-deaths-models/south_korea_expected_deaths_model.RDS")
write.csv(south_korea_results[[2]],"output-data/excess-deaths/south_korea_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Spain
spain_results <- get_excess_deaths(spain_weekly_deaths,"none","weekly",calculate=FALSE,train_model=FALSE)
write.csv(spain_results[[2]],"output-data/excess-deaths/spain_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Sweden
sweden_results <- get_excess_deaths(sweden_weekly_deaths,sweden_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(sweden_results[[1]],"output-data/expected-deaths-models/sweden_expected_deaths_model.RDS")
write.csv(sweden_results[[2]],"output-data/excess-deaths/sweden_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Switzerland
switzerland_results <- get_excess_deaths(switzerland_weekly_deaths,switzerland_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(switzerland_results[[1]],"output-data/expected-deaths-models/switzerland_expected_deaths_model.RDS")
write.csv(switzerland_results[[2]],"output-data/excess-deaths/switzerland_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Tajikistan
tajikistan_results <- get_excess_deaths(tajikistan_quarterly_deaths,tajikistan_expected_deaths_model,"quarterly",calculate=TRUE,train_model=TRUE)
saveRDS(tajikistan_results[[1]],"output-data/expected-deaths-models/tajikistan_expected_deaths_model.RDS")
write.csv(tajikistan_results[[2]],"output-data/excess-deaths/tajikistan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Taiwan
taiwan_results <- get_excess_deaths(taiwan_weekly_deaths,taiwan_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(taiwan_results[[1]],"output-data/expected-deaths-models/taiwan_expected_deaths_model.RDS")
write.csv(taiwan_results[[2]],"output-data/excess-deaths/taiwan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Thailand
thailand_results <- get_excess_deaths(thailand_monthly_deaths,thailand_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(thailand_results[[1]],"output-data/expected-deaths-models/thailand_expected_deaths_model.RDS")
write.csv(thailand_results[[2]],"output-data/excess-deaths/thailand_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Tunisia
tunisia_results <- get_excess_deaths(tunisia_weekly_deaths,tunisia_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(tunisia_results[[1]],"output-data/expected-deaths-models/tunisia_expected_deaths_model.RDS")
write.csv(tunisia_results[[2]],"output-data/excess-deaths/tunisia_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Turkey
turkey_results <- get_excess_deaths(turkey_weekly_deaths,turkey_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(turkey_results[[1]],"output-data/expected-deaths-models/turkey_expected_deaths_model.RDS")
write.csv(turkey_results[[2]],"output-data/excess-deaths/turkey_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Ukraine
ukraine_results <- get_excess_deaths(ukraine_monthly_deaths,ukraine_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(ukraine_results[[1]],"output-data/expected-deaths-models/ukraine_expected_deaths_model.RDS")
write.csv(ukraine_results[[2]],"output-data/excess-deaths/ukraine_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export the United States
united_states_results <- get_excess_deaths(united_states_weekly_deaths,united_states_expected_deaths_model,"weekly",calculate=TRUE,train_model=TRUE)
saveRDS(united_states_results[[1]],"output-data/expected-deaths-models/united_states_expected_deaths_model.RDS")
write.csv(united_states_results[[2]],"output-data/excess-deaths/united_states_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Export Uzbekistan
uzbekistan_results <- get_excess_deaths(uzbekistan_monthly_deaths,uzbekistan_expected_deaths_model,"monthly",calculate=TRUE,train_model=TRUE)
saveRDS(uzbekistan_results[[1]],"output-data/expected-deaths-models/uzbekistan_expected_deaths_model.RDS")
write.csv(uzbekistan_results[[2]],"output-data/excess-deaths/uzbekistan_excess_deaths.csv",fileEncoding = "UTF-8",row.names=FALSE)

# Step 4: combine weekly and monthly deaths together, and calculate deaths per 100,000 people and percentage change ---------------------------------------

# Combine weekly deaths and calculate per 100,000 people and percentage change
all_weekly_excess_deaths <- bind_rows(albania_results[[2]],
                                      armenia_results[[2]],
                                      australia_results[[2]],
                                      austria_results[[2]],
                                      belgium_results[[2]],
                                      britain_results[[2]],
                                      bulgaria_results[[2]],
                                      canada_results[[2]],
                                      chile_results[[2]],
                                      colombia_results[[2]],
                                      croatia_results[[2]],
                                      cyprus_results[[2]],
                                      czech_republic_results[[2]],
                                      denmark_results[[2]],
                                      estonia_results[[2]],
                                      finland_results[[2]],
                                      france_results[[2]],
                                      georgia_results[[2]],
                                      germany_results[[2]],
                                      greece_results[[2]],
                                      hungary_results[[2]],
                                      iceland_results[[2]],
                                      israel_results[[2]],
                                      italy_results[[2]],
                                      latvia_results[[2]],
                                      lithuania_results[[2]],
                                      luxembourg_results[[2]],
                                      malta_results[[2]],
                                      mexico_results[[2]],
                                      montenegro_results[[2]],
                                      netherlands_results[[2]],
                                      new_zealand_results[[2]],
                                      norway_results[[2]],
                                      poland_results[[2]],
                                      portugal_results[[2]],
                                      romania_results[[2]],
                                      slovakia_results[[2]],
                                      slovenia_results[[2]],
                                      south_africa_results[[2]],
                                      south_korea_results[[2]],
                                      spain_results[[2]],
                                      sweden_results[[2]],
                                      switzerland_results[[2]],
                                      taiwan_results[[2]],
                                      tunisia_results[[2]],
                                      turkey_results[[2]],
                                      united_states_results[[2]]) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

# Export weekly deaths
write.csv(all_weekly_excess_deaths,"output-data/excess-deaths/all_weekly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Combine monthly deaths and calculate per 100,000 people and percentage change
all_monthly_excess_deaths <- bind_rows(azerbaijan_results[[2]],
                                       belarus_results[[2]],
                                       bolivia_results[[2]],
                                       bosnia_and_herzegovina_results[[2]],
                                       brazil_results[[2]],
                                       costa_rica_results[[2]],
                                       ecuador_results[[2]],
                                       egypt_results[[2]],
                                       el_salvador_results[[2]],
                                       indonesia_results[[2]],
                                       jamaica_results[[2]],
                                       japan_results[[2]],
                                       kazakhstan_results[[2]],
                                       kosovo_results[[2]],
                                       kyrgyzstan_results[[2]],
                                       mauritius_results[[2]],
                                       moldova_results[[2]],
                                       mongolia_results[[2]],
                                       nicaragua_results[[2]],
                                       north_macedonia_results[[2]],
                                       oman_results[[2]],
                                       panama_results[[2]],
                                       paraguay_results[[2]],
                                       peru_results[[2]],
                                       philippines_results[[2]],
                                       qatar_results[[2]],
                                       russia_results[[2]],
                                       serbia_results[[2]],
                                       singapore_results[[2]],
                                       thailand_results[[2]],
                                       ukraine_results[[2]],
                                       uzbekistan_results[[2]]) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

# Export monthly deaths
write.csv(all_monthly_excess_deaths,"output-data/excess-deaths/all_monthly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)

# Combine quarterly deaths and calculate per 100,000 people and percentage change
all_quarterly_excess_deaths <- bind_rows(iran_results[[2]],
                                         malaysia_results[[2]],
                                         tajikistan_results[[2]]) %>%
  mutate(covid_deaths_per_100k = covid_deaths / population * 100000,
         excess_deaths_per_100k = excess_deaths / population * 100000,
         excess_deaths_pct_change = (total_deaths / expected_deaths) - 1)

# Export quarterly deaths
write.csv(all_quarterly_excess_deaths,"output-data/excess-deaths/all_quarterly_excess_deaths.csv",
          fileEncoding = "UTF-8",row.names=FALSE)
