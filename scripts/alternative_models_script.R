# A note on alternative model approaches to expected deaths:
# To calculate expected deaths The Economist uses a linear model with fixed effects and time trends. In consultation with demographers, we follow the approach of the peer reviewed publication https://elifesciences.org/articles/69336. This publication is written by the people that compile much of our data. It is mathematically equivalent to "the average for this time of year (week/month/quarter) plus or minus the average change in deaths from year to year". The resulting predictions are reasonable in terms of providing good predictive accuracy and being non-negative (even if this is an extreme theoretical possibility). 

# However, Yet it might be useful to explore alternative ways to model expected deaths. The following script shows a few different approaches such as count models and different ways to model time (splines vs fixed effects). Differences are very slight (less than 0.4% across any method pair summed over all countries over 2 years of predictions). A t-test on cross-validation error at the country-level for data before 2020 finds the methods to be statistically equal in predictive accuracy. Since these estimates enter into our global model of excess deaths, it is however worth asking what the impact might be of the modeling choice made here on that model's output. Below, the data shows that should the maximum difference between models extrapolate to all countries (i.e. beyond those here where we have data), the total would be 0.49m more expected deaths worldwide, or a reduction in our central estimate of global excess deaths of about 2.7% (as of early Dec. 2021).

# Load packages
library(tidyverse)
library(data.table)

# Loop over all countries:
res <- data.frame()
for(i in dir('output-data/historical-deaths')){

# Load data:
df <- read_csv(paste0("output-data/historical-deaths/", i))

if(i == "united_states_weekly_deaths.csv"){
df <- df[df$region == "United States", ]
}

df <- data.frame(df)
df$time <- df[, 8]
df <- df[df$time != 53, ] # For simplicity, we in this script exclude the 53rd week of some years 
df <- df[df$year <= 2020, ]
expected_deaths_formula <- as.formula(total_deaths ~ year + as.factor(time))

# Create a target dataframe for predicting expected deaths:
test_df <- rbind(data.frame(year = rep(2020, length(unique(df$time))), 
                            time = unique(df$time)),
                 data.frame(year = rep(2021, length(unique(df$time))), 
                            time = unique(df$time)))

# Baseline (OLS with period fixed effects and year trend)
expected_deaths_model <- lm(expected_deaths_formula, df)

expected_deaths <- test_df %>% 
  mutate(expected_deaths = predict(expected_deaths_model, newdata=.))

cv_error <- c()
for(i in unique(df$year)){
cv_m <- lm(expected_deaths_formula, df[df$year != i, ])
cv_error <- c(cv_error, sum(predict(cv_m, df[df$year == i, ]) - df$total_deaths[df$year == i]))
}  

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

a <- c(sum(expected_deaths$expected_deaths), mean(cv_error))

# Expected deaths calculation of the Farrington flexible algorithm (=Poisson):
library(surveillance)

summary(m1 <- glm(expected_deaths_formula, family="poisson", data=df))
expected_deaths <- test_df %>% 
  mutate(expected_deaths = predict(m1, newdata=., type="response"))

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

cv_error <- c()
for(i in unique(df$year)){
  cv_m <- glm(expected_deaths_formula, df[df$year != i, ], family="poisson")
  cv_error <- c(cv_error, sum(predict(cv_m, df[df$year == i, ], type = 'response') - df$total_deaths[df$year == i]))
} 

b <- c(sum(expected_deaths$expected_deaths), mean(cv_error))

# Baseline with spline instead of FE
library(splines)
expected_deaths_formula <- as.formula(total_deaths ~ year + bs(time))

expected_deaths_model <- lm(expected_deaths_formula, df)

expected_deaths <- test_df %>% 
  mutate(expected_deaths = predict(expected_deaths_model, newdata=.))

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

cv_error <- c()
for(i in unique(df$year)){
  cv_m <- lm(expected_deaths_formula, df[df$year != i, ])
  cv_error <- c(cv_error, sum(predict(cv_m, df[df$year == i, ]) - df$total_deaths[df$year == i]))
}  

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

c <- c(sum(expected_deaths$expected_deaths), mean(cv_error))

# Expected deaths calculation of the Farrington flexible algorithm (=Poisson) with splines instead of FE:
summary(m1 <- glm(expected_deaths_formula, family="poisson", data=df))
expected_deaths <- test_df %>% 
  mutate(expected_deaths = predict(m1, newdata=., type="response"))

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

cv_error <- c()
for(i in unique(df$year)){
  cv_m <- glm(expected_deaths_formula, df[df$year != i, ], family="poisson")
  cv_error <- c(cv_error, sum(predict(cv_m, df[df$year == i, ], type = 'response')- df$total_deaths[df$year == i]))
}  

sum(expected_deaths$expected_deaths)
head(expected_deaths$expected_deaths)

d <- c(sum(expected_deaths$expected_deaths), mean(cv_error))

# Combine all 4:
res <- rbind(res, 
             c(a, b, c, d, df$population[1], i))
}

colnames(res) <- c('lm', 'lm_cv', 'poisson', 'poisson_cv', "lm_spline", 'lm_spline_cv', 'poisson_spline', 'poisson_spline_cv', 'population', 'country')

for(i in 1:9){
  res[, i] <- as.numeric(res[, i])
}

# Chart these:
library(ggplot2)
ggplot(res[order(res$lm), ], aes(x=1:nrow(res), alpha = 0.5))+
  geom_line(aes(y=lm, col = "OLS with time FE"))+
  geom_line(aes(y=lm_spline, col = "OLS with time spline"))+
  geom_line(aes(y=poisson, col = "Poisson with time FE"))+
  geom_line(aes(y=poisson_spline, col = "Poisson with time spline"))+
  theme_minimal()+scale_y_continuous(trans = "log10")+
  ylab('Estimated expected deaths')+xlab('Country number')

sum(res$lm)
sum(res$lm_spline)
sum(res$poisson)
sum(res$poisson_spline)

# Max difference, extrapolating to world population:
abs(sum(res$lm)-sum(res$poisson))*((7.9*10^9)/sum(res$population))

ggplot(res[order(res$lm_cv), ], aes(x=1:nrow(res), alpha = 0.5))+
  geom_point(aes(y=lm_cv, col = "OLS with time FE"))+
  geom_point(aes(y=lm_spline_cv, col = "OLS with time spline"))+
  geom_point(aes(y=poisson_cv, col = "Poisson with time FE"))+
  geom_point(aes(y=poisson_spline_cv, col = "Poisson with time spline"))+
  theme_minimal()+scale_y_continuous(trans = "log10")+
  ylab('Cross-validation error')+xlab('Country number')

# Poisson vs LM cross-validation error:
ggplot(res, aes(x=lm_cv, y=poisson_cv))+geom_point()+geom_abline(aes(intercept=0, slope = 1))

# Test of country-level differences:
t.test(res$lm_cv, res$poisson_cv)

summary(res)
