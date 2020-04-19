#load packages
library(tidyverse)
library(lubridate)
library(rvest)
library(stringdist)
library("forecast")

#Data cleaning and engneering from 
#"https://joachim-gassen.github.io/2020/03/tidying-the-john-hopkins-covid-19-data/"
# Function to read the raw CSV files. The files are aggregated to the country
# level and then converted to long format

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% group_by(`Country/Region`) %>%
    filter(`Country/Region` != "Cruise Ship") %>%
    select(-`Province/State`, -Lat, -Long) %>%
    mutate_at(vars(-group_cols()), sum) %>% 
    distinct() %>%
    ungroup() %>%
    rename(country = `Country/Region`) %>%
    pivot_longer(
      -country, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    mutate(date = mdy(date_str)) %>%
    select(country, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw))


#select a country to do time series analysis with ARIMA model

slctcountry = "US" #This varible defines the county
countrydata <- subset(jh_covid19_data, country == slctcountry)

#time series analysis for comfirmed cases
tscfm <- ts(countrydata$confirmed, frequency = 365.25, start=c(2020,1,22))
plot.ts(tscfm)

#TS analysis with auto.arima founction
arc <- auto.arima(tscfm) 
summary(arc)

fc1 <- forecast(arc, h=7) #predict for next 7 days
summary(fc1)
plot(fc1)


#Residual analysis
acf(fc1$residuals)

pacf(fc1$residuals)

plot(fc1$residuals)

qqnorm(fc1$residuals)



##time series analysis for comfirmed deaths

tsdeath <- ts(countrydata$deaths, frequency = 365, start=c(2020,22), end=c(2020,92))
plot(tsdeath)

ard <- auto.arima(tsdeath)
summary(ard)

fc2 <- forecast(ard, h=7) #predict for next 7 days
summary(fc1)
plot(fc1)

#Residual analysis
acf(fc2$residuals)

pacf(fc2$residuals)

plot(fc2$residuals)

qqnorm(fc2$residuals)


