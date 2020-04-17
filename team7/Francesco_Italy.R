install.packages("covid19italy")
library(covid19italy)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(PerformanceAnalytics)
library(Hmisc)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(datarium)
library(githubinstall)
library(devtools)
library(usethis)
library(ISLR)
library(MASS)
library(visreg)
library(rgl)
library(knitr)
library(scatterplot3d)
library(lubridate)
library(easynls)


update_data()

data(italy_total)
data(italy_region)
data(italy_province)



plot_ly(data = italy_total,
        x = ~ date,
        y = ~home_confinement,
        name = 'Home Confinement',
        fillcolor = '#FDBBBC',
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one') %>%
  add_trace( y = ~ hospitalized_with_symptoms,
             name = "Hospitalized with Symptoms",
             fillcolor = '#E41317') %>%
  add_trace(y = ~intensive_care,
            name = 'Intensive Care',
            fillcolor = '#9E0003') %>%
  layout(title = "Italy - Distribution of Active Covid19 Cases",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Italy Department of Civil Protection"))


plot_ly(data = italy_total,
        x = ~ date,
        y = ~ cumulative_cases,
        name = 'Active',
        fillcolor = '#1f77b4',
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one') %>%
  add_trace( y = ~ death,
             name = "Death",
             fillcolor = '#E41317') %>%
  add_trace(y = ~recovered,
            name = 'Recovered',
            fillcolor = 'forestgreen') %>%
  layout(title = "Italy - Distribution of Covid19 Cases",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Italy Department of Civil Protection"))

italy_province %>%
  filter(date == max(date), region_name == "Lombardia") %>%
  plot_ly(labels = ~province_name, values = ~total_cases,
          textinfo="label+percent",
          type = 'pie') %>%
  layout(title = "Lombardia - Cases Distribution by Province") %>%
  hide_legend()

######################################################################
# By me
# I Define 2 new datasets to understand and see the differences on a day to day basis
italy_total2 <- italy_total %>%
  arrange(date) %>%
  mutate(diff_hospitalized_with_symptoms = hospitalized_with_symptoms - lag(hospitalized_with_symptoms, default = first(hospitalized_with_symptoms))) %>%
  mutate(diff_intensive_care = intensive_care - lag(intensive_care, default = first(intensive_care))) %>%
  mutate(diff_total_hospitalized = total_hospitalized - lag(total_hospitalized, default = first(total_hospitalized))) %>%
  mutate(diff_home_confinement = home_confinement - lag(home_confinement, default = first(home_confinement))) %>%
  mutate(diff_cumulative_positive_cases = cumulative_positive_cases - lag(cumulative_positive_cases, default = first(cumulative_positive_cases))) %>%
  mutate(diff_daily_positive_cases = daily_positive_cases - lag(daily_positive_cases, default = first(daily_positive_cases))) %>%
  mutate(diff_recovered = recovered - lag(recovered, default = first(recovered))) %>%
  mutate(diff_death = death - lag(death, default = first(death))) %>%
  mutate(diff_cumulative_cases = cumulative_cases - lag(cumulative_cases, default = first(cumulative_cases))) %>%
  mutate(diff_total_test = total_tests - lag(total_tests, default = first(total_tests)))

italy_total2[2:11] <- NULL


italy_region2 <- italy_region %>%
  group_by(region_code) %>%
  arrange(date) %>%
  mutate(diff_hospitalized_with_symptoms = hospitalized_with_symptoms - lag(hospitalized_with_symptoms, default = first(hospitalized_with_symptoms))) %>%
  mutate(diff_intensive_care = intensive_care - lag(intensive_care, default = first(intensive_care))) %>%
  mutate(diff_total_hospitalized = total_hospitalized - lag(total_hospitalized, default = first(total_hospitalized))) %>%
  mutate(diff_home_confinement = home_confinement - lag(home_confinement, default = first(home_confinement))) %>%
  mutate(diff_cumulative_positive_cases = cumulative_positive_cases - lag(cumulative_positive_cases, default = first(cumulative_positive_cases))) %>%
  mutate(diff_daily_positive_cases = daily_positive_cases - lag(daily_positive_cases, default = first(daily_positive_cases))) %>%
  mutate(diff_recovered = recovered - lag(recovered, default = first(recovered))) %>%
  mutate(diff_death = death - lag(death, default = first(death))) %>%
  mutate(diff_cumulative_cases = cumulative_cases - lag(cumulative_cases, default = first(cumulative_cases))) %>%
  mutate(diff_total_tests = total_tests - lag(total_tests, default = first(total_tests))) 

italy_region2[6:15] <- NULL

# plot to understand the differences in testing but quite useless
ggplot(data = italy_total2, aes(x = date)) + geom_line(aes(y = diff_total_test, color = "darkred")) + geom_point(aes(y = diff_daily_positive_cases, color="steelblue"))

# distribution of variation on a log scale of various key factors
plot_ly(data = italy_total2,
        x = ~ date,
        y = ~ log(diff_total_test),
        name = 'Daily Tests',
        color = '#1f77b4',
        type = 'scatter',
        mode = '‘lines') %>%
  add_trace( y = ~ log(diff_death),
             name = "Daily Deaths",
             color = '#E41317') %>%
  add_trace(y = ~ log(diff_cumulative_positive_cases),
            name = 'Daily Positive Cases',
            color = 'forestgreen') %>%
  layout(title = "Italy - Distribution of Covid19 Cases daily",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases per day on a log scale"),
         xaxis = list(title = "Source: Italy Department of Civil Protection"))

# same one as before but with ggplot
ggplot(data = italy_total2, aes(x = date)) + geom_line(aes(y = log(diff_total_test),color = "darkred")) + geom_line(aes(y = log(diff_cumulative_positive_cases), color = "steelblue"))


# i try to see if there is any sort of correlation with my data
cor(italy_total2$diff_test, italy_total2$diff_daily_positive_cases)
my_data <- italy_total2[, c(2:11)]
my_data.cor = cor(my_data)
my_data.rcorr = rcorr(as.matrix(my_data))
my_data.rcorr
corrplot(my_data.cor, method = "number")

# multiple regression analysis

dhws <- as.vector(italy_total2$diff_hospitalized_with_symptoms)
dic <- as.vector(italy_total2$diff_intensive_care)
dth <- as.vector(italy_total2$diff_total_hospitalized)
dhc <- as.vector(italy_total2$diff_home_confinement)
dcpc <- as.vector(italy_total2$diff_cumulative_positive_cases)
ddpc <- as.vector(italy_total2$diff_daily_positive_cases)
dr <- as.vector(italy_total2$diff_recovered)
dd <- as.vector(italy_total2$diff_death)
dcc <- as.vector(italy_total2$diff_cumulative_cases)
dtt <- as.vector(italy_total2$diff_total_test)

model = glm(dd~dhc+dcpc+dr+dcc+dtt)

summary(model)

sigma(model)/mean(italy_total2$diff_death)

fit1 <- glm(dd~., data = italy_total2)

summary(fit1)

fit2 <- glm(dd~dcpc+dr+dcc, data = italy_total2)
plot(fit2, pch = 20, cex = 0.7, col = "blue", which = 1)
# dead end


# time series analysis
# i try to change the date into a day to day format starting from the first date
x <- italy_total2$date
date <- ymd(x)
days <- yday(date) - 54
italy_total2 %>%
  mutate(days = yday(ymd(italy_total2$date)) - 54)

################################### ### GOMPERTZ

# i create a third dataset as the result of mixing the 1st and 2nd version
italy_total3 <- italy_total %>%
  arrange(date) %>%
  mutate(diff_hospitalized_with_symptoms = hospitalized_with_symptoms - lag(hospitalized_with_symptoms, default = first(hospitalized_with_symptoms))) %>%
  mutate(diff_intensive_care = intensive_care - lag(intensive_care, default = first(intensive_care))) %>%
  mutate(diff_total_hospitalized = total_hospitalized - lag(total_hospitalized, default = first(total_hospitalized))) %>%
  mutate(diff_home_confinement = home_confinement - lag(home_confinement, default = first(home_confinement))) %>%
  mutate(diff_cumulative_positive_cases = cumulative_positive_cases - lag(cumulative_positive_cases, default = first(cumulative_positive_cases))) %>%
  mutate(diff_daily_positive_cases = daily_positive_cases - lag(daily_positive_cases, default = first(daily_positive_cases))) %>%
  mutate(diff_recovered = recovered - lag(recovered, default = first(recovered))) %>%
  mutate(diff_death = death - lag(death, default = first(death))) %>%
  mutate(diff_cumulative_cases = cumulative_cases - lag(cumulative_cases, default = first(cumulative_cases))) %>%
  mutate(diff_total_test = total_tests - lag(total_tests, default = first(total_tests))) %>%
  mutate(days = yday(ymd(italy_total3$date)) - 54)

#simple plot analysis
plot(italy_total3$days, italy_total3$cumulative_cases)
plot(italy_total3$days, italy_total3$death)
plot(italy_total3$days, italy_total3$diff_cumulative_cases)
plot(italy_total3$days, italy_total3$diff_death)

# reference: https://www.youtube.com/watch?v=0ifT-7K68sk


# First Model
days_death = as.data.frame(cbind(italy_total3$days, italy_total3$diff_death))
plot(days_death)
model1 = nlsfit(days_death, model = 10, start = c(a = 600, b = 2, c = 0.1))
model1
nlsplot(days_death, model = 10, start = c(a = 600, b = 2, c = 0.1), 
        xlab = "Days" , ylab = "Diff_Death" , position = 1)

# Second Model
days_cumulative_cases = as.data.frame(cbind(italy_total3$days, italy_total3$diff_cumulative_cases))
plot(days_cumulative_cases)
model2 = nlsfit(days_cumulative_cases, model = 10, start = c(a = 4000, b = 2, c = 0.2))
model2
nlsplot(days_cumulative_cases, model = 10, start = c(a = 4000, b = 2, c = 0.2), 
        xlab = "Days" , ylab = "Diff_Cumulative_cases" , position = 1)

# Third Model can't run because is not suitable for Gompertz
days_intensive_care = as.data.frame(cbind(italy_total3$days, italy_total3$diff_intensive_care))
plot(days_intensive_care)
model3 = nlsfit(days_intensive_care, model = 10, start = c(a = 200, b = 0, c = 0.1))
model3
nlsplot(days_intensive_care, model = 10, start = c(a = 200, b = 0, c = 0.1), 
        xlab = "Days" , ylab = "Diff_Intensive_Care" , position = 1)