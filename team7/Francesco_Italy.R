install.packages("covid19italy")

# install.packages("devtools")
devtools::install_github("covid19italy/covid19Italy")

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

italy_region %>%
  filter(date == max(date)) %>%
  select(region_name, cumulative_cases, recovered, death, cumulative_positive_cases) %>%
  arrange(-cumulative_positive_cases) %>%
  mutate(region = factor(region_name, levels = region_name)) %>%
  plot_ly(y = ~ region,
          x = ~ cumulative_cases,
          orientation = 'h',
          text =  ~ cumulative_cases,
          textposition = 'auto',
          type = "bar",
          name = "Active",
          marker = list(color = "#1f77b4")) %>%
  add_trace(x = ~ recovered,
            text =  ~ recovered,
            textposition = 'auto',
            name = "Recovered",
            marker = list(color = "forestgreen")) %>%
  add_trace(x = ~ death,
            text =  ~ death,
            textposition = 'auto',
            name = "Death",
            marker = list(color = "red")) %>%
  layout(title = "Cases Distribution by Region",
         barmode = 'stack',
         yaxis = list(title = "Region"),
         xaxis = list(title = "Number of Cases"),
         hovermode = "compare",
         legend = list(x = 0.65, y = 0.9),
         margin =  list(
           l = 20,
           r = 10,
           b = 10,
           t = 30,
           pad = 2
         ))

italy_province %>%
  filter(date == max(date), region_name == "Lombardia") %>%
  plot_ly(labels = ~province_name, values = ~total_cases,
          textinfo="label+percent",
          type = 'pie') %>%
  layout(title = "Lombardia - Cases Distribution by Province") %>%
  hide_legend()
######################################################################
# By me

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


ggplot(data = italy_total2, aes(x = date)) + geom_line(aes(y = diff_total_test, color = "darkred")) + geom_point(aes(y = diff_daily_positive_cases, color="steelblue"))

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

ggplot(data = italy_total2, aes(x = date)) + geom_line(aes(y = log(diff_total_test),color = "darkred")) + geom_line(aes(y = log(diff_cumulative_positive_cases), color = "steelblue"))


cor(italy_total2$diff_test, italy_total2$diff_daily_positive_cases)

my_data <- italy_total2[, c(2:11)]
my_data.cor = cor(my_data)
my_data.rcorr = rcorr(as.matrix(my_data))
my_data.rcorr
corrplot(my_data.cor, method = "number")

# multiple regression analysis

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

library(growthmodels)
################################### ### GOMPERTZ
# alpha = 9526 beta = 9.1618 k = 0.0028
nls.gompertz <- minpack.lm::nlsLM(data$cases ˜ alpha*exp(-beta*exp(-k*data$days)), data = data, start = list(alpha = alpha, beta = beta, k = k), control = list(maxiter = 500))
coef(nls.gompertz) ## alpha = 9437, beta = 59.24, k = 0.0219
## Now fit Geompertz model
growth.gompertz <- growthmodels::gompertz(data$days, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
growth.gompertz
## Predict
predict.gompertz <-growthmodels::gompertz(days.predict, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
predict.gompertz
## the values for 18/4, 18/5, 18/6, and 18/7 predict.gompertz[c(84:87)]













