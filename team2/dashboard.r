library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggmap)
library(flexdashboard)
rm(mapData)
rm(Combined)
covid <- read_excel("team2/covid-fci-data.xlsx")
View(covid) #To viwe the data

#finding the missing values
is.na(covid)
covid$LAT[covid$LAT==""] <- NA
mean(covid)

#storing the level 1 policy as policy1 and replacing the missing values with NA
policy1=as.factor(covid$`Level 1 policy measure`)
covid$`Level 1 policy measure`[covid$`Level 1 policy measure`==""] <- NA

income=covid$`Level of income`
date=as.factor(covid$`Entry date`)

#storing the level 1 policy as policy1 and replacing the missing values with NA
policy2=as.factor(covid$`Level 2 policy measure`)
covid$`Level 2 policy measure`[covid$`Level 2 policy measure`==""] <- NA

#remove the missing values
covid <- na.omit(covid)

#ploting the cointries with respect to policy1 and income
ggplot(covid, aes(Country,date, color = policy1)) + 
  geom_point(aes(shape = income), size = 3)
#ploting the cointries with respect to policy1 and income
ggplot(covid, aes(Country,date, color = policy2)) + 
  geom_point(aes(shape = income), size = 3)

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf() +
  geom_point(data = covid, aes(x = LONG, y = LAT,fill = policy1), size = 4, 
             shape = 23)


## app.R ##
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 400,width = 1750))
    ),
    fluidRow(
      box(plotOutput("plot2", height = 400,width = 1750))
    ),
    fluidRow(
      box(plotOutput("plot3", height = 400,width = 1000))
    ),
    fluidRow(
      box(plotOutput("plot4", height = 400,width = 1000))
    ),
  )
)

server <- function(input, output,session) {
  output$plot1 <-  renderPlot({ 
    ggplot(covid, aes(Country,date, color = policy1)) + 
      geom_point(aes(shape = income), size = 3)
  })
  output$plot2 <-  renderPlot({
    ggplot(covid, aes(Country,date, color = policy2)) + 
      geom_point(aes(shape = income), size = 3)
  })
  output$plot3 <-  renderPlot({
    ggplot(data = world) +
      geom_sf() +
      geom_point(data = covid, aes(x = LONG, y = LAT,fill =policy1), size = 4, 
                 shape = 23)
  })
  output$plot4 <-  renderPlot({
    ggplot(data = world) +
      geom_sf() +
      geom_point(data = covid, aes(x = LONG, y = LAT,fill =policy2), size = 4, 
                 shape = 23)
  })
}

shinyApp(ui, server)
