library(rgdal)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(tidyverse)
library(tools)
library(reshape2)
library(ggplot2)
library(ggridges)
library(lubridate)
library(rtweet)
library(maps)
library(quanteda)
library(wordcloud)
library(tidytext)
library(sf)
knitr::opts_chunk$set(echo = TRUE, eval=FALSE)

## INCOME BY DISTRICT

library(gsheet)
income_2012 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1HosE_dBedt3Idho0f1jr-MQBt7PS_mpge7c1GIip0Yg/edit#gid=2078260586")
income_2013 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1HosE_dBedt3Idho0f1jr-MQBt7PS_mpge7c1GIip0Yg/edit#gid=1695992236")
income_2014 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1HosE_dBedt3Idho0f1jr-MQBt7PS_mpge7c1GIip0Yg/edit#gid=0")
income_2015 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1HosE_dBedt3Idho0f1jr-MQBt7PS_mpge7c1GIip0Yg/edit#gid=725888174")

income <- bind_rows(income_2012, income_2013)
income <- bind_rows(income, income_2014)
income <- bind_rows(income, income_2015)

names(income)[names(income) == "districts"] <- "lad17nm"

income$lad17nm <- gsub(" UA", "", income$lad17nm)
income$lad17nm <- gsub(" Towns", "", income$lad17nm)
income$lad17nm <- gsub("-", " ", income$lad17nm)
income$lad17nm <- gsub("The ", "", income$lad17nm)
income$lad17nm <- gsub(" City of$", "", income$lad17nm)
income$lad17nm <- gsub("Edinburgh", "City of Edinburgh", income$lad17nm)
income$lad17nm <- gsub("Down", "Newry, Mourne and Down", income$lad17nm)
income$lad17nm <- gsub("Newry and Mourne", "Newry, Mourne and Down", income$lad17nm)
income$lad17nm <- gsub("Rhondda Cynon Taff", "Rhondda Cynon Taf", income$lad17nm)
income$lad17nm <- gsub("South Buckinghamshire", "South Bucks", income$lad17nm)
income$lad17nm <- gsub("Comhairle nan Eilean Siar", "Na h Eileanan Siar", income$lad17nm)

income[which(income[,1]=="Antrim"),1] <- "Antrim and Newtownabbey"
income[which(income[,1]=="Newtownabbey"),1] <- "Antrim and Newtownabbey"
income[which(income[,1]=="Armagh"),1] <- "Armagh City, Banbridge and Craigavon"
income[which(income[,1]=="Banbridge"),1] <- "Armagh City, Banbridge and Craigavon"
income[which(income[,1]=="Craigavon"),1] <- "Armagh City, Banbridge and Craigavon"
income[which(income[,1]=="Derry"),1] <- "Derry City and Strabane"
income[which(income[,1]=="Strabane"),1] <- "Derry City and Strabane"
income[which(income[,1]=="Fermanagh"),1] <- "Fermanagh and Omagh"
income[which(income[,1]=="Omagh"),1] <- "Fermanagh and Omagh"
income[which(income[,1]=="Lisburn"),1] <- "Lisburn and Castlereagh"
income[which(income[,1]=="Castlereagh"),1] <- "Lisburn and Castlereagh"
income[which(income[,1]=="Ards"),1] <- "Ards and North Down"
income[which(income[,1]=="North Down"),1] <- "Ards and North Down"

income <- aggregate(income_tot_mean ~ lad17nm + year, data = income, mean)
income$income_tot_mean <- round(income$income_tot_mean, digits = 0)

income$lad17nm <- toTitleCase(income$lad17nm)

## POPULATION BY DISTRICT

population <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1HosE_dBedt3Idho0f1jr-MQBt7PS_mpge7c1GIip0Yg/edit#gid=1762880462")

population <- population[, -c(14:22)]

population$`Area2(sqkm)` <- NULL
population$Geography <- NULL
population$Code <- NULL

population <- gather(population, "year", "population", 2:10)

names(population)[names(population) == "Name"] <- "lad17nm"

population$year <- as.numeric(population$year)

population <- filter(population, year == 2012 | year == 2013 | year == 2014 | year == 2015)

population$lad17nm <- gsub(", City of", "", population$lad17nm)
population$lad17nm <- gsub(", County of", "", population$lad17nm)
population$lad17nm <- gsub("-", " ", population$lad17nm)
population$lad17nm <- gsub("'", "", population$lad17nm)
population$lad17nm <- gsub("St ", "St. ", population$lad17nm)
population$lad17nm <- gsub("Folkstone and Hythe", "Shepway", population$lad17nm)

population$lad17nm <- toTitleCase(population$lad17nm)

## INCOME AND POPULATION 

district_data <- left_join(population, income, by = c("lad17nm", "year"))

# store api keys (these are fake example values; replace with your own keys)
#api_key <- "2d0zkQNRVfuYUSnZewrR5rv5J"
#api<_secret_key <- "fAuHUBMXMMRGOgXuGYQHe65KILaF5B2ivSjKpykck9rMsMJdUs"
#access_token <- "1250086760857702400-Cdpb5Lb3jUnHREhCtKVAevYouphqUA"
#access_token_secret <- "qiIOW0aDddW1LFX5dQRxJbmQ5wuCqQaHEHjKMlqwWaRP6"

# authenticate via web browser
#token <- create_token(
#  app = "Covid19AnalysisWord",
#  consumer_key = api_key,
#  consumer_secret = api_secret_key,
#  access_token = access_token,
#  access_secret = access_token_secret)


#since 
#covid19_testUK2 <- search_tweets("#covid19", n=1000,  include_rts = FALSE, retryonratelimit = TRUE)
#covid19_testUK2_latLong <- lat_lng(covid19_testUK2)

#save_as_csv(covid19_testUK2_latLong, "covidTweetData2.csv")

## LOADING TWEETS
tweets.overall <- read_csv("covidTweetData2.csv")

## KEEPING TWEETS OF UK
tweets.overall.LatLong <- filter(tweets.overall, lat >= 49.771686 & lat <= 60.862568)
tweets.overall.LatLong <- filter(tweets.overall.LatLong, lng >= -12.524414 & lng <= 1.785278)
#tweets.overall.LatLong <- filter(tweets.overall, location == "UK")

## TWEETS MINING
tweets <- tweets.overall.LatLong

tweets.overall.LatLong$year <- substr(tweets.overall.LatLong$created_at, 0, 4)

tweets.LatLong <- tibble(line = 1:nrow(tweets.overall.LatLong), 
                         year = tweets.overall.LatLong$year, 
                         latitude = tweets.overall.LatLong$lat, 
                         longitude = tweets.overall.LatLong$lng)

# Cleaning
text <- tweets$text

# remove retweet entities
text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
# remove at people
text <- gsub("@\\w+", "", text)
# remove punctuation
text <- gsub("[[:punct:]]", "", text)
# remove numbers
text <- gsub("[[:digit:]]", "", text)
# remove html links
text <- gsub("http\\w+", "", text)
# remove all pictwitter
text <- gsub("pictwitter\\w+ *", "", text)
# Remove chinese language
text <- iconv(text, "latin1", "ASCII", sub="")

# Tibble format
text_df <- tibble(line = 1:length(text), text = text)

# Tokenization 
tidy_tweets <- text_df %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# Join tweets to longitude and latitude by line number
tidy_tweets_LatLong <- left_join(tidy_tweets, tweets.LatLong, by = "line")

# Words that contribute to positive and negative sentiment
AFINN <- get_sentiments("afinn") 

afinn_word_LatLong <- tidy_tweets_LatLong %>%
  inner_join(AFINN, by = "word")

afinn_word_LatLong_Tot <- aggregate(value ~ line + word + year + latitude + longitude, afinn_word_LatLong, sum)

afinn_word_LatLong_Tot$sentiment <- ifelse(afinn_word_LatLong_Tot$value > 0, "positive", 
                                           ifelse(afinn_word_LatLong_Tot$value == 0, "neutral", "negative"))

afinn_word_LatLong_Tot_PN <- filter(afinn_word_LatLong_Tot, sentiment != "neutral")


qplot(factor(sentiment), data=afinn_word_LatLong_Tot_PN, geom="bar", fill=factor(sentiment))+xlab("Sentiment Categories") + ylab("Frequency") + ggtitle("Sentiments Analysis - Covid19")
qplot(factor(value), data=afinn_word_LatLong_Tot_PN, geom="bar", fill=factor(value))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Sentiments Analysis Scores - Covid19")

afinn_word_LatLong_Tot %>% count(word, sort = TRUE) %>% top_n(10) %>% mutate(word = reorder(word,n)) %>% ggplot(aes(x=word, y=n)) + geom_col() + xlab(NULL) + coord_flip() + theme_classic() + labs(x= "Count", y="Unique words", title = "Unique words counts found in #covid19 tweets")

# sentiment analysis (bing)
afinn_covid = afinn_word_LatLong_Tot %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()


afinn_covid %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word,n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales="free_y") + labs(title = "Tweets containing #covid19", y="Contribution to sentiment", x=NULL) + coord_flip() + theme_bw()
## SHAPEFILE MAP DISTRICT UK

district <- readOGR(dsn = "./shapefiles/ladUK", 
                    layer = 'Local_Authority_Districts_December_2017_Full_Clipped_Boundaries_in_United_Kingdom_WGS84')

district@data$lad17nm <- gsub(", City of", "", district@data$lad17nm)
district@data$lad17nm <- gsub(", County of", "", district@data$lad17nm)
district@data$lad17nm <- gsub("-", " ", district@data$lad17nm)
district@data$lad17nm <- gsub("'", "", district@data$lad17nm)
district@data$lad17nm<- gsub("St ", "St. ", district@data$lad17nm)

district@data$lad17nm <- toTitleCase(district@data$lad17nm)
map <- read_sf("./shapefiles/ladUK/Local_Authority_Districts_December_2017_Full_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")

map$lad17nm <- gsub(", City of", "", map$lad17nm)
map$lad17nm <- gsub(", County of", "", map$lad17nm)
map$lad17nm <- gsub("-", " ", map$lad17nm)
map$lad17nm <- gsub("'", "", map$lad17nm)
map$lad17nm<- gsub("St ", "St. ", map$lad17nm)

map$lad17nm <- toTitleCase(map$lad17nm)

pnts <- afinn_word_LatLong_Tot_PN

pnts_sf <- st_as_sf(pnts, coords = c('longitude', 'latitude'), crs = st_crs(map))

pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, map)), 
  lad17nm = if_else(is.na(intersection), '', map$lad17nm[intersection])
) 

pnts <- na.omit(pnts)

lll <- select(afinn_word_LatLong_Tot_PN, line, year, longitude, latitude)

pnts <- left_join(pnts, lll, by = c("line", "year"))

pnts$year <- as.numeric(pnts$year)

tweets_sentiment_income_pop_latlong <- left_join(pnts, district_data, by = c("lad17nm", "year"))

tweets_sentiment_income_pop_latlong$intersection <- NULL
tweets_sentiment_income_pop_latlong$geometry <- NULL

districtID <- select(district@data, objectid, lad17cd, lad17nm)

tweets_sentiment_income_pop_latlong <- left_join(tweets_sentiment_income_pop_latlong, districtID, by = "lad17nm")

tweets_sentiment_income_pop_latlong_final <- select(tweets_sentiment_income_pop_latlong, 
                                                    line, sentiment, value, longitude, 
                                                    latitude, year, lad17nm, lad17cd, 
                                                    objectid, population, income_tot_mean)

names(tweets_sentiment_income_pop_latlong_final)[names(tweets_sentiment_income_pop_latlong_final) == "objectid"] <- "lad17id"

write.csv(tweets_sentiment_income_pop_latlong_final, "tweets_sentiment_income_pop_latlong_final.csv")
## MAP DISTRICT UK

district@data <- left_join(district@data, district_data, by = "lad17nm")

bins <- c(20000, 30000, 40000, 50000, 60000, 70000, 100000, 150000, 200000)
pal <- colorBin("YlOrRd", domain = district@data$income_tot_mean, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g £",
  district@data$lad17nm, district@data$income_tot_mean
) %>% lapply(htmltools::HTML)

palTweets <- colorFactor(c("red", "limegreen"), domain = c("positive", "negative"))

biggestSentiment <- afinn_word_LatLong_Tot
j = 1
#biggestSentiment = subset(biggestSentiment, ! (biggestSentiment$line % 2) == 0)
countTmp = (count(afinn_word_LatLong_Tot_PN)$n - 1)
for( i in 1:countTmp)
{
  if(biggestSentiment$longitude[i] == biggestSentiment$longitude[i+1])
  {
    j = j+1
  }
}
k = 0
for( i in 1:j)
{
    print(i)
    if(biggestSentiment$longitude[i-k] == biggestSentiment$longitude[i+1-k])
    {
        if(abs(biggestSentiment$value[i-k]) > abs(biggestSentiment$value[i+1-k]))
        {
            biggestSentiment <- biggestSentiment[-(i+1-k),]
            print("Here")
        }
        else
        {
            biggestSentiment <- biggestSentiment[-(i-k),]
        }
      k = k +1
    }
    else
    {
      print("Banna")
    }
}




leaflet(data = biggestSentiment) %>%
  setView(-0.118092, 51.509865, 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addFullscreenControl() %>%
  # addPolygons(data = district) %>% 
  # data = district,
  #             fillColor = ~pal(district@data$income_tot_mean),
  #             weight = 2,
  #             opacity = 1,
  #             color = "white",
  #             dashArray = "2",
  #             fillOpacity = 0.7,
  #             highlight = highlightOptions(
  #               weight = 3,
  #               color = "#666",
  #               dashArray = "",
  #               fillOpacity = 0.7,
  #               bringToFront = FALSE),
  #             label = labels,
  #             labelOptions = labelOptions(
  #               style = list("font-weight" = "normal", padding = "3px 8px"),
  #               textsize = "15px",
  #               direction = "auto")) %>% 
  # addLegend(pal = pal, 
  #           values = district@data$income_tot_mean, 
  #           opacity = 0.7, 
  #           title = "Average Total Income",
  #           position = "bottomright") %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label =biggestSentiment$word, labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
   addCircleMarkers(lng = ~longitude, lat = ~latitude,
                    radius = abs(afinn_word_LatLong_Tot_PN$value)*2,
                    color = ~palTweets(sentiment),
                    stroke = FALSE, 
                    fillOpacity = 1
  )
  
