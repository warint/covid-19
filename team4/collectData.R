# load rtweet
library(rtweet)

# ONLY THE FIRST TIME

# store api keys (these are fake example values; replace with your own keys)
api_key <- "e9S36XkRXN6jKzEYHnBJnRohv"
api_secret_key <- "0u6qHq3FAGbYF0GnaZb6YG7KngpqqwMxqUkqK53ZFE3P7Fbd84"
access_token <- "1250086760857702400-6nsXPiUGfJsjBNuATooymoqWKykK1c"
access_token_secret <- "ds9WMzpQIl5UH2v6KmkSd2LBbSsoHL0iywPjU8vLtcfOB"

# authenticate via web browser
token <- create_token(
  app = "Covid19AnalysisWord",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# AFTER THE FIRST TIME 

## check to see if the token is loaded
#get_token()

# search for 1000 tweets using the covid19 hashtag
covid19 <- search_tweets("#covid19", n=1000, include_rts = FALSE, retryonratelimit = TRUE)

# preview tweets data
covid19

# preview users data
users_data(covid19)
