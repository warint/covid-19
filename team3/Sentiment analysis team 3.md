install.packages("twitteR")
install.packages("ROAuth")
install.packages("NLP")
install.packages("syuzhet")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringi")
install.packages("topicmodels")
install.packages("syuzhet")
install.packages("ROAuth")
install.packages("wordcloud")
install.packages("ggplot2")

library("twitteR")
library("ROAuth")
library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("ROAuth")
library("wordcloud")
library("ggplot2")
library("dplyr")
library("tidyr")
library("tidytext")

tweets<-read.csv2(file = "trumptweets.csv", header=T, sep=",")
View(tweets)
tweets_text<-tweets$content
tweets_text<-tweets_text[1:1000]
head(tweets_text)

#convert all text to lower case
tweets_text<- tolower(tweets_text)

# Replace @UserName
tweets_text <- gsub("@\\w+", "",tweets_text)

# Remove punctuation
tweets_text <- gsub("[[:punct:]]", "", tweets_text)

# Remove links
tweets_text <- gsub("http\\w+", "", tweets_text)

# Remove tabs
tweets_text <- gsub("[ |\t]{2,}", "", tweets_text)

# Remove blank spaces at the beginning
tweets_text <- gsub("^ ", "", tweets_text)

# Remove blank spaces at the end
tweets_text <- gsub(" $", "", tweets_text)

head(tweets_text)

#beautiful wordcloud
wordcloud(tweets_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 100)

#getting emotions using in-built function
mysentiment_tweets<-get_nrc_sentiment((tweets_text))

#calculationg total score for each sentiment
Sentimentscores_tweets<-data.frame(colSums(mysentiment_tweets[,]))

names(Sentimentscores_tweets)<-"Score"
Sentimentscores_tweets<-cbind("sentiment"=rownames(Sentimentscores_tweets),Sentimentscores_tweets)
rownames(Sentimentscores_tweets)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_tweets,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the Trump Tweets")
’
