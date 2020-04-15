# search for 1000 tweets using the covid19 hashtag
#covid19 <- search_tweets("#covid19", n=1000, include_rts = FALSE, retryonratelimit = TRUE)


library(dplyr)
tweets.covid19 = select(covid19, screen_name, text)
tweets.covid19

# remove retweet entities
tweets.covid19$stripped_text1 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.covid19$text)

# remove at people
#tweets.covid19$stripped_text1 <- gsub("@\\w+", "", tweets.covid19$stripped_text1)

# remove punctuation
#tweets.covid19$stripped_text1 <- gsub("[[:punct:]]", "", tweets.covid19$stripped_text1)

# remove numbers
#tweets.covid19$stripped_text1 <- gsub("[[:digit:]]", "", tweets.covid19$stripped_text1)

# remove html links
tweets.covid19$stripped_text1 <- gsub("http\\w+", "", tweets.covid19$stripped_text1)

# remove all pictwitter
tweets.covid19$stripped_text1 <- gsub("pictwitter\\w+ *", "", tweets.covid19$stripped_text1)

library(tidytext)

#remove punctuation and add id for each tweet
tweets.covid19_stem <- unnest_tokens(select(tweets.covid19, stripped_text1), word, stripped_text1)
head(tweets.covid19_stem)

# remove stop words
cleaned_tweets.covid19 <- anti_join(tweets.covid19_stem, stopwordslangs)
head(cleaned_tweets.covid19)

head(tweets.covid19$text)

#TOP 10 
library(ggplot2)
cleaned_tweets.covid19 %>% count(word, sort = TRUE) %>% top_n(10) %>% mutate(word = reorder(word,n)) %>% ggplot(aes(x=word, y=n)) + geom_col() + xlab(NULL) + coord_flip() + theme_classic() + labs(x= "Count", y="Unique words", title = "Unique words counts found in #covid19 tweets")

# sentiment analysis (bing)
bing_covid = cleaned_tweets.covid19 %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

#plot
bing_covid %>%group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word,n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales="free_y") + labs(title = "Tweets containing #covid19", y="Contribution to sentiment", x=NULL) + coord_flip() + theme_bw()
