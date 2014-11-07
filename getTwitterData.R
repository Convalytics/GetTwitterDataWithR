# Get twitter data.
install.packages("twitteR")
require(twitteR)
# get app settings from https://apps.twitter.com  ... create new app.

library(RCurl) 
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "BrB9s2o7gv6SN6nQQOtLk2zEy"
consumerSecret <- "8LbAgzNgBLJDgWXyYCylbzyuoi5ao7GWI9O0nA8Q3txLqy3Hfs"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()

# Code below should return TRUE if authorized.
registerTwitterOAuth(twitCred)


##############################
## From: http://applyr.blogspot.com/2014/11/tweeting-at-imgc14-conference.html?m=1


install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("dplyr")
install.packages("ggplot2")
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

# load tweets
download.file("ftp://ftp.jax.org/petrs/other/IMGC14.rds", destfile = "IMGC14_tweets.rds", mode="wb")
tweet <- readRDS("IMGC14_tweets.rds")

# extract time, user, fav.count and retweet count
dt <- data.frame(time = sapply(tweet, function(x) as.character(x$created)), 
                 user = sapply(tweet, function(x) x$screenName),
                 nRt  = sapply(tweet, function(x) x$retweetCount),
                 nFav = sapply(tweet, function(x) x$favoriteCount))
# time zone change
dt$time <-  as.POSIXct(format(as.POSIXct(dt$time, tz="UTC"), tz="America/Thunder_Bay"))

### Number of tweets per user
dt %>% group_by(user) %>% 
  summarise(n=n()) %>%
  arrange(-n, user) %>%
  filter(n>1) %>%
  ggplot(aes(x=reorder(user,n),y=n)) +
  geom_bar(stat = "identity") + 
  ylab("Number of tweets") + 
  xlab("User") +
  ggtitle("Users with at least two #IMGC14 tweets") +
  coord_flip() +
  theme(panel.border = element_rect(colour = 'darkgrey', fill = NA)) 
ggsave("users.jpeg")

# number of users (79) 
n_distinct(dt$user)

# number of tweets (1546)
nrow(dt)

# number of Steve Munger's tweets (679)
dt %>% group_by(user) %>% 
  summarise(n=n()) %>%
  arrange(-n, user) %>%
  head(n=1)


### Time distribution of tweets

ggplot(aes(x=time), data=dt) + 
  geom_density(fill="#1B9E77", col="#1B9E77", adjust=0.2) +
  xlim(as.POSIXct("2014-10-26 4:00:00 EDT"), as.POSIXct("2014-10-30 04:00:00 EDT")) +
  xlab("Time") + ylab("Intensity of tweeting") + ggtitle("Twitter timestamps") +
  theme(panel.border = element_rect(colour = 'darkgrey', fill = NA)) 


### Most retweeted and favorited tweets
tweet[which.max(dt$nRt)]
tweet[which.max(dt$nFav)]

### WORD CLOUD

# make corpus
tweet_text <- sapply(tweet, function(x) x$getText())
tweet_text_corpus <- Corpus(VectorSource(tweet_text))

# clean corpus
tweet_text_corpus <- tm_map(tweet_text_corpus, content_transformer(tolower))
tweet_text_corpus <- tm_map(tweet_text_corpus, removePunctuation)
tweet_text_corpus <- tm_map(tweet_text_corpus, function(x) removeWords(x,stopwords("english")))
tweet_text_corpus <- tm_map(tweet_text_corpus, function(x) removeWords(x,c("imgc14", "stevemunger", "redsoxgal0407", "mice")))

# plot wordcloud
wordcloud(tweet_text_corpus, scale=c(3.5,0.5), max.words=100, min.freq=2,
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))



tweets = searchTwitter("bichicago", n=2000)
