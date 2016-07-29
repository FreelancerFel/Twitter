library(dplyr)
library(stringr)
library(twitteR)
library(sqldf)
library(data.table)
library(tidytext)
#getting data from twitter
setwd("~/GitHub/Twitter")

# getname <- function(x){
#   parts <- unlist(strsplit(x, split = ">"))
#   parts <- parts[2]
#   parts <- unlist(strsplit(parts, split = "<"))
#   parts <- parts[1]
#   return (parts)
# }

#setting up connection to R
Twitter_Authentication<-read.csv('Twitter_Authentication.csv')
setup_twitter_oauth(Twitter_Authentication$consumer_key,
                    Twitter_Authentication$consumer_secret,
                    Twitter_Authentication$access_token,
                    Twitter_Authentication$access_secret)

#Get 2500 of the Latest Clinton Tweets and Convert to Dataframe Format
HillaryClinton<-userTimeline('HillaryClinton', n=2500,
                             includeRts=TRUE, excludeReplies=TRUE) 
HillaryClinton<-twListToDF(HillaryClinton)
str(HillaryClinton)

#Get tweet from June and July 2016
HillaryClinton <-arrange(HillaryClinton, created) %>% 
  filter(created > as.POSIXct("2016-06-01 07:00:00")) %>% 
  filter(created < as.POSIXct("2016-08-01 07:00:00")) %>%
  select(-favorited,-longitude,-latitude,-replyToUID,-replyToSID,-replyToSN,-retweeted)

#Save Tweet Data
if (file.exists('Hillary_Clinton.csv') == FALSE){
  write.csv(HillaryClinton,'Hillary_Clinton.csv')
}

#Seperate Two Set Of Data, One For Tokenize.
HillaryClintonTweetFull <- HillaryClinton
HillaryClintonTokenize <- HillaryClinton

#Tokenize and then remove stop words
HillaryClintonTokenize <-  anti_join(unnest_tokens(HillaryClintonTokenize,Word, text),stop_words,by = c("Word" = "word"))

#count words
HillaryClintonTokenize %>%
  count(Word, sort = TRUE)


#sentiment analysis
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

twittersentiment <- HillaryClintonTokenize %>%
  inner_join(bing,by = c('Word'='word'))

df <- sqldf("select sentiment,count(*) as Sentiment_Score from twittersentiment group by sentiment")


