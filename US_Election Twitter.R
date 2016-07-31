library(dplyr)
library(twitteR)
library(sqldf)
library(data.table)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)
#getting data from twitter
setwd("~/GitHub/Twitter")


#setting up connection to R
Twitter_Authentication<-read.csv('Twitter_Authentication.csv')
setup_twitter_oauth(Twitter_Authentication$consumer_key,
                    Twitter_Authentication$consumer_secret,
                    Twitter_Authentication$access_token,
                    Twitter_Authentication$access_secret)

#Get 2500 of the Latest Clinton Tweets and Convert to Dataframe Format

gettweetsfrom<-function(twitterhandle){
  require(twitteR)
  require(dplyr)
  tweets<-userTimeline(twitterhandle, n=2500,
                               includeRts=TRUE, excludeReplies=TRUE) 
  tweets<-twListToDF(tweets)
  
  #Get tweet from June and July 2016
  tweets <-arrange(tweets, created) %>% 
    filter(created > as.POSIXct("2016-06-01 07:00:00")) %>% 
    filter(created < as.POSIXct("2016-08-01 07:00:00")) %>%
    select(-favorited,-longitude,-latitude,-replyToUID,-replyToSID,-replyToSN,-retweeted)

  return (tweets) 
}

HillaryClinton<-gettweetsfrom("HillaryClinton")
DonaldTrump<-gettweetsfrom("realdonaldtrump")

#Save Tweet Data
if (file.exists('Hillary_Clinton.csv') == FALSE){
  write.csv(HillaryClinton,'Hillary_Clinton.csv')
}
if (file.exists('Donald_Trump.csv') == FALSE){
  write.csv(DonaldTrump,'Donald_Trump.csv')
}

#Seperate Two Set Of Data, One For Tokenize.
HillaryClintonTweetFull <- HillaryClinton
HillaryClintonTokenize <- HillaryClinton
DonaldTrumpTweetFull <- DonaldTrump
DonaldTrumpTokenize <- DonaldTrump

#Tokenize and remove stop words
HillaryClintonTokenize <-  anti_join(unnest_tokens(HillaryClintonTokenize,Word, text),stop_words,by = c("Word" = "word"))
DonaldTrumpTokenize <-  anti_join(unnest_tokens(DonaldTrumpTokenize,Word, text),stop_words,by = c("Word" = "word"))

HillaryClintonTokenize$Date<-as.Date(HillaryClintonTokenize$created,format='%m/%d/%Y')
DonaldTrumpTokenize$Date<-as.Date(DonaldTrumpTokenize$created,format='%m/%d/%Y')

#wordcloud
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score,-lexicon)


wordcloudforelection<-function(Textfile){
  wordcloud <- Textfile %>% count(Word, sort = TRUE)
  wordcloudsentiment <- wordcloud %>% left_join(bing,by = c('Word'='word'))
  wordcloudsentiment$sentiment[is.na(wordcloudsentiment$sentiment)==TRUE] <- "Neutral"
  wordcloudsentiment <- filter(wordcloudsentiment,Word!="https" , Word!="t.co", Word!="rt")
  wordcloud(words = wordcloudsentiment$Word, freq = wordcloudsentiment$n, scale=c(4,1),
          random.order = FALSE, max.words = 100 ,rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
}

wordcloudforelection(HillaryClintonTokenize)
wordcloudforelection(DonaldTrumpTokenize)

#sentiment analysis 
                      
twittersentimentHC <- HillaryClintonTokenize %>%
inner_join(bing,by = c('Word'='word'))

twittersentimentDT <- DonaldTrumpTokenize %>%
  inner_join(bing,by = c('Word'='word'))

#Overall Sentiment Score ###
                      
HCsentiment <- HillaryClintonTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
DTsentiment <- DonaldTrumpTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

OverallSentiment <- rbind(HCsentiment,DTsentiment)
OverallSentiment$User <- c("@HillaryClinton","@RealDonaldTrump")

ggplot(OverallSentiment,aes(x=User,y=sentiment))+
  geom_bar(stat="identity",aes(fill=User),color="black")+
  labs(title="Overall Sentiment Score between June - July 2016", 
                    x = "Twitter Account", y = "Sentiment Score")+
  scale_fill_manual(values = c("blue", "red"))+
  geom_label(aes(label=sentiment),fontface=("bold"))+theme_calc()

#By Date Sentiment Score

HCsentimentDate <- HillaryClintonTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(Date, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

DTsentimentDate <- DonaldTrumpTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(Date, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

OverTimeSentiment <- full_join(HCsentimentDate,DTsentimentDate,by = c('Date'='Date'))

ggplot(OverTimeSentiment,aes(x=Date))+
  geom_line(aes(y=sentiment.x),color="blue")+
  geom_line(aes(y=sentiment.y),color="red")+
  theme_calc()

#By Tweet ID

HCsentimentID <- HillaryClintonTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(id, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

DTsentimentID <- DonaldTrumpTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>% 
  count(id, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)



