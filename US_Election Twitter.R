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
library(RColorBrewer)
library(reshape2)
library(sentimentr)
#getting data from twitter
setwd("~/GitHub/Twitter")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C");

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

HillaryClinton<-data.frame(fread("Hillary_Clinton.csv"))
DonaldTrump<-data.frame(fread("Donald_Trump.csv"))

#Seperate Three Set Of Data, One For Tokenize.
HillaryClinton$Date<-as.Date(as.POSIXct(HillaryClinton$created),format='%m/%d/%Y')
DonaldTrump$Date<-as.Date(as.POSIXct(DonaldTrump$created),format='%m/%d/%Y')

HillaryClintonTweetFull <- HillaryClinton
HillaryClintonTokenize <- HillaryClinton
DonaldTrumpTweetFull <- DonaldTrump
DonaldTrumpTokenize <- DonaldTrump
HillaryClintonTweetSentence <- HillaryClinton
DonaldTrumpTweetSentence <- DonaldTrump


#Tokenize and remove stop words
HillaryClintonTokenize <- anti_join(unnest_tokens(HillaryClintonTokenize,Word, text),stop_words,by = c("Word" = "word"))
DonaldTrumpTokenize <-  anti_join(unnest_tokens(DonaldTrumpTokenize,Word, text),stop_words,by = c("Word" = "word"))

#Number of Tweets per day (Engagement with Twitters)

NumberHCT <- HillaryClintonTweetFull %>% group_by (Date) %>% summarise (count = n())
NumberDTT <- DonaldTrumpTweetFull %>% group_by (Date) %>% summarise (count = n())
NumberHCT$User <- "Hillary"
NumberDTT$User <- "Trump"
TweetsPerDay <- rbind(NumberHCT,NumberDTT)

ggplot(TweetsPerDay,aes(x=Date))+
  geom_bar(stat="identity",aes(y=count,fill=User),position="stack",color="black")+
  labs(title="Tweets between June - July 2016", 
       x = "Twitter Account", y = "Sentiment Score")+
  annotate("text",x=(as.Date("2016-07-01")),y=100, label= paste("Mean Tweets Per Days =",round(mean(TweetsPerDay$count),0),sep=" "),color="black",size=4)+
  scale_fill_manual(values = c("blue", "red"))
  

#wordcloud for number of tweets per days
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score,-lexicon)

wordcloudforhillary<-function(){
  wordcloud <- HillaryClintonTokenize %>% count(Word, sort = TRUE)
  wordcloudsentiment <- wordcloud %>% left_join(bing,by = c('Word'='word'))
  wordcloudsentiment$sentiment[is.na(wordcloudsentiment$sentiment)==TRUE] <- "Neutral"
  wordcloudsentiment <- filter(wordcloudsentiment,Word!="https" , Word!="t.co", Word!="rt")
  wordcloud(words = wordcloudsentiment$Word, freq = wordcloudsentiment$n, scale=c(4,1),
          random.order = FALSE, max.words = 100 ,rot.per=0.35, 
          colors="blue")
}

wordcloudfortrump<-function(){
  wordcloud <- DonaldTrumpTokenize %>% count(Word, sort = TRUE)
  wordcloudsentiment <- wordcloud %>% left_join(bing,by = c('Word'='word'))
  wordcloudsentiment$sentiment[is.na(wordcloudsentiment$sentiment)==TRUE] <- "Neutral"
  wordcloudsentiment <- filter(wordcloudsentiment,Word!="https" , Word!="t.co", Word!="rt")
  wordcloud(words = wordcloudsentiment$Word, freq = wordcloudsentiment$n, scale=c(4,1),
            random.order = FALSE, max.words = 100 ,rot.per=0.35, 
            colors="red")
}

pal2 <- brewer.pal(8,"RdBu")

wordcloudall<-function(){
  AllTokenize <- rbind(DonaldTrumpTokenize,HillaryClintonTokenize)
  wordcloud <- AllTokenize %>% count(Word, sort = TRUE)
  wordcloudsentiment <- wordcloud %>% left_join(bing,by = c('Word'='word'))
  wordcloudsentiment$sentiment[is.na(wordcloudsentiment$sentiment)==TRUE] <- "Neutral"
  wordcloudsentiment <- filter(wordcloudsentiment,Word!="https" , Word!="t.co", Word!="rt")
  wordcloud(words = wordcloudsentiment$Word, freq = wordcloudsentiment$n, scale=c(4,1),
            random.order = FALSE, max.words = 75 ,rot.per=0.35,color=pal2)
}

wordcloudforhillary()
wordcloudfortrump()
wordcloudall()

#sentiment analysis using word matching only 

DonaldTrumpTokenize <- filter(DonaldTrumpTokenize,Word !="trump") #remove the word Trump because it is considered positive in dictionary.
HillaryClintonTokenize <- filter(HillaryClintonTokenize,Word !="trump") 
                      
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

#Time Series Sentiment Score

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
OverTimeSentiment <- inner_join(OverTimeSentiment,TweetsPerDay)

ggplot(OverTimeSentiment,aes(x=Date))+
  geom_line(aes(y=sentiment.x),color="blue",size=1)+
  geom_line(aes(y=sentiment.y),color="red",size=1)+labs(title="Day-by-Day Sentiment of Twitter Post",x="Date",y="Sentiment Score")+
  geom_hline( aes( yintercept=0) ,size=1)+theme(legend.position = "bottom")+
  theme_grey()+annotate("text",x=(as.Date("2016-06-13")),y=-32, label="June 13: Orlando Shooting",color="blue",size=5)+
  annotate("text",x=(as.Date("2016-07-28")),y=61, label="July 28: Hillary Clinton nominates at DNC",color="blue",size=5)+
  annotate("text",x=(as.Date("2016-06-04")),y=14, label="June 3: Paul Ryan endorse Donald Trump",color="red",size=5)+
  annotate("text",x=(as.Date("2016-07-27")),y=-32, label="July 29: Democratic National Convention",color="red",size=5)+
  scale_y_continuous("Sentiment Score",breaks=seq(-30,70,5))

# Common Positive and Negative Words

HillaryClintonTokenizeCount <- HillaryClintonTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>%
  count(Word, sentiment, sort = TRUE) %>%
  ungroup()

HillaryClintonTokenizeCount %>%
  filter(n > 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Words Used by @HillaryClinton", 
       x = "Words", y = "Contribution to Sentiment Score")

DonaldTrumpTokenizeCount <- DonaldTrumpTokenize %>%
  inner_join(bing, by = c('Word'='word')) %>%
  count(Word, sentiment, sort = TRUE) %>%
  ungroup()

DonaldTrumpTokenizeCount %>%
  filter(n > 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Words Used by @RealDonaldTrump", 
       x = "Words", y = "Contribution to Sentiment Score")

#wordcloud for negative and positive words

HillaryClintonTokenize %>%
  inner_join(bing, by =c('Word'='word')) %>%
  count(Word, sentiment, sort = TRUE) %>%
  acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

DonaldTrumpTokenize %>%
  inner_join(bing, by =c('Word'='word')) %>%
  count(Word, sentiment, sort = TRUE) %>%
  acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#sentiment on sentence level using sentimentr package. (Tweet Levels)

donaldtrumpsentence<-sentiment(DonaldTrumpTweetSentence$text) %>% 
  group_by(element_id) %>%
  summarise(sentiment = mean(sentiment),
            wordcount = sum(word_count))

donaldtrumpsentence <- cbind(DonaldTrumpTweetSentence$Date,donaldtrumpsentence)

donaldtrumpsentence$Date <- donaldtrumpsentence$V1
donaldtrumpsentence$V1 <- NULL

HillaryClintonsentence<-sentiment(HillaryClintonTweetSentence$text) %>% 
  group_by(element_id) %>%
  summarise(sentiment = mean(sentiment),
            wordcount = sum(word_count))

HillaryClintonsentence <- cbind(HillaryClintonTweetSentence$Date,HillaryClintonsentence)

HillaryClintonsentence$Date <- HillaryClintonsentence$V1
HillaryClintonsentence$V1 <- NULL

HillaryClintonsentence <-HillaryClintonsentence %>% group_by(Date) %>%
  summarise(sentiment = mean(sentiment))

donaldtrumpsentence <-donaldtrumpsentence %>% group_by(Date) %>%
  summarise(sentiment = mean(sentiment))

OverTimeSentenceSentiment <- full_join(HillaryClintonsentence,donaldtrumpsentence,by = c('Date'='Date'))

ggplot(OverTimeSentenceSentiment,aes(x=Date))+
  geom_line(aes(y=sentiment.x,colour="HillaryClinton"),size=1)+
  geom_line(aes(y=sentiment.y,colour="RealDonaldTrump"),size=1)+
  labs(title="Day-by-Day Sentiment of Twitter Post (SentimentR)",x="Date",y="Sentiment Score")+
  geom_hline( aes( yintercept=0) ,size=1)+
  scale_colour_manual(name="Line Color",values=c(HillaryClinton="blue", RealDonaldTrump="red"))+
  theme_grey()+
  annotate("text",x=(as.Date("2016-06-06")),y=-0.69, label="June 5: Trump attack judge with Mexican Heritage",color="red",size=5)+
  annotate("text",x=(as.Date("2016-06-13")),y=-0.19, label="June 13: Orlando Shooting Response",color="blue",size=5)+
  annotate("text",x=(as.Date("2016-07-09")),y=0.42, label="July 9: Dallas Shooting Response",color="red",size=5)+
  annotate("text",x=(as.Date("2016-07-04")),y=0.25, label="July 4: Independence Day",color="blue",size=5)+
  annotate("text",x=(as.Date("2016-07-28")),y=0.35, label="Democratic National Convention",color="blue",size=5)
  
#Overall Score

OverallSentenceSentiment<-OverTimeSentenceSentiment
OverallSentenceSentiment <- summarise(OverallSentenceSentiment,
                                      RealDonaldTrump = mean(sentiment.y,na.rm = TRUE),
                                      HillaryClinton = mean(sentiment.x,na.rm = TRUE))
OverallSentenceSentiment <- data.frame(t(OverallSentenceSentiment))
OverallSentenceSentiment$x <- rownames(OverallSentenceSentiment)

ggplot(OverallSentenceSentiment,aes(x=x))+
  geom_bar(stat="identity",aes(y=t.OverallSentenceSentiment.,fill=x),color="black")+
  labs(title="Overall Sentiment Score between June - July 2016", 
       x = "Twitter Account", y = "Sentiment Score")+
  scale_fill_manual(values = c("blue", "red"))+theme_calc()+theme(legend.position='none')

#Overlay SentimentR and TidyText???