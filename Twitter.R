#getting data from twitter
getthaitweet<- function(topic,n){
  timeLine<-searchTwitter(topic, resultType="recent", n=n,lang="th")
  tweet<-twListToDF(timeLine)
  myCorpus <- Corpus(VectorSource(as.character(tweet$text))) 
  a<-c(myCorpus[[1]]$content)
  i <- 2
  while (i < n){
    a <-c(a,myCorpus[[i]]$content)
    i <- i +1
  }
  return (a)
}
#a<-c(myCorpus[[1]]$content,myCorpus[[2]]$content)
#test wordcloud
#wc1<-wordcloud(a[1:1000])

library(dplyr)
library(twitteR)
library(wordcloud)
library(tm)
library(ggplot2)

getname <- function(x){
  parts <- unlist(strsplit(x, split = ">"))
  parts <- parts[2]
  parts <- unlist(strsplit(parts, split = "<"))
  parts <- parts[1]
  return (parts)
}

#setting up connection to R
consumer_key <- "45CL2r4pUBV9lNrbXJYupLXnv"
consumer_secret <- "ZSnKzJZCDUhxOR2U96bh8N2Rz2sNGhnzepHBVXnAEKZj131uzg"
access_token <- "39804408-tGbXR7dhz308jNmz8c6eszlvSGlLh0brYxvozE0nY"
access_secret <- "rzYa7uzAHboQ9TSmOFhylwkZrKONBp6YSElG81PhR4Pw6"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#getting twitter data
search <- function(term,n)
{
  #searchTwitter limit is 1500 every 15 minutes?
  getname <- function(x){
    parts <- unlist(strsplit(x, split = ">"))
    parts <- parts[2]
    parts <- unlist(strsplit(parts, split = "<"))
    parts <- parts[1]
    return (parts)
  }
  
  
  list <- searchTwitter(term, n=n)
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(term,'_stack.csv'))==FALSE) write.csv(df, file=paste(term,'_stack.csv'), row.names=F)
  #merge last access with cumulative file and remove duplicates
  stack <- read.csv(file=paste(term,'_stack.csv'))
  stack <- rbind(stack, df)
  
  stack <- subset(stack, !duplicated(stack$text))
  stack$statusSource <- as.character(stack$statusSource)
  stack$statusSource <- sapply(stack$statusSource,getname)
  
  write.csv(stack, file=paste(term,'_stack.csv'), row.names=F)
}

#convert to corpora

data<- read.csv("Fleur Delacour _stack.csv")
df<-data.frame(data$text)
df <- VCorpus(DataframeSource(df))


stemDocument(df[[1]])
a<-TermDocumentMatrix(df, control = list())
findFreqTerms(a,lowfreq=3)
#simple detection - positive or negative


df[[2]]
stemDocument(df[[1]])
#getting the score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(dplyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    #sentence <- gsub('\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence)
    words <- unlist(word.list," ")
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

#assigning postive and negative dictionary for sentimental analysis
pos.words <- scan('positive-words.txt', what='character', comment.char=';')

#plot by score?  1 to 7 with 7 being the most positive and one being the most negative.