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

setwd("~/GitHub/Twitter")

library(dplyr)
library(twitteR)
library(wordcloud)
library(tm)
library(ggplot2)
library(sqldf)

getname <- function(x){
  parts <- unlist(strsplit(x, split = ">"))
  parts <- parts[2]
  parts <- unlist(strsplit(parts, split = "<"))
  parts <- parts[1]
  return (parts)
}

#setting up connection to R
Twitter_Authentication<-read.csv('Twitter_Authentication.csv')

setup_twitter_oauth(Twitter_Authentication$consumer_key,
                    Twitter_Authentication$consumer_secret,
                    Twitter_Authentication$access_token,
                    Twitter_Authentication$access_secret)

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

#convert to text corpora for text analytics

data<- read.csv("Jason Bourne _stack.csv")
df<-data.frame(data$text)
df <- VCorpus(DataframeSource(df))

#clean up the text corpora
test <- tm_map(df,stripWhitespace)
test <- tm_map(test,content_transformer(tolower))
test <- tm_map(test,removeWords,stopwords("english"))
test <- tm_map(test,stemDocument)

#Find Frequent  Term
frequentmatrix<-DocumentTermMatrix(test)
frequent<-findFreqTerms(frequentmatrix,lowfreq=20)

#find Association
findAssocs(frequentmatrix, "jason", 0.5)

#count word present ornot
inspect(DocumentTermMatrix(test,list(dictionary = c("gun", "bourn"))))

#simple detection - positive or negative
#getting the score
#still needs a list of word for positive and negative words?
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(dplyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, " ")
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
