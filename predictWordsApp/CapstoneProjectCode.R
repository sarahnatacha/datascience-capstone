# Author: Sarah Natacha
# Code for Capstone Project- Coursera
# FINAL PROJECT Predict Nect Word 

#options(java.parameters = "-Xmx2048m")
#library(rJava)
#library("tm") # for text mining and corpus
#library("data.table")
#library("RCurl")
#library("SnowballC")
#library("stringi")
#library("wordcloud")
#library("ggplot2") # for plotting
#library("RWeka") #for tokenization
#library("RColorBrewer")
#library("Matrix")

#load("./data/unigram.Rda")
#load("./data/bigram.Rda")
#load("./data/trigram.Rda")
#load("./data/quadragram.Rda")
#load("./data/quintagram.Rda")


predictingNextWord <- function(inputString, numOfResults) {
  #inputStringClean <- cleanText(inputString)
  inputStringClean <- cleanCorpus(inputString)
  print(inputStringClean)
  inStringLength <- length(unlist(strsplit(inputStringClean, " ")))
  if(inStringLength == 0) {
    print("length0")
    return(predictGram1(inputStringClean, numOfResults))
  }
  
  if(inStringLength == 1) { print("length1")
    return(predictGram2(inputStringClean, numOfResults))
  }
  
  if(inStringLength == 2) { print("length2")
    return(predictGram3(inputStringClean, numOfResults))
  }
  
  if(inStringLength == 3) { print("length3")
    return(predictGram4(inputStringClean, numOfResults))
  }
  
  if(inStringLength > 3) { print("length3more")
    return(predictGram5(inputStringClean, numOfResults))
  }
}


predictGram1 <- function(inString, y) {
  print("predictGram1")
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=0, max=0))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- unigram.df[grepl(paste0("^"), unigram.df$word),]
  #w <- word(prediction$word[1:y],-1)
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram2 <- function(inString, y) { print("predictGram2")
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- bigram.df[grepl(paste0("^",lastngram[1]," "), bigram.df$word),]
  if(nrow(prediction) == 0) { return(predictGram1(inString, y))  }
  #w <- word(prediction$word[1:y],-1)
  #return(prediction)
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram3 <- function(inString, y) { print("predictGram3")
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- trigram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," "), trigram.df$word),]
  if(nrow(prediction) == 0) { return(predictGram2(inString, y)) }
  #w <- word(prediction$word[1:y],-1)
  #return(prediction)
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram4 <- function(inString, y) { print("predictGram4")
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quadragram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," "), quadragram.df$word),]
  if(nrow(prediction) == 0) { return(predictGram3(inString, y)) }
  #w <- word(prediction$word[1:y],-1)
  #return(w)
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram5 <- function(inString, y) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quintagram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," ",lastngram[4]," "), quintagram.df$Terms),]
  if(nrow(prediction) == 0) { return(predictGram4(inString, y)) }
  #w <- word(prediction$word[1:y],-1)
  #return(w)
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

cleanText <- function(inString) {
  badwords <- "data/Terms-to-Block.txt"
  my_stop_words <- readLines(badwords)
  my_stop_words_text <- paste(my_stop_words, collapse=" ")
  incorp <- iconv(inString, from = "UTF-8", to = "latin1")
  incorp <- gsub("(.)\1{3,}", " ", incorp)
  incorpc <- Corpus(VectorSource(incorp))
  incorpc <- tm_map(incorpc, removePunctuation)
  incorpc <- tm_map(incorpc, removeNumbers)
  incorpc <- tm_map(incorpc, stripWhitespace)
  incorpc <- tm_map(incorpc, content_transformer(tolower))
  #incorpc <- tm_map(incorpc, removeWords, my_stop_words_text)
  #incorpc <- tm_map(incorpc, removeWords, stopwords("en"))
  incorpc <- tm_map(incorpc, PlainTextDocument)
  return(incorpc$content[[1]][[1]])
}

cleanCorpus <- function(chrVector) {
  corpus<- Corpus(VectorSource(chrVector)) # create corpus from all the files
  corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
  # remove emails
  RemoveEmails <- function(x) {gsub("\\S+@\\S+", "", x)} 
  corpus <- tm_map(corpus,RemoveEmails)
  # remove URLS
  RemoveUrls <- function(x) {gsub("http[[:alnum:]]*","",x)}
  corpus <- tm_map(corpus,RemoveUrls)
  # Remove Twitter hashtags
  RemoveHashtags <- function(x) {gsub("#[[:alnum:]]*","",x)}
  corpus <- tm_map(corpus,RemoveHashtags)
  # remove Twitter handles (e.g. @username)
  RemoveHandles <- function(x) {gsub("@[[:alnum:]]*","",x)}
  corpus <- tm_map(corpus,RemoveHandles)
  
  corpus <- tm_map(corpus, removeWords, c("rt","pm","p m")) # remove twitter  terms like RT (retweet) and PM (private message)
  
  # remove punctuation, numbers, whitespace, numbers  and stop words
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,removeNumbers)
  #corpus <- tm_map(corpus, removeWords, stopwords("en"))
  #remove bad words 
  badwords <- readLines("./data/Terms-to-Block.txt",encoding= "UTF-8", warn = F)
  corpus <- tm_map(corpus, removeWords, badwords)
  corpus<- tm_map(corpus, PlainTextDocument)
  return(corpus$content[[1]][[1]])
  
}