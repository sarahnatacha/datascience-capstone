# Author: Sarah Natacha
# Code for Capstone Project- Coursera
# TASK 4- Prediction model

#source("Task2_EDA.R")

# Return Data Frame for each n-grams for prediction use 
#create quadragram
if (!exists("quadragram.df")){
  quadragram.df <- CreateNgram(3," \\r\\n\\t.,;:\"()?!")
}

#create quintagram
if (!exists("quintagram.df")){
  quintagram.df <- CreateNgram(3," \\r\\n\\t.,;:\"()?!")
}

#save ngrams in data files
#save(unigram.df, file="./data/unigram.Rda")
#save(bigram.df, file="./data/bigram.Rda")
#save(trigram.df, file="./data/trigram.Rda")
#save(quadragram.df, file="./data/quadragram.Rda")
#save(quintagram.df, file="./data/quintagram.Rda")


predict1gram <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=0, max=0))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- unigram.df[grepl(paste0("^"), unigram.df$word),]
  return(prediction[1:3])
}

predict2gram <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- bigram.df[grepl(paste0("^",lastngram[1]," "), bigram.df$word),]
  if(nrow(prediction) == 0) { return(predict2gram(inString))  }
  return(prediction[1:3])
}

predict3gram <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- trigram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," "), trigram.df$word),]
  if(nrow(prediction) == 0) { return(predict2gram(inString)) }
  return(prediction[1:3])
}

predict4gram <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quadragram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," "), quadragram.df$word),]
  if(nrow(prediction) == 0) { return(predict3gram(inString)) }
  return(prediction[1:3])
}

predict5gram <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quintagram.df[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," ",lastngram[4]," "), quintagram.df$word),]
  if(nrow(prediction) == 0) { return(predict4gram(inString)) }
  return(prediction[1:3])
}

cleanText <- function(inString) {
  my_stop_words <- readLines("./data/Terms-to-Block.txt",encoding= "UTF-8", warn = F)
  my_stop_words_text <- paste(my_stop_words, collapse=" ")
  incorp <- iconv(inString, from = "UTF-8", to = "latin1")
  incorp <- gsub("(.)\1{3,}", " ", incorp)
  incorpc <- Corpus(VectorSource(incorp))
  incorpc <- tm_map(incorpc, removePunctuation)
  incorpc <- tm_map(incorpc, removeNumbers)
  incorpc <- tm_map(incorpc, stripWhitespace)
  incorpc <- tm_map(incorpc, content_transformer(tolower))
  incorpc <- tm_map(incorpc, removeWords, my_stop_words_text)
  incorpc <- tm_map(incorpc, PlainTextDocument)
  return(incorpc$content[[1]][[1]])
}

predictAll <- function(inputdata) {
  cleandata <- cleanText(inputdata)
  inStringLength <- length(unlist(strsplit(cleandata, " ")))
  
  
  if(inStringLength == 0) {
    return(predict1gram(cleandata))
  }
  
  if(inStringLength == 1) {
    return(predict2gram(cleandata))
  }
  
  
  if(inStringLength == 2) {
    return(predict3gram(cleandata))
  }
  
  
  if(inStringLength == 3) {
    return(predict4gram(cleandata))
  }
  
  if(inStringLength > 3) {
    return(predict5gram(cleandata))
  }
  
}


gc()



#cleandata <- cleanText("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
#inStringLength <- length(unlist(strsplit(cleandata, " ")))
#predict1gram(cleandata)