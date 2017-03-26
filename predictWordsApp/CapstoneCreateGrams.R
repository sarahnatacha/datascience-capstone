# Author: Sarah Natacha
# Code for Capstone Project- Coursera
# TASK 1- Cleaning and Sampling


#install.packages("tm",dependencies = TRUE) 
#install.packages("data.table",dependencies = TRUE)
#install.packages("RCurl",dependencies = TRUE)
#install.packages("SnowballC",dependencies = TRUE)
#install.packages("stringi",dependencies = TRUE)
#install.packages("wordcloud",dependencies = TRUE)
#install.packages("ggplot2",dependencies = TRUE) 
#install.packages("RWeka",dependencies = TRUE) 
#install.packages("RColorBrewer",dependencies = TRUE)
#install.packages("Matrix",dependencies = TRUE)

# clean environment with rm(list=ls())

options(java.parameters = "-Xmx2048m")
library(rJava)
library("tm") # for text mining and corpus
library("data.table")
library("RCurl")
library("SnowballC")
library("stringi")
library("wordcloud")
library("ggplot2") # for plotting
library("RWeka") #for tokenization
library("RColorBrewer")
library("Matrix")

setwd(".") #set the directory to the current one

#functions for the analysis of the files
if (!exists("DocumentCount") || !exists("DocumentSummary")) {
  DocumentCount <- function(doc) {
    c(length(doc), sum(stri_count(doc, regex='\\S+')), sum(nchar(doc)))
  }
  DocumentSummary <- function(docs) {
    summary.table <- data.frame()
    row.names <-c()
    for (i in seq(docs)) {
      summary.table <- rbind(summary.table, DocumentCount(docs[[i]]$content))
      row.names <- c(row.names, docs[[i]]$meta$id)
    }
    row.names(summary.table) <- row.names
    names(summary.table) <- c("Lines", "Words", "Chars")
    summary.table
  }
}
#get the data
#open connections to the files in order to read them completely
if (!exists("twitter")) {
  con_twitter <- file("./data/en_US.twitter.txt", open = "rb")
  con_news <- file("./data/en_US.news.txt", open = "rb")
  con_blogs <- file("./data/en_US.blogs.txt", open = "rb")
  
  twitter <- readLines(con_twitter, warn = F)
  news <- readLines(con_news,encoding= "UTF-8", warn = F)
  blogs <- readLines(con_blogs,encoding= "UTF-8", warn = F)
  badwords <- readLines("./data/Terms-to-Block.txt",encoding= "UTF-8", warn = F)
  
  # close the connection to free the memory
  close(con_twitter)
  close(con_news)
  close(con_blogs)
  rm (con_twitter,con_news,con_blogs) #delete the connection objects
}

#documents <- Corpus(DirSource("./data/"))
#inspect(documents)

#print the file information
#print(DocumentSummary(documents))


## sampling and cleaning
subset.pct = 0.05
s.blogs <- blogs[sample(length(blogs),subset.pct*length(blogs))]
s.news <- news[sample(length(news),subset.pct*length(news))]
s.twitter <- twitter[sample(length(twitter),subset.pct*length(twitter))]


CleanCorpus <- function(chrVector) {
  corpus<- Corpus(VectorSource(list(chrVector))) # create corpus from all the files
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
  corpus <- tm_map(corpus, removeWords, badwords)
  corpus<- tm_map(corpus, PlainTextDocument)
  
  corpus
}

## create a clean corpus
s.all<- c(s.blogs,s.news,s.twitter)
my.cleancorpus<- CleanCorpus(s.all)

#remove unnessary objects
rm(badwords)
#rm(documents)
rm(blogs,twitter,news)
rm(s.blogs,s.twitter,s.news)
rm(s.all)

#PART 2 Create grams


CreateNgram <- function(minmaxgram,delimiterin=NULL) {
  if (!is.null(delimiterin)){
    ngram <- NGramTokenizer(corpus.df, Weka_control(min = minmaxgram, max = minmaxgram,delimiters =delimiterin))
  }else{
    ngram <- NGramTokenizer(corpus.df, Weka_control(min = minmaxgram, max = minmaxgram))
  }
  
  gram.df <- data.frame(V1 = as.vector(names(table(unlist(ngram)))), V2 = as.numeric(table(unlist(ngram))))
  rm(ngram) # remove the ngram that will not be used anymore
  names(gram.df) <- c("word","freq")
  gram.df <- gram.df[with(gram.df, order(-gram.df$freq)),]
  row.names(gram.df) <- NULL
  #gram.df$cumsum <- cumsum(gram.df$freq)
  #gram.df$pct <- (gram.df$freq/sum(gram.df$freq))*100
  #gram.df$cumpct <- cumsum(gram.df$pct)
  gram.df
}


if (!exists("corpus.df")){corpus.df <-data.frame(text=unlist(sapply(my.cleancorpus, `[`, "content")), stringsAsFactors=F)}
#create unigram
if (!exists("unigram.df")){
  unigram.df <- CreateNgram(1)
}

#create bigram
if (!exists("bigram.df")){
  #bigram.df <- CreateNgram(2)
  bigram.df <- CreateNgram(2, " \\r\\n\\t.,;:\"()?!")
  
}

#create trigram
if (!exists("trigram.df")){
  #trigram.df <- CreateNgram(3)
  trigram.df <- CreateNgram(3," \\r\\n\\t.,;:\"()?!")
  
}

if (!exists("quadragram.df")){
  quadragram.df <- CreateNgram(4," \\r\\n\\t.,;:\"()?!")
}

#create quintagram
if (!exists("quintagram.df")){
  quintagram.df <- CreateNgram(5," \\r\\n\\t.,;:\"()?!")
}


#save ngrams in data files
save(unigram.df, file="./data/unigram.Rda")
save(bigram.df, file="./data/bigram.Rda")
save(trigram.df, file="./data/trigram.Rda")
save(quadragram.df, file="./data/quadragram.Rda")
save(quintagram.df, file="./data/quintagram.Rda")
