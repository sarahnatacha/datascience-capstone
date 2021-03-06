# Author: Sarah Natacha
# Code for Capstone Project- Coursera
# TASK 1- Cleaning and Sampling


#Large databases comprising of text in a target language are commonly used when generating language models 
#for various purposes. 
#In this exercise, you will use the English database but may consider three other databases in German, 
#Russian and Finnish.

#The goal of this task is to get familiar with the databases and do the necessary cleaning. 
#After this exercise, you should understand what real data looks like and how much effort you need to put 
#into cleaning the data. When you commence on developing a new language, the first thing is to understand the 
#language and its peculiarities with respect to your target. You can learn to read, speak and write the language. 
#Alternatively, you can study data and learn from existing information about the language through literature and the 
#internet. At the very least, you need to understand how the language is written: writing script, existing input methods, 
#some phonetic knowledge, etc.

#Note that the data contain words of offensive and profane meaning. They are left there intentionally to highlight 
#the fact that the developer has to work on them.

#Tasks to accomplish

#1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a 
#file as input and returns a tokenized version of it.
#2. Profanity filtering - removing profanity and other words you do not want to predict.
#Tips, tricks, and hints

#Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily need to load the entire 
#dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a smaller subset 
#of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. You can also loop over 
#each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks 
#at a time. Reading pieces of the file at a time will require the use of a file connection in R. For example, the 
#following code could be used to read the first few lines of the English Twitter dataset:
#con <- file("en_US.twitter.txt", "r") readLines(con, 1) ## Read the first line of text readLines(con, 1) 
# Read the next line of text readLines(con, 5) ## Read in the next 5 lines of text close(con) 
# It's important to close the connection when you are done
#See the ?connections help page for more information.

#Sampling. To reiterate, to build models you don't need to load in and use all of the data. Often relatively few randomly 
#selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all 
#the data. Remember your inference class and how a representative sample can be used to infer facts about a population. 
#You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it 
#out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom 
#function to "flip a biased coin" to determine whether you sample a line of text or not.


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

documents <- Corpus(DirSource("./data/"))
#inspect(documents)

#print the file information
print(DocumentSummary(documents))


## sampling and cleaning
subset.pct = 0.01
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
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
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
rm(documents)
rm(blogs,twitter,news)
rm(s.blogs,s.twitter,s.news)
rm(s.all)