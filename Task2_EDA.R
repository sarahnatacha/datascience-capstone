# Author: Sarah Natacha
# Code for Capstone Project- Coursera
# TASK 2- Tokenizing ngrams

#source("Task1_cleaning.R")


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
  gram.df$cumsum <- cumsum(gram.df$freq)
  gram.df$pct <- (gram.df$freq/sum(gram.df$freq))*100
  gram.df$cumpct <- cumsum(gram.df$pct)
  gram.df
}


CreatePlotNgram <- function(gram.dataframe,title) {
  ggplot(head(gram.dataframe,15), aes(x=reorder(word,-freq), y=freq, fill=freq)) +
    geom_bar(stat="Identity") +
    geom_text(aes(label=freq), vjust = -0.5) +
    ggtitle(title) +
    ylab("Frequency") +
    xlab("Term")  + theme(axis.text.x=element_text(angle=45, hjust=1))
}


if (!exists("corpus.df")){corpus.df <-data.frame(text=unlist(sapply(my.cleancorpus, `[`, "content")), stringsAsFactors=F)}
#create unigram
if (!exists("unigram.df")){
  unigram.df <- CreateNgram(1)
}
#display graph for unigram word frequency  
CreatePlotNgram(unigram.df,"Unigram")  

#create bigram
if (!exists("bigram.df")){
  bigram.df <- CreateNgram(2, " \\r\\n\\t.,;:\"()?!")
}
#display graph for bigram word frequency  
CreatePlotNgram(bigram.df,"Bigram")  

#create trigram
if (!exists("trigram.df")){
  trigram.df <- CreateNgram(3," \\r\\n\\t.,;:\"()?!")
}
#display graph for trigram word frequency  
CreatePlotNgram(trigram.df,"Trigram")  


summarytable.grams <- data.frame(
  "Grams"=c("unigram","Bigram","Trigram"),
  "Example"=c(paste(unigram.df $word[2]),paste(bigram.df$word[2]),paste(trigram.df$word[2])),
  "Count"=c(length(unigram.df $word),length(bigram.df$word),length(trigram.df$word))
)
summarytable.grams
