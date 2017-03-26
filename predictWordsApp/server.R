#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(RColorBrewer)
library(NLP)


source("CapstoneProjectCode.R", local=TRUE)
load("./data/unigram.Rda")
load("./data/bigram.Rda")
load("./data/trigram.Rda")
load("./data/quadragram.Rda")
load("./data/quintagram.Rda")

shinyServer(function(input, output) {
  
  output$textForNextWordEntered <- renderText({cleanText(input$textForNextWord)})
  
  output$predictionFound <- renderText({
    if(nchar(input$textForNextWord)) {
      x <- predictingNextWord(input$textForNextWord, input$numResults)
      x
    }
    else{
      x <- ""
      x
    }
      
  })
  
})
