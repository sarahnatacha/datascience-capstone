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


#source("Task6.R", local=TRUE)
#load('data/ngrams.RData')

shinyServer(function(input, output) {
  
  output$input <- renderText({cleanText(input$text)})
  
  output$prediction <- renderText({
    if(nchar(input$text)) {
      x <- predict(input$text, input$numResults)
      x
    }
  })
  
  output$wordcloudPlot <- renderPlot({
    if(nchar(input$text)) {
      x <- predict(input$text, input$numResults)
      corpus <- tokenize(x)
      myDfm <- dfm(corpus)
      plot(myDfm, max.words = input$numResults, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
    }
  })
})
