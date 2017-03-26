#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
                  
                  # Application title
                  titlePanel("Next Word Prediction"),
                  
                  tabsetPanel(
                    tabPanel(p(icon("line-chart"), "Prediction"),
                             hr(),
                             sidebarLayout(
                               sidebarPanel(
                                 hr(), selectInput("numResults", "Choose the number of results :",
                                                   list(1,2,3)),
                                 textInput("textForNextWord", "Please enter a phrase below:", ""),
                                 submitButton("Submit")
                               ),
                               mainPanel(
                                 h5("You entered:"),
                                 fluidRow(column(10, verbatimTextOutput("textForNextWordEntered"))),
                                 h5("Predicted next word(s):"),
                                 fluidRow(column(10, verbatimTextOutput("predictionFound")))
                               )
                             )
                    
                    ),
                    tabPanel(p(icon("info"), "About"),
                             hr("This is a Coursera Data Science Capstone project. The shiny application is predicting the next word of a sentence.")
                    )
                  )
))
