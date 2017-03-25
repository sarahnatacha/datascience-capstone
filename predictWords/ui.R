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
                                 hr(),
                                 textInput("text", "Please enter a phrase below:", ""),
                                 submitButton("Submit")
                               ),
                               mainPanel(
                                 h5("You entered:"),
                                 fluidRow(column(10, verbatimTextOutput("input"))),
                                 h5("The predicted next word is:"),
                                 fluidRow(column(10, verbatimTextOutput("prediction"))),
                                 plotOutput("wordcloudPlot")
                               )
                             )
                    
                    ),
                    tabPanel(p(icon("info"), "About"),
                             hr("This is a Coursera Data Science Capstone project.")
                    )
                  )
))
