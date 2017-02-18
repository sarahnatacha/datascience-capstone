# Milestone Report 
The goal of this project is to display that we have gotten used to working with the data and that we are on track to create our prediction algorithm. 

# Repository content 
This repository contains:
* README.md 
* Data directory
* R code files used to complete the tasks and test the code
* CapstoneProjectTasks.Rmd describes the tasks to accomplish and questions to consider when working on the project
* Milestone.Rmd: The report can be found on R Pubs (http://rpubs.com/sarahnatacha/capstonereport). It explains the exploratory analysis and the goals for the eventual app and algorithm.  
* Final Project directory: Data Product with a Shiny App and a presentation 


# Tasks

## Task 0- Getting the data
Obtaining the data: In order to process the data in R (http://www.r-project.org/), we first needed to load the necessary packages. Then we downloaded the source data from http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip. For this project, we only used the English (United States) data. 

## Task 1- Cleaning the data
Tokenization: Identify, sample and clean the data from punctuation, numbers. We need to remove Capital letters, Unknown characters, Numbers, Special characters, Extra spaces, Punctuation except dot and single quotes, bad words and other words we do not want to predict.

## Task 2- Exploratory Data Analysis
Create a corpus, a sample of the data that we analyze. The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The goal of this task is to understand the basic relationships observed in the data and prepare to build the first linguistic models.
Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

## Task 3- Modeling
Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed. Publish a Milestone Report Rpub document.

## Task 4- Prediction Model
Use the n-gram and backoff models  built in previous tasks to build and evaluate the predictive model. The goal is to make the model efficient and accurate.
Build a predictive model based on the previous data modeling steps.
Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of the model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

## Task 5- Creative Exploration
Use all the resources we have available to improve the predictive accuracy while reducing computational runtime and model complexity
Explore new models and data to improve the predictive model.
Evaluate the new predictions on both accuracy and efficiency.

## Task 6- Data Product
Create a data product to show off the prediction algorithm via a Shiny app that accepts an n-gram and predicts the next word.

## Task 7
Write 5 slides using RStudio Presenter explaining and promoting the product  
