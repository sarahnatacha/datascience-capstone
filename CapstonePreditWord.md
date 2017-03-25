<style>

body {
  background: white;
  background-color: white; 
}

.footer {
    color: black;
    background: #E8E8E8;
    position: fixed;
    top: 90%;
    text-align:center;
    width:100%;
}

.reveal h1, .reveal h2, .reveal h3 {
  word-wrap: normal;
  moz-hyphens: none;
  color: black;
}

</style>


Data Science Captone: Prediction of the next word
========================================================
author: Sarah Natacha
date: March 20th 2017
autosize: true

Project Overview
========================================================
This project is based on files named LOCALE.blogs.txt where LOCALE is the each of the four locales en_US, de_DE, ru_RU and fi_FI. The data is from a corpus called HC Corpora (www.corpora.heliohost.org). 
The data is downloaded from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
A Shiny app that takes as input a phrase (multiple words), one clicks submit, and it predicts the next word.

The algorithm
========================================================
The algorithm predicts the next US word based on the highest probability. The data contains three corpora of news, tweets and blogs with more than 3M lines. 
- We took a 20% sample of the three files and cleaned it from the special characters, emails, white spaces, bad words
- We built 5 n-grams and sorted them by frequency

The code can be found

Application
========================================================
The application is a Shiny app that:
- Predicts the next US word based on the highest probability
- The word or sentence is entered in a text box

The application can be live tested at XXXXX


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Performance and comments
========================================================
The accuracy of the result depends on the sample that was used for the tokenization. In order to render a fast result we used n-grams that was small but if we were using a larger sample it would provide more options.

