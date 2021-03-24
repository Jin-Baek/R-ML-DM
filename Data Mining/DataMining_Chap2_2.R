###########################################
# Chap 2 Week 3-2 Lecture for Data Mining #
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")

## Load library 

# Crawling html 
install.packages("rvest")
library(rvest)

# Permission for crawling path 
install.packages("robotstxt")
library(robotstxt)

# Matching string 
install.packages("stringr")
library(stringr)

# To use tibble
install.packages("tidyverse")
library(tidyverse)




## HTML document example 

simple_html <- "
<html>
  <head>
    <title>Web Scraping</title>
  </head>
  
  <body>
    <h1>Using rvest</h1>
    <p> To get started....</p>
  </body>
  
</html>
"

htmlExample <- read_html(simple_html)
htmlExample

## Web crawling example 1

paths_allowed("http://www.imdb.com")
paths_allowed("http://www.facebook.com")

page <- read_html("https://www.imdb.com/chart/top")
page

# Refer my own crawling study source within Github
titles <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text()

titles

years <- page %>%
  html_nodes(".secondaryInfo") %>%
  html_text() %>%
  str_replace("\\(","") %>%
  str_replace("\\)","") %>%
  as.numeric()

years

scores <- page %>%
  html_nodes(".imdbRating") %>%
  html_text() %>%
  as.numeric()

scores

# Make crawling data to data.frame
data <- data.frame(title=titles,year=years,score=scores)
data
View(data)

# Make crawling data to tibble 
data_tibble <- tibble(title=titles,year=years,score=scores)
data_tibble
glimpse(data_tibble)

# bid rank number to each Movie
data <- data %>%
  mutate(rank=1:nrow(data))

data
