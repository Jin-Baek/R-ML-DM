###########################################
# Chap 2 Week 3-2 Lecture for Data Mining #
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")

## Load library 

# Crawling html 
library(rvest)

# Permission for crawling path 
library(robotstxt)

# Matching string 
library(stringr)

# To use tibble
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
  html_nodes(".imdbRating strong") %>%
  html_text() %>%
  as.numeric()

scores

test <- page%>%
  html_nodes(".imdbRating strong")%>%
  html_attr("title") %>%
  str_split(" ",n=6)

test


for(count in test){
  audience<-append(audience,count[4])
}

audience

# Make crawling data to data.frame
data <- data.frame(title=titles,year=years,score=scores,
                   audience=audience)
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
View(data)




###################################################### practice ####################################################

shopping <- read_html("https://search.shopping.naver.com/best100v2/detail.nhn?catId=50000000&listType=B10002")
shopping

paths_allowed("www.naver.com")

name <- shopping  %>%
  html_nodes("._itemSection .cont a")%>%
  html_text()

name

price <- shopping %>%
  html_nodes(".price .num") %>%
  html_text() %>%
  str_remove(",") %>%
  as.numeric()

price

links <- shopping %>%
  html_nodes(".type_normal .cont a") %>%
  html_attr("href")

links

for(link in links){
  
  item_page <- read_html(link)
  
  delivery <- item_page %>%
    html_nodes(".lowestPrice_delivery_price__3f-2l") %>%
    html_text() 
  
  print(delivery)
}












