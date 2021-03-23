#############################
# Chap 2 Self-Checking quiz #
#############################


## Load library 

# Crawling html 
library(rvest)

# Permission for crawling path 
library(robotstxt)

# Matching string 
library(stringr)

# Visualize 
library(ggplot2)

library(tidyverse)

page <- read_html("https://www.imdb.com/chart/tvmeter")

titles <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text()

titles

years <- page %>%
  html_nodes(".lister-list .titleColumn .secondaryInfo") %>%
  html_text() %>%
  str_replace("\\(","") %>%
  str_replace("\\)","") 

years <- as.numeric(years[!str_detect(years,'\n')])
years 

ratings <- page %>%
  html_nodes(".lister-list strong") %>%
  html_text() %>%
  as.numeric() %>%
  append("NA",5)%>%
  append("NA",29)

ratings

links <- page %>%
  html_nodes(".titleColumn a") %>%
  html_attr('href')%>%
  as.character()

links

for( link in links){
  movie <- read_html(paste0("https://www.imdb.com",link))
  
  count <- movie %>%
    html_nodes(".imdbRating a")%>%
    html_text()
  
  print(count)
}

## movieData <- data.frame(title=titles,openDate=years,rating=ratings)
## View(movieData)
