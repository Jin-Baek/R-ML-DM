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

# str_dect 로 \n가 들어간 부분들이 아닌 것들만 가져옴. return 이 T,F 로 되기 때문에 !T=F 인 것들만 가져오기 때문에 년도만 가져올 수 있음음
years <- as.numeric(years[!str_detect(years,'\n')])
years 

ratings <- page %>%
  html_nodes(".imdbRating") %>%
  html_text() %>%
  as.numeric() 

ratings

links <- page %>%
  html_nodes(".titleColumn a") %>%
  html_attr('href')%>%
  as.character()

links

movie_type <-c()
for( link in links){
  movie <- read_html(paste0("https://www.imdb.com",link))
  
  count <- movie %>%
    html_nodes(".imdbRating a")%>%
    html_text()
  
  type <- movie %>%
    html_nodes(".title_wrapper .subtext a") %>%
    html_text()
  
  movie_type <- append(movie_type,type[1:3])
  #print(type)
  
  #print(count)
}
movie_type

## movieData <- data.frame(title=titles,openDate=years,rating=ratings)
## View(movieData)




############################### practice #############################################


moive_page <- read_html("https://www.imdb.com/chart/tvmeter")

moive_page

rate <- moive_page %>%
  html_nodes(".imdbRating strong")%>%
  html_text() %>%
  as.numeric()

rate


links <- moive_page %>%
  html_nodes(".titleColumn a") %>%
  html_attr("href")

links

review <- c()

for(link in links){
  each_page <- read_html(paste0("https://www.imdb.com",link))
  
  reviews <- each_page %>%
    html_node(".titleReviewbarItemBorder .subText a")%>%
    html_text() 
  
  print(reviews)
  #review <- append(review,reviews)
}

review











