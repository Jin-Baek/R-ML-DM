###########################################
# Chap 2 Week 3-3 Lecture for Data Mining #
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

# Visualize 
library(ggplot2)

library(tidyverse)

## Previous code to crawling Movie information
page <- read_html("https://www.imdb.com/chart/top")

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

data <- data.frame(title=titles,year=years,score=scores)

data <- data %>%
  mutate(rank=1:nrow(data))

#### Analyze #### 

# Which 1995 movie made
data %>%
  filter(year == 1995)

# Which year have the most movie
data %>%
  group_by(year)%>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Which movie's score is more the mean score 
data %>%
  filter(score >= mean(score))

# How to visualize average yearly score
names(data)

ggplot(data,aes(x=year, y=score)) + 
  geom_point() + 
  geom_smooth(method = lm)+
  theme_classic()+
  xlab("year") + 
  ylab("avg_score")

?ggplot

#################### Do Self Checking Quiz !!!#############
