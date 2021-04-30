###########################################
# Chap 3 Week 4-3 Lecture for Data Mining #
# Data Selection                           # 
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)



## load data 
data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\yrbss_demo.csv")




## filter 
?filter

# using filter
bmi20_data <- data %>% filter(bmi>20)
bmi20_data

# using Bracket method 
# caution ! index is start at 1 
idx <- which(data$bmi > 20)
bmi20_data <- data[idx,]
bmi20_data


# example
sample1<- data %>% filter(bmi <20,stweight<50,sex=="Male")
sample1

sample2<- data %>% filter(!(grade =="9th"),sex=="Female")
sample2

# %in%  # https://zetawiki.com/wiki/R_%ED%8F%AC%ED%95%A8%EC%97%B0%EC%82%B0%EC%9E%90_%25in%25 #
  
sample3 <- data %>% filter(grade %in% c("10th","11th"))
sample3

sample4<- data %>% filter(is.na(bmi))
sample4

sample5 <- data %>%
  filter(race4 %in% "White",race7 %in% "White")
sample5

sample6 <- data %>%
  filter(!is.na(grade))
sample6




## select
?select

# using select
data %>%
  select(record,grade,sex)

# using Bracket method
data[,c("record","grade","sex")]

# example 
sample1 <- data %>% select(record:sex)
sample1

sample2<- data %>% select(one_of(c("age","stweight")))
sample2

sample3<- data %>% select(-(record:sex))
sample3

sample4<- data %>% select(starts_with("r"))
sample4

sample5<- data %>% select(-contains("r"))
sample5

sample6<- data %>% select(-record,everything())
sample6





## get columns name
colnames(data)






## rename the columns
data %>% 
  rename(id = record)

colnames(data)[1] <- "id"
data






## mutate 
newdata <- data %>%
  mutate(height_m = sqrt(stweight / bmi))

newdata %>% select(stweight,bmi,height_m)

# example
data %>%
  mutate(bmi_high =(bmi>30))

data %>%
  mutate(male=(sex =="Male"))

data %>%
  mutate(male =1*(sex=="Male"))

data %>%
  mutate(grade_num=as.numeric(str_remove(grade,"th")))

data %>% 
  mutate(bmi_measure = ifelse(bmi>30,"High bmi","Low bmi"))


?str_remove

## My Data cleaning
data<-data%>% 
  separate(age,c("age"),sep=" ") %>%
  mutate(grades = str_remove(grade,"th")) %>%
  select(-grade)%>%
  mutate(bmi_state = case_when(
    bmi >= 25 ~ "obesity",
    bmi > 23 & bmi < 25 ~ "overweight",
    bmi > 18.5 & bmi < 23 ~ "normal",
    bmi < 18.5 ~ "underweight")
    )
data  

data%>%
 select(starts_with("ra")) %>%
  filter(race4 == race7)

data %>% unite("races",race4,race7,sep=":") %>% select(races)

