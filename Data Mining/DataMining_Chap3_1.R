#################################################
# Chap 3 Week 4-1 & 4-2 Lecture for Data Mining #
# Introduction Data Cleaning                    #
# Data cleaning practice part 1                 #   
#################################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)

# to use dmy()
library(lubridate)



## data.frame vs tibble  
df <- data.frame(name = c("Minseok","Kimchi","Chulsoo"),
                 rank = 1:3,
                 age = c(34,35,36),
                 city = c("Sejong","Seoul","Suwon"))
df
View(df)   

dfTibble <- tibble(name = c("Minseok","Kimchi","Chulsoo"),
                   rank = 1:3,
                   age = c(34,35,36),
                   city = c("Sejong","Seoul","Suwon"))
dfTibble


## Check variable type 
class(df)
class(dfTibble)




## load csv file in data.frame 
csvDf <- read.csv("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝\\data\\small_data.csv")
class(csvDf)
View(csvDf)




## load csv file in tibble 
csvTibble <- read_csv("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝\\data\\small_data.csv")
class(csvTibble)
View(csvTibble)




## load file in data.frame (Since it is a csv file, it must be separated by ',')
Df <- read.table("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝\\data\\small_data.csv", sep=",")
Df

# If parameter header = True, row counts start from after feature's names
Df <- read.table("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝\\data\\small_data.csv", sep=",",header=TRUE)
Df





## data.frame vs tibble with some basic method

# type of each feature 
glimpse(csvDf)
glimpse(csvTibble)


# similar with glimpse 
str(csvDf)
str(csvTibble)


head(csvDf)
head(csvTibble)


# same with python-describe
summary(csvDf)
summary(csvTibble)





## untidy data example 1
untidy_data <- tibble(
  name = c("Ana","Bob","Cara"),
  meds = c("advil 600mg 2xday","tylenol 650mg 4xday","advil 200mg 3xday")
)

untidy_data

# shape of data
dim(untidy_data)


# split to tidy data 

? str_remove # https://m.blog.naver.com/PostView.nhn?blogId=1stwook&logNo=220669068544&proxyReferer=https:%2F%2Fwww.google.com%2F #
? mutate # https://m.blog.naver.com/pmw9440/221727431154 #
? separate # https://gomguard.tistory.com/229 #
? gsub # https://quantumcomputer.tistory.com/99 #

untidy_data <- untidy_data %>%
  separate(col=meds,into=c("medicine_name","dose_mg","times_per_day"),
           sep=" ") %>%
  mutate(times_per_day = as.numeric(str_remove(times_per_day,"xday")),
         dose_mg = as.numeric(str_remove(dose_mg,"mg")))

View(untidy_data)






## untidy data example 2

?gather # https://blog.naver.com/juhy9212/220843749610 #
?dmy # https://kuduz.tistory.com/1201 #

untidy_data2 <- tibble(
  name = c("Ana","Bob","Cara"),
  wt_07_01_2018 = c(100,150,140),
  wt_08_01_2018 = c(104,155,138),
  wt_09_01_2018 = c(NA,160,142)
)

untidy_data2 <- untidy_data2 %>%
  gather(key="date",value="weight",-name) %>%
  mutate(date = str_remove(date,"wt_"), date = dmy(date))

untidy_data2

