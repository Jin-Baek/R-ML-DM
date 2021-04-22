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
csvDf <- read.csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\small_data.csv")
class(csvDf)
View(csvDf)




## load csv file in tibble 
csvTibble <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\small_data.csv")
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

untidy_data %>%
  separate(meds,c("med_name","dose","interval"),sep=" ") %>%
  mutate(dose=str_remove(dose,"mg"),
         interval=str_remove(interval,"xday")) %>%
  mutate_at(vars(dose:interval),as.numeric)

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
untidy_data





## untidy data example 2

?gather # https://blog.naver.com/juhy9212/220843749610 #
?dmy # https://kuduz.tistory.com/1201 #

untidy_data2 <- tibble(
  name = c("Ana","Bob","Cara"),
  wt_07_01_2018 = c(100,150,140),
  wt_08_01_2018 = c(104,155,138),
  wt_09_01_2018 = c(NA,160,142)
)

##### practice ############

a <- untidy_data2 %>%
  gather(key="date",value="weight",wt_07_01_2018:wt_09_01_2018) %>%
  mutate(weight=replace_na(weight,0))

a<-a%>% 
  mutate(date = str_remove(date,"wt_"),
         date=dmy(date)) 

to_wide <- a %>%
  spread(key=date,value=weight,sep=":")

to_wide

#####################

untidy_data2 <- untidy_data2 %>%
  gather(key="date",value="weight",-name) %>%
  mutate(date = str_remove(date,"wt_"), date = dmy(date))

untidy_data2

untidy_spread <- untidy_data2%>%
  spread(key="date",value="weight",sep="_")

untidy_spread

# group_by 로 각 사람들의 날짜별로 측정한 몸무게의 평균 내기 
untidy_data2<- untidy_data2 %>%
  group_by(name) %>%
  summarise(weight_avg=mean(weight,na.rm=TRUE))

# My_example

my <- tibble(
  name=c("A","B","C","D","E","F"),
  cough_count_2020_04_12 =c(23,4,50,2,0,4),
  cough_count_2020_04_13 =c(33,2,42,1,3,3),
  cough_count_2020_04_14 =c(20,6,NA,2,1,3),
  cough_count_2020_04_15 =c(18,10,30,0,1,5),
  cough_count_2020_04_16 =c(NA,15,27,0,0,1),
  cough_count_2020_04_17 =c(43,25,17,0,0,3),
  cough_count_2020_04_18 =c(37,19,8,NA,2,5),
)

View(my)

my<-my %>% 
  gather(key="date",value="count",-name) %>%
  mutate(date = str_remove(date,"cough_count_"), date=ymd(date))

my %>%
  group_by(name) %>%
  summarise(count=mean(count,na.rm=TRUE))

my %>%
  group_by(date) %>%
  summarise(count=mean(count,na.rm=TRUE))


#################################### How to calculate age from birth ##########################################

people <- tibble(birth=c("27-04-98","30-01-95","18-02-87"))
people

people %>%
  mutate(birth=dmy(birth))

people %>% 
  mutate(life_time = as.period(interval(dmy(birth),today())))%>%
  separate(life_time,c("age"),sep=" " , remove = FALSE) %>%
  mutate(age=as.numeric(str_remove(age,"y"))) 




