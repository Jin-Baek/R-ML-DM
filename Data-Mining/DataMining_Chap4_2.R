###################################################
# Chap 4 Week 6-1,6-2 Lecture for Data Mining     # 
# DataMerging                                     # 
# DataCleansing                                   #                                  
###################################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)

# for data cleaning 
library(janitor)
library(glue)

library(stringr)
library(lubridate)


###### Join 한국어 정리 ###############################################
# join : 피처명을 다 적고 기준 열에서 join 종류에 따라 특정 행들 적고 
# 나머지 부분 채우기                                                  
#                                                                   
# left_join : 모든 피처 + 왼쪽 행들                                   
# right_join : 모든 피처 + 오른쪽 행들                               
# inner_join : 모든 피처 + 겹치는 행만                                
# full_join : 모든 피처 + 모든 행들                                   
#######################################################################



## Join Example

df1 <- tibble(a=c(1,2),b=2:1,c=c(3,6),d=4:3)
df2 <- tibble(a=c(1,3),c=10:11,d=c(12,11))

df1
df2

left_join(df1,df2)
left_join(df1,df2,by=c("a","c"),suffix=c(":left",":right"))
right_join(df1,df2)
inner_join(df1,df2)
full_join(df1,df2)




## Join Example with real data

demo_data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\yrbss_demo.csv")
colnames(demo_data)
View(demo_data)
 
qn_data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\yrbss_qn.csv")
colnames(qn_data)
View(qn_data)

intersect(colnames(demo_data),colnames(qn_data))

#example
te<- c(1,2,3,4,5)
te2<- c(1,7,8,9,10)
intersect(te,te2)

left_merged_data <- left_join(demo_data,qn_data,by="record")
full_merged_data <- full_join(demo_data,qn_data)

View(left_merged_data) 
View(full_merged_data)

dim(demo_data); dim(qn_data); dim(left_merged_data); dim(full_merged_data)

#####################################################################################
# Wide vs long data
#
# wide 는 흔히 우리가 알고 있는 데이터 형태 
#
# long 은 wide 에서 피처명이 시간별,기간별,횟수 처럼 시계열 데이터일 때 
#      더 적은 피처로 묶어버린 데이터 형태
#####################################################################################



## From the wide form to long form data

?gather

BP_wide <- tibble(id=letters[1:4],
                  sex=c("F","M","M","F"),
                  SBP_v1=c(130,120,130,119),
                  SBP_v2=c(110,116,136,106),
                  SBP_v3=c(112,122,138,118))

BP_wide

BP_wide %>%
  gather(key="visit",value="SBP",SBP_v1:SBP_v3)%>%
  mutate(visit=str_remove(visit,"SBP_v")) %>%
  spread(key="visit",value="SBP",sep=".") 


  


# gather 하려는 columns 을 선택하거나 
BP_long <- BP_wide %>%
  gather(key = "visit",value = "SBP",SBP_v1:SBP_v3) 

# gather 안하는 colums 을 제외하거나
BP_long2 <- BP_wide %>%
  gather(key = "visit",value = "SBP",-id,-sex) 

BP_long
BP_long2


## Make visit-column leaves only number of visit

# first way
BP_long1 <- BP_long %>%
  separate(visit,c(NA,"visit"),sep="_v") %>%
  mutate_at(vars(visit),as.numeric)
  

BP_long1

# second way
BP_long2 <- BP_long %>%
  mutate(visit=str_replace(visit,"SBP_v",""))

BP_long2





## From the long form to wide form data 

?spread

BP_long %>%
  spread(key="visit",value="SBP", sep="_")

BP_long1 %>%
  spread(key="visit",value="SBP", sep="_")




## ppt p.33 practice code
DBP_wide <- tibble(id=letters[1:4],
                   sex=c("F","M","M","F"),
                   v1.DBP=c(88,84,102,70),
                   v2.DBP=c(78,78,96,76),
                   v3.DBP=c(94,82,94,74),
                   age=c(23,56,41,38)
                   )

DBP_wide

#1
DBP_long <- DBP_wide %>% 
  gather(key=visit,value = DBP,v1.DBP:v3.DBP)

DBP_long

#2
DBP_long <- DBP_long %>%
  separate(visit,c("visit",NA),sep=".D") %>%
  mutate(visit=str_replace(visit,"v","")) 
  

#3
DBP_wide2 <- DBP_long%>%
  spread(key=visit,value = DBP,sep=".")

DBP_wide2

#4
DBP_long 
BP_long2 

full_join(DBP_long,BP_long2)
left_join(DBP_long,BP_long2)


## Removing missing data : drop_na()

mydata <- tibble(id=7:9,
                 name=c("Bo","Al","Juan"),
                 height=c(2,NA,1.8),
                 years=c(51,35,NA))

mydata

mydata %>%
  drop_na()

mydata %>%
  drop_na(height)

mydata %>%
  filter(is.na(height))

## Replace NA s with another value : replace_na()

mydata %>%
  mutate(height=replace_na(height,"Unknown"),
         years=replace_na(years,0))


# replace_na() example 

qn_data

qn_data<-qn_data %>%
  mutate_at(vars(starts_with("q")),
            .funs=list(~replace_na(.,"No answer")))

qn_data <- qn_data %>% add_column(qn_yes =1)
qn_data

all_data <- left_join(demo_data,qn_data)

all_data

all_data %>%
  tabyl(qn_yes)

all_data %>%
  tabyl(qn_yes,grade)

all_data %>%
  tabyl(qn_yes,sex)

all_data %>%
  tabyl(q8,grade)





## convert to na_if()

all_data %>% tabyl(race4)

all_data %>%
  mutate(race4=na_if(race4,"All other races")) %>%
  tabyl(race4)

# na_if() example - unfinished

smalldata <- read_csv("data/small_data.csv",na=c("","9999","NA"))

smalldata
smalldata <- read_csv("data/small_data.csv")





## Use stringr package

## str_detect()
mydata <- tibble(name=c("J.M.","Ella","Jay"),state=c("New Mexico",
                                                    "New York","Oregon"))
mydata

mydata %>% filter(str_detect(name,"J"))

mydata %>%
  mutate(new_state = str_detect(state,"New"))

## str_replace_all()
mydata %>% 
  mutate(state_old = str_replace_all(state,"New","Old")) 
 
mydata %>% mutate(
  name2=str_replace(name,"l","-"),
  name3=str_replace_all(name,"l","-"),
  name4=str_replace_all(name,fixed("."),"")
)





##  Paste string together with glue()

all_data %>%
  mutate(info=glue("Stduent {record} is {age} with BMI = {round(bmi,1)}"))%>%
  select(record,info) %>% head(5)

# glue() example 

demo_data %>%
  group_by(sex) %>%
  summarize(n_sex = n(),
            bmi_mean = mean(bmi,na.rm= TRUE ),
            bmi_sd = sd(bmi,na.rm= TRUE )) %>%
  mutate(bmi_mean_se = glue( "{round(bmi_mean,1)} ({signif(bmi_sd/sqrt(n_sex),2)})" ))










## lubridate package : wrangle dates

timedata <-
  tibble(name = c( "Yi"  , "Bo" , "DJ" ),
         dob=c( "10/31/1952" , "1/12/1984" , "2/02/2002" ))

timedata %>%
  mutate(dob_date = mdy(dob),
         dob_wrong = dmy(dob))

timedata

# Math with dates

timedata %>% mutate(
  dob = mdy(dob),
  dob_year = year(dob),
  time_since_birth = interval(dob ,today()),
  age = time_since_birth %/% years( 1 ),
  dobplus = dob + days( 10 )
)












## janitor package

# clean_names()

mydata <- tibble( "First Name" = c( "Yi" , "DJ" ), "last init" = c( "C" , "R" ),
                  "% in" = c( 0.1 , 0.5 ), "ñ$$$" = 1 : 2 , " " = 3 : 2 , " hi" =c( "a" , "b" ),
                  "null" =c( NA , NA ))
mydata

mydata %>% clean_names() 

mydata %>% clean_names() %>%
  remove_empty(c( "rows" , "cols" ))

# example 
library (readxl)
read_excel( "data/messy_names.xlsx" , .name_repair = janitor::make_clean_names)

tmp <- read_excel( "data/messy_names.xlsx")
tmp
tmp %>% clean_names()
 





# chapter 4 p.44 
messy_data <- tibble(NAME=c("J N","A C","D E"),
                     'months follow up' = c("",10,11),
                     'Date of visit' = c("July 31, 2003", "Nov 12, 2005","Aug 3, 2007"))

messy_data

#1
messy_data <- messy_data %>%
  clean_names()

messy_data

#2
messy_data<-messy_data %>%
  mutate(months_follow_up=na_if(months_follow_up,""))

messy_data

#3
messy_data <- messy_data %>%
  mutate_at(vars(months_follow_up),as.numeric)

messy_data

#4
messy_data <- messy_data %>%
  mutate(date_of_visit=str_replace(date_of_visit," ","")) %>%
  mutate(date_of_visit=mdy(date_of_visit))


messy_data

#5
messy_data <- messy_data %>%
  mutate(date_last_visit= date_of_visit+months(1))

messy_data

#6
messy_data <- messy_data %>%
  drop_na()

messy_data

#7
messy_data<-messy_data %>% 
  mutate(name=str_replace(name," ",""))

messy_data


as.period(interval(mdy("08-11-1998"),today()))
mdy("08-11-1998")
today()

as.period(interval(mdy("08-11-1998"),today()))

attributes(messy_data)



