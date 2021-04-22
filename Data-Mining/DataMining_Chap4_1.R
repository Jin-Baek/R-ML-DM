###################################################
# Chap 4 Week 5-1,5-2,5-3 Lecture for Data Mining #
# Variable generation                             # 
# Tabyl Function                                  #
# Combining Data                                  #
###################################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)

# for dmy()
library(lubridate)

# for data cleaning 
library(janitor)
library(glue)

library(stringr)

library(MASS)
library(car)
library(broom)
library(tidyverse)
library(mosaic)
library(ggfortify)
library(huxtable)
library(jtools)
library(latex2exp)
library(pubh)
library(sjlabelled)
library(sjPlot)
library(sjmisc)


## Continuing at chap3_2

## Loading demo data
demo_data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\yrbss_demo.csv")
demo_data

head(demo_data)
tail(demo_data)





## case_when() with mutate

?case_when

demo_data2 <- demo_data%>%
  mutate(bmi_group=case_when(
    bmi<18.5 ~"underweight",
    bmi>=18.5 & bmi<=24.9 ~"normal",
    bmi>24.9 & bmi<=29.9 ~ "overweight",
    bmi>29.9 ~"obese")
  )

demo_data2 %>% select(bmi,bmi_group) %>% head()





## separate : divide column data into several columns 
## unite : merge column

? separate 
? unite

demo_data %>%
  separate(age,c("A","B","C","D","E"),sep=" ") %>%
  rename(age=A) %>%
  select(-B,-C,-D,-E)

demo_data %>%
  separate(age,c("A",NA,NA,NA,NA),sep=" ",remove=FALSE) %>%
  rename(age_transform=A)
  
demo_data %>%
  unite("NewUniteVariable",sex,grade,sep="_")

# example 

demo_data %>%
  separate(age,c("agenum","yrs"),sep=" ") 

demo_data %>%
  separate(age,c("agenum","yrs"),sep=" ",remove=FALSE)

demo_data %>%
  separate(grade,c("grade_n",sep="th"))

demo_data %>%
  separate(race4,c("race4_1","race4_2"),sep="/")

demo_data %>%
  unite("sex_grade",sex,grade,sep="::")

demo_data %>%
  unite("race",race4,race7,sep="|")





## remove all NA 
demo_data %>%
  na.omit()






## distinct : remove rows with duplicated data

data_dups <- tibble(
  name = c("Ana","Bob","Cara","Ana"),
  race= c("Hispanic","Other","White","Hispanic")
)

data_dups

data_dups %>%
  distinct()





## arrange : sort in ascending or descending
demo_data %>%
  arrange(desc(bmi),stweight) %>%
  select(bmi,stweight) %>%
  head(20)




# ===================================================== #
# Chapter3 ppt 40 page : Practice  반드시 풀기       #
# ===================================================== #
# 1
demo_data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\yrbss_demo.csv")
demo_data

# 2
demo_data<-demo_data%>%
  mutate(grade_num=str_remove(grade,"th"),grade_num=as.numeric(grade_num))

# 3
demo_data%>%
  filter(grade_num>=11)

# 4
demo_data<-demo_data%>%
  filter(!is.na(bmi))

demo_data

# 5
demo_data%>%
  mutate(bmi_normal=ifelse(bmi>18.5 & bmi<24.9,1,0))

demo_data%>%
  mutate(bmi_normal=case_when(
    bmi>18.5 & bmi<24.9 ~ 1,
    bmi<18.5 |bmi>24.9 ~ 0))

demo_data%>%
  mutate(bmi_normal=1*(bmi>18.5 & bmi<24.9))

# 6
demo_data %>%
  arrange(desc(grade_num))

# 7
newdata<-demo_data%>%
  mutate(grade_num=str_remove(grade,"th"),grade_num=as.numeric(grade_num))%>%
  filter(grade_num>=11)%>%
  filter(!is.na(bmi)) %>%
  mutate(bmi_normal=ifelse(bmi>18.5 & bmi<24.9,1,0))%>%
  arrange(desc(grade_num))
  
newdata

# ===================================================== #


## Mutating multiple columns at once 


# mutate_if(A,B) : if there is columns satisfy A, apply B 

? mutate_if
demo_data %>%
  mutate_if(is.numeric,as.character)

demo_data %>%
  mutate_if(is.character,tolower) # make into lower alphabet

demo_data %>%
  mutate_if(is.double,round,digit=1) # rounds to 'digit' th decimal place


# mutate_at(vars(A),B...): A columns should apply B  

?mutate_at
demo_data %>%
  mutate_at(vars(age:grade),toupper)

demo_data %>%
  mutate_at(vars(bmi,stweight),log)

demo_data %>%
  mutate_at(vars(contains("race")),str_detect,pattern="White")


# mutate_all 

demo_data %>%
  mutate_all(as.character)







## Selecting & renaming multiple columns

# select_if 
demo_data %>% 
  select_if(is.numeric)

# rename_all
demo_data %>%
  rename_all(toupper)

#rename_if
demo_data %>%
  rename_if(is.character,toupper)

# rename_at
demo_data %>%
  rename_at(vars(contains("race")),toupper)





# ============== Start Chapter 4 ppt =================== #




## Frequency tables : janitor package's tabyl function



# tabyl example
tab<-demo_data %>%
  tabyl(grade)

tab%>%
  mutate(percent_100 = percent*100,
         valid_percent_100=valid_percent*100)%>%
  mutate_if(is.double,round,digit=2)%>%
  select(grade,n,contains("100"))

demo_data %>%
  tabyl(grade) %>%
  select(-n)

demo_data %>%
  tabyl(grade) %>%
  adorn_totals("row") %>% # add all frequency 
  adorn_pct_formatting(digits=2) # set percent with digit decimal 



# 2 x 2 tabyls example : different with single tabyls

# first variable = norm , second variable = follow
# similar with arrange
demo_data %>%
  tabyl(grade,sex)

demo_data %>%
  tabyl(grade,sex,race4)

demo_data %>% tabyl(grade,sex) %>%
  adorn_percentages(denominator = "col")
  # percentages of sex based on grade 

demo_data %>% tabyl(grade,sex) %>%
  adorn_percentages(denominator = "col") %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

demo_data %>% tabyl(grade,sex) %>% 
  adorn_percentages(denominator = "col") %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()





## summarize 
demo_data %>%
  tabyl(grade,sex)

demo_data %>%
  summarize(bmi_mean=mean(bmi,na.rm=T),
            bmi_sd = sd(bmi,na.rm=T))


demo_data%>%
  group_by(grade) %>%
  summarize(bmi_mean=mean(bmi,na.rm=T),
            bmi_sd=sd(bmi,na.rm=T))




## group_by
demo_data %>%
  group_by(grade) %>%
  summarize(bmi_mean=mean(bmi,na.rm=T),
            bmi_sd = sd(bmi,na.rm=T))

demo_data %>%
  group_by(grade) %>%
  summarize(n_per_group=n(),
            bmi_mean=mean(bmi,na.rm=T),
            bmi_sd = sd(bmi,na.rm=T))

# 아래 두 식은 같은 결과를 보인다. 
demo_data %>%
  summarize_at(vars(bmi,stweight),funs(mean,sd),na.rm=T)

demo_data %>%
  summarize(bmi_mean=mean(bmi,na.rm=T),
            bmi_sd=sd(bmi,na.rm=T),
            stweight_mean=mean(stweight,na.rm=T),
            stweight_sd=sd(stweight,na.rm=T))



## Combining data

# create tibble
data1<- tibble(id=1:2,name=c("Nina","Yi"),height=c(2,1),
               age=c(4,2))

data2<- tibble(id=7:9,name=c("Bo","AI","Juan"),
               height=c(2,1.7,1.8),years=c(3,1,2))
data1
data2


# vertical way - 굳이 column 명을 맞춰 줄 필요가 없다.
bind_rows(data1,data2)


# horizontal way : need matching column
bind_cols(data1,data2)



#============================================================================================#
#1
data <- read_csv("G:\\내 드라이브\\2021-1\\데이터마이닝\\data\\demo_data.csv")
data
#2
data <- data %>% 
  mutate(grade_num=as.numeric(str_remove(grade,"th")))
data
#3
data %>%
  filter(grade_num >= 11)
#4
data<-data %>%
  drop_na(bmi)

#5
data <- data %>% 
  mutate(bmi_normal = 1 * (bmi>18.5 & bmi < 24.9))

data %>% 
  mutate(bmi_normal = ifelse(bmi>18.5 & bmi<24.9,1,0))

#6
data %>%
  arrange(desc(grade_num))
#7
newdata<- data
newdata

newdata<-newdata %>%
  var_labels(grade='your grade')

View(newdata)
