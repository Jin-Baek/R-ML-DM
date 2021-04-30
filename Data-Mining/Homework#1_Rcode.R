###########################################
# Homework 1 Data Mining                  #
# Wikipedia : Crawling & Data cleansing   #
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝\\Homework1")
getwd()

## Load library
library(rvest)
library(robotstxt)
library(stringr)
library(tidyverse)
library(janitor)
library(glue)

Sys.setlocale("LC_ALL", "English")

#=== 웹 크롤링해오기 ========================================================================
wikiPage <- read_html("https://en.wikipedia.org/wiki/COVID-19_vaccine", encoding="UTF-8")

name <- wikiPage %>%
  html_nodes(".wikitable") %>%
  html_table()

View(name)

#==== 첫 번째 tibble data cleansing ========================================================================
authorized <- name[[3]]

authorized <- authorized %>% 
  separate(colnames(authorized)[1],c("Vaccine",NA),sep="[[:(]") %>%
  separate("Country of origin",c("origin_country_1","origin_country_2"),
           sep=", ") %>%
  rename(Type=colnames(authorized)[3]) %>%
  separate(Type,c("Type",NA),sep="[[:(]") %>%
  separate("Current phase (participants)",c("Current_phase","participants",NA),
           sep="[(:)]") 

View(authorized)
authorized <- authorized[,-c(5,6,9)]
  
authorized <- authorized %>%
  mutate(participants=str_replace(participants,",",""))

authorized <- authorized %>%
  mutate_at(vars(participants),as.numeric)

authorized <- authorized %>%
  mutate(Status="authorized")

au_country2_na <- authorized%>%
  filter(!is.na(origin_country_2))

au_country2_na <- au_country2_na %>%
  select(-origin_country_1) %>%
  rename(origin_country_1 = origin_country_2)

authorized<- authorized%>%
  select(-origin_country_2)

authorized <- bind_rows(authorized,au_country2_na)

## 원래 data 형태 
View(name[[2]])

## cleansing 한 data 형태
View(authorized)

# ========= 두 번째 tibble data cleansing ====================================================================
candidate <- name[[4]]

colnames(candidate)

candidate <- candidate %>%
  separate(colnames(candidate)[1],c("Vaccine",NA),sep="[[:(]") %>%
  separate("Country of origin",c("origin_country_1","origin_country_2"),
             sep=", ")%>%
  separate("Type (technology)",c("Type",NA),sep="[[:(:/]")%>%
  separate("Current phase (participants)design",c("Current_phase","participants",NA),
             sep="[(:)]")

candidate <- candidate[,-c(7,8)]

candidate <- candidate %>%
  drop_na("participants") 

candidate <- candidate %>%
  mutate(participants=str_replace(participants,",",""))

candidate <- candidate %>%
  mutate_at(vars(participants),as.numeric)

candidate <- candidate %>%
  mutate(Status="candidiate")

can_country_2_na <- candidate%>%
  filter(!is.na(origin_country_2))

can_country_2_na <- can_country_2_na %>%
  select(-origin_country_1) %>%
  rename(origin_country_1 = origin_country_2)

candidate<- candidate%>%
  select(-origin_country_2)

candidate <- bind_rows(candidate,can_country_2_na)


## 원래 data 형태 
View(name[[3]])

## cleansing 한 data 형태
View(candidate)  

#======== 두 개의 tibble data 합치고 부족한 부분 cleansing ==================================================

mydata<-bind_rows(authorized,candidate)

mydata <- mydata %>%
  mutate(Current_phase=case_when(
    Current_phase == "Phase III " ~ "3",
    Current_phase == "Phase II " ~ "2",
    Current_phase == "Phase I " ~ "1",
    Current_phase == "Phase II–III " ~ "2~3",
    Current_phase == "Phase I–II " ~ "1~2")
    ) 

mydata <- mydata %>%
  rename(origin_country = origin_country_1)

mydata <- mydata%>%
  mutate(Type=gsub(" ","",Type))

## 최종 data 형태 
View(mydata)

#======== 완성된 데이터 분석하기 =================================================

# 1st:  Vaccine 명이 중첩된 것을 지우고 Vaccine 의 허가 상태로 몇개 씩 있는지 구분 
vaccine_status <- distinct(mydata,Vaccine,Status)
vaccine_status %>% tabyl(Status)


# 2nd: Vaccine 명이 중첩된 것을 지우고 백신들의 current phase가 전체적으로 얼마나 되는지 
phase_count <- distinct(mydata,Vaccine,Current_phase)
phase_count %>% group_by(Current_phase) %>% summarise(count=n())
phase_count %>% tabyl(Current_phase)

# 3rd: 국가별로 몇 개의 백신을 개발했는지 파악하기 (Status,phase 에 상관없이)
mydata %>% 
  group_by(origin_country)%>%
  summarise(count=n()) %>%
  arrange(desc(count))
  

# 4th : 어떤 타입의 백신이 가장 많이 개발되었는지 , 그 타입에서 phase가 높게 나타난 순서대로 정렬 시키기
vaccine_type <- distinct(mydata,Vaccine,Type,Current_phase)
vaccine_type %>% tabyl(Type,Current_phase) %>% adorn_totals("row") 

# 5th : 각 phase 별로 몇명의 참가자를 통해서 임상 시험을 통과하였는지 파악하기
participant <- distinct(mydata,Vaccine,Type,Current_phase,participants)
participant %>% group_by(Current_phase) %>% summarise(mean=mean(participants))