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

## Linear Regression Application
data(birthwt,package="MASS")
View(birthwt)

str(birthwt)

birthwt<-birthwt %>% 
  mutate(smoke = factor(smoke,labels= c("Non-smoker","Smoker")),
         race = factor(race,labels = c("White","African American","Other"))) %>%
  var_labels(bwt="Birth Weight(g)",
             smoke="Smoking status",
             race="Race")

get_labels(birthwt)
View(birthwt)

birthwt %>%
  group_by(race,smoke) %>%
  summarise(
    n=n(),
    Mean=mean(bwt),
    SD = sd(bwt),
    Median = median(bwt),
    CV = rel_dis(bwt)
  ) %>% 
  as_hux() %>% theme_pubh(1)

?as_hux
?theme_pubh()

birthwt %>%
  box_plot(bwt~smoke, fill=~race) %>%
  axis_labs()

?box_plot
?axis_labs

birthwt %>%
  box_plot(bwt~smoke, fill=~race) 

birthwt %>%
  gen_bst_df(bwt~ race|smoke) %>%
  as_hux()%>% theme_pubh(1)

?gen_bst_df

model_norm <- lm(bwt~smoke + race +age, data=birthwt)
model_norm
summary(model_norm)
?lm

plot(model_norm)
model_norm %>% Anova()

## 나머지 실습은 추후 진행행

