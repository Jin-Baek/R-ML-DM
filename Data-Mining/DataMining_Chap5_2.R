###################################################
# Chap 5 Week 9 Lecture for Data Mining           #                                                               
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
library(ggplot2)

## Logistic Regression Example 

data(diet,package="Epi")

??diet

View(diet)

diet <- diet %>%
  mutate(
    chd = factor(chd,labels=c("No CHD","CHD"))
  ) %>%
  var_labels(
    chd = "Coronary Heart Disease",
    fibre = "Fibre intake (10 g/day)"
  )

diet %>% estat(~ fibre|chd) %>%
  as_hux() %>% theme_pubh(2)

# pubh plot
diet %>% na.omit()%>%
  copy_labels(diet) %>%
  box_plot(fibre ~ chd) %>%
  axis_labs()

# ggplot
diet %>% na.omit() %>%
  ggplot(aes(x=chd,y=fibre)) + geom_boxplot() + labs(x="Coronary Heart Disease",y="Fibre intake (10 g/day)")

# logistic model
model_binom <- glm(chd~fibre,data=diet,family=binomial)
model_binom



?glm















