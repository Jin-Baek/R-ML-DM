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

model_binom %>%
  summ(confint=TRUE,model_info=FALSE,exp=TRUE)

# logistic model2 
model_binom2 <- glm(chd~fibre+height+weight,data=diet,family=binomial)

model_binom2 %>%
  summ(confint=TRUE,model_info=FALSE,exp=TRUE)

model_binom%>%
  glm_coef(labels=model_labels(model_binom)) %>%
  as_hux() %>% set_align(everywhere,2:3,"right")%>%
  theme_pubh(1)%>%
  add_footnote(get_r2(model_binom),font_size=9)

model_labels(model_binom)
model_labels(model_binom2)

model_binom%>%
  glm_coef() 






## Poisson Regression Example 

data(quine)
levels(quine$Eth) <- c("Aboriginal","White")
levels(quine$Sex) <- c("Female","Male")

# dataset 의 feature 들 먼저 파악하기
# Days feature = count variable -> use poisson regression 
View(quine)
?quine

quine <- quine %>%
  var_labels(
    Days="Number of absent days",
    Eth = "Ethnicity",
    Age="Age group"
  )

stat <- quine %>%
  group_by(Eth,Sex,Age) %>%
  summarise(
    n=n(),
    Mean=mean(Days,na.rm=TRUE),
    SD = sd(Days,na.rm=TRUE),
    Median = median(Days,na.rm=TRUE),
    CV =rel_dis(Days)
  ) %>%
  as_hux() %>% theme_pubh(1)

View(stat)

quine %>%
  box_plot(Days~Age|Sex,fill=~Eth)%>%
  axis_labs() %>% gf_labs(fill="")
 
## Modeling 1

model_pois <- glm(Days~ Eth+Sex+Age, family=poisson, data = quine)

model_pois

model_pois %>%
  glm_coef(labels=model_labels(model_pois),se_rob=TRUE)%>%
  as_hux() %>% set_align(everywhere,2:3,"right") %>%
  theme_pubh(1) %>%
  add_footnote(get_r2(model_pois),font_size=9)

## Modeling 2

model_pois2 <- glm(Days~Sex+Age, family=poisson, data = quine)


model_pois2 %>%
  glm_coef(labels=model_labels(model_pois2),se_rob=TRUE)%>%
  as_hux() %>% set_align(everywhere,2:3,"right") %>%
  theme_pubh(1) %>%
  add_footnote(get_r2(model_pois),font_size=9)













