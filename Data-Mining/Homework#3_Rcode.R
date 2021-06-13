###################################################
# Homework 3 Data - Mining                        # 
###################################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)
library(datarium)
library(caret) 

library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)
library(leaps)
library(sjlabelled)

# lda qda using train() method
library(e1071)
library(naivebayes)
library(xgboost)

# fda mda
library(earth)
library(mda)

library(lars)

# Stochastic
library(gbm)


######################### Data set 1 : German Credit Data  #############################

# 1. Load the data
data(GermanCredit)
data <- GermanCredit

#View(data)
glimpse(data)
dim(data)

# 2. Cleansing the data ( understand the meaning of variable , data type, handle NA , factorize )
 
colnames(data)

## Remove meaningless variable
# We can not remove variable because we do not know which variable is meaningless.
# I will perform Feature selection later
# In here, just remove variable that I think meaningless
data<-data%>%
  dplyr::select(-c(starts_with("Personal"),starts_with("Purpose"),Age))

## Pre-processing 
# When using classification model like svm,knn,lda,qda... we need to make independent variable to
# numeric data. So we usually change variable type with "one hot encoding"
# But, in this data set, pre-processing is already done. 


## Remove NA because of modeling
dim(data)
data <- data %>%
  na.omit()

## Final data set
cleanData <- data
str(cleanData)

cleanData%>%group_by(Class)%>%
  summarise(n=n())

# 3. Shuffle data

randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]

# 4. Divide data into train and test
## Using sample() method, we already mix the data 
createTrainTestSet <- function(data,propTrain=0.8){
  propTest = 1- propTrain
  
  total_row <- propTest * nrow(data)
  testSetIdx <- 1:total_row
  
  output <- list(testSet=data.frame(data[testSetIdx,]),
                 trainSet =data.frame(data[-testSetIdx,]))
  return(output)
}

result = createTrainTestSet(cleanData,0.8)
trainSet <- result$trainSet
testSet <- result$testSet

dim(testSet)
dim(trainSet)

# 5. Feature selection 

## LDA,QDA model perform feature selection with same model
## Other model(svm,knn...) perform feature selection with random forest model
## The reason is specify on report

rfeparam <- rfeControl(functions = rfFuncs,method = "repeatedcv",number = 10,repeats = 3)
best_feature<-rfe(Class~.,data=trainSet,rfeControl = rfeparam)
best_feature
best_feature$optVariables

# 6. Generating Cross-validation & Modeling 

### All model apply same CV (fold=10, repeat=3) ###
train_control <- trainControl(method = "repeatedcv",number=10,repeats=3)

### Decision Tree ###
dtModel <- train(Class~.,trainSet,method="rpart",trControl=train_control)
prediction_dt <- predict(dtModel,newdata=testSet)
prediction_dt
confusion_dt <- confusionMatrix(prediction_dt,testSet$Class)
confusion_dt

### LDA ###

## Feature selection 
rfeparam <- rfeControl(functions = caretFuncs,method = "repeatedcv",number = 10,repeats=3)
best_feature<-rfe(Class~.,data=trainSet,rfeControl = rfeparam,method="lda")
best_feature
best_feature$optVariables

## Modeling 
ldaModel <- train(Class~.,trainSet,method ="lda", trControl = train_control)
prediction_lda <- predict(ldaModel,newdata=testSet)
confusion_lda <- confusionMatrix(prediction_lda,testSet$Class)
confusion_lda
# We need to evaluate based on balanced accuracy. Why? class variable is so imbalance

### QDA ### (QDA does not work well)

## Feature selection 
# rfeparam <- rfeControl(functions = caretFuncs,method = "repeatedcv",number = 10,repeats=3)
# best_feature<-rfe(Class~.,data=trainSet,rfeControl = rfeparam,method="qda")
# best_feature
# best_feature$optVariables

## Modeling 
# qdaModel <- train(Class~.,trainSet,method = "qda", trControl = train_control)
# prediction_qda <- predict(qdaModel,newdata=testSet)
# confusion_qda <- confusionMatrix(prediction_qda,testSet$Class)
# confusion_qda

### KNN ###

# Confirm hyper-parameter on Model
caret::modelLookup("knn")

knnModel <- train(Class~.,trainSet,method = "knn", trControl = train_control,tuneGrid=expand.grid(k=c(5,7,9,11,13,15)))
knnModel
prediction_knn <- predict(knnModel,newdata=testSet)
confusion_knn <- confusionMatrix(prediction_knn,testSet$Class)
confusion_knn

### SVM ###
caret::modelLookup("svmRadialSigma")

svmModel <- train(Class~.,trainSet,method = "svmRadialSigma", trControl = train_control)
svmModel
prediction_svm <- predict(svmModel,newdata=testSet)
confusion_svm <- confusionMatrix(prediction_svm,testSet$Class)
confusion_svm

### Naive Bayes ###

# Confirm hyper-parameter on Model
caret::modelLookup("naive_bayes")

nbModel <- train(Class~.,trainSet,method = "naive_bayes", trControl = train_control)
nbModel
prediction_nb <- predict(nbModel,newdata=testSet)
confusion_nb <- confusionMatrix(prediction_nb,testSet$Class)
confusion_nb

### Flexible Discriminant Analysis (FDA) ###
caret::modelLookup("fda")

fdaModel <- train(Class~.,trainSet,method = "fda", trControl = train_control)
fdaModel
prediction_fda <- predict(fdaModel,newdata=testSet)
confusion_fda <- confusionMatrix(prediction_fda,testSet$Class)
confusion_fda

### Stochastic Gradient Boosting ###
caret::modelLookup("gbm")

gbmModel <- train(Class~.,trainSet,method = "gbm", trControl = train_control)
gbmModel
prediction_gbm <- predict(gbmModel,newdata=testSet)
confusion_gbm <- confusionMatrix(prediction_gbm,testSet$Class)
confusion_gbm

# http://topepo.github.io/caret/train-models-by-tag.html

######################### Data set 2 : Kelly Blue Book  #############################

# 1. Load the data
data(cars)
data <- cars
#View(data)
summary(data) ## Confirm quantile information 

# 2. Cleansing the data ( understand the meaning of variable , data type, handle NA , factorize )

## Make Price(class variable) to categorical variable based on quantile values
quantile(data$Price)

data<-data %>% mutate(Price=case_when(
  Price >= quantile(data$Price)[1] & Price < quantile(data$Price)[3] ~ "cheap",
  Price >= quantile(data$Price)[3] & Price <= quantile(data$Price)[5] ~ "expensive"
))

data<-data%>% 
  mutate(Price=as.factor(Price))

data %>%
  group_by(Price)%>%
  summarise(n=n())

dim(data)

## Essential to remove NA when modeling
data <- data %>%
  na.omit()

## Final data set
cleanData <- data
str(cleanData)

# 3. Shuffle data
randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]
cleanData

# 4. Divide data into train and test

createTrainTestSet <- function(data,propTrain=0.8){
  propTest = 1- propTrain 
  
  total_row <- propTest * nrow(data)
  ## We already mix the data with sample() method
  testSetIdx <- 1:total_row
  
  output <- list(testSet=data.frame(data[testSetIdx,]),
                 trainSet =data.frame(data[-testSetIdx,]))
  return(output)
}

result = createTrainTestSet(cleanData,0.8)
trainSet <- result$trainSet
testSet <- result$testSet

dim(trainSet)
dim(testSet)

# 5. Feature selection 
rfeparam <- rfeControl(functions = rfFuncs,method = "repeatedcv",number = 10,repeats = 3)
best_feature<-rfe(Price~.,data=trainSet,rfeControl = rfeparam)
best_feature
best_feature$optVariables

# 6. Generating Cross-validation & Modeling 

### All model apply same CV (fold=10, repeat=3) ###
train_control <- trainControl(method = "repeatedcv",number=10,repeats=3)

### Decision Tree ###
dtModel <- train(Price~.,trainSet,method="rpart",trControl=train_control)
prediction_dt <- predict(dtModel,newdata=testSet)
confusion_dt <- confusionMatrix(prediction_dt,testSet$Price)
confusion_dt

### LDA ###

## Feature selection 
rfeparam <- rfeControl(functions = caretFuncs,method = "repeatedcv",number = 10,repeats=3)
best_feature<-rfe(Price~.,data=trainSet,rfeControl = rfeparam,method="lda")
best_feature
best_feature$optVariables

## Modeling 
ldaModel <- train(Price~Cylinder+Cruise+Chevy+Saab+Cadillac+Sound+coupe+Saturn+Buick+convertible+sedan+hatchback+Mileage+Pontiac+Leather+Doors,trainSet,method ="lda", trControl = train_control)
prediction_lda <- predict(ldaModel,newdata=testSet)
confusion_lda <- confusionMatrix(prediction_lda,testSet$Price)
confusion_lda
# We need to evaluate based on balanced accuracy. Why? class variable is so imbalance


### QDA ### (QDA does not wrok well)

## Feature selection 
# rfeparam <- rfeControl(functions = caretFuncs,method = "repeatedcv",number = 10,repeats=3)
# best_feature<-rfe(Price~.,data=trainSet,rfeControl = rfeparam,method="qda")
# best_feature
# best_feature$optVariables

## Modeling
# qdaModel <- train(Price~.,trainSet,method = "qda", trControl = train_control)
# prediction_qda <- predict(qdaModel,newdata=testSet)
# confusion_qda <- confusionMatrix(prediction_qda,testSet$Price)
# confusion_qda

### KNN ###

## Feature selection 
rfeparam <- rfeControl(functions = caretFuncs,method = "repeatedcv",number = 10,repeats=3)
best_feature<-rfe(Price~.,data=trainSet,rfeControl = rfeparam,method="knn")
best_feature
best_feature$optVariables

## Modeling
# Confirm hyper-parameter on Model
caret::modelLookup("knn")

knnModel <- train(Price~Cylinder+Cruise+Chevy+Saab+Cadillac+Sound+coupe+Saturn,trainSet,method = "knn", trControl = train_control,tuneGrid=expand.grid(k=5))
prediction_knn <- predict(knnModel,newdata=testSet)
confusion_knn <- confusionMatrix(prediction_knn,testSet$Price)
confusion_knn

### SVM ###
caret::modelLookup("svmRadialSigma")
svmModel <- train(Price~.,trainSet,method = "svmRadialSigma", trControl = train_control)
prediction_svm <- predict(svmModel,newdata=testSet)
confusion_svm <- confusionMatrix(prediction_svm,testSet$Price)
confusion_svm

### Naive Bayes ###
caret::modelLookup("naive_bayes")
nbModel <- train(Price~.,trainSet,method = "naive_bayes", trControl = train_control)
prediction_nb <- predict(nbModel,newdata=testSet)
confusion_nb <- confusionMatrix(prediction_nb,testSet$Price)
confusion_nb

### Flexible Discriminant Analysis (FDA) ###
caret::modelLookup("fda")
fdaModel <- train(Price~.,trainSet,method = "fda", trControl = train_control)
prediction_fda <- predict(fdaModel,newdata=testSet)
confusion_fda <- confusionMatrix(prediction_fda,testSet$Price)
confusion_fda

### Stochastic Gradient Boosting ###
caret::modelLookup("gbm")
gbmModel <- train(Price~.,trainSet,method = "gbm", trControl = train_control)
prediction_gbm <- predict(gbmModel,newdata=testSet)
confusion_gbm <- confusionMatrix(prediction_gbm,testSet$Price)
confusion_gbm
