###################################################
# Chap 6 Week 12 Lecture for Data Mining          # 
###################################################


# Set repositories 
setRepositories(ind = 1:8)

# Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝")
getwd()

# Load library 
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

# Practice for data subsetting

data <- swiss
?swiss
View(data)

## class variable : Fertility is continuous, using Linear Regression Model 
hist(data$Fertility) 

nrow(data)

## randomly choosing sample
sample_n(data,5)



###############################################################################

# Approach 1: Basic Validation set approach
trainSampleIndex <- data$Fertility %>%
  createDataPartition(p=0.5,list=F) ## randomly divide data into train set and test set

## createDataPartition  returns divided index of whole data into train and test, so using that index as a row index , then we can get specific rows data.
trainData <- data[trainSampleIndex,]
testData <- data[-trainSampleIndex,]

?createDataPartition

View(trainData)

## No intersect rows, between train set and test set
intersect(rownames(trainData),rownames(testData))


## Use Linear Regression with all feature 
model<-lm(Fertility ~ .,data=trainData)
summary(model)
### With summary method, we can know that Examination feature is not a important feature to predict Fertility, because P-value is over 0.05


## Use "feature selection" ( backward elimination, forward selection, stepwise selection) to apply the meaningful feature in Linear Regression model ( reference. ABCLab/Linear Regression.pptx )
forward_model <-step(model,direction = "forward")
forward_model

backward_model <-step(model,direction = "backward")
backward_model

stepwise_model <-step(model,direction = "both")
stepwise_model


## predict with Linear Regression model
prediction <- model %>% predict(testData)
prediction_forward <- forward_model %>% predict(testData)
prediction_backward <- backward_model %>% predict(testData)
prediction_stepwise <- stepwise_model %>% predict(testData)

prediction

## measuring R2 , RMSE, MAE
result_model <-data.frame(R2=R2(prediction,testData$Fertility), 
           RMSE=RMSE(prediction,testData$Fertility),
           MAE=MAE(prediction,testData$Fertility))

result_model_forward <-data.frame(R2=R2(prediction_forward,testData$Fertility), 
                          RMSE=RMSE(prediction_forward,testData$Fertility),
                          MAE=MAE(prediction_forward,testData$Fertility))

result_model_backward <-data.frame(R2=R2(prediction_backward,testData$Fertility), 
                                  RMSE=RMSE(prediction_backward,testData$Fertility),
                                  MAE=MAE(prediction_backward,testData$Fertility))

result_model_stepwise <-data.frame(R2=R2(prediction_stepwise,testData$Fertility), 
                                  RMSE=RMSE(prediction_stepwise,testData$Fertility),
                                  MAE=MAE(prediction_stepwise,testData$Fertility))

result <- rbind(result_model,result_model_forward,result_model_backward,result_model_stepwise)

result

## This time, use each different feature(independent variable) instead of "feature selection"
model2<- lm(Fertility ~ Agriculture,data=trainData)
model3<- lm(Fertility ~ Agriculture+Education,data=trainData)
model4<- lm(Fertility ~ Agriculture+Examination,data=trainData)

prediction <- model %>% predict(testData)
prediction2 <- model2 %>% predict(testData)
prediction3 <- model3 %>% predict(testData)
prediction4 <- model4 %>% predict(testData)

prediction

result_model2 <-data.frame(R2=R2(prediction2,testData$Fertility), 
                                  RMSE=RMSE(prediction2,testData$Fertility),
                                  MAE=MAE(prediction2,testData$Fertility))

result_model3 <-data.frame(R2=R2(prediction3,testData$Fertility), 
                           RMSE=RMSE(prediction2,testData$Fertility),
                           MAE=MAE(prediction2,testData$Fertility))

result_model4 <-data.frame(R2=R2(prediction4,testData$Fertility), 
                           RMSE=RMSE(prediction2,testData$Fertility),
                           MAE=MAE(prediction2,testData$Fertility))

result <- rbind(result_model,result_model2,result_model3,result_model4)

result ### So now, we can find meaningful model 


#############################################################################


# Approach 2 : Leave-one-out cross validation (LOOCV) 

?trainControl ## cross validation function
?train

trainControl <- trainControl(method="LOOCV")
model <- train(Fertility ~ .,data,trControl=trainControl,method="lm")
## train the model with specific feature,validation method, type of model 
## we can add pre-processing step. 
 
model
## At result, there is a number of validation set to predict with & evaluation





# How to implement LOOCV logic without the package

predicted_Fertility <- c()

for(i in 1:nrow(data)){
  testSet<- data[i,] ## data[i,] means each samples (row)
  ## testSet is regard as validation set in this step.
  trainSet<- data[-i,] ## in data, except testSet is all trainSet
  print(testSet)
  model<- lm(Fertility~.,data=trainSet) ## train model with trainSet
  predicted_Fertility[i] <- model %>% predict(testSet) ## predict model with testSet
  print(predicted_Fertility[i])
}

## Evaluation step with R square, RMSE, MAE 
result_LOOCV <-data.frame(
        R2=R2(predicted_Fertility,data$Fertility), 
        RMSE=RMSE(predicted_Fertility,data$Fertility),
        MAE=MAE(predicted_Fertility,data$Fertility))

predicted_Fertility
result_LOOCV


###############################################################################

# Approach 3 : K-fold cross validation

trainControl <- trainControl(method="cv",number=10,repeats=3)

model<- train(Fertility~.,data,trControl=trainControl,method="lm")
model





# How to implement K-fold logic without the package (Homework)   Wrong!!! need revise

k = 10
predicted_Fertility <- c()

testSet <- c()
trainSet<- c()

folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE) 
## cut the data with specific breaks , and we can named each range with labels using c()
folds


for(i in 1:k){
  testIndex <- which(folds==i,arr.ind = TRUE)

  ## find specific value's index that satisfy condition
  ## When arr.ind is True return index, if false return value 
  testSet[[i]]<- data[testIndex,]
  trainSet[[i]] <- data[-testIndex,]
  #print(testSet[[i]])
  
  model<- lm(Fertility ~ .,data=trainSet[[i]])
  predicted_Fertility <- model %>% predict(testSet[[i]])
  
  print(predicted_Fertility) 
  print(1)
}

result_Kfolds <- data.frame(R2=R2(predicted_Fertility,data$Fertility), 
                            RMSE=RMSE(predicted_Fertility,data$Fertility),
                            MAE=MAE(predicted_Fertility,data$Fertility))
result_Kfolds
















