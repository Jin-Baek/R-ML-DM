###################################################
# Homework 2 Data - Mining                        # 
###################################################


# Set repositories 
setRepositories(ind = 1:8)

# Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝\\Homework2")
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

library(leaps)


# preparing data 
data <- swiss

############### Q1 K-fold customized function  ##############

testMethodKfoldCV <- function(data,k){
  
  ## Algorithm to divide the data into k-folds properly
  dcount <- nrow(data)
  
  group_type1 <- dcount %% k
  group_type2 <- k - group_type1
  
  type1_data <- floor(dcount/k+1) 
  type2_data <- floor(dcount/k) 
  
  folds <- append(rep(1:group_type1,each=type1_data),rep((group_type1+1):k,each=type2_data))
  
  print(folds)

  testset <- c()
  trainset <- c()
  
  for(i in 1:k){
    ## Check the folds index that is divided
    print(length(which(folds==i)))
    
    testIndex <- which(folds==i,arr.ind = TRUE)
    testset[[i]] <- data[testIndex,]
    trainset[[i]] <- data[-testIndex,]
  }
  
  ## The way to return more than two variables
  return(list(test=testset,train=trainset))
}


# Call customized Method
k=10
result <- testMethodKfoldCV(data,k)  
testset <- result$test
trainset <- result$train 

# Modeling
predicted_Fertility <- c()
temp <- c()
R2sum = 0
RMSEsum = 0
MAEsum = 0

for(i in 1:k){
  model <- lm(Fertility ~ .,data=trainset[[i]])
  predicted_Fertility <- model %>% predict(testset[[i]])
  temp <- c()
  for(j in 1:length(predicted_Fertility)){
    temp[j] <- predicted_Fertility[[j]]
  }
  
  R2 <- R2(temp,testset[[i]]$Fertility)
  print(R2)
  R2sum <- R2sum + R2
  
  RMSE <- RMSE(temp,testset[[i]]$Fertility)
  print(RMSE)
  RMSEsum <- RMSEsum + RMSE
  
  MAE <- MAE(temp,testset[[i]]$Fertility)
  print(MAE)
  MAEsum <- MAEsum + MAE
  
}

# Evaluation 

R2 <- R2sum/10
RMSE <- RMSEsum/10
MAE <- MAEsum/10

df <- data.frame(RMSE=RMSE,R2=R2,MAE=MAE)
df

# Using K-fold with caret package

data <- swiss

trainControl <- trainControl(method="cv",number=10)

model<- train(Fertility~.,data,trControl=trainControl,method="lm")

model


# Using LOOCV with caret package 

trainControl <- trainControl(method="LOOCV")
model <- train(Fertility ~ .,data,trControl=trainControl,method="lm")

model

# Using customized function : LOOCV
predicted_Fertility <- c()

for(i in 1:nrow(data)){
  testSet<- data[i,] ## data[i,] means each samples (row)
  ## testSet is regard as validation set in this step.
  trainSet<- data[-i,] ## in data, except testSet is all trainSet
  print(testSet)
  model<- lm(Fertility~.,data=trainSet) ## train model with trainSet
  predicted_Fertility[i] <- model %>% predict(testSet) ## predict model with testSet
}

## Evaluation 
result_LOOCV <-data.frame(
  R2=R2(predicted_Fertility,data$Fertility), 
  RMSE=RMSE(predicted_Fertility,data$Fertility),
  MAE=MAE(predicted_Fertility,data$Fertility))

result_LOOCV


############## Q2 K-fold repeat customized function ################
data <- swiss

testMethodKfoldCVRepeat <- function(data,k,repeats){
  Repeated_testset <- c()
  Repeated_trainset<- c()
  
  for(t in 1:repeats){

    folds <- c()
    
    ## Algorithm to divide the data into k-folds properly
    dcount <- nrow(data)
    
    # data 에 따라 몇 개의 data로 구성된 그룹이 몇개가 있어야 하는지 파악
    group_type1 <- dcount %% k #7
    group_type2 <- k - group_type1 #3
    
    type1_data <- floor(dcount/k+1) #5
    type2_data <- floor(dcount/k) #4
    
    # 그룹 중 상대적으로 1개씩 덜 들어간 그룹을 랜덤으로 매번 지정.
    group1 <- 1:k
    group2 <- sample(1:k,group_type2)
    
    # 1개씩 덜 들어간 그룹 외의 그룹 구성 
    for(i in 1:length(group2)){
      group1<-group1[-which(group1==group2[i])]
    }
  
    # 그룹별 데이터 갯수가 정해졌으므로 이를 기반으로 Kfold에 사용할 fold index 구현 
    folds <- append(rep(group2,each=type2_data),rep(group1,each=type1_data))
    folds <- sample(folds,length(folds))
    
    print(folds)
    
    testset <- c()
    trainset <- c()
    
    for(i in 1:k){
      ## Check the folds index that is divided
      print(length(which(folds==i)))
      
      testIndex <- which(folds==i,arr.ind = TRUE)
      testset[[i]] <- data[testIndex,]
      trainset[[i]] <- data[-testIndex,]
    }
    
    Repeated_testset[[t]] <- testset
    Repeated_trainset[[t]] <- trainset
  }
  
  ## The way to return more than two variables
  return(list(test=Repeated_testset,train=Repeated_trainset))

}

# Set number of fold, number of repeat 
k=10
rep=3

# Call customized Method
Q2_result<-testMethodKfoldCVRepeat(data,k,rep)
testset <- Q2_result$test
trainset <- Q2_result$train

# Modeling

## 예측 된 결과를 담을 vector
predicted_Fertility <- c()
R2sum = 0
RMSEsum = 0
MAEsum = 0

for(r in 1:rep){
  for(i in 1:k){
    model <- lm(Fertility~.,data=trainset[[r]][[i]])
    predicted_Fertility <- model %>% predict(testset[[r]][[i]])
    print(predicted_Fertility)
    
    ## 예측된 결과 중 Fertility 값만 넣어둔 vector
    temp <- c()
    for(j in 1:length(predicted_Fertility)){
      temp[j] <- predicted_Fertility[[j]]
    }
    
    R2 <- R2(temp,testset[[r]][[i]]$Fertility)
    R2sum <- R2sum + R2
    
    RMSE <- RMSE(temp,testset[[r]][[i]]$Fertility)
    RMSEsum <- RMSEsum + RMSE
    
    MAE <- MAE(temp,testset[[r]][[i]]$Fertility)
    MAEsum <- MAEsum + MAE
  }
}

repeatKfold_df <- data.frame(RMSE=RMSEsum/(k*rep),R2=R2sum/(k*rep),MAE=MAEsum/(k*rep))
repeatKfold_df


# Repeated K-fold cross validation
trainControl <- trainControl(method="repeatedcv",number=10,repeats=3)

model<- train(Fertility~.,data,trControl=trainControl,method="lm")
model


############## Q3 Find best model to predict swiss data set ################

data <- swiss
data

# (1) Model fit and variable selection using all data 

trainSampleIndex <- data$Fertility %>%
  createDataPartition(p=0.5,list=F) ## randomly divide data into train set and test set

trainData <- data[trainSampleIndex,]
testData <- data[-trainSampleIndex,]

model <- lm(Fertility~.,trainData)
summary(model)

forward_model <-step(model,direction = "forward")
summary(forward_model)

backward_model <-step(model,direction = "backward")
summary(backward_model)

stepwise_model <-step(model,direction = "both")
summary(stepwise_model)

# (2) Model fit and variable selection using LOOCV 
train_control <- trainControl(method = "LOOCV")

LOOCV_backward_model <- train(Fertility ~., data = swiss,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train_control
)
LOOCV_backward_model
summary(LOOCV_backward_model)


LOOCV_forward_model <- train(Fertility ~., data = swiss,
                              method = "leapForward", 
                              tuneGrid = data.frame(nvmax = 1:5),
                              trControl = train_control
)
LOOCV_forward_model
summary(LOOCV_forward_model)


LOOCV_stepwise_model <- train(Fertility ~., data = swiss,
                              method = "leapSeq", 
                              tuneGrid = data.frame(nvmax = 1:5),
                              trControl = train_control
)
LOOCV_stepwise_model
summary(LOOCV_stepwise_model)

# (3) Model fit and variable selection using 10-fold cross validation
set.seed(101)

train_control <- trainControl(method = "cv",number=10)

Kfold_backward_model <- train(Fertility ~., data = swiss,
                              method = "leapBackward", 
                              tuneGrid = data.frame(nvmax = 1:5),
                              trControl = train_control
)
Kfold_backward_model
summary(Kfold_backward_model)


Kfold_forward_model <- train(Fertility ~., data = swiss,
                             method = "leapForward", 
                             tuneGrid = data.frame(nvmax = 1:5),
                             trControl = train_control
)
Kfold_forward_model
summary(Kfold_forward_model)


Kfold_stepwise_model <- train(Fertility ~., data = swiss,
                              method = "leapSeq", 
                              tuneGrid = data.frame(nvmax = 1:5),
                              trControl = train_control
)
Kfold_stepwise_model
summary(Kfold_stepwise_model)