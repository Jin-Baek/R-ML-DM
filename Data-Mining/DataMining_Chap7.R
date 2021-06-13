###################################################
# Chap 7   Classification                         # 
###################################################


## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("G:\\내 드라이브\\2021-1\\데이터마이닝")
getwd()

## Load library 
library(tidyverse)
library(datarium)
library(caret) ### Very very important package ( contain almost all classification method)
library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)

####################### Decision tree #################################

# Step1 : Loading Titanic data ( predicting survival )
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv")

View(data)

randomIdx <- sample(1:nrow(data)) # ! mix all the sample's order first !
data <- data[randomIdx,]

dim(data)
str(data)
head(data)

# Step2 : Cleansing the data
data$age <- as.numeric(data$age)
data$fare <- as.numeric(data$fare)

sum(is.na(data$fare))
sum(is.na(data$age))

## remove meaningless variable
## make some variable to factor to understand easier 
## essential to remove NA when modeling
cleanData <- data %>% 
  dplyr::select(-c(cabin,home.dest,name,x,ticket)) %>%
  mutate(pclass = factor(pclass,levels=c(1,2,3),labels=c("Upper","Middel","Lower")),
         survived = factor(survived,levels=c(0,1),labels=c("No","Yes")))%>%
  na.omit()
 
  
glimpse(cleanData)

# Step3 : Generating Cross-validation data (train and test set)
## Or using LOOCV , K-fold 

createTrainTestSet <- function(data,propTrain=0.8){
  propTest = 1- propTrain
  
  total_row <- propTest * nrow(data)
  ## We already mix the data with sample() method
  testSetIdx <- 1:total_row
  
  output <- list(testSet=data.frame(data[testSetIdx,]),
                trainSet =data.frame(data[-testSetIdx,]))
  return(output)
}

foldData <- createTrainTestSet(cleanData,0.5)
foldData $trainSet
foldData $testSet

dim(foldData$trainSet)
dim(foldData $testSet)

# Step4(not essential) : Check the target variable that is well divided in train and test set
prop.table(table(foldData$testSet$survived))
prop.table(table(foldData$trainSet$survived))

prop.table(table(cleanData$trainSet$survived))


# Step5 : Modeling 
?rpart
## rpart is tree type model like Regression tree, Decision tree etc 
model <- rpart(survived~.,data=foldData$trainSet, method="class")
rpart.plot(model,extra=106)


model <- rpart(survived~.,data=foldData$testSet, method="class")
rpart.plot(model,extra=106)

## trainset tree and testset tree plot will be different
## This is why is must use K-fold CV


# Step6 : Prediction & Error estimation
predictResult <- predict(model,foldData$testSet,type="class")
data.frame(predictResult)

## with table method we can recognize "Confusion Matrix"
confusion <- table(foldData$testSet$survived,predictResult)
confusion

accuracy <- sum(diag(confusion)) / sum(confusion) * 100
accuracy

sensitivity <- confusion[2,2] / sum(confusion[,2]) * 100
sensitivity

specificity <- confusion[1,1] / sum(confusion[,1]) * 100
specificity


######################### LDA, QDA , KNN , SVM ##########################

# Step1 : Load Data
webAddress <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

Data <- read.csv(webAddress,header = F, stringsAsFactors = T)
#View(Data)

# Step2 : Cleansing the data 

# Changes feature name 
feature <- c("radius","texture","parimeter","area","smoothness","compactness","convavity","concave_points","synmetry","fractal_dimension")

colnames(Data) <- c("id","diagnosis",paste0(feature,"_mean"),paste0(feature,"_se"), paste0(feature,"_worst"))

glimpse(Data)

cleanData <- Data[,3:ncol(Data)]

rownames(cleanData) <- Data$id
#View(cleanData)


cleanData <- cbind(cleanData,Data$diagnosis)


# Step3 : shuffle data
randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]

cleanData


#Step4 : Generating Cross-validation data

## Not use like below. You must use K-fold 
foldData <- createTrainTestSet(cleanData,0.5)

dim(foldData$testSet)
dim(foldData$trainSet)

prop.table(table(foldData$testSet$Data.diagnosis))
prop.table(table(foldData$trainSet$Data.diagnosis))

prop.table(table(cleanData$'Data$diagnosis'))



#Step5 : Modeling & prediction & error estimation


## LDA
ldaModel <- lda(Data.diagnosis~.,data=foldData$trainSet)

prediction_lda <- predict(ldaModel,newdata=foldData$testSet)

prediction_lda$class

confusion_lda <- table(predicted = prediction_lda$class,
                  Diagnosis = foldData$testSet$Data.diagnosis)

confusion_lda

accuracy <- sum(diag(confusion_lda)) / sum(confusion_lda) * 100
accuracy

sensitivity <- confusion_lda[2,2] / sum(confusion_lda[2,]) * 100
sensitivity

specificity <- confusion_lda[1,1] / sum(confusion_lda[1,]) * 100
specificity




## QDA 
qdaModel <- qda(Data.diagnosis~.,data=foldData$trainSet)

prediction_qda <- predict(qdaModel,newdata=foldData$testSet)

confusion_qda <- table(predicted = prediction_qda$class,
                       Diagnosis = foldData$testSet$Data.diagnosis)

confusion_qda





## KNN 
?kknn

## KNN method include training and prediction together
knnModel <- kknn(Data.diagnosis~.,train=foldData$trainSet,test=foldData$testSet, k=7)

confusion_3NN <- table(Predicted=fitted(knnModel),
                       Diagnosis = foldData$testSet$Data.diagnosis)

confusion_3NN





## SVM 
?ksvm
svmModel <- ksvm(Data.diagnosis~.,data=foldData$trainSet, kernel="rbf", type="C-svc")

prediction_svm <- predict(svmModel,newdata=foldData$testSet)

confusion_svm <- table(predicted = prediction_svm,
                       Diagnosis = foldData$testSet$Data.diagnosis)

confusion_svm



## adaboost

library(fastAdaboost)

?adaboost

adaboostModel <- adaboost(Data.diagnosis~.,data=foldData$trainSet,10)

prediction_ada <- predict(adaboostModel,newdata=foldData$testSet)

confusion_ada <- table(predicted = prediction_ada$class,
                       Diagnosis = foldData$testSet$Data.diagnosis)

confusion_ada


