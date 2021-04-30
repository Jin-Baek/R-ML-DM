###########################################
# Chap 2 Week 3-1 Lecture for Data Mining #
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Set working directory
setwd("C:\\Users\\kucis16\\Jinbaek\\3-1\\데이터마이닝")

## Load library

# To use tibble
install.packages("tidyverse")
library(tidyverse)

## Use data.frame
Height <- rnorm(100,mean=173.2,sd=5)
Height

Gender <- c(rep("Male",50),rep("Female",50))
Gender

data <- data.frame(Height,Gender)
data

View(data)

## Access to data
data$Height
data$Height[1]
data[1]
data[1,1]
data[,2]
data[3,]

## Use tibble 
## (data type, shape information added at data.frame)
df <- tibble(x=1:3, y=c("a","b","c"))
df

tibbleData <- tibble(Height,Gender)
tibbleData

# Change the position of rows and columns in tibble
glimpse(tibbleData)
str(tibbleData)

# Check overall information about data.frame 
attributes(tibbleData)
rownames(tibbleData)
colnames(tibbleData)

# Check variable type 
class(tibbleData$Height)
class(tibbleData$Gender)
