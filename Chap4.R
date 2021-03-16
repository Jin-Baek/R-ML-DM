###########################################
## Chapter 4 : Learning about data frame ##
###########################################

## Set repositories 
setRepositories(ind = 1:8)

## Load library 

# To read Excel file 
library(readxl)




## How to make data frame 
english <- c(90,80,60,70)
english

math <- c(50,60,100,20)
math

df_midterm <- data.frame(english,math)
df_midterm

class <- c(1,1,2,2)
class

df_midterm <- data.frame(english,math,class)
df_midterm

## How to access to data frame element 
mean(df_midterm$english)
mean(df_midterm$math)

## How to make data frame at once
df_midterm <- data.frame(english=c(90,80,70,60),
                         math=c(50,60,100,20),
                         class=c(1,1,2,2))
df_midterm





## Read excel file (basic)
df_exam <- read_excel("G:\\내 드라이브\\R\\excel_exam.xlsx")
df_exam

# analysis
mean(df_exam$english)
mean(df_exam$science)
mean(df_exam$math)

## Read excel file (parameters)

# Problem
df_exam_novar <- read_excel("G:\\내 드라이브\\R\\excel_exam_novar.xlsx")
df_exam_novar

# Want to regard first row as data, not columns name 
df_exam_novar <- read_excel("G:\\내 드라이브\\R\\excel_exam_novar.xlsx",col_names = F)
df_exam_novar

# If excel file has several sheet
df_exam_novar <- read_excel("G:\\내 드라이브\\R\\excel_exam_novar.xlsx", sheet=3)
df_exam_novar




## Read csv file (basic) / No need packages
a <- read_csv("your path")

## Read csv file (parameters)

# same with above col_names = F
a <- read_csv("yout path",header=F)

# if csv file has string type data
a <- read_csv("yout path",stringsAsFactors=F)



## Write csv file
write.csv(dataframe, file="filename.csv")


## Delete data frame
rm(dataframe)


## Read RDS file 
a <- readRDS("your path")
## Write RDS file 
saveRDS(dataframe,file="filename.rds")




