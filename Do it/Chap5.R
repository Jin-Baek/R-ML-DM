##############################################
## Chapter 5 : Learning about data analysis ##
##############################################

## Set repositories 
setRepositories(ind = 1:8)

## Load library 

library(ggplot2)

# To use rename method 
library(dplyr)




## Understanding data

# read csv file
exam <- read.csv("G:\\내 드라이브\\R\\csv_exam.csv")
exam

head(exam)
head(exam,10)

tail(exam)
tail(exam,10)

View(exam)

# check data frame shape
dim(exam)

# check data attributes information 
str(exam)

# same function with python : describe()
summary(exam)





## handling mpg data (car data)

# change mpg data to data frame type 
mpg <- as.data.frame(ggplot2::mpg)
mpg

head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)
?mpg




## handling variable

df_sample <- data.frame(var1=c(1,2,1),
                        var2=c(2,3,2))
# make copy data frame
df_copy <- df_sample
df_copy

# change variable name
df_copy <- rename(df_copy,v2=var2)
df_copy

# Example : using mpg data 
mpg
mpg <- rename(mpg,city=cty,highway=hwy)
str(mpg)





## Derived variable : A new column created from multiple columns of data
df <- data.frame(var1=c(4,3,8),
                 var2=c(2,6,1))

df$var_sum = df$var1 + df$var2
df

# derived variable on mpg 
# (city = city Fuel efficiency / hwy = highway Fuel efficiency )
mpg$total = (mpg$city + mpg$highway)/2
head(mpg)



## Use "mpg$total" above to separate into pass and fail 
## with specific norm (= median) 

# Check median value availability as a norm 
summary(mpg$total)

# histogram 
hist(mpg$total)

# conditional statements / new derived variable
mpg$test <- ifelse(mpg$total >= 20,"pass","fail")
head(mpg$test,15)
tail(mpg$test,15)

# table() = python value_counts()
table(mpg$test)

# Visualization in bar
qplot(mpg$test)




## Use "mpg$total" above to separate into A B C grade
## with specific norm 

mpg$grade <- ifelse(mpg$total >=30,"A",
                    ifelse(mpg$total >= 20,"B","C"))

head(mpg,5)
table(mpg$grade)

qplot(mpg$grade)
