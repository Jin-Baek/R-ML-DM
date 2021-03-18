##############################################
## Chapter 7 : Learning about data cleaning ##
##############################################

## Set repositories 
setRepositories(ind = 1:8)

## Load library 
library(ggplot2)
library(dplyr)


###################### Cleaning NA data #############################

# Make data.frame using random 
index_sex <- sample(x=1:3,size=50,replace=T)
sex_arr = c("M","F",NA)

index_height <- sample(x=1:13,size=50,replace=T)
height_arr = c(160,162,164,166,168,170,172,174,176,178,180,182,NA)

df <- data.frame(sex=sex_arr[index_sex],
                 height=height_arr[index_height])
df



# Check NA data , return False and True from all data 
is.na(df)



# Count NA data
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$height))


# Extract NA row 
df %>%
  filter(is.na(sex))

## Extract not NA row
df %>%
  filter(!is.na(sex))

## some example 
df_noNa <- df %>%
  filter(!is.na(sex) & !is.na(height))
df_noNa
mean(df_noNa$height)





# Handle data excluded NA (on basic function)
mean(df$height,na.rm = T)
sum(df$height,na.rm=T)

## same in summarise()
df %>%
  summarise(mean_height=mean(height,na.rm = T),
            median_height=median(height,na.rm = T))



# Replace NA with mean value 
df$height <- ifelse(is.na(df$height),mean(df$height,na.rm=T),df$height)
df$height





########################### Cleaning outlier data ###############################

boxplot(mpg$hwy)

# statistic about boxplot - down outlier, 1 quartile, median , 3 quartile, up outlier
boxplot(mpg$hwy)$stats

# Replace outlier with NA 
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37,NA,mpg$hwy)
mpg$hwy
table(is.na(mpg$hwy))

# Handle NA data
mean(mpg$hwy,na.rm = T)
