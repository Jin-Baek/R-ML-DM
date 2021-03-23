##################################################################
## Chapter 6 : Learning about data analysis using dplyr package ##
##################################################################

## Set repositories 
setRepositories(ind = 1:8)

## Load library 
library(dplyr)

## Load data
exam <- read.csv("G:\\내 드라이브\\R\\csv_exam.csv")
exam



## pipe operator %>%
## -> similar with $ operator, but more simpler and useful



## filter : extract specific row
exam %>% 
  filter(class == 1)

exam %>% 
  filter(class != 1)

exam %>% 
  filter(math >50)

# AND
exam %>% 
  filter(class==1 & math >= 50)

# OR
exam %>% 
  filter(math>=90 | english >= 90)

exam %>%
  filter(class==1|class==3|class==5)

# simplify the above code using %in% 
# %in% = check if value contains at conditional list(조건목록) 
exam %>%
  filter(class %in% c(1,3,5))






## select : extract specific feature
exam %>%
  select(math)

exam%>%
  select(class,math)

#  - : exclude specific feature
exam %>%
  select(-math,-english)







## filter + select
exam  %>% 
  filter(class==1) %>%
  select(english)

exam %>%
  filter(math>=60 & english>=60) %>%
  select(science)%>%
  head(3)







## arrange : sort data

# ascending
exam %>% 
  arrange(math)

# descending
exam %>%
  arrange(desc(math))

# sorting in order
exam %>%
  arrange(math,english)








## mutate : add derived feature(=variable) from original data
exam %>%
  mutate(total=math+english+science) %>%
  head

exam_sample <-exam %>%
  mutate(total=math+english+science,
         mean = total/3) %>%
  arrange(total)

exam_sample

# use ifelse
exam_sample %>%
  mutate(test=ifelse(mean>=70,"pass","fail")) %>%
  arrange(desc(mean))%>%
  head(10)







## group_by, summarise : summarize with in group 
exam %>%
  summarise(mean_math = mean(math))

# math score mean about each class 
exam %>%
  group_by(class)%>%
  summarise(mean_math = mean(math))

# check several feature in once is possible
exam %>%
  group_by(class) %>%
  summarise(mean_math= mean(math),
            sum_math = sum(math),
            median_math = median(math),
            count = n(),
            sd = sd(math))

# multi group_by
mpg %>%
  group_by(manufacturer,drv) %>%
  summarise(mean_cty = mean(city)) %>%
  head(10)

# practice
mpg %>%
  group_by(manufacturer) %>%
  filter(class=="suv") %>%
  mutate(total=(city+highway)/2) %>%
  summarise(mean_total =mean(total)) %>%
  arrange(desc(mean_total)) %>%
  head(5)









## data merge 

# data frame ex 1
test1<- data.frame(id=c(1,2,3,4,5),
                   midterm = c(60,80,70,90,85))
# data frame ex 2
test2<- data.frame(id=c(1,2,3,4,5),
                   final=c(70,83,65,95,80))

test1
test2

# left join : combine horizontally
total <- left_join(test1,test2,by='id')
total


# new data frame
name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("kim","lee","park","choi","jung"))

# left join exam data with name data
exam_new <- left_join(exam,name,by="class")
exam_new

# bind_rows : combine vertically
# if feature name is different, use rename()
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,70,80,65,100))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test=c(90,85,100,80,85))
group_all <- bind_rows(group_a,group_b)
group_all
