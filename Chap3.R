# 변수 만들기 
a <- 1 
a
b <- 3
a+b

# 배열 만들기 
arr <- c(1,2,3)
arr
arr2 <- c(3:5)
arr2

var <- seq(2,5)
var
var + 3

var2 <- seq(1,10,by=3)
var2

var3 <- seq(0.1,5,2)
var3

var+var2

# 문자열 
str1 <- 'a'
str1
str2 <- "Hello World!"
str2

# 문자열 배열 
str3 <- c('a',"bc","this")
str3
str3[2]

# 함수 적용 
x <- c(1,3,5)
mean(x)
max(x)
min(x)

# 문자 처리 전용 함수 
str4<- c("Hi","My","name","is","A")
str4
paste(str4,collapse = ',')
str5<- paste(str4,collapse = " ")
str5

# 패키지 사용법
# 패키지 설치 -> 패키지 로드(R 스튜디오 시작할 때마다) -> 함수 사용하기
install.packages("ggplot2")
library(ggplot2)

x <- c("a","a","b","c")

qplot(x)

# 간단한 그래프 커스터마이징 ( 자세한건 추후에 ) 
# mpg 는 특정 데이터셋 , hwy 는 자동차가 고속도로에서 1갤런에 몇 마일을 가는지 나타내는 변수 
qplot(data=mpg,x=hwy)

qplot(data=mpg,x=cty)
qplot(data=mpg,x=drv,y=hwy)
qplot(data=mpg,x=drv,y=hwy,geom = "line")
qplot(data=mpg,x=drv,y=hwy,geom = "boxplot")

# 함수 기능 살펴보기 
?qplot
      