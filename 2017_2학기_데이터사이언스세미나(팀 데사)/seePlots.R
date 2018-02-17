
# 연도별 engine 제작시기를 봄  
library(ggplot2)
h <- ggplot(data = planes, aes(x = year))
h + geom_histogram(binwidth = 1,fill = "steelblue", colour = "black")

# -> 1983년 이후로 2000년까지 급격히 증가하다가 2000년 이후로
#    감소 추세를 보임

str(flights)



#dep_delay hist 확인하기
testset <- fw %>% filter( dep_delay <=100)
h <- ggplot(data = testset, aes(x = dep_delay))
h + geom_histogram(binwidth = 1,fill = "steelblue", colour = "black")

boxplot(testset$dep_delay)
summary(fw$dep_delay)

