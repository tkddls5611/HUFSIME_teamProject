install.packages("corrplot")
library(corrplot)
library(dplyr) 
#exam <- read.csv("movieInfo.csv")
#exam <- read.csv("movieRate.csv")
#library(xlsx)
#install.packages("readxl")
#library(readxl)
#ex_mFcsv<-read_excel("movieFiltered.xlsx")
#write.csv(ex_mFcsv,file="movieFiltered2.csv")
#moviefilter<-read("movieFiltered.xlsx")
exam <- read.csv("movieFiltered.csv")
exam2 <- read.csv("screen.csv")
#상관관계분석수치형 데이터선택
exam_se <-exam %>% select(allpeople,allmoney,star,directorEff)
exam2 <- exam2[,-1]
exam_se <- cbind(exam_se,exam2)
exam_se%>% head(10)
colnames(exam_se)[5] <- "screen" 

table(is.na(exam_se$screen))
#별점na값삭ㅈ 
exam_na<-exam_se%>%filter(!is.na(allpeople))
table(is.na(exam_na$directorEff))
cor.test(exam_na$allpeople,exam_na$star)

library(nortest)

hist(exam_na$)
ad.test(exam_na$allpeople)

#상관관계분석 
exam_cor<-cor(exam_logst) 
exam_cor
round(exam_logst, 2) 
corrplot(exam_logst, method='shade', shade.col=NA, tl.col='black', tl.srt=45)


#attach(movieInfo)

#data1<-xtabs(~+exer, data=exam)
#chisq.test(exam)

#head(exam)



str(exam)
head(movieInfo)
nrow(data)