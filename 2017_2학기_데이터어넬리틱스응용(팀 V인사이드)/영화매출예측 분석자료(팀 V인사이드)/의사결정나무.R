set.seed(100)
# install.packages("rpart.plot")
# install.packages("caret")
# install.packages("e1071")

library(rpart)
library(rpart.plot) 
library(caret)
library(e1071)
library(dplyr)

#data
movie <- read.csv("movieFiltered22.csv")
movie<-movie%>%select(rate,   directorEff,   actorEff,   distributorEff,
                      season,   release.1,productContinent, showtime,
                      trailershow,allpeople,star.1,genre액션,
                      genre범죄,   genre드라마,   genre코미디,
                      genre어드벤처,   genre멜로.로맨스,   genreSF,
                      genre다큐멘터리,   genre공연,   genre판타지,
                      genre애니메이션,genre공포.호러.,
                      genre성인물.에로.,   genre사극,   genre미스터리)

summary(movie)

#전처리
movie$season <- as.factor(movie$season)
movie$genre액션<- as.factor(movie$genre액션)
movie$genre범죄<- as.factor(movie$genre범죄)
movie$genre드라마<- as.factor(movie$genre드라마)
movie$genre코미디<- as.factor(movie$genre코미디)
movie$genre어드벤처<- as.factor(movie$genre어드벤처)
movie$genre멜로.로맨스.<- as.factor(movie$genre멜로.로맨스)
movie$genreSF<- as.factor(movie$genreSF)
movie$genre다큐멘터리<- as.factor(movie$genre다큐멘터리)
movie$genre공연<- as.factor(movie$genre공연)
movie$genre판타지<- as.factor(movie$genre판타지)
movie$genre애니메이션<- as.factor(movie$genre애니메이션)
movie$genre공포.호러.<- as.factor(movie$genre공포.호러.)
movie$genre성인물.에로.<- as.factor(movie$genre성인물.에로.)
movie$genre사극<- as.factor(movie$genre사극)
movie$genre미스터리<- as.factor(movie$genre미스터리)
str(movie)

omit<-na.omit(movie)
nrow(movie)
nrow(omit)

table(movie$star.1)
table(omit$star.1)

intrain<-createDataPartition(y=omit$star.1, p=0.7, list=FALSE) 
trainomit<-omit[intrain, ]
testomit<-omit[-intrain, ]

nrow(trainomit)
nrow(testomit)

table(trainomit$star.1)
table(testomit$star.1)

###rpart
movie_rpart<-rpart(star.1~ directorEff+actorEff+ distributorEff+rate+season+release.1+productContinent+ showtime+ trailershow+genre액션+genre범죄+genre드라마+genre코미디+ genre어드벤처+genre멜로.로맨스+genreSF+genre다큐멘터리+genre공연+genre판타지+genre애니메이션+genre공포.호러.+genre성인물.에로.+genre사극+genre미스터리, data=trainomit)

movie_rpart
summary(movie_rpart) 
rpart.plot(movie_rpart, cex=0.7)
p1_m2 <- predict(movie_rpart, testomit, type="class") 
confusionMatrix(p1_m2,testomit$star.1)

# Prune: choose a final tree size (cptable) 
plotcp(movie_rpart) 
printcp(movie_rpart)
rpart_pruned <- prune(movie_rpart, cp=0.01,"CP") 
rpart.plot(rpart_pruned,cex=0.8)

# Prediction for test data sets 
p1_m1 <- predict(rpart_pruned, testomit, type="class") 
p1_m1 
summary(p1_m1)
# Construct Confusion matrix 
confusionMatrix(p1_m1,testomit$star.1)
