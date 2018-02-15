
### 회귀분석

movieOrigin <- read.csv("movieFiltered22.csv")
head(movieOrigin)

library(dplyr)
library(psych)

movieOrigin <- movieOrigin %>% select(-title, -release, -distributor,
                                      -director,-X,-star.1,-firstppl)
head(movieOrigin)
str(movieOrigin)
movieOrigin$season <- as.factor(movieOrigin$season)
for(i in 10:24) movieOrigin[,i] <- as.factor(movieOrigin[,i])
str(movieOrigin)
movieOrigin <- movieOrigin[movieOrigin$allpeople >50,]

## log or sqrt
movielog <- movieOrigin
movielog$allpeople <- log(movielog$allpeople)
movielog$firstppl <- log(movielog$firstppl)
movielog$directorEff <- log(movielog$directorEff)
movielog$actorEff <- log(movielog$actorEff)
movielog$distributorEff <- log(movielog$distributorEff)
movielog$trailershow <- log(movielog$trailershow)
movielog[which(is.infinite(movielog$firstppl)),] <- median(movielog$firstppl)
movielog[which(is.infinite(movielog$allpeople)),] <- median(movielog$allpeople)
movielog[which(is.infinite(movielog$directorEff)),] <- median(movielog$directorEff)
movielog[which(is.infinite(movielog$actorEff)),] <- median(movielog$actorEff)
movielog[which(is.infinite(movielog$distributorEff)),] <- median(movielog$distributorEff)
# movielog[which(is.infinite(movielog$trailershow)),] <- 0 # Inf 없음 
movielog$showtime <- scale(movielog$showtime)
colnames(movielog)[c(2,3,4,9,25)] <- paste0("log",colnames(movielog)[c(2,3,4,9,25)])

pairs.panels(movielog %>% select(logdirectorEff, logactorEff, logdistributorEff,
                                 showtime,logtrailershow, 
                                 logallpeople), method = "spearman")

set.seed(100)
seventhree <- sample(c(0,1),nrow(movielog),replace = T, prob = c(0.3,0.7))

train <- movielog[seventhree ==1, ]
test <- movielog[seventhree ==0, ]
train <- na.omit(train)
test <- na.omit(test)
nrow(train) ; nrow(test)
rownames(train) <- 1:nrow(train)
#train <- train[-c(246,717,187,614,139),]
# rownames(train) <- 1:nrow(train)
# train <- train[-c(717,187,614,139,521,209,620,609,467)]

###################### model1 : just all
model1 <- lm(logallpeople~., data = train)
summary(model1)
model1step <- step(model1, direction = "both")
summary(model1step)

pred1 <- predict.lm(model1step, test,type = "response")
plot(test$logallpeople,pred1)
abline(0,1)
plot(model1step)
# predict.lm(model1step, ex1,interval = "confidence")
###################### model5 : model4 + season 복구 outlier 제거 
model5 <- lm(logallpeople~rate+
               logdirectorEff*logactorEff + 
               logdirectorEff*logdistributorEff+
               logactorEff*logdistributorEff+
               showtime+logtrailershow+ 
               season + 
               genre범죄 + genre공연 + genre액션 + 
               genre드라마+ genre어드벤처+  genre코미디+
               genre멜로.로맨스+genreSF  +
               genre판타지+genre다큐멘터리 + genre애니메이션+
               genre공포.호러. + genre사극 +
               genre미스터리+ 
               I(logactorEff^2) + I(logdistributorEff^2)+ 
               I(logdirectorEff^2) + I(logtrailershow^2) 
             , data = train)
summary(model5)
model5step <- step(model5, direction = "both")
summary(model5step)

pred5 <- predict.lm(model5step, test,type = "response")
plot(test$logallpeople,pred5)
abline(0,1)
plot(model5step)
require(car)
vif(model5step)
sqrt(vif(model5step))
# > 정규성 만족 
 qqPlot(model5step,id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 독립성 만족
 durbinWatsonTest(model5step)
# > 선형성 만족
 crPlots(model5step)
# > 등분산성 만족
 ncvTest(model5step) ; spreadLevelPlot(model5step)
outlierTest(model5step)
###################### model7 : model6 + 이상치 제거 668 527
###################### 7번 최종 확정
model7 <- lm(logallpeople~rate+
               logdirectorEff+ logactorEff+ logdistributorEff+
               showtime+logtrailershow+ 
               season +
               genre범죄 + genre공연 + genre액션 + 
               genre어드벤처+  genre코미디+ genre드라마+
               genre멜로.로맨스+genreSF  +
               genre판타지+genre다큐멘터리 + genre애니메이션+
               genre공포.호러. + genre사극 +
               genre미스터리+ 
               I(logactorEff^2)+ I(logdistributorEff^2)
             , data = train[-c(668,527,246,193,717,
                              187,614,139,
                             521,209,620,609,467),])
# data = train[-c(668,527,246,193,717,
#                 187,614,139,
#                 521,209,620,609,467),]
summary(model7)
model7step <- step(model7, direction = "both")
summary(model7step)

pred7 <- predict(model7step, test,type = "response")
plot(test$logallpeople,pred7)
abline(0,1)
plot(model7step)
require(car)
vif(model7step)
# > 정규성 만족 
qqPlot(model7step,id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 독립성 만족
durbinWatsonTest(model7step)
# > 선형성 만족
crPlots(model7step)
# > 등분산성 만족
ncvTest(model7step) ; spreadLevelPlot(model7step)
# > 이상치 확인
outlierTest(model7step)
shapiro.test(model7step$residuals)

###################### model8 : model7 + 드라마 제거 
model8 <- lm(logallpeople~rate+
               logdirectorEff+ logactorEff+ logdistributorEff+
               showtime+logtrailershow+ 
               season +
               genre범죄 + genre공연 + genre액션 + 
               genre어드벤처+  genre코미디+
               genre멜로.로맨스+genreSF  +
               genre판타지+genre다큐멘터리 + genre애니메이션+
               genre공포.호러. + genre사극 +
               genre미스터리+ 
               I(logactorEff^2)+ I(logdistributorEff^2)
             ,  data = train[-c(668,527,521,246,193,
                                614,139,
                                521,209,620,609,467,226, 717,187),])
# data = train[-c(668,527,246,193,717,
#                 187,614,139,
#                 521,209,620,609,467),]
summary(model8)
model8step <- step(model8, direction = "both")
summary(model8step)

pred8 <- predict(model8step, test)
plot(test$logallpeople,pred8)
abline(0,1)
plot(model8step)
require(car)
vif(model8step)
outlierTest(model8step)

cor(pred8, test$logallpeople)
cor(exp(pred8),exp(test$logallpeople))
shapiro.test(model8step$residuals)
ad.test(model7step$residuals)













