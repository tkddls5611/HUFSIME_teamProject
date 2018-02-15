###################감독 NA 채우기################################

movie22 <- read.csv("movieFiltered21.csv")
head(movie22)

meandf <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
colnames(meandf) <- colnames(movie22)[10:24]

for(i in 1:15){ # i=1
  meandf[1,i] <- mean(movie22[which(movie22[,i+9]==1),"directorEff"]
                      ,na.rm = T)
}
sumall <- 0
for(i in 1:nrow(movie22)){
  if(is.na(movie22[i,"directorEff"])){
    for(j in 1:15){
      if(movie22[i,j+9]==1) sumall <- sum(sumall,meandf[1,j])
    }
    movie22[i,"directorEff"] <- sumall
  }
  sumall <- 0
}
rm(sumall)
rm(meandf)
moviestar <- read.csv("movieFiltered20.csv")
title <- moviestar$title
release <- moviestar$release
distributor <- moviestar$distributor
director <- moviestar$director
movie23 <- cbind(title, release, distributor, director, movie22)

write.csv(movie23, "movieFiltered22.csv")


### 상위 5% 하위 95%##################
quantile(movie23$allpeople, probs = 0.90)

movie5 <- movie23[movie23$allpeople >= 242554.2 , ]
movie95 <- movie23[movie23$allpeople < 242554.2 , ]
nrow(movie5)
nrow(movie95)

write.csv(movie5,"movieu.csv",row.names = F)
write.csv(movie95,"movied.csv",row.names = F)
