season <- read.csv("season.csv")
#install.packages(nortest)
library(dplyr) 
library(nortest)
head(season)
season_se <-season %>% select(title,season,allpeople)
season_se%>% head(10)
str(season_se)
shapiro.test(season_se$allpeople)
hist(season_se$allpeople)
#plot(season,allpeople)

season<-as.character(season)
#anova이용한 방법
anova_out<-anova(lm(allpeople~season,data=season_se))
anova_out
#oneway이용한방법 
oneway_out<-oneway.test(allpeople~season,data=season_se)
oneway_out
#aov 이용 (out2)
aov_out<-aov(allpeople~season,data=season_se)
aov_out
#정규분포아니면 kruskal이용
kruskal.test(allpeople~season,data=season_se)


# moviefiltered
head(movieFiltered)
head(season)
movieFiltered$season <- NA
for(i in 1:nrow(movieFiltered)){ # i =1
  matchrow <- match(movieFiltered$title[i], season$title)
  movieFiltered$season[i] <- season$season[matchrow]
}
write.csv(movieFiltered, "movieFiltered.csv")
