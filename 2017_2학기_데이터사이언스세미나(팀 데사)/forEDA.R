
library(nycflights13)
library(dplyr)
library(ggplot2)

airlines <- as.data.frame(nycflights13::airlines)
airports <- as.data.frame(nycflights13::airports)
flights <- as.data.frame(nycflights13::flights)
planes <- as.data.frame(nycflights13::planes)
weather <- as.data.frame(nycflights13::weather)
state <- read.csv("us-airports.csv",header = T)

#weather의 time_hour이 kst이므로 이를 utc로 바꿈
attr(weather$time_hour,"tzone") <- "UTC"

# 자료분석
a <- function(x){
  sum(is.na(x))
}
b <- function(x){
  levels(as.factor(x))
}

#flights와 weather결합 (fw = flights + weather)

fw <- left_join(flights, weather, by = c("origin", "time_hour"))

#필요없는 변수 제거
fw <- subset(fw, select = -c(year.y,month.y,day.y,hour.y,pressure))
#### NA 처리는 우선 제거하는걸로(dep_delay) > 이거 더 나은 처리방법 필요!!
fw <- fw %>%filter(!is.na(dep_delay))
#이름 바꾸고
names(fw)[c(1,2,3,17)] <- c("year","month","day","hour")
# origin factor
fw$origin <- as.factor(fw$origin)
# 12-2 : winther, 3-5 : spring, 6-8 : summer, 9-11 : fall
fw <- fw %>% mutate(season = ifelse(month == 12 | month ==1 | month ==2,"winter",
                              ifelse(month==3 | month==4 | month ==5,"spring",
                                     ifelse(month==6 | month ==7 | month==8,"summer",
                                            ifelse(month==9 | month ==10 | month ==11,"fall",NA)))))
# season factor화
fw$season <- as.factor(fw$season)
# hour 5-9 : morning , 10-14 : afternoon , 15-18 : late afternoon , 
# 19-22 : evening&night
fw <- fw %>% mutate(partOfDay = ifelse(hour ==5 | hour ==6 | hour ==7 | hour ==8 | hour ==9,"Morning",
                                       ifelse(hour ==10 | hour ==11 | hour ==12 | hour ==13 | hour ==14,"Noon",
                                              ifelse(hour ==15 | hour ==16 | hour ==17 | hour ==18,"Late Afternoon","Night"))))
fw$partOfDay <- as.factor(fw$partOfDay)

# planes data join하기
beforePlanesjoin <- planes %>% select(tailnum,type,manufacturer,
                                      model,engines,seats)
fw <- left_join(fw, beforePlanesjoin,by = "tailnum")

fw <- fw %>% mutate(planeSize = ifelse(seats <= 30, "small",
                                       ifelse(seats <= 260, "medium","large")))

# airports data join 하기
airports2 <- left_join(airports, state, by = "faa")
airports2 <- airports2 %>% select(-name.y, -tzone, -dst,-tz,-alt,-name.x,
                                  -lat,-lon) 

destination <- levels(as.factor(fw$dest))
airports2 <- airports2 %>% filter(faa %in% destination)
colnames(airports2)[1] <- "dest"

fw <- left_join(fw, airports2, by = "dest")
fw <- fw %>% filter(!is.na(state))
fw <- fw %>% mutate(regions = ifelse(state %in% c("AK","WA","OR","ID","MT","WY","CA","NV","AZ","UT","CO","NM","HI"), "west",
                                     ifelse(state %in% c("ND","SD","NE","KS","MN","IA","MO","WI","IL","IN","MI","OH"),"central",
                                            ifelse(state %in% c("PA","NY","NJ","CT","RI","MA","VT","NH","ME"),"northeast","south"))))

fw <- fw %>% mutate(stdPrecip = ifelse(precip ==0,NA,
                                       ifelse(precip <= 0.10,"0.01~0.1",
                                              ifelse(precip <= 0.20,"0.11~0.20",
                                                     ifelse(precip <= 0.30,"0.21~0.30",
                                                            ifelse(precip <= 0.40,"0.31~0.40","Over 0.40"))))))

fw <- fw %>% mutate(stdVisib = ifelse(visib ==0,"0",
                                       ifelse(visib <= 1,"0~1",
                                              ifelse(visib <= 2,"1~2",
                                                     ifelse(visib <= 3,"2~3",
                                                            ifelse(visib <= 5,"3~5",
                                                                   ifelse(visib <= 8,"5~8",NA)))))))

colnames(airlines)[2] <- "airlineName"
fw <- left_join(fw, airlines,by = "carrier")
fw$airlineName <- as.factor(fw$airlineName)

#####아 이것도 일단 그냥 0 이하면 delay 기준 못잡겠다.
fw <- fw %>% mutate(checkDelay = ifelse(dep_delay >0, "delayed","ontime"))


##*******************************************************************##

### Where does planes go to?
# LGA, JFK, EWR
where <- fw %>% filter(origin == "LGA" & season == "winter" & !is.na(regions))

d <- where %>% 
  group_by(regions) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)*100)

p <- ggplot(d, aes(x ="", y= perc, 
                    fill = regions)) +
  geom_bar(stat="identity", width = 1)  

p + coord_polar(theta="y", start = 0)



### Plane Size (Pie chart)

dataplane <- fw %>% filter(origin =="LGA" 
                           & season == "winter"
                           & !is.na(planeSize))

d1 <- dataplane %>% 
  group_by(planeSize) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)*100)

p <- ggplot(d1, aes(x ="", y= perc, 
                    fill = planeSize)) +
  geom_bar(stat="identity", width = 1)  

p + coord_polar(theta="y", start = 0)


###Chart partOfDay

# if origin : EWR , season: winter 일 때,
dataVisib <- fw %>% filter(origin=="LGA" & season =="winter")

d2 <- dataVisib %>% 
  group_by(partOfDay,checkDelay) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

p<- ggplot(d2, aes(x = factor(partOfDay), 
               y = perc*100, 
               fill = factor(checkDelay))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Part of Day", y = "percent", fill = "checkDelay") +
  theme_minimal(base_size = 14)

###Chart heatmap

# if origin : EWR , season: winter 일 때,
dataHeat <- fw %>% filter(origin=="LGA" & season =="winter" &
                             !is.na(stdPrecip) & !is.na(stdVisib)  & checkDelay =="delayed")

dataHeating <- dataHeat %>% group_by(stdPrecip, stdVisib) %>% 
  summarize(delayed = n())
hchart(dataHeating, "heatmap", 
       hcaes(x = stdPrecip, y = stdVisib, value = delayed)) 


############################################################





