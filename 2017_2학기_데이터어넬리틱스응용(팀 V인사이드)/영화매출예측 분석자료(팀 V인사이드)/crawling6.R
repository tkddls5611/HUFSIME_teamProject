library(dplyr)

# 영화 상영시간 추가 크롤링
movie2 <- read.csv("movieFiltered10.csv")
colnames(movie2)
movie3 <- movie2 %>% filter(is.na(showtime)) %>% select(title, showtime)

######################################################
######Crawling
#######################################################

library(RSelenium)
library(XML)

# function for crawling
subcrawl <- function(path){
  result <- NA
  tryCatch(suppressMessages({
    get <- remDr$findElement(using = "xpath",path)
    result <- unlist(get$getElementText())
  }),
  error = function(e){ result <- NA }
  )
  return(result)
}

loc <- 0

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://movie.naver.com/"
remDr$navigate(url)

for(i in 1:nrow(movie3)){
  tryCatch(suppressMessages({
    webElem <- remDr$findElement(using= "xpath","//*[@id=\"ipt_tx_srch\"]")
    webElem$sendKeysToElement(list(movie3$title[i], key = "enter")) # 영화 제목
    
    allcrawl <- subcrawl("//*[@id=\"old_content\"]/ul[2]/li/dl/dd[2]")
    spices <- strsplit(allcrawl, split = "\\|")[[1]]
    for(j in 1:4){ if(length(grep("분",spices[j]))==1){loc <- j ; break}} # j=1
    movie3$showtime[i] <- as.numeric(substr(spices[loc],1,nchar(spices[loc])-2)) # 상영시간 
  }),error = function(e){print(paste(movie3$title[i],"error!"))})
  
  if(i %% 20 ==0){print(paste(i, "finished!"))}
  loc <-0
}


remDr$close()
driver$server$stop()


for(i in 1:nrow(movie3)){ # i =1
  matchrow <- match(movie3$title[i], movie2$title)
  if(is.na(movie2$showtime[matchrow])){ 
    movie2$showtime[matchrow] <-  movie3$showtime[i]
  }
}
