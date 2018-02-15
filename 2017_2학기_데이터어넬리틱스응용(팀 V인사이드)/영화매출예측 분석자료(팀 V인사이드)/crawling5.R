#### 영화 상영 시간 & 예고편 조회수 

movieFiltered <- read.csv("movieFiltered.csv")
title <- movieFiltered$title
addf <- data.frame(title = movieFiltered$title,
                    showtime = NA, trailertime  = NA)

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

crawlhere <- 5
foundmain <- F
time <- 0
played <- 0

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://movie.naver.com/movie/running/movieclip.nhn?subcategoryid=TRAILER&order="
remDr$navigate(url)

for(i in 1:nrow(addf)){ # i =5
  tryCatch({
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"searchKeyword\"]")
    webElem$sendKeysToElement(list(addf$title[i], key = "enter")) # 영화 제목
    # 메인예고편 찾기
    for(j in 5:1){ # j =5
      maintitle <- subcrawl(paste0("//*[@id=\"content\"]/div[1]/div[1]/div[2]/ul/li[",j,"]/dl/dt/a"))
      if(!is.na(maintitle) & length(grep(pattern = "메인",maintitle)) == T){
        crawlhere <- j ; foundmain <- T
        break
      }
    } # j end
    if(foundmain){ # 메인 예고편 찾음
      allcrawl <- subcrawl(paste0("//*[@id=\"content\"]/div[1]/div[1]/div[2]/ul/li[",crawlhere,"]/dl/dd[1]/dl/dd[1]"))
      spices <- strsplit(allcrawl, split = "\\|")[[1]]
      time <- as.numeric(substr(spices[2],2,nchar(spices[2])-2)) # 상영시간 
      played <- as.numeric(gsub(",","",substr(spices[4],5,nchar(spices[4])))) # 예고편 조회수 
    }else{
      for(j in 1:5){ # j =1 ; 메인 예고편 못찾음 
        allcrawl <- subcrawl(paste0("//*[@id=\"content\"]/div[1]/div[1]/div[2]/ul/li[",j,"]/dl/dd[1]/dl/dd[1]"))
        if(!is.na(allcrawl)){
          spices <- strsplit(allcrawl, split = "\\|")[[1]]
          time <- max(time,as.numeric(substr(spices[2],2,nchar(spices[2])-2))) # 상영시간 
          played <- max(played,as.numeric(gsub(",","",substr(spices[4],5,nchar(spices[4]))))) # 예고편 조회수
        }
      } # j end
    } # 메인 예고편 찾음/못찾음 end
    addf$showtime[i] <- time
    addf$trailertime[i] <- played 
  },error = function(e){
    errortitle <- addf$title[i]
    print(paste0("error at (",i,") : ",errortitle))
  })
  
  crawlhere <- 5
  foundmain <- F
  time <- 0
  played <- 0
  
} # i end


remDr$close()
driver$server$stop()

write.csv(addf,"addf.csv")
# NAN 값에 대한 수동 크롤링 진행
alldis2 <- read.csv("alldis.csv")

head(disdf)
for(i in 1:nrow(disdf)){ # i = 1
  sumall <- 0
  countA <- 2 - sum(is.na(disdf[i,2:3]))
  for(j in 1:countA){ # j = 1
    matchnm <- match(disdf[i,j+1],alldis2$distributor)
    if(!is.na(alldis2$distributorEff[matchnm])){
      sumall <- sum(sumall, alldis2$distributorEff[matchnm])
    } # avgppl NAN NA check end
  }# j end
  if(countA != 0){
    disdf$distributorEff[i] <- sumall/countA 
  }else{ 
    disdf$distributorEff[i] <- NA
  }
}# i end 

write.csv(disdf, "distriEff.csv")

# cbind

movieFiltered <- read.csv("movieFiltered.csv")
alldis3 <- read.csv("distriEff.csv")

head(alldis3)
movieFiltered <- cbind(movieFiltered, alldis3$distributorEff) ; colnames(movieFiltered)[15] <- "distributorEff"
head(movieFiltered) 




