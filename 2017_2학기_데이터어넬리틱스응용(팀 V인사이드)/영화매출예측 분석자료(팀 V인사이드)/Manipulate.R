# 0. 위치 설정 및 필요 library
library(xlsx)

# 1. 파일(131개)를 가져오기 위한 작업(제목)
## 결과물 : filename

filename <- as.data.frame(matrix(nrow = 131, ncol =3))
colnames(filename) <- c("StartDate","name", "withXlsx")
start <- as.Date("15.01.01","%y.%m.%d")
end <- start + 6
for(i in 1:131){
  # filename$start
  formatStart <- as.character(format(start,"%y.%m.%d"))
  start <- start +7
  
  # filename$end
  if( i == 53 | i == 105){
    formatEnd <- as.character(format(end, "%y.%m.%d"))
  }else{ formatEnd <- as.character(format(end, "%m.%d")) }
  end <- start + 6
  
  # filename input
  filename$StartDate[i] <- formatStart
  filename$name[i] <- paste(formatStart, formatEnd, sep = "-")
  filename$withXlsx[i] <- paste0(filename$name[i],".xlsx")
}

head(filename)

# 2. 파일 불러오기 및 첫 null 행 제거
## 결과물 : X날짜 variable 131개
## * 데이터셋 중 15.03.25-04.01.xlsx와 15.07.25-07.29.xlsx는 
##   일주일 간격에서 벗어남(제목의 오류) -> 직접 변수 지정(변수 이름도 올바르게)

for(i in 1:131){ # i =1
  nam <- paste0("X",filename$StartDate[i])
  if(i == 13){ # 15.03.25-04.01.xlsx
    xlsxName <- paste0(getwd(), "/movieData/15.03.25-04.01.xlsx")
    sheetName <- "15.03.25-04.01"
    assign(nam, read.xlsx2(file = xlsxName, 
                           sheetName = sheetName, startRow = 3,stringsAsFactors = F))
  }else if(i == 30){ # 15.07.25-07.29.xlsx
    xlsxName <- paste0(getwd(), "/movieData/15.07.25-07.29.xlsx")
    sheetName <- "15.07.25-07.29"
    assign(nam, read.xlsx2(file = xlsxName, 
                           sheetName = sheetName, startRow = 3,stringsAsFactors = F))
  }else{
    xlsxName <- paste0(getwd(),"/movieData/",
                       filename$withXlsx[i])
    assign(nam, read.xlsx2(file = xlsxName, 
                           sheetName = filename$name[i],
                           startRow = 3,stringsAsFactors = F))
  }
}
# 1행 지우기(빈 열) & 열 이름 지정 & format 정리 
for(i in 1:131){ # i =1
  nam <- paste0("X",filename$StartDate[i])
  dataset <- get(paste0("X",filename$StartDate[i]))
  dataset <- dataset[-1,]
  dataset <- dataset[-nrow(dataset),]
  colnames(dataset) <- c("rank","title","release","sales","salesShare",
                         "accSales","audience","accAudience","screenN","filmedN",
                         "nation1st","nation","production","distributor",
                         "rate","genre","director","actor")
  dataset$rank <- as.numeric(dataset$rank)
  dataset$release <- as.numeric(dataset$release)
  dataset$sales <- as.numeric(dataset$sales)
  dataset$salesShare <- as.numeric(dataset$salesShare)
  dataset$accSales <- as.numeric(dataset$accSales)
  dataset$audience <- as.numeric(dataset$audience)
  dataset$accAudience <- as.numeric(dataset$accAudience)
  dataset$screenN <- as.numeric(dataset$screenN)
  dataset$filmedN <- as.numeric(dataset$filmedN)
  assign(nam, dataset)
  
}


# 데이터를 크게 2가지로 나눈다.
# 영화 자체에 대한 정보 : 영화명, 개봉일, 대표국적, 국적, 제작사, 
#                         배급사, 등급, 장르, 감독, 배우  > movieInfo
# 영화 상영 후 변화하는 정보 : 영화명 , 순위, 매출액, 매출액점유율, 누적매출액, 
#                              관객수, 누적관객수, 스크린수, 상영횟수 > movieAS(movie-After-Screening)


# 영화 자체에 대한 정보 : movieInfo
movieInfo <- as.data.frame(matrix(ncol = 9))
colnames(movieInfo) <- c("title", "release", "nation1st","nation","production",
                         "distributor", "rate","genre","director")
# # 영화 상영 후 정보 (movie-After-Screening): movieAs
# movieAs <- as.data.frame(matrix(ncol = 10))
# colnames(movieAs) <- c("title","date","rank","sales","salesShare",
#                        "accSales","audience","accAudience","screenN","filmedN")
# only 배우(movieAct)
actor <- as.data.frame(matrix(ncol = 2,nrow = nrow(movieFiltered)))
colnames(actor) <- c("title","actor")
actor$title <- movieFiltered$title

# actor
for(i in 1:131){ # i =1
  dataset <- get(paste0("X",filename$StartDate[i]))
  for(j in 1:nrow(dataset)){ # j = 1
    # actor
    movierow <- match(dataset$title[j],actor$title)
    if(is.na(actor$actor[movierow]) & !is.na(movierow)) actor$actor[movierow] <- dataset$actor[j]
  }
}

for(i in 1:nrow(actor)){ # i = 2386
  if(nchar(actor$actor[i])==0 | is.na(actor$actor[i])) actor$actor[i] <- NA
}
write.csv(actor,"actorX.csv")

  #movieInfo
for(i in 1:131){ # i =1
  dataset <- get(paste0("X",filename$StartDate[i]))
  for(j in 1:nrow(dataset)){ # j = 1
    # movieInfo
    if(dataset$title[j] %in% movieInfo$title){
      movierow <- match(dataset$title[j],movieInfo$title)
      for(k in 2:9){ # k=2
        if(is.na(movieInfo[movierow,k]) | length(movieInfo[movierow,k])==0){
          missingcol <- colnames(movieInfo[k])
          movieInfo[movierow,k] <- dataset[j,missingcol]
        }
      }
    }else{
      movieInfo <- rbind(movieInfo, dataset[j,c(2,3,11,12,13,14,15,16,17)])
    }
  }
  
}
movieInfo <- movieInfo[-1,]

#movieAs (??�� ??)
# tempset <- movieAs
# for(i in 1:131){ # i =1
#   dataset <- get(paste0("X",filename$StartDate[i]))
#   for(j in 1:nrow(dataset)){ # j = 1
#     tempset[1] <- dataset[j,2]
#     tempset[2] <- filename$StartDate[i]
#     tempset[c(3:10)] <- dataset[j,c(1,4,5,6,7,8,9,10)]
#     movieAs <- rbind(movieAs, tempset)
#   }
# }
# movieAs <- movieAs[-1,]


### ?????????? 

# movieInfo <- read.csv("movieInfo.csv")
# 
# # ??ȭ��?? ?????????? ???? ??????
# cum <- as.data.frame(matrix(nrow = nrow(movieInfo), ncol= 3))
# colnames(cum) <- c("title", "??????????", "??????????")
# cum$title <- movieInfo$title
#  <- 0
# cum$?????????? <- 0
# 
# for(i in 130:131){ # i =98
#   dataset <- get(paste0("X",filename$StartDate[i]))
#   for(j in 1:nrow(dataset)){ # j = 1
#     if(dataset$title[j] %in% cum$title){
#       movierow <- match(dataset$title[j],cum$title)
#       cum$??????????[movierow] <- max(cum$??????????[movierow],dataset$accAudience[j])
#       cum$??????????[movierow] <- max(cum$??????????[movierow], dataset$accSales[j])
#     }
#   }
#   
# }
# # ???߿? ?غ??? 
# compare <- as.data.frame(matrix(nrow = nrow(movieInfo), ncol= 3))
# colnames(compare) <- c("title", "first","last")
# compare$title <- cum$title
# compare$last <- cum$??????????
# compare$first <- 0
# 
# for(i in 130:131){ # i =98
#   dataset <- get(paste0("X",filename$StartDate[i]))
#   for(j in 1:nrow(dataset)){ # j = 1
#     if(dataset$title[j] %in% compare$title){
#       movierow <- match(dataset$title[j],compare$first)
#       if(compare$first[movierow] ==0) compare$first[movierow] <- dataset$accSales[j]
#     }
#   }
# }

################################################################
#####
################################################################

#### 영화 1차 필터
## 1. 일본 청불 & 2014년 초과 
library(dplyr)
movieInfo <- read.csv("movieInfo.csv",fileEncoding = "euc-kr")
movieInfo$release <-  as.Date(movieInfo$release,origin = "1899-12-30")
movieInfo <- movieInfo %>% 
  filter(as.numeric(substr(movieInfo$release,1,4)) > 2014 )

movieInfo <- movieInfo %>% filter(nation1st != "일본" &
                              (rate != "청소년관람불가" |
                              rate != "" |
                              rate != "??��" |
                              rate != "18????????" |
                              rate != "18?? ?̸??? ?ڴ? ?????? ?? ???? ????" |
                              rate != "?̼????ڰ????Ұ?" |
                              rate != "?????л??????Ұ?,û?ҳ??????Ұ?"))
finalmovie <- movieInfo
write.csv(finalmovie, "filteredInfo.csv")

## 2. 최종 누적값, 누적 관람객 예측
# finalmovie <- read.csv("filteredInfo.csv")
head(finalmovie)
finalmovie <- finalmovie[,-1]
finalmovie$allpeople <- NA
finalmovie$allmoney <- NA
finalmovie$star <- NA

# pplmoney <- read.csv("pplmoney.csv",fileEncoding="euc-kr")

for(i in 1:nrow(finalmovie)){ # 누적관람객, 누적매출액 
    getlocation <- match(finalmovie$title[i],pplmoney$title)
    finalmovie$allpeople[i] <- pplmoney$ppl[getlocation]
    finalmovie$allmoney[i] <- pplmoney$money[getlocation]
}

# star <- read.csv("movieRate.csv",fileEncoding="euc-kr")
star <- star[,1:4]
for( i in 1:nrow(finalmovie)){ # 별점 수 
  getlocation <- match(finalmovie$title[i],star$title)
  finalmovie$star[i] <- as.numeric(as.character(star$totalRate[getlocation]))
}

# NA 별점 추가

## movieFiltered <- read.csv("movieFiltered.csv",fileEncoding= "euc-kr")

starfill <- movieFiltered[,c("title","release","star")]

library(RSelenium)
require(XML)

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

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# url naviagate
url <- "http://movie.naver.com/index.nhn"
remDr$navigate(url)

# 로그인 
webElem <- remDr$findElement(using = "xpath", "//*[@id=\"gnb_login_button\"]/span[3]")
webElem$clickElement()

# 아이디 비번 
webElem <- remDr$findElement(using = "xpath","//*[@id=\"id\"]")
webElem$sendKeysToElement(list("tkddls011"))
webElem <- remDr$findElement(using = "xpath","//*[@id=\"pw\"]")
webElem$sendKeysToElement(list("tkfkdgogksqudtj!"))
webElem <- remDr$findElement(using = "xpath","//*[@id=\"frmNIDLogin\"]/fieldset/input")
webElem$clickElement()
webElem <- remDr$findElement(using = "xpath","//*[@id=\"frmNIDLogin\"]/fieldset/span[2]")
webElem$clickElement()

for(i in 57:nrow(starfill)){ # i=6
  tryCatch(suppressMessages({
    # 영화 검색 
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"ipt_tx_srch\"]")
    if(is.na(starfill$star[i])){
      webElem$sendKeysToElement(list(as.character(starfill$title[i]), key = "enter"))
      webElem <- remDr$findElement(using = "xpath","//*[@id=\"old_content\"]/ul[2]/li/dl/dt/a")
      webElem$clickElement()
      tempvec <- c()
      for(j in 1:4){ # j=1
        empath <- paste0("//*[@id=\"pointNetizenPersentBasic\"]/em[",j,"]")
        tempvec[j] <- subcrawl(empath)      
      }
      starfill$star[i] <- as.numeric(paste0(tempvec[1],tempvec[2],tempvec[3],tempvec[4]))
      
    }  
  }),error = function(e){
    starfill$star[i] <- NA
  })
  
}

write.csv(starfill,"starfill.csv")



remDr$close()
driver$server$stop()


################################################################
#####
################################################################

# 스크린 수 누적 
screen <- data.frame(title = movieFiltered$title, allscreen = 0)

#movieInfo
for(i in 1:131){ # i =1
  dataset <- get(paste0("X",filename$StartDate[i]))
  for(j in 1:nrow(screen)){ # j=1
    if(!is.na(match(as.character(screen$title[j]), dataset$title))){
      getlocation <- match(as.character(screen$title[j]), dataset$title)
      screen$allscreen[j] <- sum(screen$allscreen[j],dataset$screenN[getlocation])
    }
  }
  print(paste0(paste0("X",filename$StartDate[i])," finished!", i, "번째"))
}
