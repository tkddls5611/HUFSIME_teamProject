
library(RSelenium)
require(XML)
library(xlsx)

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

# 영화 이름 검색
## 영화 이름 검색 할 목록(movieInfo.csv)
movieInfo <- read.csv("movieInfo.csv")
title <- as.vector(movieInfo$title)
length(title)

release <- movieInfo$release
release <-  as.Date(release,origin = "1899-12-30")
release <- substr(release,1,4)


# 결과 data frame
movieRate <- as.data.frame(matrix(nrow = length(title), ncol = 15))
colnames(movieRate) <- c("title","totalRate", "rateN",
                         "maleRate","femaleRate","malePer","femalePer",
                         "age10","age20","age30","age40",
                         "age10Per","age20Per","age30Per","age40Per")
movieRate$title <- title
path <- c("//*[@id=\"actual_point_tab_inner\"]/div/em[1]",
          "//*[@id=\"actual_point_tab_inner\"]/div/em[3]",
          "//*[@id=\"actual_point_tab_inner\"]/div/em[4]", # totalRate
          "//*[@id=\"actual_point_tab_inner\"]/span/em", # rateN
          "//*[@id=\"actual_group_graph\"]/div[1]/ul/li[1]/span[2]/em", # maleRate
          "//*[@id=\"actual_group_graph\"]/div[1]/ul/li[2]/span[2]/em", #femaleRate
          "//*[@id=\"actualGenderPointGraph\"]/svg/text[1]", # maleRate
          "//*[@id=\"actualGenderPointGraph\"]/svg/text[2]", # femaleRate
          "//*[@id=\"actual_group_graph\"]/div[2]/ul/li[1]/span/em", # age10
          "//*[@id=\"actual_group_graph\"]/div[2]/ul/li[2]/span/em", # age20
          "//*[@id=\"actual_group_graph\"]/div[2]/ul/li[3]/span/em", # age30
          "//*[@id=\"actual_group_graph\"]/div[2]/ul/li[4]/span/em", # age40
          "//*[@id=\"actualAgePointGraph\"]/svg/text[1]", # age10Per
          "//*[@id=\"actualAgePointGraph\"]/svg/text[2]", # age20Per
          "//*[@id=\"actualAgePointGraph\"]/svg/text[3]", # age30Per
          "//*[@id=\"actualAgePointGraph\"]/svg/text[4]") # age40Per

# movieRate <- as.data.frame(read.csv("movieRate.csv"))
# movieRate <- movieRate[,-1]

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
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

for(titleN in 1:length(title)){ 
  tryCatch(suppressMessages({
    ## 검색
    webElem <- remDr$findElement(using = "xpath", "//*[@id=\"ipt_tx_srch\"]")
    webElem$sendKeysToElement(list(title[titleN], key = "enter"))
    ## 첫번째 클릭
    if(is.na(release[titleN])==T){
      webElem <- remDr$findElement(using = "xpath","//*[@id=\"old_content\"]/ul[2]/li/dl/dt/a")
      webElem$clickElement()
    }else{
      for(original in 1:5){
        tempPath <- paste0("//*[@id=\"old_content\"]/ul[2]/li[",original,"]/dl/dt/a")
        webElem <- remDr$findElement(using = "xpath",tempPath)
        webElem$clickElement()
        tempY <- subcrawl("//*[@id=\"content\"]/div[1]/div[2]/div[1]/strong") # 제목 + 년도
        checkY <- substr(tempY, nchar(tempY)-3,nchar(tempY)) # 년도 
        checkY2 <- 123
        tryCatch(suppressMessages({
          checkY2 <- subcrawl("//*[@id=\"content\"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[4]/a[1]")
          if(is.na(checkY2)){ checkY2 <- "000"}
        }),error = function(e){ checkY2 <- "000"})
        
        if(release[titleN] == checkY | release[titleN] == checkY2){
          break
        }else{
          remDr$goBack()
        }
      }
    }
    for(rating in 1:10){
      if(subcrawl(paste0("//*[@id=\"movieEndTabMenu\"]/li[",rating,"]/a")) == "평점"){
        ratingpath <- paste0("//*[@id=\"movieEndTabMenu\"]/li[",rating,"]/a")
        break
      }
    } ## 평점 클릭
    webElem <- remDr$findElement(using = "xpath",ratingpath)
    webElem$clickElement()
    Sys.sleep(0.5)
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"actual_point_tab_inner\"]/div")  # 관람객 평점 클릭
    webElem$clickElement()
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"actual_group\"]") # 남녀별,연령별 평점 클릭
    webElem$clickElement()
    for(i in 2:15){
      if(i ==2){
        movieRate[titleN,i] <- paste0(subcrawl(path[1]),".",subcrawl(path[2]),subcrawl(path[3]))
      }else{
        movieRate[titleN,i] <- subcrawl(path[i+1])
      }
    }  
  }),
  error = function(e){ 
    movieRate[titleN,2:15] <- NA
    errortitle <- movieRate$title[titleN]
    print(paste0("error at(",titleN,") : ",errortitle))}
  )
  
  if(titleN %% 30 ==0){
    print(paste0(titleN," finished"))
  }
  if(titleN %% 20 == 0){
    write.csv(movieRate,file = paste0(getwd(),"/movieRate.csv"))
  }
}

remDr$close()
driver$server$stop()
