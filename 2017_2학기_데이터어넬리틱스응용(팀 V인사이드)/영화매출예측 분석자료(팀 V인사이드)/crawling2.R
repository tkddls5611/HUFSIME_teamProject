#### 감독 효과 

finalmovie <- read.csv("filteredInfo.csv")
num <- grep(",",finalmovie$director)
finalmovie$director <- as.character(finalmovie$director)
finalmovie$release <-  as.Date(finalmovie$release,origin = "1899-12-30")

# 감독이 2명이상인 경우 처음 감독만 선택 
for(i in 1:nrow(finalmovie)){ # i=526
  if(i %in% num){
    location <- regexpr(",",finalmovie$director[i])[1]
    finalmovie$director[i] <- substr(finalmovie$director[i],1,location-1)
  }
}


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

dir <- finalmovie$director
money <- 0
ppl <- 0
countA <- 0

last3Director <- as.data.frame(matrix(nrow = nrow(finalmovie), ncol = 4))
colnames(last3Director) <- c("title","director","avgmoney","avgppl")
last3Director$title <- finalmovie$title
last3Director$director <- dir

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://www.kobis.or.kr/kobis/business/mast/peop/searchPeopleList.do"
remDr$navigate(url)

for(i in 2336:length(dir)){ # i =4
  tryCatch(suppressMessages({
    if(i !=1 ){ 
      webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[2]")
      webElem$clickElement()
    }
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[1]/input")
    webElem$sendKeysToElement(list(dir[i])) # 감독이름
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[2]/span/input")
    webElem$sendKeysToElement(list(finalmovie$title[i])) # 필모그라피
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[1]")
    webElem$clickElement() # 조회
    Sys.sleep(1)
    
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/table/tbody/tr/td[1]/a")
    webElem$clickElement() # 감독클릭
    Sys.sleep(0.5)
    
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/ul/li[2]/a")
    webElem$clickElement() # 필모그래피  
    Sys.sleep(1)
    
    for(j in 1:20){ # j=11
      checkY <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[2]/span[1]")  
      get <- remDr$findElement(using = "xpath",checkY)
      result1 <- as.numeric(unlist(get$getElementText()))
      checkDir <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[1]")
      get <- remDr$findElement(using = "xpath",checkDir)
      result2 <- unlist(get$getElementText())
      if(grep("감독",result2) ==T & result1 < 2015 & result1 > 2011){
        moneypath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,
                            "]/dl/dd[3]/em[1]")
        moneyst <- subcrawl(moneypath)
        moneynum <- as.numeric(gsub(",","",substr(moneyst,1,nchar(moneyst)-1)))
        if(!is.na(moneynum))  money <- sum(money, moneynum)
        pplpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,
                          "]/dl/dd[3]/em[2]")
        pplst <- subcrawl(pplpath)
        pplnum <- as.numeric(gsub(",","",substr(pplst,1,nchar(pplst)-1)))
        if(!is.na(pplnum)) ppl <- sum(ppl,pplnum)
        countA <- countA+1
      }else if(result1 <= 2011){
        break
      }
    } # j end
  }),
  error = function(e){ 
    errortitle <- finalmovie$title[i]
    print(paste0("error at(",i,") : ",errortitle))}
  )
  tryCatch(suppressMessages({
    last3Director$avgmoney[i] <- money/countA
    last3Director$avgppl[i] <- ppl/countA
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/a[2]/img")
    webElem$clickElement() # 닫기
    
  }),error = function(e){
    last3Director[i,3:4] <- NA
  })
  
  money <- 0
  ppl <- 0
  countA <- 0
  
}# i end

write.csv(last3Director,"last3Director2336-.csv")



remDr$close()
driver$server$stop()
















