#### 배우 효과 

actor <- read.csv("actorX.csv")
actvc <- actor$actor
actordf <- data.frame(title = actor$title,actor1 = NA, actor2 = NA, 
                      actor3 = NA, avgppl = NA)
for(i in 1:nrow(actordf)){ #i=1
   splitdata <- strsplit(as.character(actvc[i]),",")
   actordf$actor1[i] <- splitdata[[1]][1]
   actordf$actor2[i] <- splitdata[[1]][2]
   actordf$actor3[i] <- splitdata[[1]][3]
}

allact <- data.frame(title = c(as.character(actordf$title), as.character(actordf$title),
                               as.character(actordf$title)),
                     actor = c(actordf$actor1, actordf$actor2,actordf$actor3))
allact2 <- allact[!duplicated(allact$actor),]

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

allact2$avgppl <- NA
ppl <- 0 # 총 관람객 수 
countA <- 0 # 영화 갯수 
pagenum <- 4 # 페이지 넘길때 (2page = 4)

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://www.kobis.or.kr/kobis/business/mast/peop/searchPeopleList.do"
remDr$navigate(url)

for(i in 3037:nrow(allact2)){ # i =
  tryCatch(suppressMessages({
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[1]/input")
    webElem$sendKeysToElement(list(allact2$actor[i])) # 배우이름
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[2]/span/input")
    webElem$sendKeysToElement(list(allact2$title[i])) # 필모그라피
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[1]")
    webElem$clickElement() ; Sys.sleep(1) # 조회
    
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/table/tbody/tr/td[1]/a")
    webElem$clickElement() # 배우 클릭
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/ul/li[2]/a")
    webElem$clickElement() ; Sys.sleep(1) # 필모그래피 
    
    for(j in 1:10){ # j=1
                           
      checkyear <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[2]/span[1]")
      tryCatch(suppressMessages({
        get <- remDr$findElement(using = "xpath",checkyear)
        year <- as.numeric(unlist(get$getElementText()))
      }), error = function(e){year <<- NA})
      if(year == 2014 | year == 2013 | year == 2012){ # year check
        
        checkactor <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[1]")
        get <- remDr$findElement(using = "xpath",checkactor)
        isactor <- unlist(get$getElementText())
        
        if(grep("주연",isactor) ==T){ # 주연 crawling
        pplpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",2,
                          "]/dl/dd[3]/em[2]")
        pplst <- subcrawl(pplpath)
        pplnum <- as.numeric(gsub(",","",substr(pplst,1,nchar(pplst)-1)))
        if(!is.na(pplnum)){ 
          ppl <- sum(ppl,pplnum)
          countA <- countA+1 
        }
        } # 주연 crawling end
      }else if(year < 2012 | is.na(year)){
        break
      } # year check end
      if(j ==10 & year >= 2012 ){
        j <- 1
        webElem <- remDr$findElement(using = "xpath",paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[",pagenum,"]"))
        webElem$clickElement() # 배우 클릭
        pagenum <- pagenum +1
      }
    } # j end
  }),
  error = function(e){ 
    errortitle <- allact2$title[i]
    print(paste0("error at(",i,") : ",errortitle))
  }
  )
  tryCatch(suppressMessages({
    allact2$avgppl[i] <- ppl/countA
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/a[2]/img")
    webElem$clickElement() # 닫기
  }),error = function(e){
    allact2$avgppl[i] <<- NA
  })

  webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[2]")
  webElem$clickElement() # 초기화 
  ppl <- 0
  countA <- 0
  pagenum <- 4
}# i end

remDr$close()
driver$server$stop()

head(actordf)
for(i in 1:nrow(actordf)){ # i = 9
  sumall <- 0
  countA <- 3 - sum(is.na(actordf[i,2:4]))
  for(j in 1:countA){ # j = 1
    matchnm <- match(actordf[i,j+1],allact2$actor)
    if(!is.nan(allact2$avgppl[matchnm]) & !is.na(allact2$avgppl[matchnm])){
      sumall <- sum(sumall, allact2$avgppl[matchnm])
    } # avgppl NAN NA check end
  }# j end
  if(countA != 0){ actordf$avgppl[i] <- sumall/countA}else{ actordf$avgppl[i] <- 0}
}# i end 

write.csv(actordf, "actorEff.csv")











