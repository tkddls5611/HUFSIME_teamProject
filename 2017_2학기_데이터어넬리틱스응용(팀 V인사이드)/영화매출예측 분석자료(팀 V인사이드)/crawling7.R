### 감독, 배우 효과 보강 ###
movie14 <- read.csv("movieFiltered14.csv")
head(movie14)

######get director##########
library(dplyr)
dirplus <- movie14 %>% select(title, director,directorEff)
head(dirplus)
dirplus <- dirplus[!duplicated(dirplus$director),]
nrow(dirplus)
sum(!is.na(dirplus$directorEff))
dirna <- dirplus %>% filter(is.na(dirplus$directorEff))
nrow(dirna)


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

# dirna$notworkNonew <- F
ppl <- 0 # 총 관람객 수 
countA <- 0 # 영화 갯수 
pagenum <- 4 # 페이지 넘길때 (2page = 4)
firstPage <- T
loopout <- 1

driver<- rsDriver(port = 4444L,browser = "chrome",version = "3.7.1")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://www.kobis.or.kr/kobis/business/mast/peop/searchPeopleList.do"
remDr$navigate(url)

for(i in 1412:nrow(dirna)){ # i =10
  tryCatch(suppressMessages({
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[1]/input")
    webElem$sendKeysToElement(list(dirna$director[i])) # 감독 명 
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[2]/span/input")
    webElem$sendKeysToElement(list(dirna$title[i], key = "enter")) # 필모그라피
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/table/tbody/tr/td[1]/a")
    webElem$clickElement() # 감독 클릭 
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/ul/li[2]/a")
    webElem$clickElement() ; Sys.sleep(1) # 필모그래피 
    
    while(1){
      for(j in 1:10){ # j=3
        checkyear <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[2]/span[1]")
        tryCatch(suppressMessages({
          get <- remDr$findElement(using = "xpath",checkyear)
          year <- as.numeric(unlist(get$getElementText()))
        }), error = function(e){year <<- NA})
        #if(year < 2010 & !is.na(year) & ppl ==0){dirna$notworkNonew[i] <- T}
        if(year < 2010 | is.na(year)){
          loopout <- 0
          break
        }else if(year <= 2014 & year >= 2010){ # year check
          checkdis <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[1]")
          isdis <- subcrawl(checkdis)
          if(length(grep("감독",isdis)) ==T){ # 감독 crawling
            pplpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[3]/em[2]")
            pplst <- subcrawl(pplpath)
            pplnum <- as.numeric(gsub(",","",substr(pplst,1,nchar(pplst)-1)))
            if(!is.na(pplnum)){ 
              ppl <- sum(ppl,pplnum)
              countA <- countA+1 
            }
          } # 감독 crawling end
        } # year check end
        if(j ==10 & year >= 2012){
          if(firstPage){ # nextpage?
            nextpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[2]") ; firstPage <- F
          }else if(pagenum == 13){
            # nextpath <- "/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[12]/img"
            # pagenum <- 3
            break
          }else{ 
            nextpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[",pagenum,"]")
          }
          
          webElem <- remDr$findElement(using = "xpath",nextpath)
          webElem$clickElement() # nextpage
          pagenum <- pagenum +1
          #print(paste("nextpage ",pagenum))
        } # nextpage? end
      } # j end 
      if(loopout == 0){break} 
    } # while end
    
  }),
  error = function(e){ 
    print(paste0("error at(",i,") : ",dirna$director[i]))
  }
  )
  tryCatch(suppressMessages({
    dirna$directorEff[i] <- ppl/countA
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/a[2]/img")
    webElem$clickElement() # 닫기
  }),error = function(e){
    dirna$directorEff[i] <<- NA
  })
  print(paste0(dirna$director[i], " : ", dirna$directorEff[i]))
  
  webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[2]")
  webElem$clickElement() # 초기화 
  ppl <- 0
  countA <- 0
  pagenum <- 4
  firstPage <- T
  loopout <- T
}# i end

remDr$close()
driver$server$stop()

for(i in 1:nrow(dirplus)){ # i =33
  matchrow <- match(dirplus$title[i],dirna$title)
  if(!is.na(matchrow)){
    dirplus$directorEff[i] <- dirna$directorEff[matchrow]
  }
}

for(i in 1:nrow(movie14)){ # i =1
  matchrow <- match(movie14$title[i], dirplus$title)
  if(!is.na(matchrow)){
    movie14$directorEff[i] <- dirplus$directorEff[matchrow]
  }
}

write.csv(movie14,"movieFiltered15.csv")

####################################################################################################################

movie15 <- read.csv("movieFiltered15.csv")
head(movie15)

######get director##########
library(dplyr)
actplus <- allact2
head(actplus)
actplus <- actplus[!duplicated(actplus$actor),]
nrow(actplus)
colnames(actplus)[3] <- "actorEff"
sum(is.na(actplus$actorEff))
actna <- actplus %>% filter(is.na(actplus$actorEff))
nrow(actna)


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

ppl <- 0 # 총 관람객 수 
countA <- 0 # 영화 갯수 
pagenum <- 4 # 페이지 넘길때 (2page = 4)
firstPage <- T
loopout <- 1

driver<- rsDriver(port = 4444L,browser = "chrome",version = "3.7.1")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://www.kobis.or.kr/kobis/business/mast/peop/searchPeopleList.do"
remDr$navigate(url)

for(i in 2104:nrow(actna)){ # i =10
  tryCatch(suppressMessages({
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[1]/input")
    webElem$sendKeysToElement(list(actna$actor[i])) # 배우 명 
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[2]/span/input")
    webElem$sendKeysToElement(list(actna$title[i], key = "enter")) # 필모그라피
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/table/tbody/tr/td[1]/a")
    webElem$clickElement() # 배우 클릭 
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/ul/li[2]/a")
    webElem$clickElement() ; Sys.sleep(1) # 필모그래피 
    
    while(1){
      for(j in 1:10){ # j=3
        checkyear <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[2]/span[1]")
        tryCatch(suppressMessages({
          get <- remDr$findElement(using = "xpath",checkyear)
          year <- as.numeric(unlist(get$getElementText()))
        }), error = function(e){year <<- NA})
        #if(year < 2010 & !is.na(year) & ppl ==0){dirna$notworkNonew[i] <- T}
        if(year < 2010 | is.na(year)){
          loopout <- 0
          break
        }else if(year <= 2014 & year >= 2010){ # year check
          checkdis <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[1]")
          isdis <- subcrawl(checkdis)
          if(length(grep("주연",isdis)) ==T){ # 감독 crawling
            pplpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[3]/em[2]")
            pplst <- subcrawl(pplpath)
            pplnum <- as.numeric(gsub(",","",substr(pplst,1,nchar(pplst)-1)))
            if(!is.na(pplnum)){ 
              ppl <- sum(ppl,pplnum)
              countA <- countA+1 
              # print(paste0(j,"번째 pplnum : ",pplnum,", ppl : ",ppl))
            }
          } # 주연 crawling end
        } # year check end
        if(j ==10 & year >= 2010){
          if(firstPage){ # nextpage?
            nextpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[2]") ; firstPage <- F
          }else if(pagenum == 13){
            # nextpath <- "/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[12]/img"
            # pagenum <- 3
            break
          }else{ 
            nextpath <- paste0("/html/body/div[2]/div[2]/section[2]/div[1]/article/p/a[",pagenum,"]")
          }
          
          webElem <- remDr$findElement(using = "xpath",nextpath)
          webElem$clickElement() # nextpage
          pagenum <- pagenum +1
          #print(paste("nextpage ",pagenum))
        } # nextpage? end
      } # j end 
      if(loopout == 0){break} 
    } # while end
    
  }),
  error = function(e){ 
    print(paste0("error at(",i,") : ",actna$actor[i]))
  }
  )
  tryCatch(suppressMessages({
    actna$actorEff[i] <- ppl/countA
    webElem <- remDr$findElement(using = "xpath","/html/body/div[2]/div[1]/section/div/a[2]/img")
    webElem$clickElement() # 닫기
  }),error = function(e){
    actna$actorEff[i] <<- NA
  })
  print(paste0(actna$actor[i], " : ", actna$actorEff[i]))
  
  webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[2]")
  webElem$clickElement() # 초기화 
  ppl <- 0
  countA <- 0
  pagenum <- 4
  firstPage <- T
  loopout <- T
}# i end

remDr$close()
driver$server$stop()

for(i in 1:nrow(actplus)){ # i =1
  matchrow <- match(actplus$actor[i],actna$actor)
  if(!is.na(matchrow)){
    actplus$actorEff[i] <- actna$actorEff[matchrow]
  }
}

for(i in 1:nrow(actordf)){ # i = 1
  sumall <- 0
  countA <- 3 - sum(is.na(actordf[i,2:4]))
  for(j in 1:countA){ # j = 1
    matchnm <- match(actordf[i,j+1],actplus$actor)
    if(!is.nan(actplus$actorEff[matchnm]) & !is.na(actplus$actorEff[matchnm])){
      sumall <- sum(sumall, actplus$actorEff[matchnm])
    } # avgppl NAN NA check end
  }# j end
  if(countA != 0){ actordf$avgppl[i] <- sumall/countA}else{ actordf$avgppl[i] <- NA}
}# i end 


head(movie15)
movie15$`actordf$avgppl` <- actordf$avgppl
movie16 <- movie15
movie16 <- movie16[,-8]
colnames(movie16)[16] <- "actorEff"
head(movie16)


write.csv(movie16, "movieFiltered17.csv")
