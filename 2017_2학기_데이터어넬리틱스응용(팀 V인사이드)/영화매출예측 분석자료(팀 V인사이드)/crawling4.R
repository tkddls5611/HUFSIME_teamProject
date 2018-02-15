#### 배급사효과

movieFiltered <- read.csv("movieFiltered.csv")
dis <- movieFiltered$distributor
disdf <- data.frame(title = movieFiltered$title,
                    dis1 = NA, dis2 = NA, distributorEff = NA)
for(i in 1:nrow(disdf)){ #i=1
   splitdata <- strsplit(as.character(dis[i]),",")
   disdf$dis1[i] <- splitdata[[1]][1]
   disdf$dis2[i] <- splitdata[[1]][2]
}

alldis <- data.frame(title = c(as.character(disdf$title), 
                               as.character(disdf$title)),
                     distributor = c(as.character(disdf$dis1),
                                     as.character(disdf$dis2)))
alldis <- alldis[!duplicated(alldis$distributor),]

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

alldis$distributorEff <- NA
ppl <- 0 # 총 관람객 수 
countA <- 0 # 영화 갯수 
pagenum <- 4 # 페이지 넘길때 (2page = 4)
firstPage <- T
loopout <- T

driver<- rsDriver(port = 4444L,browser = "chrome")
remDr <- driver[["client"]]

# 첫 페이지 naviagate
url <- "http://www.kobis.or.kr/kobis/business/mast/comp/searchCompanyList.do"
remDr$navigate(url)

for(i in 286:nrow(alldis)){ # i =3
  tryCatch(suppressMessages({
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[1]/input")
    webElem$sendKeysToElement(list(alldis$distributor[i])) # 배급사 명 
    # webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/div[1]/table/tbody/tr/td[2]/span/input")
    # webElem$sendKeysToElement(list(allact2$title[i])) # 필모그라피
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/form[1]/fieldset/section/p/button[1]")
    webElem$clickElement() ; Sys.sleep(1) # 조회
    webElem <- remDr$findElement(using = "xpath","//*[@id=\"content\"]/table/tbody/tr/td[1]/a")
    webElem$clickElement() # 배급사 클릭
    webElem <- remDr$findElement(using = "xpath","/html/body/div[3]/div[1]/section/div/ul/li[2]/a")
    webElem$clickElement() ; Sys.sleep(1) # 필모그래피 
    
    while(1){
      for(j in 1:10){ # j=9
        checkyear <- paste0("/html/body/div[3]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[2]/span[1]")
        tryCatch(suppressMessages({
          get <- remDr$findElement(using = "xpath",checkyear)
          year <- as.numeric(unlist(get$getElementText()))
        }), error = function(e){year <<- NA})
        if(year < 2012 | is.na(year)){ loopout <- F;break
        }else if(year == 2014 | year == 2013 | year == 2012){ # year check
          checkdis <- paste0("/html/body/div[3]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[1]")
          get <- remDr$findElement(using = "xpath",checkdis)
          isdis <- unlist(get$getElementText())
          
          if(length(grep("배급사",isdis)) ==T){ # 배급 crawling
            pplpath <- paste0("/html/body/div[3]/div[2]/section[2]/div[1]/article/ul[2]/li[",j,"]/dl/dd[3]/em[2]")
            pplst <- subcrawl(pplpath)
            pplnum <- as.numeric(gsub(",","",substr(pplst,1,nchar(pplst)-1)))
            if(!is.na(pplnum)){ 
              ppl <- sum(ppl,pplnum)
              countA <- countA+1 
            }
          } # 배급사 crawling end
        } # year check end
        if(j ==10 & year >= 2012){ # nextpage
          if(firstPage){ nextpath <- paste0("/html/body/div[3]/div[2]/section[2]/div[1]/article/p/a[2]") ; firstPage <- F
          }else if(pagenum == 13){
            nextpath <- "/html/body/div[3]/div[2]/section[2]/div[1]/article/p/a[13]/img"
            pagenum <- 3
          }else{ nextpath <- paste0("/html/body/div[3]/div[2]/section[2]/div[1]/article/p/a[",pagenum,"]")}
          webElem <- remDr$findElement(using = "xpath",nextpath)
          webElem$clickElement() # nextpage
          pagenum <- pagenum +1
          #print(paste("nextpage ",pagenum))
        } # nextpage? end
      } # j end 
      if(loopout == F){break} 
    } # while end
    
  }),
  error = function(e){ 
    errortitle <- alldis$distributor[i]
    print(paste0("error at(",i,") : ",errortitle))
  }
  )
  tryCatch(suppressMessages({
    alldis$distributorEff[i] <- ppl/countA
    webElem <- remDr$findElement(using = "xpath","/html/body/div[3]/div[1]/section/div/a[2]/img")
    webElem$clickElement() # 닫기
  }),error = function(e){
    alldis$distributorEff[i] <<- NA
  })

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




