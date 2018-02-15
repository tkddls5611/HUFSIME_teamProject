#######################첫 주 관객수 가져오기 
##

head(a$title)
firstweek <- data.frame(title = a$title, firstppl = 0)

#firstweek
for(i in 1:131){ # i =1
  dataset <- get(paste0("X",filename$StartDate[i]))
  for(j in 1:nrow(firstweek)){ # j=1
    if(!is.na(match(as.character(firstweek$title[j]), dataset$title)) &
       firstweek$firstppl[j] ==0){
      getlocation <- match(as.character(firstweek$title[j]), dataset$title)
      firstweek$firstppl[j] <- dataset$audience[getlocation]
    }
  }
  print(paste0(paste0("X",filename$StartDate[i])," finished!", i, "번째"))
}

write.csv(firstweek, "firstweek.csv")
