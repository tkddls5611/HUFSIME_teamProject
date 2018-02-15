############회귀분석을 위한 genre 더미변수 처리

movie19 <- read.csv("movieFiltered19.csv")

head(movie19)
genre <- movie19$genre15
head(genre)
genrevec <- unlist(strsplit(x = as.character(genre), split = ","))
genrevec <- unique(genrevec)
genredf <- as.data.frame(matrix(0,nrow = nrow(movie19), ncol = length(genrevec)))
colnames(genredf) <- genrevec

genrelist <- strsplit(x = as.character(genre),split = ",")
for(i in 1:length(genrelist)){ # i =3
  for(j in 1:length(genrelist[[i]])){ # j =1
    matchcol <- match(genrelist[[i]][j], colnames(genredf))
    genredf[i,matchcol] <- 1
  }
}
