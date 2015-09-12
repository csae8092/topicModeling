setwd("C:/Users/pandorfer/ownCloud/GIT/zfdg")
input.dir <- "./data"
files.v <- dir(path = input.dir)


for (i in 1:length(files.v))
{
  words.v <- paste(scan(file.path(input.dir,files.v[i]), what = "character", encoding = "UTF-8"), 
                   collapse = " ")
  words.l <- strsplit(words.v, "\\W")
  words.v <- unlist(words.l)
  words.v <- c(words.v[which(words.v!="")])
  words.l <- strsplit(words.v, "\\W")
  textlenght <- length(words.v)
  tokens <- length(unique(words.v))
  text_info <- list(files.v[i], paste(words.v, collapse=" "), textlenght, tokens)
  corpus <- rbind(corpus, text_info)
} 

corpus <- as.data.frame(corpus)

allText <- corpus[[2]]