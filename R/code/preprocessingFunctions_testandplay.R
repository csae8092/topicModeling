# Function takes a vector of file names and a directory path and
# returns a list in which each item in the list is an ordered
# vector of words from one of the files in the vector of file names
# taken from Jokers, Text Analysis, S. 76/89

setwd("C:/Users/pandorfer/ownCloud/GIT/zfdg")
input.dir <- "./data/"
files.v <- dir(input.dir)
text.v <- scan(paste(input.dir,files.v[4], sep=""), what = "character", encoding = "UTF-8", sep = "\n")
#convert text vector to a single string
text.v <- paste(text.v, collapse = " ")
#split the string on non word characters - returns a list
text.words.v <- strsplit(text.v, "\\W")
#unlist the text.words.v
text.words.v <- unlist(text.words.v)
#remove blanks
text.words.v <- text.words.v[which(text.words.v!="")]

makeFlexTextChunks <- function(text.v, chunk.size=150){
  #convert text vector to a single string
  text.v <- paste(text.v, collapse = " ")
  #split the string on non word characters - returns a list
  text.words.v <- strsplit(text.v, "\\W")
  #unlist the text.words.v
  text.words.v <- unlist(text.words.v)
  #remove blanks
  text.words.v <- text.words.v[which(text.words.v!="")]
  x <- seq_along(text.words.v)
  chunks.l <- split(text.words.v, ceiling(x/chunk.size))
  #deal with small chunks at the end
  if(length(chunks.l[[length(chunks.l)]]) <=
     length(chunks.l[[length(chunks.l)]])/2){
    chunks.l[[length(chunks.l)-1]] <-
      c(chunks.l[[length(chunks.l)-1]],
        chunks.l[[length(chunks.l)]])
    chunks.l[[length(chunks.l)]] <- NULL
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}

hansi <- makeFlexTextChunks(text.words.v, 220)
make.file.word.v.l <- function(files.v, input.dir){
  text.word.vector.l <- list()
  for(i in 1:length(files.v)){
    #read the file and save its text as a vector
    text.v <- scan(paste(input.dir,files.v[i], sep=""),
                   what = "character", encoding = "UTF-8",
                   sep = "\n")
    #convert text vector to a single string
    text.v <- paste(text.v, collapse = " ")
    #split the string on non word characters - returns a list
    text.words.v <- strsplit(text.v, "\\W")
    #unlist the text.words.v
    text.words.v <- unlist(text.words.v)
    #remove blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #use the filenames as indexes for the text.word.vector.l
    text.word.vector.l[[files.v[i]]] <- text.words.v
  }
  return(text.word.vector.l)
}

my.corpus.l <- make.file.word.v.l(files.v, input.dir)