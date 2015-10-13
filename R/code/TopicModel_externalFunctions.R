#function to chunk a text (chr-vector) takes a char-vector and
# an integer as parameters and returns a dataframe
# most parts of the code is taken from Jokers, Text Analysis p. 141

makeFlexTextChunks <- function(text.v, chunk.size=150){
  text.v <- paste(text.v, collapse = " ") #convert text vector to a single string
  text.words.v <- strsplit(text.v, "\\W") #split the string on non word characters - returns a list
  text.words.v <- unlist(text.words.v) #unlist the text.words.v.
  text.words.v <- text.words.v[which(text.words.v!="")] #remove blanks
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