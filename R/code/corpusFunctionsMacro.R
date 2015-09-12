getTEIWordTableList <- function(doc.object){
  paras <- getNodeSet(doc.object,
                      "/tei:TEI/tei:text/tei:body//tei:p",
                      c(tei = "http://www.tei-c.org/ns/1.0"))
  words <- paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <- tolower(words)
  words.l <- strsplit(words.lower, "\\W")
  word.v <- unlist(words.l)
  book.freqs.t <- table(word.v[which(word.v!="")])
  book.freqs.rel.t <- 100*(book.freqs.t/sum(book.freqs.t))
  return(book.freqs.rel.t)
}



makeFlexTextChunks <- function(doc.object, chunk.size=1000, percentage=TRUE){
  paras <- getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
  words <- paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <- tolower(words)
  words.lower <- gsub("[^[:alnum:][:space:]']", " ", words.lower)
  words.l <- strsplit(words.lower, "\\s+")
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
         length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-c(chunks.l[[length(chunks.l)-1]],
                                         chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}

makeFlexTextChunksThun <- function(doc.object, chunk.size=100, percentage=TRUE){
  paras <- getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p//text()",
                      c(d = "http://www.tei-c.org/ns/1.0"))
  words <- paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <- words
  words.l <- strsplit(words.lower, "\\s+")
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
         length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-c(chunks.l[[length(chunks.l)-1]],
                                         chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}