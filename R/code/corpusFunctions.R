#FUnction that takes a XML-Document, extract the text in every /tei:body//tei:p,
#calculates the relative frequency of the text and saves the data into a table
getTEIWordTableList <- function(doc.object){
  paras <- getNodeSet(doc.object, "/tei:TEI/tei:text/tei:body//tei:p", c(tei = "http://www.tei-c.org/ns/1.0 "))
  words <- paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <- tolower(words)
  words.l <- strsplit(words.lower, "//W")
  word.v <- unlist(words.l)
  book.freqs.t <- table(word.v[which(word.v!="")])
  book.freqs.rel.t <- 100*(book.freqs.t/sum(book.freqs.t))
  return(book.freqs.rel.t)
}



# Function to print a vector of file names in user
# friendly format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i, file.name.v[i], "\n", sep=" ")
  }
}


#Function takes a vector of file names and a directory path and
# returns a list in which each item in the list is an ordered
# vector of words from one of the files in the vector of file names
make.file.word.v.l <- function(files.v, input.dir){
  #set up an empty container
  text.word.vector.l <- list()
  #iterate over the files
  for(i in 1:length(files.v)){
    #read the files in
    text.v <-scan(paste(input.dir, files.v[i], sep="/"), 
                  what="character", sep="\n")
    #convert to single string
    text.v <- paste(text.v, collapse=" ")
    #lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    text.words.v <- strsplit(text.lower.v, "\\W")
    text.words.v <- unlist(text.words.v)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #use the index id from the files.v vector as "name in the list
    text.word.vector.l[[files.v[i]]] <- text.words.v
  }
  return(text.word.vector.l)
}