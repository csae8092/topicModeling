setwd("C:/Users/csae8092/SkyDrive/digital-archiv/R/Text Analysis with R/TextAnalysisWithR/TextAnalysisWithR")
#setwd("D:/OneDrive/digital-archiv/R/Text Analysis with R/TextAnalysisWithR/TextAnalysisWithR")
source("code/corpusFunctionsMacro.R")
inputDir <- "data/XMLThunCorpus"
files.v <- dir(path=inputDir, pattern=".*xml")
library(XML)
i <- 1

file.path(inputDir, files.v[i])

topic.m <- NULL
for(i in 1:length(files.v)){
  doc.object <- xmlTreeParse(file.path(inputDir, files.v[i]),
                             useInternalNodes=TRUE)
  chunk.m <- makeFlexTextChunksThun(doc.object, chunk.size=5000,
                                    percentage=FALSE)
  textname <- gsub("\\..*","", files.v[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="_"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")

library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "data/stoplist.csv",
                                  token.regex="[\\p{L}']+")

topic.model <- MalletLDA(num.topics=50)
topic.model$loadDocuments(mallet.instances)

topic.model$setAlphaOptimization(40, 80)

topic.model$train(400)

#install.packages("wordcloud")
library(wordcloud)

topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
dimnames(doc.topics.m) <-list(documents$id)
write.csv(topic.words.m, "docTopics.csv",row.names=TRUE)
file.ids.v <- documents[1,]

row.names(doc.topics.m) <-documents[,1]
column.names(topic.top.words)

topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)

hansi <- mallet.top.words(topic.model, topic.words.m[4,], 75)
words <- hansi[,1]
numbers <- hansi[,2]
pal2 <- brewer.pal(30,"Dark2")
wordcloud(words, numbers,
          scale=c(5,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=TRUE,
          colors=pal2) 
          

#wordcloud(words, numbers, c(1,.1), rot.per=0, random.order=F)

y <- 1
first.word.m <- NULL
for(y in 1:50){
  first.word.part <- mallet.top.words(topic.model, topic.words.m[y,], 1)
  first.word.m <- rbind(first.word.m,first.word.part)
}
colnames(doc.topics.m) <- first.word.m[,1]
filenames <- documents[,1]
file.name.part <- strsplit(filenames[1], "_")

filenames.l <- NULL
i <- 1
for (i in 1:length(filenames)){
  file.name.part <- strsplit(filenames[i], "_")
  file.name.part <- unlist(file.name.part)[1]
  filenames.l <-rbind(filenames.l, file.name.part)
}
row.names(doc.topics.m) <-filenames.l
#row.names(doc.topics.m) <-documents[,1]
heatmap <- heatmap(doc.topics.m,
                       Rowv=NA, Colv=NA,
                       col = heat.colors(256), 
                       scale="row", margins=c(8,18),
                   main="Themen der Thun-Korrespondenz",
                   ylab="Dokumente", xlab="Themen")
#install.packages("gplots")
library(gplots)
heatmap.2(doc.topics.m,
          Colv=FALSE,
          dendrogram="none",  
          scale="row",
          colsep=(1:50),
          rowsep=(1:69),
          sepcolor="white",
          sepwidth=c(0.001,0.001),
          heat.colors(256),
          trace="none",
          margin = c(8,18),
          density.info="none",
          main="Themen der Thun-Korrespondenz") 
