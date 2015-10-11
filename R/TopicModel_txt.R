setwd("C:/Users/pandorfer/ownCloud/GIT/topicModeling")

###################
## preprocessing ##
###################

input.dir <- "./data" #loads the directory, where the .txt-files are stored
files.v <- dir(path = input.dir, pattern=".*txt") #stores all filenames in a vector
source("R/code/TopicModel_externalFunctions.R") # loads additional helper function
topic.m <- NULL #creates an empty matrix which will store the text(chunks) and their according file names
for (i in 1:length(files.v)){ #iterates over all files stored in the data-directory
  text.v <- scan(paste(input.dir,files.v[i], sep="/"), what = "character", encoding = "UTF-8", sep = "\n")
  #convert text vector to a single string
  text.v <- paste(text.v, collapse = " ")
  #split the string on non word characters - returns a list
  text.words.v <- strsplit(text.v, "\\W")
  #unlist the text.words.v
  text.words.v <- unlist(text.words.v)
  #remove blanks
  text.words.v <- text.words.v[which(text.words.v!="")]
  chunk.m <- makeFlexTextChunks(text.words.v, 200)
  textname <- gsub("\\..*","", files.v[i])
  segments.m <- cbind(paste(textname, segment=1:nrow(chunk.m), sep="_"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}
documents <- as.data.frame(topic.m, stringsAsFactors = FALSE)
colnames(documents) <- c("id", "text")

#writes the documents data frame to a csv file
write.csv2(documents, file ='csv/documents.csv',row.names=FALSE)

#############################################################
#create a stopword list based on the preprocessed text      #
# turns out that this is not very useful, as e.g. "kirche"  #
# is part of the top50 words in the corpus
#therefore I use a stopwordlist, created from a former tm-run
#############################################################
#alltext.v <- documents$text
#alltext.string <- paste(alltext.v, collapse = " " )
#alltext.l <- strsplit(alltext.string, "\\W")
#alltext.v <- unlist(alltext.l)
#alltext.freqs.t <- table(alltext.v)
#sorted.alltext.fregs.t <- sort(alltext.freqs.t, decreasing = TRUE)
#stopwords <- sorted.alltext.fregs.t[1:50]
#kirche <- sorted.alltext.fregs.t["kirche"]

#################
#run topic-model#
#################

library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "R/stoplist.csv",
                                  FALSE)
topic.model <- MalletLDA(num.topics=53) #hier wird die Zahl der Topics festgelegt
topic.model$loadDocuments(mallet.instances)

topic.model$setAlphaOptimization(40, 80)

topic.model$train(400)

#install.packages("wordcloud")
library(wordcloud)

topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
dimnames(doc.topics.m) <-list(documents$id)

topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)

hansi <- mallet.top.words(topic.model, topic.words.m[53,], 150) #hier wird festgelegt, welches Topic als Wordcloud visualisiert wird
words <- hansi[,1]
numbers <- hansi[,2]
pal2 <- brewer.pal(3,"Dark2")
#pdf("plots.pdf")
wordcloud(words, numbers,
          scale=c(4,0.5),
          max.words=100,
          random.order=FALSE,
          rot.per=0.0,
          use.r.layout=FALSE,
          #col=grey(seq(1,0,-0.01)))
          col=pal2)
#dev.off()
###################################
#create pdfs with topic-wordclouds#
###################################

for(i in 1:length(topic.words.m)){
  ppi <- 100
  hansi <- mallet.top.words(topic.model, topic.words.m[i,], 150)
  words <- hansi[,1]
  numbers <- hansi[,2]
  pal2 <- brewer.pal(3,"Dark2")
  png(paste("plots/wordclouds/",i, ".png", sep = ""), width=6*ppi, height=6*ppi, res=ppi)
  wordcloud(words, numbers,
            scale=c(2,0.5),
            max.words=150,
            random.order=FALSE,
            rot.per=0.35,
            use.r.layout=FALSE,
            #col=grey(seq(1,0,-0.01)))
            col=pal2)
  dev.off()
}

#####################################
#create heatmap of topicdistribution#
#####################################
y <- 1
first.word.m <- NULL
for(y in 1:length(topic.words.m)){
  first.word.part <- mallet.top.words(topic.model, topic.words.m[y,], 1)
  first.word.m <- rbind(first.word.m,first.word.part)
}
#colnames(doc.topics.m) <- first.word.m[,1]
colnames(doc.topics.m) <- c(1:nrow(topic.words.m))
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
#install.packages("gplots")
library(gplots)
ppi <- 150
png("plots/heatmap.png", width=90*ppi, height=70*ppi, res=ppi)
#pdf("plots/heatmap.pdf", width=60, height=40)
heatmap.2(doc.topics.m,
          Colv=FALSE,
          dendrogram="none",
          scale="row",
          colsep=c(1:ncol(doc.topics.m)),
          rowsep=c(1:nrow(doc.topics.m)),
          sepcolor="white",
          sepwidth=c(0.01,0.1),
          heat.colors(256),
          trace="none",
          margin = c(8,18),
          density.info="none",
          main="Themen der Thun-Korrespondenz")
dev.off()