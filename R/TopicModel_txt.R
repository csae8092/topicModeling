# set your wd to one level lower than the source file´s location
setwd("../")
###################
## preprocessing ##
###################
input.dir <- "./data" #loads the directory, where the .txt-files are stored
files.v <- dir(path = input.dir, pattern=".*txt") #stores all filenames in a vector
source("R/code/TopicModel_externalFunctions.R") # loads additional helper function
topic.m <- NULL #creates an empty matrix which will store the text(chunks) and their according file names
for (i in 1:length(files.v)){ #iterates over all files stored in the data-directory
  text.v <- scan(paste(input.dir,files.v[i], sep="/"), what = "character", encoding = "UTF-8", sep = "\n") # reads the text of every .txt file in a character vector   
  text.v <- paste(text.v, collapse = " ") #convert text vector to a single string 
  text.words.v <- strsplit(text.v, "\\W") #split the string on non word characters - returns a list
  text.words.v <- unlist(text.words.v) #unlist the text.words.v
  text.words.v <- text.words.v[which(text.words.v!="")] #remove blanks
  chunk.m <- makeFlexTextChunks(text.words.v, 200) # calls functions which breaks the text 
  textname <- gsub("\\..*","", files.v[i]) # removes .txt from filenames
  segments.m <- cbind(paste(textname, segment=1:nrow(chunk.m), sep="_"), chunk.m) # combines textchunks and their 'names'
  topic.m <- rbind(topic.m, segments.m) #adds the chunks (and their names) to a matrix
}
documents <- as.data.frame(topic.m, stringsAsFactors = FALSE) # transorms id/chunks matrix to a id/chunks data frame
colnames(documents) <- c("id", "text") # adds 'headers' or column names to the data frame
write.csv2(documents, file ='csv/documents.csv',row.names=FALSE) #writes the documents data frame to a csv file

#############################################################
# create a list of the 100 most common words in the corpus 
# which might be used as a stopword list 
# turns out that this is not very useful                   
# therefore use a stopwordlist, based on first runs of tm
#############################################################
# alltext.v <- documents$text
# alltext.string <- paste(alltext.v, collapse = " " )
# alltext.l <- strsplit(alltext.string, "\\W")
# alltext.v <- unlist(alltext.l)
# alltext.freqs.t <- table(alltext.v)
# sorted.alltext.fregs.t <- sort(alltext.freqs.t, decreasing = TRUE)
# stopwords <- sorted.alltext.fregs.t[1:100]
# kirche <- sorted.alltext.fregs.t["kirche"]


#################
#run topic-model#
#################

library(mallet) #load tm package mallet
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "./R/stoplist.csv",
                                  FALSE) # create a mallet instance
topic.model <- MalletLDA(num.topics=53) #create a trainer object and set the number of topics
topic.model$loadDocuments(mallet.instances) # load the data into the trainer object
topic.model$setAlphaOptimization(40, 80) # some obscure parameters, values taken from Jockers
topic.model$train(400) #train the model with 400 iterations

##################################
# visualize the results with
# help of wordclouds and heatmaps
#################################

#install.packages("wordcloud")
library(wordcloud) 
topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE) # a unique word(column) per topic (row) frequency matrix
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T, normalized=T) # a matrix with one row for every document and one column for every topic
dimnames(doc.topics.m) <-list(documents$id) #add the chunk id´s as header (row)

###################################
#create pngs with topic-wordclouds#
###################################

for(i in 1:length(topic.words.m)){ #iterate ofer each topic, create a wordcloud out of the top frquent 150 words, and save it as .png file
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
#create heatmap of topic distribution#
#####################################

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