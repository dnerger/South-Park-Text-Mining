#this file contains fragments of other files. because of time constrains everything was done in one file
#and later split between the other files

library(stringr)
library(tm)
library(RWeka)



# create term document matrix
options(mc.cores=1)
allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
ep.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))

# remove sparse terms 
ep.tdm.70 <- removeSparseTerms(ep.tdm, 0.90) # 1279 / 27536

# save as a simple data frame
count.ep <- data.frame(inspect(ep.tdm.70)) 
count.ep$word <- row.names(count.ep)

write.csv(count.ep, "southpark_tdm_episode.csv", row.names=FALSE)








season.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
terms
library(proxy)
proxy::dist(as.matrix(terms), method = "cosine")
inspect(terms[5:10, 21100:21105])
terms <- removeSparseTerms(terms, 0.95)
findFreqTerms(terms,10)

tf_idf_mat <- as.matrix(terms)

tf_idf_v <- sort(colSums(tf_idf_mat), decreasing=TRUE)
head(tf_idf_v, 20)
tf_idf_v[1]<-0
dfxxx <- data.frame(word = names(tf_idf_v), value = tf_idf_v)
characters <- unique(dialogue$Character)
major_characters <- count()
character_appear <- aggregate(data.frame(count = dialogue$Character), list(value = dialogue$Character), length)
character_appear <- character_appear[order(character_appear$count, decreasing = TRUE),]

transcripts_3 <- dialogue[which(dialogue$Character %in% character_appear$value[1:40]), ]
library(tidyr)
library(reshape2)
speaker_scene_matrix <- transcripts_3 %>%
  acast(Character ~ Episode, fun.aggregate = length)

data_matrix <- as.matrix(t(speaker_scene_matrix))
total_occurrences <- colSums(t(speaker_scene_matrix))

co_occurrence <- t(data_matrix) %*% data_matrix

library(igraph)
g <- graph.adjacency(co_occurrence, weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight/500000)

library(igraph)
g <- graph.adjacency(co_occurrence,
                     weighted = TRUE,
                     diag = FALSE,
                     mode = "upper")

g <- simplify(g, remove.multiple = FT, remove.loops = T, edge.attr.comb = c(weight = "sum", type = "ignore"))

females <- c("Liane", "Sharon", "Sheila", "Wendy") 

V(g)$gender <- ifelse(V(g)$name %in% females, "female", "male")

plot(g,
     vertex.label.family = "Helvetica",
     vertex.label.font = 1,
     vertex.shape = "sphere",
     vertex.size=total_occurrences/230,
     vertex.label.cex=0.9,
     vertex.color=c( "pink", "skyblue")[1+(V(g)$gender=="male")],
     vertex.label.color="black",
     vertex.frame.color = NA,
     edge.width = E(g)$weight/300000,
     edge.curved=.1,
     layout=layout_in_circle)

norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)

head(by.episode)






library(SnowballC)
m <- as.matrix(season.tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)




library(tm)
library(quanteda)
textxx<-paste(by.season$text, collapse=" // ")
collocations(textxx, size = 2:3)
print(removeFeatures(collocations(textxx, size = 2:3), stopwords("english")))



library(quanteda)
textxx<- gsub('[[:punct:] ]+',' ',textxx)
textxx <- tolower(textxx)
textxx<- removeWords(textxx, stopwords("english"))
ssss <- stopwords('english')
dfmtest <- dfm(textxx, stem=TRUE, remove_punct=TRUE, ngrams=4:4, verbose = FALSE, concatenator=" ")
dftest <- data.frame(word=dfmtest@Dimnames$features, freq=dfmtest@x)
dftest <- arrange(dftest, -freq)

within(dftest, {
  word1 <- strsplit(word," ")(1)
  word2 <- strsplit(word," ")(2)
  word3 <- strsplit(word," ")(3)
  word4 <- strsplit(word," ")(4)
})
print(dftest)


library(R.utils)
library(doParallel)
library(slam) # maybe not needed
require(dplyr)
library(data.table)
parallelizeTask <- function(task, ...) {
  # Calculate the number of cores
  ncores <- detectCores() - 1
  # Initiate cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  #print("Starting task")
  r <- task(...)
  #print("Task done")
  stopCluster(cl)
  r
}

makeSentences <- function(input) {
  output <- tokenize(input, what = "sentence", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = TRUE)
  output <- removeFeatures(output, getProfanityWords())
  unlist(lapply(output, function(a) paste('#s#', toLower(a), '#e#')))
}

# Returns a vector of profanity words
getProfanityWords <- function(corpus) {
  profanityFileName <- "profanity.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, method = "curl")
  }
  
  if (sum(ls() == "profanity") < 1) {
    profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
    profanity <- profanity$V1
    profanity <- profanity[1:length(profanity)-1]
  }
  
  profanity
}

makeTokens <- function(input, n = 1L) {
  tokenize(input, what = "word", removeNumbers = TRUE,
           removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = FALSE, removeHyphens = TRUE,
           ngrams = n, simplify = TRUE)
}

sentences <- parallelizeTask(makeSentences, corpus)
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)

dfm1 <- parallelizeTask(dfm, ngram1)
dfm2 <- parallelizeTask(dfm, ngram2)
dfm3 <- parallelizeTask(dfm, ngram3)
dfm4 <- parallelizeTask(dfm, ngram4)

dt4 <- data.table(ngram = features(dfm4), count = colSums(dfm4), key = "ngram")
# Store the total number of ngrams (features in quanteda terminology) for later use
nfeats <- nfeature(dfm4)
dt2 <- data.table(ngram = features(dfm2), count = colSums(dfm2), key = "ngram")

hits <- DT[ngram %like% paste("^", regex, "_", sep = ""), ngram]
dt2

allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))
tdm <- removeSparseTerms(tdm, 0.5)
tdm
tdmMatrix <- as.matrix(tdm)

tdmMatrix[1:10,1:10]

tdmMatrix[tdmMatrix<15] <- 0
tdmMatrix[tdmMatrix>=15] <- 1


termMatrix <- tdmMatrix %*% t(tdmMatrix)
termMatrix

 
 

 library(NMF)
 nmf_overall <- nmf(as.matrix((tdm)),20,"lee", .options="t" ) 
nmf_overall2 <- fit(nmf_overall)
nmf_hat <- fitted(nmf_overall)
summary(nmf_hat)
base <-basis(nmf_overall)
base
coeff <- coef(nmf_overall)
coeff
algorithm(nmf_overall)
coefmap(nmf_overall, subsetRow = TRUE)
base[,1]
nmf_w <- nmf_overall2@W
nmf_h <- nmf_overall2@H
nmf_h
nmf_test[,1]
nmf_test <- sort(nmf_w[,20], decreasing = TRUE)
coefmap(nmf_overall2)
names(nmf_test)
nmf_overall.multi.method <- nmf(as.matrix((tdm)), 3, list("brunet", "lee", "ns"),
                        seed = 123456, .options = "t")
plot(nmf_overall.multi.method)
which.max(nmf_h[,241])

organizations[18]
findFreqTerms(organizations)


library(stm)
library(tidytext)
install.packages("tidytext")

processed <- textProcessor(by.episode$text, metadata = by.episode)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 0, upper.thresh = 25)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)


southparkPrevFit <- stm(documents = out$documents, vocab = out$vocab,K=25, data = out$meta)


southparkPrevFit <- stm(documents = out$documents, vocab = out$vocab,K=4, data = out$meta)
yyyyy<-labelTopics(southparkPrevFit, c(1,2,3,4,5))
yyyyy
plot(southparkPrevFitSeason18, type = "summary", main="Top Topics of Season 18")

install.packages("maps")
install.packages("ggplot2")
library("ggmap")
library(maptools)
library(maps)

season18_locations <- locations[18]
season18_locations <- c("Sweden", "West Africa", "Canada", "Atlanta", "South Park", "New Zealand", "Canada", "America", "Washington", "Michigan", "Arlington", "Texas", "India", "Kansas", "Colorado", "Lincoln", "Edmonton","Japan", "San Francisco", "Arctic", "New Hampshire","LOs Angeles", "Salzburg")
ll.visited <- geocode(season18_locations)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(visit.x,visit.y, col="red", pch=16)


install.packages("RTextTools")
library(RTextTools)

# Configure the training data
corpus_new <- Corpus(DataframeSource(by.episode), readerControl=list(reader=myReader))
corpus_new <- tm_map(corpus_new,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
corpus_new <- tm_map(corpus_new, content_transformer(tolower))
corpus_new <- tm_map(corpus_new, content_transformer(removePunctuation))
corpus_new <- tm_map(corpus_new, content_transformer(removeNumbers))
corpus_new <- tm_map(corpus_new, content_transformer(stripWhitespace))
corpus_new <- tm_map(corpus_new, removeWords, stopwords("english"))
corpus_new <- tm_map(corpus_new, stemDocument, language = "english")
dtMatrix <- DocumentTermMatrix(corpus_new, control = list(tokenize = allTokenizer))

mat.df <- as.data.frame(data.matrix(dtMatrix), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, by.episode$season)

# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "category"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .50))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "category"]

# Create model data and remove "category"
modeldata <- mat.df[,!colnames(mat.df) %in% "category"]

# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train])

# Confusion matrix
conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
conf.mat

# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test) * 100)

# Create data frame with test data and predicted category
df.pred <- cbind(knn.pred, modeldata[test, ])
write.table(df.pred, file="output.csv", sep=";")

lda_show <- toLDAvis(southparkPrevFitSeason18, docs = out$documents)
lda_show <- toLDAvis(southparkPrevFit, docs = out$documents)



library(LDAvis)
help(createJSON, package = "LDAvis")
xyyy<- apply(southparkPrevFit$theta, 1, which.max) 

topic_episodes <- data.frame(xyyy)
p <- ggplot(topic_episodes,aes(seq_along(xyyy), xyyy, colour=xyyy)) + geom_point()
p + xlab("Episode") + ylab("Topic")+theme(legend.position="none")

createJSON(phi=southparkPrevFit$eta, theta=southparkPrevFit$theta, vocab = southparkPrevFit$vocab)
