library(stringr)
library(tm)
library(RWeka)

dialogue <- read.csv("dialogue.csv",stringsAsFactors = FALSE, sep=" ")


for(h in seq_along(dialogue[,1])) if(nchar(dialogue$Episode[h]) < 2) dialogue$Episode[h] <- paste("0", dialogue$Episode[h], sep="")
dialogue$ES <- paste(dialogue$Season, dialogue$Episode, sep=".")
for(j in c(1,5)) dialogue[,j] <- as.numeric(dialogue[,j])
dialogue <- dialogue[complete.cases(dialogue), ]
episodes <- unique(dialogue$ES)
episodes <- episodes[order(episodes)]


by.episode <- NULL
for(g in seq_along(episodes)){
  
  subset <- dialogue[dialogue$ES==episodes[g], ]
  subset <- subset[complete.cases(subset), ]
  text <- str_c(subset$Line, collapse=" ")
  #if (subset$Season[1]==18){
  row <- data.frame(episode.number=g, episode.code=episodes[g], season=subset$Season[1], text=text)
  by.episode <- rbind(by.episode, row)
  #}
  
}

seasons <-unique(dialogue$Season)
seasons <-sort.int(as.numeric(as.character(seasons)))
seasons
by.season <- NULL;
for(g in seq_along(seasons)){
  subset <- dialogue[dialogue$Season==seasons[g], ]
  subset <- subset[complete.cases(subset), ]
  text <- str_c(subset$Line, collapse=" ")
  row <- data.frame( season=seasons[g], text=text)
  by.season <- rbind(by.season, row)
}

# create corpus
myReader <- readTabular(mapping=list(content="text", id="episode.number"))
corpus <- Corpus(DataframeSource(by.episode), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

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


dialogue <- read.csv('all-seasons.csv',stringsAsFactors = FALSE)
namevector <-c('person','location','organization')
by.season[,namevector] <- NA
library(NLP)
library(openNLP)
library(magrittr)
install.packages('rJava')



word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

full_sents <- NULL
for (g in seasons){
  
  full_dialogue = paste(by.season$text[g], collapse = " ")
  nchar(full_dialogue)
  full_dialogue <-as.String(full_dialogue)
  
  first_half <- substr(full_dialogue,0,nchar(full_dialogue)/2)
  second_half <- substr(full_dialogue,nchar(full_dialogue)/2, nchar(full_dialogue))

  full_dialogue <- first_half
  dialogue_annotations <- annotate(full_dialogue, pipeline)
  dialogue_doc <- AnnotatedPlainTextDocument(full_dialogue, dialogue_annotations)
  org1 <-entities(dialogue_doc, kind = "organization")
  loc1 <-entities(dialogue_doc, kind = "location")
  pers1<-entities(dialogue_doc, kind = "person")
  
  full_sents <- c(full_sents, sents(dialogue_doc))
  
  full_dialogue <- second_half
  dialogue_annotations <- annotate(full_dialogue, pipeline)
  dialogue_doc <- AnnotatedPlainTextDocument(full_dialogue, dialogue_annotations)
  
  org1 <-rbind(org1,entities(dialogue_doc, kind = "organization"))
  loc1 <-rbind(loc1,entities(dialogue_doc, kind = "location"))
  pers1<-rbind(pers1,entities(dialogue_doc, kind = "person"))

  by.season$organization[g]<-str_c(org1, collapse= ";")
  by.season$location[g]<-str_c(loc1, collapse=";")
  by.season$person[g]<-str_c(pers1, collapse=";")
  full_sents <- c(full_sents, sents(dialogue_doc))
}

full_dialogue = paste(by.episode$text[1], collapse = " ")
dialogue_annotations <- annotate(as.String(full_dialogue), pipeline)
dialogue_doc <- AnnotatedPlainTextDocument(full_dialogue, dialogue_annotations)
org1 <-entities(dialogue_doc, kind = "organization")
loc1 <-entities(dialogue_doc, kind = "location")
pers1<-entities(dialogue_doc, kind = "person")
org1
loc1
pers1

full_sents
testxx<-sents(dialogue_doc)
by.season$organization[18]
organizations <- strsplit(by.season$organization, ";")
persons <- strsplit(by.season$person, ";")
locations <- strsplit(by.season$location, ";")




library(tidytext)
myReader <- readTabular(mapping=list(content="text", id="season"))
corpus <- Corpus(DataframeSource(by.season), readerControl=list(reader=myReader))

corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument, language = "english")
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

library("wordcloud")
library("RColorBrewer")
par(bg="white")
png(file="WordCloud-TFIDF.png",width=1000,height=700, bg="grey30")
wordcloud(dfxxx$word, dfxxx$value, col=terrain.colors(length(d$word), alpha=0.9), max.words=100, random.order=FALSE, rot.per=0.3 )
title(main = "Words by tf-idf score over 18 seasons of South Park", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

par(bg="white")
png(file="WordCloud-ngram.png",width=1000,height=700, bg="grey30")
wordcloud(dftest$word, dftest$freq, col=terrain.colors(length(d$word), alpha=0.9), max.words=100, random.order=FALSE, rot.per=0.3 )
title(main = "Quadgrams over 18 seasons of South Park", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

dev.new()


install.packages("devtools")
devtools::install_github("exploratory-io/exploratory_func")
library(exploratory_func)
  
trigram.seasonTdm <- tm::TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
freq.trigram.season <- data.frame(word = trigram.seasonTdm$dimnames$Terms, frequency = trigram.seasonTdm$v)
freq.trigram.season <- plyr::arrange(freq.trigram.season, -frequency)



library(SnowballC)
m <- as.matrix(season.tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  options <- stringi::stri_opts_brkiter(type="word", skip_word_none = skip_word_none)
  
  function(x) {
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stringi::stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stringi::stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}


library("wordcloud")
library("RColorBrewer")
par(bg="grey30")
png(file="WordCloud.png",width=1000,height=700, bg="grey30")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), max.words=500, random.order=FALSE, rot.per=0.3 )
title(main = "Most used words in South Park", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()
dev.new()
nrc_sents <- get_nrc_sentiment(paste(by.season$text[18], collapse=" ")) 
nrc_posneg <- nrc_sents[9:10]
nrc_sents <- nrc_sents[1:8]
nrc_t<- data.frame(t(nrc_sents))

#Transformation and  cleaning
names(nrc_t)[1] <- "count"
nrc_t <- cbind("sentiment" = rownames(nrc_t), nrc_t)


library("ggplot2")
sentiment <- NULL
nrc_t <- data.frame(sentiment=nrc_t$sentiment, count = nrc_t$count)
nrc_t
qplot(sentiment, data=nrc_t, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments in Season 18")

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

library(igraph)
library(ggraph)

ngram_graph <- dfmtest %>%
  filter(freq > 10) %>%
  graph_from_data_frame()
set.seed(2017)

ggraph(ngram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


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
tdm <- removeSparseTerms(tdm, 0.3)
tdm
tdmMatrix <- as.matrix(tdm)

tdmMatrix[1:10,1:10]

tdmMatrix[tdmMatrix<15] <- 0
tdmMatrix[tdmMatrix>=15] <- 1


termMatrix <- tdmMatrix %*% t(tdmMatrix)
termMatrix
library(igraph)
 # build a graph from the above matrix
   g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
 # remove loops
   g <- simplify(g)
 # set labels and degrees of vertices
  V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
pdf("term-network.pdf")
plot(g, layout=layout1)
 dev.off()
 
 

 library(NMF)
 nmf_overall <- nmf(as.matrix((tdm)),3, .options="t") 
nmf_overall2 <- fit(nmf_overall)
nmf_hat <- fitted(nmf_overall)
summary(nmf_hat)
base <-basis(nmf_overall)
base
coeff <- coef(nmf_overall)
coeff
algorithm(nmf_overall)
coefmap(nmf_overall, subsetRow = TRUE)

nmf_overall.multi.method <- nmf(as.matrix((tdm)), 3, list("brunet", "lee", "ns"),
                        seed = 123456, .options = "t")
plot(nmf_overall.multi.method)


organizations[18]
findFreqTerms(organizations)


library(stm)
library(tidytext)
install.packages("tidytext")

processed <- textProcessor(by.season$text, metadata = by.season)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 5)

southparkPrevFit <- stm(documents = out$documents, vocab = out$vocab,K=10, data = out$meta)
yyyyy<-labelTopics(southparkPrevFit, c(1,2,3,4,5))
yyyyy
plot(southparkPrevFit, type = "summary")

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
container <- create_container(dtMatrix, by.episode$text, trainSize=1:200, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)
predictionData <- list(by.episode$text[205], by.episode$text[207])
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
results
