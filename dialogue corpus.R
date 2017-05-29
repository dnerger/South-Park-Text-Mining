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
  row <- data.frame(episode.number=g, episode.code=episodes[g], season=subset$Season[1], text=text)
  by.episode <- rbind(by.episode, row)
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
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
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



full_sents
testxx<-sents(dialogue_doc)
by.season$organization[18]
organizations <- strsplit(by.season$organization, ";")
persons <- strsplit(by.season$person, ";")
locations <- strsplit(by.season$location, ";")




library(tidytext)
myReader <- readTabular(mapping=list(content="text", id="season"))
corpus <- Corpus(DataframeSource(by.season), readerControl=list(reader=myReader))

corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
season.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))

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
nrc_sents <- get_nrc_sentiment(paste(by.season$text, collapse=" ")) 
nrc_sents <- nrc_sents[1:8]
nrc_t<- data.frame(t(nrc_sents))

#Transformation and  cleaning
names(nrc_t)[1] <- "count"
nrc_t <- cbind("sentiment" = rownames(nrc_t), nrc_t)


library("ggplot2")
sentiment <- NULL
nrc_t <- data.frame(sentiment=nrc_t$sentiment, count = nrc_t$count)
nrc_t
qplot(sentiment, data=nrc_t, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments in South Park")
by.season %>% unnest_tokens(word, text)

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
dfmtest <- dfm(textxx, ngrams=4:4, verbose = FALSE)
dftest <- data.frame(word=dfmtest@Dimnames$features, freq=dfmtest@x)
dftest <- arrange(dftest, -freq)
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
