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
seasons <-sort(seasons, as.numeric(as.character(seasons)))
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
by.season
by.season %>% unnest_tokens(word, text)
