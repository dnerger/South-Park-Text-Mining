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
seasons <-seasons[order(seasons)]
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
full_dialogue = paste(by.season$text[18], collapse = " ")

library(NLP)
library(openNLP)
library(magrittr)
install.packages('rJava')

full_dialogue <-as.String(full_dialogue)
full_dialogue <- substr(full_dialogue,1,100000)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

dialogue_annotations <- annotate(full_dialogue, list(sent_ann, word_ann))

class(dialogue_annotations)
head(dialogue_annotations)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

dialogue_doc <- AnnotatedPlainTextDocument(full_dialogue, dialogue_annotations)
sents(dialogue_doc) %>% head(10)


pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

dialogue_annotations <- annotate(full_dialogue, pipeline)
dialogue_doc <- AnnotatedPlainTextDocument(full_dialogue, dialogue_annotations)

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

entities(dialogue_doc, kind = "person")
sents(dialogue_doc)
