#install.packages(c("stringr","NMF","tm","RWeka"))
library(stringr)
library(NMF)
library(tm)
library(RWeka)


directory <- "~/GitHub/South-Park-Text-Mining"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

dialogue <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)


seasons <-unique(dialogue$Season)
seasons <-sort.int(as.numeric(as.character(seasons)))

by.season <- NULL;
for(g in seq_along(seasons)){
  subset <- dialogue[dialogue$Season==seasons[g], ]
  subset <- subset[complete.cases(subset), ]
  text <- str_c(subset$Line, collapse=" ")
  row <- data.frame( season=seasons[g], text=text)
  by.season <- rbind(by.season, row)
}


#create corpus
myReader <- readTabular(mapping=list(content="text", id="season"))

corpus <- Corpus(DataframeSource(by.season), readerControl=list(reader=myReader))
#preprocessing
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument, language = "english")

allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

tdm <- TermDocumentMatrix(corpus,control = list(tokenize = allTokenizer))
tdm <- removeSparseTerms(tdm, 0.4)

nmf_overall <- nmf(as.matrix((tdm)),3,"lee", .options="t" ) 
nmf_overall2 <- fit(nmf_overall)

base <-basis(nmf_overall)

coeff <- coef(nmf_overall)

#show clusters of topics
coefmap(nmf_overall, subsetRow = TRUE)

#
nmf_overall.multi.method <- nmf(as.matrix((tdm)), 3, list("brunet", "lee", "ns"),
                                seed = 123456, .options = "t")
plot(nmf_overall.multi.method)