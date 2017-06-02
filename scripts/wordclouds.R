library(stringr)
library(wordcloud)
library(RColorBrewer)
library(quanteda)
library(tm)
library(igraph)
library(ggraph)

directory <- "~/GitHub/South-Park-Text-Mining"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

dialogue <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)

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

#can exchange by.season with by.episode, delivers different results for tf-idf
corpus <- Corpus(DataframeSource(by.season), readerControl=list(reader=myReader))
#preprocessing
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument, language = "english")

#dtm with tf-idf weighting
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
#remove sparse terms to decrease size
terms <- removeSparseTerms(terms, 0.99)


tf_idf_mat <- as.matrix(terms)

tf_idf_v <- sort(colSums(tf_idf_mat), decreasing=TRUE)
head(tf_idf_v, 20)
#first word is  'ã,â', no idea why
tf_idf_v[1]<-0
tf_idf_wordcloud <- data.frame(word = names(tf_idf_v), value = tf_idf_v)

png(file="WordCloud-TFIDF.png",width=1000,height=700, bg="grey30")
wordcloud(tf_idf_wordcloud$word, tf_idf_wordcloud$value, col=terrain.colors(length(tf_idf_wordcloud$word), alpha=0.9), max.words=100, random.order=FALSE, rot.per=0.3 )
title(main = "Words by tf-idf score over 18 seasons of South Park", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()


#build plot for n-grams

episodes_text<-paste(by.episode$text, collapse=" // ")

episodes_text<- gsub('[[:punct:] ]+',' ',episodes_text)
episodes_text <- tolower(episodes_text)
episodes_text<- removeWords(episodes_text, stopwords("english"))
ngram_dfm <- dfm(episodes_text, stem=TRUE, remove_punct=TRUE, ngrams=4:4, verbose = FALSE, concatenator=" ")
ngram_df <- data.frame(word=ngram_dfm@Dimnames$features, freq=ngram_dfm@x)
ngram_df <- arrange(ngram_df, -freq)

png(file="WordCloud-ngram.png",width=1000,height=700, bg="grey30")
wordcloud(ngram_df$word, ngram_df$freq, col=terrain.colors(length(ngram_df$word), alpha=0.9), max.words=100, random.order=FALSE, rot.per=0.3 )
title(main = "Quadgrams over 18 seasons of South Park", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

#graph linking n-grams, doesn't provide insights
ngram_graph <- ngram_df %>%
  filter(freq > 10) %>%
  graph_from_data_frame()
set.seed(2017)

ggraph(ngram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#additional term-network, doesn't really provide any insights

tdm <- TermDocumentMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm <- removeSparseTerms(tdm, 0.4)
tdmMatrix <- as.matrix(tdm)

#remove connections below 10
tdmMatrix[tdmMatrix<10] <- 0
tdmMatrix[tdmMatrix>=10] <- 1


termMatrix <- tdmMatrix %*% t(tdmMatrix)
library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)/30000
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
pdf("term-network.pdf")
plot(g, layout=layout1)
dev.off()



