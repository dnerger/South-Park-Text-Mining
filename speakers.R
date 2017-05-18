library(tm)
library(RWeka)
library(stringr)

tvshow <- "southpark"
directory = paste("~/Data Analysis/files/", tvshow, sep="")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

# read in the data set

scripts <- read.csv("dialogue.csv", stringsAsFactors=FALSE, sep=" ")

# condense to get rid of season / episode
scripts$Character <- gsub("Mrs. Garrison", "Mr. Garrison", scripts$Character) # combine Garrisons
scripts$Character <-gsub("\n", "", scripts$Character)
by.speaker <- NULL
unique(scripts$Character)
for(speaker in unique(scripts$Character)){
  subset <- scripts[scripts$Character==speaker, ]
  text <- str_c(subset$Text, collapse=" ")
  row <- data.frame(speaker, text)
  by.speaker <- rbind(by.speaker, row)
}
by.speaker[2]
# condense low-volume speakers into one to create a manageable corpus
# this keeps 27
by.speaker.big <- by.speaker[nchar(as.character(by.speaker$text)) > 5500, ]
by.speaker.big[2]
# save the rest of the text into one big speaker "All others"
kept.speakers <- unique(as.character(by.speaker.big$speaker))
other.text <- by.speaker[!(by.speaker$speaker %in% kept.speakers), ]
other.text <- str_c(other.text$text, collapse=" ")
other <- data.frame(speaker="All others", text=other.text)

# add it back in
by.speaker <- rbind(by.speaker.big, other); rm(by.speaker.big)

# create corpus
myReader <- readTabular(mapping=list(content="text", id="speaker"))
corpus <- Corpus(DataframeSource(by.speaker), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# create term document matrix
options(mc.cores=1)
allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
all.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))

all.tdm.nonsparse <- as.matrix(all.tdm)
# remove sparse terms
all.tdm.75 <- removeSparseTerms(all.tdm, 0.75) # 3117 / 728215
all.tdm.75nonsparse <- as.matrix(all.tdm.75)
write.csv(all.tdm.75nonsparse, "southpark_tdm_speakers.csv")
# save as a simple data frame
count.all <- data.frame(inspect(all.tdm)) 
count.all$word <- row.names(count.all)
write.csv(count.all, "southpark_tdm_all.csv", row.names=FALSE)





library(wordcloud)

count.all <- read.csv("southpark_tdm_speakers.csv", stringsAsFactors=FALSE)

findFreq <- function(df){
  df$total <- rowSums(df[,2:22])
  freqs <- df[,c(1,23)]
  freqs <- freqs[order(-freqs$total), ]
  return(freqs)
}
freq1 <- findFreq(count.all)
cloud <- wordcloud(freq1$X, freq1$total, scale=c(2.5,0.5), min.freq=100, max.words=200, random.order=FALSE)

#dev.copy(png,'plots/southpark_wordcloud.png')
#dev.off()