library(stringr)
library(stm)
library(LDAvis)
library(ggplot2)


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
by.episode.season18 <-NULL
for(g in seq_along(episodes)){
  
  subset <- dialogue[dialogue$ES==episodes[g], ]
  subset <- subset[complete.cases(subset), ]
  text <- str_c(subset$Line, collapse=" ")
  row <- data.frame(episode.number=g, episode.code=episodes[g], season=subset$Season[1], text=text)
 
   #for analysis of season 18 only
  if (subset$Season[1]==18){
    by.episode.season18 <- rbind(by.episode.season18, row)
  }
  by.episode <- rbind(by.episode, row)
  
  
}


processed <- textProcessor(by.episode$text, metadata = by.episode)
processedSeason18 <- textProcessor(by.episode.season18$text, metadata = by.episode.season18)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 1, upper.thresh = 25)
outSeason18 <- prepDocuments(processedSeason18$documents, processedSeason18$vocab, processedSeason18$meta, lower.thresh = 1, upper.thresh = 4)


southparkSTM <- stm(documents = out$documents, vocab = out$vocab,K=25, data = out$meta)
southparkSTMSeason18 <- stm(documents = outSeason18$documents, vocab = outSeason18$vocab,K=4, data = outSeason18$meta)

#plots of topics
plot(southparkSTM, type = "summary", main="Top Topics of South Park")
plot(southparkSTMSeason18, type = "summary", main="Top Topics of Season 18")

#shows top topics of, opens in browser
toLDAvis(southparkSTM, docs = out$documents)
toLDAvis(southparkSTMSeason18, docs = outSeason18$documents)

topicTimeline<- data.frame(apply(southparkSTM$theta, 1, which.max))
topicTimelineSeason18<- data.frame(apply(southparkSTMSeason18$theta, 1, which.max))
colnames(topicTimeline) <- "topic"
colnames(topicTimelineSeason18) <- "topic"

#plot for all seasons
p <- ggplot(topicTimeline,aes(seq_along(topic), topic, colour=topic)) + geom_point()
p + xlab("Episode") + ylab("Topic")+theme(legend.position="none")
#plot for season 18
p <- ggplot(topicTimelineSeason18,aes(seq_along(topic), topic, colour=topic)) + geom_point()
p + xlab("Episode") + ylab("Topic")+theme(legend.position="none")


