#install.packages(c("stringr","dplyr","NLP","openNLP","magrittr","ggplot2","syuzhet","viridis"))
#also needs OpenNLPmodels.en from <http://datacube.wu.ac.at/>
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(stringr)
library(dplyr)
library(NLP)
library(openNLP)
library(magrittr)
library(ggplot2)
library(syuzhet)
library(viridis)


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
namevector <-c('person','location','organization')
by.season[,namevector] <- NA

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
season18_sents <-NULL
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
  if (g==18){
    season18_sents<- c(season18_sents, sents(dialogue_doc))
  }
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
  if (g==18){
    season18_sents<- c(season18_sents, sents(dialogue_doc))
  }
  
}

process_sentiment <- function (rawtext, mymethod) {
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 100)) %>% 
    summarize(text = str_c(x, collapse = " "))
  mySentiment <- data.frame(cbind(linenumber = chunkedtext$linenumber, 
                                  sentiment = get_sentiment(chunkedtext$text, method = mymethod)))
}

full_sentiments <- process_sentiment(full_sents,"nrc")
season18_sentiments <- process_sentiment(season18_sents,"nrc")

plot_sentiment <- function (mySentiment) {
  g <- ggplot(data = mySentiment, aes(x = linenumber, y = sentiment)) +
    geom_bar(stat = "identity", color = "midnightblue") + 
    
    theme_minimal() +
    labs(y = "Sentiment", caption = "Text sourced from Southpark Wikia") +
    scale_x_discrete(expand=c(0.02,0)) +
    theme(plot.caption=element_text(size=8)) +
    theme(axis.text.y=element_text(margin=margin(r=-10))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
}

p <- plot_sentiment(full_sentiments)
p

p <- plot_sentiment(season18_sentiments)
p


fourier_sentiment <- function (sentiment) {
  as.numeric(get_transformed_values(sentiment[,2], 
                                    low_pass_size = 3,
                                    scale_vals = TRUE,
                                    scale_range = FALSE))
}

#plot fourier transformation over all 18 seasons   
plotshape <- data_frame(linenumber = 1:100, ft = fourier_sentiment(full_sentiments)) %>% mutate(series="South Park")

pp <- ggplot(data = plotshape,aes(x = linenumber, y = ft, fill = series)) + geom_area(alpha = 0.7) + 
  theme_minimal() + ylab("Transformed Sentiment Value") +
  labs(title = "Sentiment over 18 seasons of South Park") +xlab("Position in script")+scale_fill_viridis(end = 0.1, discrete=TRUE) +
  scale_x_discrete(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(plot.caption=element_text(size=9)) +
  theme(legend.position="none") 
pp


#plot fourier transformation of season 18   
plotshape <- data_frame(linenumber = 1:100, ft = fourier_sentiment(season18_sentiments)) %>% mutate(series="South Park")

pp <- ggplot(data = plotshape,aes(x = linenumber, y = ft, fill = series)) + geom_area(alpha = 0.7) + 
  theme_minimal() + ylab("Transformed Sentiment Value") +
  labs(title = "Sentiment timeline season 18") +xlab("Position in script")+scale_fill_viridis(end = 0.1, discrete=TRUE) +
  scale_x_discrete(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(plot.caption=element_text(size=9)) +
  theme(legend.position="none") 
pp

#remove [18] for full sentiment count
nrc_sents <- get_nrc_sentiment(paste(by.season$text[18], collapse=" ")) 
nrc_posneg <- nrc_sents[9:10]
nrc_sents <- nrc_sents[1:8]
nrc_t<- data.frame(t(nrc_sents))

#Transformation and  cleaning
names(nrc_t)[1] <- "count"
nrc_t <- cbind("sentiment" = rownames(nrc_t), nrc_t)



sentiment <- NULL
nrc_t <- data.frame(sentiment=nrc_t$sentiment, count = nrc_t$count)
nrc_t
qplot(sentiment, data=nrc_t, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments in Season 18")



  