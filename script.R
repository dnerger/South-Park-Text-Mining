library(rvest)
library(tm)
library(tau)
library(xml2)
library(RWeka)
library(plyr)
library(ggplot2)


tvshow <- "southpark"
directory = paste("~/Data Analysis/files/", tvshow, sep="")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

baseurl <- "http://southpark.wikia.com"
url <- "http://southpark.wikia.com/wiki/Portal:Scripts"

scrape_url <- read_html(url)
season_selector <- ".lightbox-caption a"

season_urls <- html_nodes(scrape_url, season_selector) %>% html_attr("href")
head(season_urls)


#execute together
counter <- 1
for (i in season_urls){
  uri <- read_html(paste(baseurl, i, sep="/"))
  
  episode_selector <- ".lightbox-caption a"
  episode_urls <- html_nodes(uri, episode_selector) %>%html_attr("href")
  counter_new <- 1
  for (j in episode_urls){
    if (counter<10){
      separator <- "_0"
    }else{
      separator<- "_"
    }
    if (counter_new<10){
      separator2 <- "-0"
    }else{
      separator2 <- "-"
    }
    script_url <- read_html(paste(baseurl, j, sep =""))
    episode_script <-html_nodes(script_url, 'td span') %>% html_text()
    write.csv(episode_script, file = paste(directory, "/", tvshow, separator,  counter,separator2, counter_new, ".txt", sep=""), row.names = FALSE)
    episode_scripts <-html_nodes(script_url, '.wikitable tr td') %>% html_text()
    episode_speakers <- html_nodes(script_url, '.wikitable tr th') %>% html_text()
    write.csv(episode_scripts, file = paste(directory, "/", tvshow, separator,  counter,separator2, counter_new, "b.txt", sep=""), row.names = FALSE)
    write.csv(episode_speakers, file = paste(directory, "/", tvshow, separator,  counter,separator2, counter_new, "b-speakers.txt", sep=""), row.names = FALSE)
    episode_cast <-html_nodes(script_url, "#mw-content-text div ul li") %>% html_text()
    write.csv(episode_cast, file = paste(directory, "/", tvshow, separator,  counter,separator2, counter_new, "-cast.txt", sep=""),row.names = FALSE)
    counter_new <- counter_new +1
    if (counter_new == 10){
      Sys.sleep(1)
    }
   
    
  }
  counter <- counter+1
  
  Sys.sleep(1)
}


#removes files that are too small to be relevant (b-files)
files_in_directory <- dir(directory, pattern = ".txt")

for (i in 1:length(files_in_directory)){
  if(file.size(files_in_directory[i])<200){
    file.remove(files_in_directory[i])
  }
}



cast_files <- list.files(pattern="'*cast*", recursive = TRUE)
cast_data <- list()
for (i in cast_files){
  cast_data[i] <-read.table(i, header=TRUE, sep="\n")
}

#creates cast-appeareances for every episode as csv
  current_name<-(names(cast_data[1]))
  current_data <- cast_data[1]
  current_data["season"] <-NA
  current_data$season <- substr(current_name, 16,17)
  current_data["episode"] <-NA
  current_data$episode <- substr(current_name, 19,20)
  
  write.table(current_data, "character-appearences.csv", row.names=FALSE, col.names = c("Character", "Season", "Episode"))
  
  for (i in 2:length(cast_data)){
    current_name<-(names(cast_data[i]))
    current_data <- cast_data[i]
    current_data["season"] <-NA
    current_data$season <- substr(current_name, 16,17)
    current_data["episode"] <-NA
    current_data$episode <- substr(current_name, 19,20)
    write.table(current_data, "character-appearences.csv", row.names=FALSE,col.names=FALSE, append = TRUE)
  }

cast_csv <- read.csv("character-appearences.csv",stringsAsFactors = FALSE, sep=" ")
cast_csv$Character <- gsub("[\r\n]","",cast_csv$Character)
cast_csv$Character <- trimws(cast_csv$Character)
cast_csv$Character <- gsub("Mrs. Garrison", "Mr. Garrison", cast_csv$Character) # combines the  Garrisons
#fixes other name discrepancies
cast_csv$Character<- gsub("^Stan$", "Stan Marsh", cast_csv$Character)
cast_csv$Character<-gsub("^Cartman$", "Eric Cartman", cast_csv$Character)
cast_csv$Character<-gsub("^Butters$", "Butters Stotch", cast_csv$Character)
cast_csv$Character<-gsub("^Chef$", "Jerome \"Chef\" McElroy", cast_csv$Character)
cast_csv$Character<-gsub("^Kyle$", "Kyle Broflovski", cast_csv$Character)
cast_csv$Character<-gsub("^Kenny$", "Kenny McCormick", cast_csv$Character)


number_of_appearances <- count(cast_csv, "Character")
df <- data.frame(Character = number_of_appearances$Character, Appearances = number_of_appearances$freq)

p <- ggplot(data=subset(df, df$Appearances>40), aes(x=Character, y= Appearances)) + geom_bar(stat="identity", fill="steelblue")
p <- p + theme(axis.text.x=element_text(angle=20, hjust=1, size=12, color="steelblue"))
p <- p + geom_text(aes(label=Appearances), vjust=1.6, color="white", size=3.5)
p



cname <- file.path(directory)
(docname <- dir(cname))

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
cast_corpus <- Corpus(DirSource(cname, pattern="*cast.txt"), readerControl = list(id = cast_files))

cast_corpus.p <-tm_map(cast_corpus, content_transformer(tolower))
cast_corpus.p <- tm_map(cast_corpus.p, removeWords, stopwords('english'))

cast_corpus.p <- tm_map(cast_corpus.p, stemDocument)

tdm <- TermDocumentMatrix(cast_corpus.p, control = list(tokenize = BigramTokenizer))
cast_files <- gsub("southpark_", "",cast_files)
cast_files <- gsub("-cast.txt", "",cast_files)
cast_files <- paste("s",cast_files, sep="")
colnames(tdm) <- cast_files
options(mc.cores =1)

tdm
inspect(tdm[260:277,1:20])
dtm <- DocumentTermMatrix(cast_corpus.p)
rownames(dtm) <- cast_files
dtm

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq,20)

tf <- data.frame(term=names(freq), freq=freq)   
head(tf,20)

tf$term <- factor(tf$term, levels = tf$term[order(-tf$freq)])
library(ggplot2)

#Not necessary
p <- ggplot(subset(tf, freq>50), aes(term, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 

p

library(text2vec)


current_dir_files = list.files(pattern="'*cast*", recursive = TRUE)
files_iterator = ifiles(current_dir_files)
dir_files_iterator = idir(path= ".")


#all files that are parts of an episode
episode_files <- grep(list.files(pattern="southpark", recursive = TRUE), pattern="cast", inv=T, value=T)
episode_files

#files that need to be merged
episode_files_to_merge <- grep(episode_files, pattern="b", value=T)
episode_files_to_merge

episode_data_to_merge <-  list()
for (i in episode_files_to_merge){
  episode_data_to_merge[i] <-read.table(i, header=TRUE, sep="\t", quote="\"")
}

episode_data_to_merge[69]
episode_data_to_merge[70]




testxy <- data.frame(episode_data_to_merge[1], episode_data_to_merge[2])
testxy$data.southpark_03.01b.txt[[4]]
testxy$data.southpark_03.01b.txt <- gsub("\"", "", testxy$data.southpark_03.01b.txt)
write.table(testxy, file="testxy.csv", row.names=FALSE, col.names=FALSE)


ins <- function(a, to.insert=list(), pos=c()) {

c(a[seq(pos[1])], 
  to.insert[[1]], 
  a[seq(pos[1]+1, pos[2])], 
  to.insert[[2]], 
  a[seq(pos[2]+1, length(a))]
)
}

for (i in 1:length(episode_data_to_merge)){
  if (i%%2==1){
    i
    episode_data=list()
    episode_name<-(names(episode_data_to_merge[i]))
    

   
    episode_data["Season"] <-NA
    #data/southpark_ length is 15, so +1 and then get season & episode
    episode_data$Season <- substr(episode_name, 16,17)
    episode_data["Episode"] <-NA
    episode_data$Episode <- substr(episode_name, 19,20)
    
    episode_speaking <- unlist(episode_data_to_merge[i])
    episode_speaking <- gsub("\"","", episode_speaking)
    
    episode_data$Character <- episode_speaking
   
    episode_text <-unlist(episode_data_to_merge[i+1])
    
    episode_text <- gsub("\"","", episode_text)
    if (i==3){
      episode_text <- episode_text[-144]
    }
    if (i==21){
      episode_text <- episode_text[-c(430,431)]
      
    }
    if (i==41){
      episode_text <- episode_text[-c(395,396)]
    }
    if (i==47){
      episode_text <- episode_text[-c(346,347)]
    }
    if (i==51){
      episode_text <- ins(episode_text, list(" ", " "), pos=c(371, 372))
      episode_text <- episode_text[-375]
      
    }
    if (i==65){
      episode_text <- episode_text[-187]
    }
    if (i==69){
      episode_text <- c(episode_text[seq(56)], " ", episode_text)
     
    }
    episode_data$Text <- episode_text
    
    write.table(episode_data, "dialogue.csv", row.names=FALSE,col.names = TRUE, append = TRUE)
    
  }
}






