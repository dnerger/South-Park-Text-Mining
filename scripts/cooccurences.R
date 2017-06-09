#if packages missing, uncomment install.packages
#install.packages(c("stringr","tidyr","reshape2","igraph", "ggplot2"))
library(stringr)
library(tidyr)
library(reshape2)
library(igraph)
library(ggplot2)

directory <- "~/GitHub/South-Park-Text-Mining"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

dialogue <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)
for(h in seq_along(dialogue[,1])) if(nchar(dialogue$Episode[h]) < 2) dialogue$Episode[h] <- paste("0", dialogue$Episode[h], sep="")
dialogue$ES <- paste(dialogue$Season, dialogue$Episode, sep=".")
for(j in c(1,5)) dialogue[,j] <- as.numeric(dialogue[,j])
dialogue <- dialogue[complete.cases(dialogue), ]
list(value = dialogue$Episode)

characters <- unique(dialogue$Character)
xy <-data.frame(count = dialogue$Character)
character_appear <- aggregate(data.frame(count=dialogue$Character), list(value = dialogue$Character),length)
character_appear <- character_appear[order(character_appear$count, decreasing = TRUE),]

#lines of 20 major characters
major_character_lines <- dialogue[which(dialogue$Character %in% character_appear$value[1:20]), ]
character_percentage <- table(major_character_lines$Character)/70896*100
character_percentage <- sort(character_percentage, decreasing = TRUE)
character_percentage_frame <- as.data.frame(character_percentage)
character_percentage_frame$Freq


ggplot(character_percentage_frame, aes(x=character_percentage_frame$Var1,character_percentage_frame$Freq))+
geom_bar(stat="identity", aes(fill=Var1)) + 
 
  theme_classic()+   labs(title="Cartman speaks the most") + 
  ylab("Lines in %") + 
  xlab("")   +
  geom_text(aes(x=Var1, y=Freq-0.2, label=ceiling(Freq*100) / 100), 
            color="white", fontface="bold", size=2)+
  theme(legend.position=1,plot.title = element_text(size=20), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.text.x = element_text(angle = 35, hjust = 1, size=12),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14))


speaker_scene_matrix <- major_character_lines %>%
  acast(Character ~ ES, fun.aggregate = length)

data_matrix <- as.matrix(t(speaker_scene_matrix))
total_occurrences <- colSums(t(speaker_scene_matrix))

co_occurrence <- t(data_matrix) %*% data_matrix

g <- graph.adjacency(co_occurrence,
                     weighted = TRUE,
                     diag = FALSE,
                     mode = "upper")

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = c(weight = "sum", type = "ignore"))

females <- c("Liane", "Sharon", "Sheila", "Wendy") 

V(g)$gender <- ifelse(V(g)$name %in% females, "female", "male")
#plot co-occurences
plot(g,
     vertex.label.family = "Helvetica",
     vertex.label.font = 1,
     vertex.shape = "sphere",
     vertex.size=total_occurrences/230,
     vertex.label.cex=0.9,
     vertex.color=c( "pink", "skyblue")[1+(V(g)$gender=="male")],
     vertex.label.color="black",
     vertex.frame.color = NA,
     edge.width = E(g)$weight/15000,
     
     edge.curved=.1,
     layout=layout_in_circle)


#dendogram
norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)
