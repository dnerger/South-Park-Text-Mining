
library(dplyr)
library(stringr)
library(syuzhet)
library(ggplot2)
library(viridis)

process_sentiment <- function (rawtext, mymethod) {
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 100)) %>% 
    summarize(text = str_c(x, collapse = " "))
  mySentiment <- data.frame(cbind(linenumber = chunkedtext$linenumber, 
                                  sentiment = get_sentiment(chunkedtext$text, method = mymethod)))
}
xy <- process_sentiment(full_sents,"nrc")

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

p <- plot_sentiment(xy)
p


fourier_sentiment <- function (sentiment) {
  as.numeric(get_transformed_values(sentiment[,2], 
                                    low_pass_size = 3,
                                    scale_vals = TRUE,
                                    scale_range = FALSE))
}

plotshape <- data_frame(linenumber = 1:100, ft = fourier_sentiment(xy)) %>% mutate(series="South Park")


pp <- ggplot(data = plotshape) + geom_area(alpha = 0.7) + 
  theme_minimal() +
pp$data$ft
pp
   

  