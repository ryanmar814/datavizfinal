##Open/install relevant packages and libraries.
getwd()
library(rmarkdown)
library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tibble)
library(MASS)
library(dplyr)
library(plotly)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(wordcloud2)
library(SnowballC)

#Publisher wordcloud
filePath <- "https://raw.githubusercontent.com/ryanmar814/datavizfinal/master/books_publisher.csv"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function(x, pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c ("books", "publishing", "company")) #set stopwords?
docs <- tm_map(docs, removePunctuation)
#docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##Title Cloud
filePath <- "https://raw.githubusercontent.com/ryanmar814/datavizfinal/master/books_titles.csv"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function(x, pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c ( "part", "make", "made")) #set stopwords?
docs <- tm_map(docs, removePunctuation)
#docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=100, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
          
##plotting pages v average rating, top 100
ggplot(top100, aes(x=num_pages, y=average_rating)) + 
  geom_point()

##plotting pages v average rating, bottom 100
ggplot(bottom100, aes(x=num_pages, y=average_rating)) + 
  geom_point()

## plotting month v average rating
## no sig difference
ggplot(month_averagerating, aes(x=Month, y=Average.Rating)) +
  geom_bar(stat="identity") +
  ylim(NA, 4)

##publication date v number of books
ggplot(month_numofbooks, aes(x=Month, y=Number.of.Books)) +
  geom_bar(stat="identity")

##barline
ggplot(month_numbooks_numpages) + 
  geom_col(aes(x = Month, y = Number.of.Books), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = Month, y = 3*Average.Number.of.Pages), size = 1.5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Average Number of Pages"))
