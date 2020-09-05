
apple <- read.csv(file.choose(), header = T)
str(apple)


library(tm)
library(NLP)
library(utf8)
corpus <- iconv(apple$text, to = "utf-8")

corpus <- Corpus(VectorSource(corpus))
+inspect(corpus[1:5])


corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])


cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeA <- function(a) gsub('[^A-Za-z]',' ', a)
cleanset <- tm_map(cleanset, content_transformer(removeA))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple','get','qqq','fang','pos','sylvacap','things'))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'hoofinance', 
                   replacement = 'finance')
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'yafinance', 
                   replacement = 'finance')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


w <- rowSums(tdm)

w <- subset(w, w>=25)

barplot(w,las = 2, col = rainbow(50),main="Most repeated Tweeted Word")

help("barplot")


# Word cloud
library(wordcloud)
library(RColorBrewer)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          #max.words = 150,
          # random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.93),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)






#part 2-----------------------------------
# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = 'utf-8')

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')

