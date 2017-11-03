suppressMessages(rm(list = ls()))
options(encoding = "UTF-8")

options(stringsAsFactors = FALSE)
source('c:/dev/r-course/10-capstone/include.r')
library(pacman)
install_standard_packages()
p_load("tm")
p_load("SnowballC")
p_load("RColorBrewer")
p_load("wordcloud")
p_load("igraph")
p_load("fpc")
p_load("biclust")
p_load("cluster")
p_load("quanteda")
p_load("Rgraphviz")
p_load("Rcpp")
p_load("tidytext")
using("revealjs")
 
suppressMessages(setwd("c:/dev/r-course/10-capstone/"))

download_zip_files <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    if (!file.exists("data.zip")) {
        save_file(url, "data.zip")
        unzip(zipfile = "c:/dev/r-course/10-capstone/data.zip", exdir = "c:/dev/r-course/10-capstone/final/")
    }
}

read_file <- function(path) {
    con <- file(path, encoding = "UTF-8" )
    data <- readLines(con)
    close(con)
    #rm(con)
    data
}


#download_zip_files()
# read files and adjust for encoding
data.blogs <- read_file("final/en_US/en_US.blogs.txt")
data.news <- read_file("final/en_US/en_US.news.txt")
data.twitter <- read_file("final/en_US/en_US.twitter.txt")
#data.all <- c(data.blogs, data.news, data.twitter)


#sample data to speed things up
sample.blogs <- sample(data.blogs, 20000)
sample.news <- sample(data.news, 20000)
sample.twitter <- sample(data.twitter, 20000)
sample.all <- sample(c(sample.blogs, sample.news, sample.twitter), size = 10000, replace = TRUE)



#stop()
# build corpus
lst <- list(sample.blogs, sample.news, sample.twitter)
corpus.data <- Corpus(VectorSource(lst))
inspect(corpus.data)
 



##  custom content transformers
toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))

corpus.data <- tm_map(corpus.data, tolower)
corpus.data <- tm_map(corpus.data, removePunctuation)
corpus.data <- tm_map(corpus.data, removeNumbers)
corpus.data <- tm_map(corpus.data, removeWords, stopwords("english"))
corpus.data <- tm_map(corpus.data, stripWhitespace)  
corpus.data <- tm_map(corpus.data, toEmpty, "#\\w+")  
corpus.data <- tm_map(corpus.data, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")  
corpus.data <- tm_map(corpus.data, toEmpty, "@\\w+")  
corpus.data <- tm_map(corpus.data, toEmpty, "http[^[:space:]]*")  
corpus.data <- tm_map(corpus.data, toSpace, "/|@|\\|")

dtm <- DocumentTermMatrix(corpus.data)
nDocs(dtm)
nTerms(dtm)

#Tokenize sample into Unigrams, Bigrams and Trigrams
tokenizer.uni <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tokenizer.bi <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
 
matrix.uni <- DocumentTermMatrix(corpus.data, control = list(tokenize = tokenizer.uni))
matrix.bi <- DocumentTermMatrix(corpus.data, control = list(tokenize = tokenizer.bi))

freq.terms <- findFreqTerms(matrix.uni, 3,3)
freq.expressions <- findFreqTerms(matrix.bi, 10)

plot(dtm, terms = freq.terms[1:5], corThreshold = 0.5)

#https://stackoverflow.com/questions/17294824/counting-words-in-a-single-document-from-corpus-in-r-and-putting-it-in-dataframe
df <- as.data.frame(as.matrix(dtm))
# and transpose for plotting
df <- data.frame(t(df))
setDT(df, keep.rownames = TRUE)[]

cols <- c("word", "blog_count", "news_count", "twitter_count")
colnames(df) <- cols
top.blogwords <- sqldf("select word, blog_count from df order by blog_count desc limit 5")
top.newswords <- sqldf("select word, news_count from df order by news_count desc limit 5")
top.twitterwords <- sqldf("select word,twitter_count from df order by twitter_count desc limit 5")



df %>%
    count(df$word) %>%
    with(wordcloud(df$word, n, max.words = 100))