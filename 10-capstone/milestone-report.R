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
sample.blogs <- sample(data.frame(text = unlist(sapply(data.blogs, `[`, "content")), stringsAsFactors = F), 20000)
sample.news <- sample(data.frame(text = unlist(sapply(data.news, `[`, "content")), stringsAsFactors = F), 20000)
sample.twitter <- sample(data.frame(text = unlist(sapply(data.twitter, `[`, "content")), stringsAsFactors = F), 20000)
sample.all <- sample(c(sample.blogs, sample.news, sample.twitter), size = 10000, replace = TRUE)


# build corpus
corpus <- Corpus(VectorSource(list(sample.blogs, sample.news, sample.twitter)))

#workspace cleanup
#workspace cleanup
remove(data.blogs)
remove(data.news)
remove(data.twitter)
remove(sample.blogs)
remove(sample.news)
remove(sample.twitter)
gc()

# clean 
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)  
corpus <- tm_map(corpus, toEmpty, "#\\w+")  
corpus <- tm_map(corpus, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")  
corpus <- tm_map(corpus, toEmpty, "@\\w+")  
corpus <- tm_map(corpus, toEmpty, "http[^[:space:]]*")  
corpus <- tm_map(corpus, toSpace, "/|@|\\|")

writeCorpus(corpus, filenames = "corpus.txt")

#Tokenize sample into Unigrams, Bigrams and Trigrams
tokenizer.uni <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tokenizer.bi <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tokenizer.tri <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

matrix.uni <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer.uni))
matrix.bi <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer.bi))
matrix.uni <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer.tri))

freqTerms <- findFreqTerms(uniGramMatrix, lowfreq = 2000)
 
