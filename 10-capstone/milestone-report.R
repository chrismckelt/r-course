#---
#title: "<b>Milestone Report</b>"
#subtitle: "Use the arrow keys to navigate slides"
#author: "Chris McKelt"
#date: "November 2017"
#output:
  #revealjs::revealjs_presentation:
    #theme: solarized
    #highlight: pygments
    #center: true
#---

##```{r setup, include=FALSE}
suppressMessages(rm(list = ls()))
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_151\\')
source('c:/dev/r-course/10-capstone/include.r')
library(pacman)
install_standard_packages()
using("tm")
using("rJava")
using("RWeka")
using("RWeka")
using("stringi")
using("NLP")
using("SnowballC")
using("RColorBrewer")
using("wordcloud")
using("igraph")
using("fpc")
using("biclust")
using("cluster")
using("quanteda")
using("Rgraphviz")
using("Matrix")
using("dplyr")
suppressMessages(setwd("c:/dev/r-course/10-capstone/"))
options(mc.cores = 1)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
 
##```

## Overview

#This is a milestone report for the Coursera Data Science unit to build predictive model for text input.

#The predictive model could be a combination of probabilistic models (N-grams, others), and rule-based models 

#The data for analysis to be used is from

#https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

#The texts inside the zip file we will analyse are under the 'en_US' folder

#![image](https://user-images.githubusercontent.com/662868/32357634-a02e21c4-c079-11e7-9136-8277964e9f8b.png)

## Gather the data

##```{r, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE}

download_zip_files <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    if (!file.exists("data.zip")) {
        save_file(url, "data.zip")
        unzip(zipfile = "c:/dev/r-course/10-capstone/data.zip", exdir = "c:/dev/r-course/10-capstone/final/")
    }
}

smaller <- function(x) {
    x <- sample(x, length(x) * 0.0009999)
}

read_file <- function(path) {
    con <- file(path, open = "rb")
    data <- readLines(con, skipNul = TRUE, encoding = "UTF-8")
    close(con)
    data
}

download_zip_files()
#```

##First read files and adjust for encoding and sample each to optimise performance of the analyse.

#```{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}

data.blogs <- read_file("final/en_US/en_US.blogs.txt")
data.news <- read_file("final/en_US/en_US.news.txt")
data.twitter <- read_file("final/en_US/en_US.twitter.txt")
data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
data.news = iconv(data.news, "latin1", "ASCII", sub = "")
data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")

sample.blogs <- smaller(data.blogs)
sample.news <- smaller(data.news)
sample.twitter <- smaller(data.twitter)
sample.all <- smaller(c(sample.blogs, sample.news, sample.twitter))

#```

# Data overview

#```{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
    data_summary <- data.frame(
    Blogs=c((summary(nchar(data.blogs)))),News =c((summary(nchar(data.news)))),
    Twitter =c((summary(nchar(data.twitter)))))
    kable(data_summary)
#```

## Build and clean the corpus
#Create a corpus document from the given texts and clean up to create a NGram model we can analyse.

#```{r, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE}


group_texts <- list(sample.blogs, sample.news, sample.twitter)
group_texts <- tolower(group_texts)
group_texts <- removeNumbers(group_texts)
group_texts <- removePunctuation(group_texts, preserve_intra_word_dashes = TRUE)
group_texts <- gsub("http[[:alnum:]]*", "", group_texts)
group_texts <- stripWhitespace(group_texts)
group_texts <- gsub("\u0092", "'", group_texts)
group_texts <- gsub("\u0093|\u0094", "", group_texts)

corpus.data <- VCorpus(VectorSource(group_texts))
toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))
corpus.data <- tm_map(corpus.data, toEmpty, "#\\w+")
corpus.data <- tm_map(corpus.data, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")
corpus.data <- tm_map(corpus.data, toEmpty, "@\\w+")
corpus.data <- tm_map(corpus.data, toEmpty, "http[^[:space:]]*")
corpus.data <- tm_map(corpus.data, toSpace, "/|@|\\|")

save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
bad_words <- readLines("./bad_word_list.txt")
corpus.data <- tm_map(corpus.data, removeWords, bad_words)
 
corpus.data <- tm_map(corpus.data, stemDocument)
  
#``` 

## Explore the data
 
#Tokenize the sample to get with Uni and Bigrams to perform an N-grams analysis.  

#This will allow us to see the most used words and expressions. 
    options(mc.cores = 1)
#```{r, echo=FALSE,eval=TRUE, warning=FALSE, message=FALSE}
 
    gram1Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
    gram2Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
    gram3Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

    tdm1 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram1Tokenizer))
    tdm2 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram2Tokenizer))
    tdm3 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram3Tokenizer))

    gram1freq <- data.frame(word = tdm1$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm1$i, j = tdm1$j, x = tdm1$v)))
    gram1freq <- as.data.frame(sqldf("select * from gram1freq order by freq desc limit 10"))

    gram2freq <- data.frame(word = tdm2$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm2$i, j = tdm2$j, x = tdm2$v)))
    gram2freq <- sqldf("select * from gram2freq order by freq desc limit 10")

    gram3freq <- data.frame(word = tdm3$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm3$i, j = tdm3$j, x = tdm3$v)))
    gram3freq <- sqldf("select * from gram3freq order by freq desc limit 10")

#``` 


#```{r, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE}
 
g1 <- ggplot(gram1freq, aes(x = word, y = freq)) +
    geom_bar(stat = "identity", fill = "red") +
    ggtitle("1-gram") +
    xlab("1-grams") + ylab("Frequency")
g1


g2 <- ggplot(gram2freq, aes(x = word, y = freq)) +
    geom_bar(stat = "identity", fill = "yellow") +
    ggtitle("2-gram") +
    xlab("2-grams") + ylab("Frequency")
g2

g3 <- ggplot(gram3freq, aes(x = word, y = freq)) +
    geom_bar(stat = "identity", fill = "blue") +
    ggtitle("3-gram") +
    xlab("3-grams") + ylab("Frequency")
g3

#``` 

## Wordcloud
#Analyse via a wordcloud can illustrate word frequencies very effectively as Unigram, Bigram, Trigram and Tetragram is as follows respectively:-
#```{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
create_wordcloud <- function(tdm, palette = "Dark2") {
    mtx = as.matrix(tdm)
    # get word counts in decreasing order
    word_freqs = sort(rowSums(mtx), decreasing = TRUE)
    # create a data frame with words and their frequencies
    dm = data.frame(word = names(word_freqs), freq = word_freqs)
    dm <- sqldf("select * from dm limit 100")
    # plot wordcloud
    wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, palette))
}
create_wordcloud(tdm1, "Set1")
create_wordcloud(tdm2, "Set2")
create_wordcloud(tdm3, "Set3")

#``` 