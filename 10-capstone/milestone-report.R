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

read_file <- function(path) {
    con <- file(path, "rb", encoding = "UTF-8")
    data <- readLines(con)
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

sample.blogs <- sample(data.blogs, 20000)
sample.news <- sample(data.news, 20000)
sample.twitter <- sample(data.twitter, 20000)
sample.all <- sample(c(sample.blogs, sample.news, sample.twitter), size = 10000, replace = TRUE)

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

    lst <- list(sample.blogs, sample.news, sample.twitter)
    corpus.data <- Corpus(VectorSource(lst))
    toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))

    corpus.data <- tm_map(corpus.data, tolower)
    corpus.data <- tm_map(corpus.data, removePunctuation)
    corpus.data <- tm_map(corpus.data, removeNumbers)
    corpus.data <- tm_map(corpus.data, removeWords, stopwords("english"))
    corpus.data <- tm_map(corpus.data, stripWhitespace)
    corpus.data <- tm_map(corpus.data, PlainTextDocument)
    corpus.data <- tm_map(corpus.data, toEmpty, "#\\w+")  
    corpus.data <- tm_map(corpus.data, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")  
    corpus.data <- tm_map(corpus.data, toEmpty, "@\\w+")  
    corpus.data <- tm_map(corpus.data, toEmpty, "http[^[:space:]]*")  
    corpus.data <- tm_map(corpus.data, toSpace, "/|@|\\|")
#``` 

## Explore the data
 
#Tokenize the sample to get with Uni and Bigrams to perform an N-grams analysis.  

#This will allow us to see the most used words and expressions. 
options(mc.cores = 1)
#```{r, echo=FALSE,eval=TRUE, warning=FALSE, message=FALSE}
    delim <- " \\r\\n\\t.,;:\"()?!"
    gram1Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
    gram2Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2), delim = delim) }
    gram3Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3), delim = delim) }

    tdm1 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram1Tokenizer))
    tdm2 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram2Tokenizer))
    tdm3 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram3Tokenizer))

    gram1freq <- data.frame(word = tdm1$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm1$i, j = tdm1$j, x = tdm1$v)))
    gram1freq <- arrange(gram1freq, desc(freq))

    gram2freq <- data.frame(word = tdm2$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm2$i, j = tdm2$j, x = tdm2$v)))
    gram2freq <- arrange(gram2freq, desc(freq))

    gram3freq <- data.frame(word = tdm3$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm3$i, j = tdm3$j, x = tdm3$v)))
    gram3freq <- arrange(gram3freq, desc(freq))

#``` 

data.graph <-    data.frame(gram1 = gram1freq$word[1:10], gram2 = gram2freq$word[1:10], gram3 = gram3freq$word[1:10])


#```{r, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE}
 
g1 <- ggplot(data.graph[0], aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "red") +
    ggtitle("1-gram") +
    xlab("1-grams") + ylab("Frequency")
g1
#``` 

## Wordcloud
#Analyse via a wordcloud can illustrate word frequencies very effectively as Unigram, Bigram, Trigram and Tetragram is as follows respectively:-
#```{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
 
#``` 