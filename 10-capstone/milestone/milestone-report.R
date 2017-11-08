## ----setup, include=FALSE------------------------------------------------
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
using("SparseM")
 
using("foreach")

suppressMessages(setwd("c:/dev/r-course/10-capstone/milestone/"))
options(mc.cores = 1)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)

## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE----------------

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

trim <- function(x) {
    # http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

read_file <- function(path) {
    con <- file(path, open = "rb")
    data <- readLines(con, skipNul = TRUE, encoding = "UTF-8")
    close(con)
    data
}

download_zip_files()


## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------

data.blogs <- read_file("../final/en_US/en_US.blogs.txt")
data.news <- read_file("../final/en_US/en_US.news.txt")
data.twitter <- read_file("../final/en_US/en_US.twitter.txt")
data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
data.news = iconv(data.news, "latin1", "ASCII", sub = "")
data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")

sample.blogs <- smaller(data.blogs)
sample.news <- smaller(data.news)
sample.twitter <- smaller(data.twitter)
sample.all <- c(sample.blogs, sample.news, sample.twitter)


## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
    data_summary <- data.frame(
    Blogs=c((length(nchar(data.blogs)))),News =c((length(nchar(data.news)))),
    Twitter =c((length(nchar(data.twitter)))))
    kable(data_summary)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------

	group_texts <- list(sample.blogs, sample.news, sample.twitter)
	group_texts <- tolower(group_texts)
	group_texts <- removeNumbers(group_texts)
	group_texts <- removePunctuation(group_texts, preserve_intra_word_dashes = TRUE)
	group_texts <- gsub("http[[:alnum:]]*", "", group_texts)
	group_texts <- stripWhitespace(group_texts)
	group_texts <- gsub("\u0092", "'", group_texts)
	group_texts <- gsub("\u0093|\u0094", "", group_texts)

    corpus.data <- PCorpus(VectorSource(group_texts), dbControl = list(dbName = "pcorpus.db", dbType = "DB1"))
	toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
	toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))
	corpus.data <- tm_map(corpus.data, toEmpty, "#\\w+")
	corpus.data <- tm_map(corpus.data, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")
	corpus.data <- tm_map(corpus.data, toEmpty, "@\\w+")
	corpus.data <- tm_map(corpus.data, toEmpty, "http[^[:space:]]*")
	corpus.data <- tm_map(corpus.data, toSpace, "/|@|\\|")
 

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------

	save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
	bad_words <- readLines("./bad_word_list.txt")
	corpus.data <- tm_map(corpus.data, removeWords, bad_words)
 

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------


	corpus.data <- tm_map(corpus.data, stemDocument)
 

## ---- echo=FALSE,eval=TRUE, warning=FALSE, message=FALSE-----------------
   
    gram1Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
    gram2Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
    gram3Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

    tdm1 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram1Tokenizer))
    tdm2 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram2Tokenizer))
    tdm3 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram3Tokenizer))

    gram1freq <- data.frame(word = tdm1$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm1$i, j = tdm1$j, x = tdm1$v)))
    #gram1freq <- as.data.frame(sqldf("select * from gram1freq order by freq desc limit 10"))

    gram2freq <- data.frame(word = tdm2$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm2$i, j = tdm2$j, x = tdm2$v)))
    #gram2freq <- sqldf("select * from gram2freq order by freq desc limit 10")

    gram3freq <- data.frame(word = tdm3$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm3$i, j = tdm3$j, x = tdm3$v)))
    #gram3freq <- sqldf("select * from gram3freq order by freq desc limit 10")

convert_counts <- function(x) {
    x <- ifelse(x>0, "Yes","No")
}


f1 <- findFreqTerms(tdm3, 5)
count.train <- (tdm3$nrow * .7) # 70 %
dtm.train <- tdm3[1:count.train, ]
dtm.test <- tdm3[count.train:tdm3$nrow,]
dtm.train.labels <- tdm3[1:count.train,]$dimnames$Terms
dtm.test.labels <- tdm3[count.train:tdm3$nrow,]

freq.train <- dtm.train[, f1]
freq.test <- dtm.test[, f1]


# convert dtm to dtm_matrix using sparse storage
dtm_matrix = as.matrix(dtm.train)
#specify the features, vector to be predicted, and kernel method in the svm model
svm_model <- svm(dtm_matrix, dtm.train.labels, kernel = "linear")
summary(svm_model)
#inspect results
pred <- predict(svm_model, dtm)
table(pred, classvec)

using("caret")


# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(dtm.train)
train <- cbind(train, c(0, 1))
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

