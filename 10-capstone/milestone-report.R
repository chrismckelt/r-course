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
    rm(con)
    data
}


#download_zip_files()
data.blogs <- read_file("final/en_US/en_US.blogs.txt")
data.news <- read_file("final/en_US/en_US.news.txt")
data.twitter <- read_file("final/en_US/en_US.twitter.txt")

path.src <- "c:/dev/r-course/10-capstone/final/en_US/"

sample.blogs <- sample(data.frame(text = unlist(sapply(data.blogs, `[`, "content")), stringsAsFactors = F), 80)
sample.news <- sample(data.frame(text = unlist(sapply(data.news, `[`, "content")), stringsAsFactors = F), 80)
sample.twitter <- sample(data.frame(text = unlist(sapply(data.twitter, `[`, "content")), stringsAsFactors = F), 80)
sample.all <- c(sample.blogs, sample.news, sample.twitter)

#data.all <- c(data.blogs, data.news, data.twitter)

corpus <- Corpus(VectorSource(list(sample.all)))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace) # Remove extra white space
corpus <- tm_map(corpus, toEmpty, "#\\w+") # Hashtags (#justsaying)
corpus <- tm_map(corpus, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)") # Email addresses and shout outs (e.g. foo@demo.net)
corpus <- tm_map(corpus, toEmpty, "@\\w+") # shout outs (e.g. @yoyo)
corpus <- tm_map(corpus, toEmpty, "http[^[:space:]]*") #- only removes things starting with http
corpus <- tm_map(corpus, toSpace, "/|@|\\|") # slashes

# Remove all non english characters as they cause issues down the road
#data.blogs <- iconv(data.blogs, "latin1", "ASCII", sub = "")
#data.news <- iconv(data.news, "latin1", "ASCII", sub = "")
#data.twitter <- iconv(data.twitter, "latin1", "ASCII", sub = "")
 
