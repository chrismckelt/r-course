suppressMessages(rm(list = ls()))
options(encoding = "UTF-8")

options(stringsAsFactors = FALSE)
source('c:/dev/r-course/10-capstone/include.r')
library(pacman)
p_load("tm")
p_load("stringr")
p_load("readr")
p_load("DescTools")
library(tm)
suppressMessages(setwd("c:/dev/r-course/10-capstone/"))

download_zip_files <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    if (!file.exists("data.zip")) {
        save_file(url, "data.zip")
        unzip(zipfile = "c:/dev/r-course/10-capstone/data.zip", exdir = "c:/dev/r-course/10-capstone/final")
    }
}

read_file <- function(path) {
    con <- file(path)
    data <- readLines(con)
    close(con)
    data
}


#download_zip_files()
data.blogs <- read_file("final/en_US/en_US.blogs.txt")
data.news <- read_file("final/en_US/en_US.news.txt")
data.twitter <- read_file("final/en_US/en_US.twitter.txt")


