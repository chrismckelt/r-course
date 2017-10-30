suppressMessages(rm(list = ls()))
options(encoding = "UTF-8")
#options(shiny.error = browser)
options(shiny.reactlog = TRUE)
source('c:/dev/r-course/10-capstone/include.r')
library(pacman)
p_load("tm")
p_load("stringr")
suppressMessages(setwd("c:/dev/r-course/10-capstone/week2"))

download_zip_files <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    if (!file.exists("data.zip")) {
        save_file(url, "data.zip")
        unzip(zipfile = "c:/dev/r-course/10-capstone/data.zip", exdir = "c:/dev/r-course/10-capstone/final")
    }
}


#download_zip_files()

blogs <- readLines("../final/en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("../final/en_US/en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("../final/en_US/en_US.twitter.txt", encoding = "UTF-8")