suppressMessages(rm(list = ls()))
options(encoding = "UTF-8")
#options(shiny.error = browser)
options(shiny.reactlog = TRUE)
source('c:/dev/r-course/10-capstone/include.r')
library(pacman)
p_load("tm")
p_load("stringr")
suppressMessages(setwd("c:/dev/r-course/10-capstone"))

# Question 2
# The en_US.twitter.txt has how many lines of text?  invalid input found on input connection
 
#blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8")
length(twitter) # Question 2
# The en_US.twitter.txt has how many lines of text?

#blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8")
length(twitter)
