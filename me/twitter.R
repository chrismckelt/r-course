require(ROAuth)
require(twitteR)
library(RCurl)
require(stringr)

rm(list = ls())
setwd("C:/dev/r-course/me")

options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

reqURL <- "https://api.twitter.com/oauth/request_token"

accessURL <- "http://api.twitter.com/oauth/access_token"

authURL <- "http://api.twitter.com/oauth/authorize"

source("d:/GDrive/Software/code/settings.R")

download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

user <- twitteR::getUser("chris_mckelt")
followers <- user$getFollowers()
twitter_follower_names <- sapply(followers, FUN = function(row) paste(row$screenName, sep = " "))

 
currentDate <- Sys.Date()
csvFileName <- paste("twitter_follower_names_", currentDate, ".csv", sep = "")
write.table(twitter_follower_names, csvFileName, sep = ",", col.names = FALSE, row.names = FALSE)