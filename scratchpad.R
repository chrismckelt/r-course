# load package RCurl
library(RCurl)
# google docs spreadsheets url
google_docs = "https://docs.google.com/spreadsheet/"
# public key of data 'cars'
cars_key = "pub?key=18TlTr27vsiyNQAgfAtJy4zRRjrtERz8sStA-K438G88&output=csv"
# download URL of data file
cars_csv = getURL(paste(google_docs, cars_key, sep = ""))
# import data in R (through a text connection)
cars2004 = read.csv(textConnection(cars_csv), row.names = 1, header = TRUE)