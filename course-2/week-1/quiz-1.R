
library(plyr)
library(XLConnect)
library(XML) 
library(httr) 
library(WriteXLS)
library(data.table)
library(XLConnectJars)
library(rJava)
library(RCurl)
library(dplyr)

# csv
csvUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
csvFile <- "data.csv"
#download.file(url = csvUrl, destfile = csvFile, method = "wget")

#pdf variables
pdfUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
pdfFile <- "variables.pdf"
#download.file(url = pdfUrl, destfile = pdfFile, method = "wget")

datos <- read.csv("C:\\dev\\r-course\\course-2\\csvFile.csv")

count(datos$VAL == 24)
