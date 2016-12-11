# Title: analysis.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

####packages and setup

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr","ggplot2", "knitr", "markdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

path <- getwd()
path

####create results folder for output if required
results_folder <- "results"

if (!file.exists(results_folder)) {
    print("create results folder")
    dir.create(results_folder)
}


#------
#functions start

####download zip and extract to folder
download_project_files <- function(url) {
    filename <- "Dataset.zip"
    if (!file.exists(path)) {
        dir.create(path)
    }
    zip_file <- file.path(path, filename)
    download.file(url, zip_file)
    unzip(zip_file, exdir = path)
}

####read extract and convert it to a data.frame
get_tables <- function(filename, cols = NULL) {
    print(paste("Getting table:", filename))
    f <- paste(data_folder, filename, sep = "/")
    data <- data.frame()
    if (is.null(cols)) {
        data <- read.table(f, sep = "", stringsAsFactors = F)
    } else {
        data <- read.table(f, sep = "", stringsAsFactors = F, col.names = cols)
    }
    data
}

####save to results folder ./results
save_results <- function(data, name) {
    print(paste("saving results", name))
    file <- paste(results_folder, "/", name, ".csv", sep = "")
    write.csv(data, file)
}


#------
#step 1 - load in data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#------
#------
##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


#------


####output codebook.md
library(knitr)
library(markdown)
rmd <- file.path(getwd(), "analysis.r")
knit(rmd, output = "codebook.md", encoding = "ISO8859-1", quiet = FALSE)
markdownToHTML("codebook.md", "codebook.html")
