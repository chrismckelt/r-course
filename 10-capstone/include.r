
#' using
#'
#' @param packageName
#'
#' @return
#' @export
#'
#' @examples
using <- function(packageName) {
    if (!require(packageName, character.only = TRUE) && !(packageName %in% installed.packages())) {
        install.packages(dput(packageName), dependencies = TRUE, quiet = FALSE)
    }

   library(packageName, character.only = TRUE)
}

using("pacman")

#' Load "devtools", "tidyverse", "knitr", "markdown", "moments", "e1071", "data.table", "sqldf", "downloader", "magrittr", "ggplot2", "lubridate"
#'
#' @return
#' @export
#'
#' @examples
install_standard_packages <- function() {
    ## setup - install missing packages and reference
    packs <- c("devtools", "tidyverse", "knitr", "markdown", "moments", "e1071", "data.table",  "downloader", "magrittr", "ggplot2", "lubridate")
    p_load("foreach")
    foreach(n = 1:length(packs)) %do%  using(packs[n])
}

devtools::install_github("ggrothendieck/sqldf")

#' Download and save a file from the given url
#'
#' @param url
#' @param name
#'
#' @return
#' @export
#'
#' @examples
save_file = function(url, name) {
    if (!file.exists(name)) {
        library(downloader)
        download(url, destfile = name)
    }
}

#' Download zip file for project and unzip to folder
#'
#' @return
#' @export
#'
#' @examples
download_zip_files <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    if (!file.exists("data.zip")) {
        save_file(url, "data.zip")
        output_folder <- "c:/dev/r-course/10-capstone/data"
        if (!dir.exists(output_folder)) dir.create(output_folder)
        unzip(zipfile = "c:/dev/r-course/10-capstone/data.zip",exdir = output_folder)
    }
}

#' Take a small sample from a given data frame
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
small_sample <- function(x, size) {
    x <- sample(x, length(x) * size)
}

#' Trim leading and trailing string space
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
trim <- function(x) {
    # http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#' Read text from a file with  skipNul = TRUE, encoding = "UTF-8"
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_file <- function(path) {
    con <- file(path, open = "rb")
    data <- readLines(con, skipNul = TRUE, encoding = "UTF-8")
    close(con)
    data
}

#' Convert an RMD file to an R script. Removes all markdown so script is runnable
#'
#' @return
#' @export
#'
#' @examples#
covert_rmd_to_r <- function() {
    library(knitr)
    purl("milestone-report.Rmd")
}


#covert_rmd_to_r()

#' Remove empty spaces in string
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
str_remove_whitespace <- function(str) {
    y <- str_replace_all(string = str, pattern = " ", repl = "")
    y
}

#' Make a path to a file in a folder (defaults to 'data')
#'
#' @param filename
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
get_data_file_path <- function(filename, dir ="/data/") {
    y <- paste0(trim(getwd()), dir, filename)
    y
}


#' Create an RMD file from a R script
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
generate_rmd <- function(path = 'c:/dev/r-course/10-capstone/milestone-report.RMD') {
    library(knitr)
    rmarkdown::render()
}



#' Generic function for parallelizing any task  
#'
#' @param task
#'
#' @return
#' @export
#'
#' @examples
parallelize_task  <- function(task, ...) {
   
    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    flog.debug(paste("Task starting", match.call()[2]))
    r <- task(...)
    flog.debug(paste("Task complete", match.call()[2]))
    stopCluster(cl)
    r
}

parallelize_task_chunked <- function(task, df.big, chunk_size = 1000) {
    p_load("itertools")

    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)

    flog.debug(paste("Task starting", match.call()[2]))
   
    df.merged <- foreach(df.split = isplitRows(df.big, chunkSize = chunk_size), .combine = cbind, .inorder = TRUE) %dopar% {
        rbind(sapply(df.split, task))
    }

    flog.debug(paste("Task complete", match.call()[2]))
    stopCluster(cl)
    df.merged
}


#' This function allows to do some text cleaning with several
#               options such as coverting to lowercase, removing numbers,
#               removing punctuation symbols, removing extra white spaces
#'
#' @param x
#' @param lowercase
#' @param numbers
#' @param punctuation
#' @param spaces
#'
#' @return
#' @export
#'
#' @examples
clean_data_text <- function(input) {
    txt <- tolower(input)

    #stem text
    require(SnowballC)
    txt <- wordStem(txt, language = 'en')

    for (word in corpus::stopwords_en) {
        patt <- paste0('\\b', word, '\\b')
        repl <- paste(word, " ")
        txt <- gsub(patt, repl, txt)
    }

    for (word in bad_words) {
        patt <- paste0('\\b', word, '\\b')
        repl <- paste(word, " ")
        txt <- gsub(patt, repl, txt)
    }
    #txt <- gsub("[0-9](?:st|nd|rd|th)", "", txt, ignore.case = F, perl = T) #remove ordinal numbers
    #txt <- gsub("[.\\-!]", " ", txt, ignore.case = F, perl = T) #remove punctuation
    #txt <- gsub("[^\\p{L}'\\s]+", "", txt, ignore.case = F, perl = T) #remove punctuation, leaving '
    #txt <- gsub("^\\s+|\\s+$", "", txt) #trim leading and trailing whitespace
    #get rid of unnecessary spaces
   # txt <- str_replace_all(txt, " ", " ")
    # Get rid of URLs
    #txt <- str_replace_all(txt, "http://t.co/[a-z,A-Z,0-9]*{8}", "")
    # Take out retweet header, there is only one
   # txt <- str_replace(txt, "RT @[a-z,A-Z]*: ", "")
    # Get rid of hashtags
    #txt <- str_replace_all(txt, "#[a-z,A-Z]*", "")
    # Get rid of references to other screennames
   # txt <- str_replace_all(txt, "@[a-z,A-Z]*", "")
    txt <- rm_non_words(txt, TRUE, TRUE)

    txt <- gsub("http"," ", txt)
    txt <- gsub("mailto", " ", txt)
  

    # Remove excessive spacing
    txt <- gsub("^ +| +$|( ) +", " ", txt)
    #single quotes
    txt <- gsub('[[:punct:] ]+', ' ', txt)
    #remove non-alphanumeric symbols from a string
    txt <- gsub("[^[:alnum:] ]", "", txt)
    
    # Remove 1-2 letter words
    txt <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", txt)

    #remove extra whitespaces
    txt <- stripWhitespace(txt)
    txt <- stri_trim_both(txt)
    txt
}


#' Given a sentence get the last word
#'
#'http://stringr.tidyverse.org/reference/word.html
str_get_last_word <- function(str) {
    sentences <- c(str)
    last <- stringr::word(sentences, -1)
    last
}


#' Given a sentence get the last words
str_get_words <- function(txt, total){
    
    paste(tail(strsplit(txt, "\\s+")[[1]], total), collapse = " ")
}


is_data_frame_valid <- function(df) {

    tryCatch({
        if (length(df) == 0) return(FALSE)
    })

    tryCatch({
        if (nrow(df) > 0) return(TRUE)
    })


    return (FALSE)
}

