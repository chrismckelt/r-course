
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
clean_data_text <- function(txt) {
    # txt: character string
    
    #stem text
    require(SnowballC)
    txt <- wordStem(txt, language = 'en')

    #lower case text
    txt <- tolower(txt)

    txt <- gsub("[0-9](?:st|nd|rd|th)", "", txt, ignore.case = F, perl = T) #remove ordinal numbers
    txt <- gsub("[.\\-!]", " ", txt, ignore.case = F, perl = T) #remove punctuation
    txt <- gsub("[^\\p{L}'\\s]+", "", txt, ignore.case = F, perl = T) #remove punctuation, leaving '
    txt <- gsub("^\\s+|\\s+$", "", txt) #trim leading and trailing whitespace

    txt <- stripWhitespace(txt)
    #dont split compound words, like e-reader, e-mail, etc...
    #txt <- gsub("(^|\\s)([a-z]{1,2})-([a-z]+)", "", txt)
    ##remove RTs
    #txt <- gsub("\\brt\\b", " ", txt)
    #txt <- gsub("rt2win", " ", txt)
    #txt <- gsub("<3RT", " ", txt)
    ##replace email addresses with special mark
    #txt <- gsub("\\w*@\\w*\\.\\w*", " ", txt)
    ##replae hashtags with special marker
    #txt <- gsub("(\\#[a-zA-Záéíóúàèìòùäöüßñ0-9']+)", " ", txt)
    ##replace twitter-like names with special marker
    #txt <- gsub("\\@\\w*[a-zA-Záéíóúàèìòùäöüßñ0-9]+\\w*", " ", txt)
    ## replace URLs with special mark
    #txt <- gsub("\\b([a-z]{3,6}://)?([\\0-9a-z\\-]+\\.)+([a-z]{2,6})+(/[\\0-9a-z\\?\\=\\&\\-_]*)*", " ", txt)
    ##remove slashes
    #txt <- gsub("[\\\\\\/\\|]+", " ", txt)
    ##single words between quotes or parenthesis, remove the quotes or parenthesis
    #txt <- gsub("\\s“([a-z]+)”\\s", " ", txt)
    #txt <- gsub("\\s’([a-z]+)’\\s", " ", txt)
    #txt <- gsub("\\s'([a-z]+)'\\s", " ", txt)
    #txt <- gsub('\\s"([a-z]+)"\\s', " ", txt)
    #txt <- gsub("\\((\\s?[a-z]+\\s?)\\)", " ", txt)
    ##only one type of apostrophes, and only one at a time
    #txt <- gsub("[’`']+", " ", txt)
    #txt <- gsub("(\\.( )+)+", " ", txt)
    ## acronym or short form with apostrophe, just remove the period
    #txt <- gsub("(^|\\.|\\s)([a-záéíóúàèìòùäöüßñ<>]+)\\.'([a-z])(\\s|\\.|$)", " ", txt)
    ## replace numbers with special marker <N>
    #txt <- gsub("([0-9]+([,\\.]?[0-9]+)?)", " ", txt)
    ##expressions refering time replaced with <T>
    #txt <- gsub("<N>\\s:\\s<N>\\s?(a\\s|a\\.|p\\s|p\\.|am|pm|a\\.m|p\\.m)\\.?,?(\\s|$)", " ", txt)
    #txt <- gsub("<N>\\s:\\s<N>(\\s|$)", " ", txt)
    #txt <- gsub("(<N>)\\s+(am|pm|a\\.m|p\\.m|a\\s|a\\.|p\\s|p\\.)\\.?,?(\\s)?", " ", txt)
    #txt <- gsub("(<T>)\\s+(am|pm|a\\.m|p\\.m|a\\s|a\\.|p\\s|p\\.)\\.?,?(\\s)?", " ", txt)
    #txt <- gsub("(<N>)(\\s+)?-\\s+(<T>)", " ", txt)
    #txt <- gsub("(<T>)\\s+-\\s+(<N>)", " ", txt)
    #txt <- gsub("(<T>)(\\s+)?-(\\s+)?(<T>)", " ", txt)
    #txt <- gsub("<N>\\s+to\\s+<T>", " ", txt)
    #txt <- gsub("<T>\\s+to\\s+<N>", " ", txt)
    #txt <- gsub("(<T>)\\s+(am|pm|a\\.m|p\\.m|a|p)\\.?,?(\\s|$)", " ", txt)

    ##numbers in parenthesis, remove parenthesis
    #txt <- gsub("\\(\\s?<N>\\s?\\)", " ", txt)
    ##numbers reffering to dates nov. 3, dec.1, etc...
    #txt <- gsub(" (jan|feb|mar|apr|may|jun|jul|aug|sept|oct|nov|dec)\\.?,?\\s\\s?(<N>|<NN>)", " ", txt)
    #txt <- gsub("(january|february|march|april|may|june|july|august|september|october|november|december),?\\s\\s?(<N>|<NN>)", " ", txt)
    ##days of the week, short form, remove periods
    #txt <- gsub(" (mon|tue|wed|thu|fri|sat|sun)\\.", " ", txt)
    ##dont split compound words, like e-reader, e-mail, etc...
    #txt <- gsub("(^|\\s)([a-z]{1,2})-([a-z]+)(\\s|\\.|\\$)", " ", txt)
    ##replaces commas with whitespace, don't break the sentence
    #txt <- gsub("(,)+", " ", txt)
    ##replace the rest of punctuation with single dots
    #txt <- gsub("( )+-( )+", " . ", txt)
    #txt <- gsub('[—!?;:…“”\\"()\\{\\}]+', " ", txt)
    ##remove remaining non characters
    #txt <- gsub("[^0-9A-Za-záéíóúàèìòùäöüßñ.'<>]+", " ", txt)
    ##remove periods in acronyms, so we can analyze them as a single word
    ##txt <- gsub("([a-z])\\.([a-z])\\.(([a-z])\\.)?(([a-z])\\.)?(\\s|$)?", " \\1\\2\\4\\6 ", txt)
    ##remove duplicate number marks, mostly due to decimal points, but should be fixed
    #txt <- gsub("(<N>[ .']+){2,}", " ", txt)
    ##replace punctuation with special mark
    #txt <- gsub("[.]+", "", txt)
    ##replace end and start of string also with special mark
    ##txt <- gsub("^(.*?)$", "<S> \\1 <S>",txt)
    ##remove duplicate sentence marks, and marks at beggining or end of line
    #txt <- gsub("(<S>[ ]*)+", " ", txt)
    #txt <- gsub("<S>[ ]*$", "", txt)
    #txt <- gsub("^[ ]*<S>", "", txt)
    ##remove duplicates apostrophes


    txt <- gsub("http"," ", txt)
    txt <- gsub("mailto", " ", txt)

    #remove genitive signs that are alone
    #txt <- gsub("(\\s)'s(\\s|$)", " ", txt)

    #remove single apostrophes in front of a word or at the end
    txt <- gsub("^'", " ", txt)
   
    for (word in corpus::stopwords_en) {
        patt <- paste0('\\b', word, '\\b')
        repl <- paste(word, " ")
        txt <- gsub(patt, repl, txt)
    }
    
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

    if (length(df) > 0) return(FALSE)

    if (is.null(df)) return(FALSE)

    if (is.na(df)) return(FALSE)

    if (nrow(df)==0) return(FALSE)

    return (TRUE)
}

