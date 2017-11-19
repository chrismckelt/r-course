
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
    # x: character string
    print("clean_data_text start")
    txt = tolower(txt)
    txt = gsub("[[:digit:]]", "", txt)
    txt = gsub("[[:punct:]]", "", txt)
    txt = gsub("[ \t]{2,}", " ", txt)
    txt = gsub("^\\s+|\\s+$", "", txt)
    #short form of common names: st., av., mr, etc.., remove periods
    txt <- gsub("\\s([A-Z])\\.\\s", " \\1", txt)
    txt <- gsub("\\s([A-Z][a-z]{1,3})\\.\\s", " \\1", txt)
    txt <- gsub("^([A-Z])\\.\\s", " \\1", txt)
    txt <- gsub("^([A-Z][a-z]{1,3})\\.\\s", " \\1", txt)
    #lower case text
    txt <- stri_trans_tolower(txt)
    #remove smileys
    txt <- gsub("<3|</3|\\bxd\\b|\\bx-d\\b|:&|:-&|:p\\b|:-p\\b|\\b=p\\b|\\b:d\\b|;d\\b|\\b:o\\)\\b|\\b8\\)|\\b8d\\b|\\b8-d\\b|:3\\b|:-x\\b|:x\\b|:o\\)|:-d\\b|:-o\\b|:o\\b|o_o\\b|o-o\\b|=p\\b|:s\\b|\\bd:", " ", txt)
    # remove lower greater symbols
    txt <- gsub("[<>]+", " ", txt)
    txt <- gsub("&", " and ", txt)
    #dont split compound words, like e-reader, e-mail, etc...
    txt <- gsub("(^|\\s)([a-z]{1,2})-([a-z]+)", " \\2\\3 ", txt)
    #remove RTs
    txt <- gsub("\\brt\\b", " ", txt)
    txt <- gsub("rt2win", " ", txt)
    txt <- gsub("<3RT", " ", txt)
    #replace email addresses with special mark
    txt <- gsub("\\w*@\\w*\\.\\w*", " <EMAIL> ", txt)
    #replae hashtags with special marker
    txt <- gsub("(\\#[a-zA-Záéíóúàèìòùäöüßñ0-9']+)", " <HASH> ", txt)
    #replace twitter-like names with special marker
    txt <- gsub("\\@\\w*[a-zA-Záéíóúàèìòùäöüßñ0-9]+\\w*", " <TW> ", txt)
    # replace URLs with special mark
    txt <- gsub("\\b([a-z]{3,6}://)?([\\0-9a-z\\-]+\\.)+([a-z]{2,6})+(/[\\0-9a-z\\?\\=\\&\\-_]*)*", " <URL> ", txt)
    #remove slashes
    txt <- gsub("[\\\\\\/\\|]+", " ", txt)
    #single words between quotes or parenthesis, remove the quotes or parenthesis
    txt <- gsub("\\s“([a-z]+)”\\s", " \\1 ", txt)
    txt <- gsub("\\s’([a-z]+)’\\s", " \\1 ", txt)
    txt <- gsub("\\s'([a-z]+)'\\s", " \\1 ", txt)
    txt <- gsub('\\s"([a-z]+)"\\s', " \\1 ", txt)
    txt <- gsub("\\((\\s?[a-z]+\\s?)\\)", "\\1", txt)
    #only one type of apostrophes, and only one at a time
    txt <- gsub("[’`']+", "'", txt)
    txt <- gsub("(\\.( )+)+", ". ", txt)
    # acronym or short form with apostrophe, just remove the period
    txt <- gsub("(^|\\.|\\s)([a-záéíóúàèìòùäöüßñ<>]+)\\.'([a-z])(\\s|\\.|$)", " \\2'\\3 ", txt)
    # replace numbers with special marker <N>
    txt <- gsub("([0-9]+([,\\.]?[0-9]+)?)", " <N> ", txt)
    #expressions refering time replaced with <T>
    txt <- gsub("<N>\\s:\\s<N>\\s?(a\\s|a\\.|p\\s|p\\.|am|pm|a\\.m|p\\.m)\\.?,?(\\s|$)", "<T> ", txt)
    txt <- gsub("<N>\\s:\\s<N>(\\s|$)", "<T>\\1", txt)
    txt <- gsub("(<N>)\\s+(am|pm|a\\.m|p\\.m|a\\s|a\\.|p\\s|p\\.)\\.?,?(\\s)?", "<T>\\3", txt)
    txt <- gsub("(<T>)\\s+(am|pm|a\\.m|p\\.m|a\\s|a\\.|p\\s|p\\.)\\.?,?(\\s)?", "<T>\\3", txt)
    txt <- gsub("(<N>)(\\s+)?-\\s+(<T>)", "<T> ", txt)
    txt <- gsub("(<T>)\\s+-\\s+(<N>)", "<T> ", txt)
    txt <- gsub("(<T>)(\\s+)?-(\\s+)?(<T>)", "<T> ", txt)
    txt <- gsub("<N>\\s+to\\s+<T>", "<T> to <T>", txt)
    txt <- gsub("<T>\\s+to\\s+<N>", "<T> to <T>", txt)
    txt <- gsub("(<T>)\\s+(am|pm|a\\.m|p\\.m|a|p)\\.?,?(\\s|$)", "<T>\\3", txt)
    #numbers reffering to % amounts, replaced with <PN>
    txt <- gsub("<N>( )*%", "<PN> ", txt)
    #numbers reffering to some year, like 80s, 90s... replaced with <YN>
    txt <- gsub("<N>\\s+('?s)\\.?(\\s|$)", "<YN> ", txt)
    #ordinal numbers replaced with <ON>
    txt <- gsub("<N>\\s+(st|th|nd|rd|er|ers)\\.?(\\s|$)", "<ON> ", txt)
    #number intervals, can be scores, telephone numbers, ...
    txt <- gsub("(<N>)\\s+-\\s+(<N>)", "<NN> ", txt)
    #numbers in parenthesis, remove parenthesis
    txt <- gsub("\\(\\s?<N>\\s?\\)", "<N>", txt)
    #numbers reffering to dates nov. 3, dec.1, etc...
    txt <- gsub(" (jan|feb|mar|apr|may|jun|jul|aug|sept|oct|nov|dec)\\.?,?\\s\\s?(<N>|<NN>)", "\\1 <D>", txt)
    txt <- gsub("(january|february|march|april|may|june|july|august|september|october|november|december),?\\s\\s?(<N>|<NN>)", "\\1 <D>", txt)
    #days of the week, short form, remove periods
    txt <- gsub(" (mon|tue|wed|thu|fri|sat|sun)\\.", "\\1", txt)
    #dont split compound words, like e-reader, e-mail, etc...
    txt <- gsub("(^|\\s)([a-z]{1,2})-([a-z]+)(\\s|\\.|\\$)", "\\2\\3", txt)
    #replaces commas with whitespace, don't break the sentence
    txt <- gsub("(,)+", " ", txt)
    #replace the rest of punctuation with single dots
    txt <- gsub("( )+-( )+", " . ", txt)
    txt <- gsub('[—!?;:…“”\\"()\\{\\}]+', " . ", txt)
    #remove remaining non characters
    txt <- gsub("[^0-9A-Za-záéíóúàèìòùäöüßñ.'<>]+", " ", txt)
    #remove periods in acronyms, so we can analyze them as a single word
    txt <- gsub("([a-z])\\.([a-z])\\.(([a-z])\\.)?(([a-z])\\.)?(\\s|$)?", " \\1\\2\\4\\6 ", txt)
    #remove duplicate number marks, mostly due to decimal points, but should be fixed
    txt <- gsub("(<N>[ .']+){2,}", " <N> ", txt)
    #replace punctuation with special mark
    txt <- gsub("[.]+", " <S> ", txt)
    #replace end and start of string also with special mark
    #txt <- gsub("^(.*?)$", "<S> \\1 <S>",txt)
    #remove duplicate sentence marks, and marks at beggining or end of line
    txt <- gsub("(<S>[ ]*)+", " <S> ", txt)
    txt <- gsub("<S>[ ]*$", "", txt)
    txt <- gsub("^[ ]*<S>", "", txt)
    #remove duplicates apostrophes
    txt <- gsub("(^|\\s)('(\\s)*)+(\\s|$)", " ' ", txt)
    txt <- gsub("''", "'", txt)
    #remove apostrophes that are alone, ie, with any word
    txt <- gsub("(^|\\s)'(\\s|$)", " ", txt)

    #remove genitive signs that are alone
    txt <- gsub("(\\s)'s(\\s|$)", " ", txt)

    #remove single apostrophes in front of a word or at the end
    txt <- gsub("^'", "", txt)
    txt <- gsub("(^|\\s)'([a-záéíóúàèìòùäöüßñ<>]+)'?(\\s|$)", "\\1\\2 ", txt)
    txt <- gsub("(\\s)?([a-záéíóúàèìòùäöüßñ<>]+)'(\\s|$)", "\\1\\2 ", txt)
    txt <- gsub("\\s'([a-z])", " \\1", txt)

    #fix <S> marks placed incorrectly
    txt <- gsub(" st <S>", " st ", txt)
    txt <- gsub(" ft <S>", " ft ", txt)
    txt <- gsub(" sq <S>", " sq ", txt)
    txt <- gsub("^st <S>", "st ", txt)
    txt <- gsub("^ft <S>", "ft ", txt)
    txt <- gsub("^sq <S>", "sq ", txt)
    txt <- gsub(" av <S>", " av ", txt)
    txt <- gsub("^av <S>", " av ", txt)
    txt <- gsub(" mr <S>", " mr ", txt)
    txt <- gsub("^mr <S>", "mr ", txt)
    txt <- gsub("^sgt <S>", "sgt ", txt)
    txt <- gsub(" sgt <S>", " sgt ", txt)
    txt <- gsub(" dep <S>", " dep ", txt)
    txt <- gsub("^dep <S>", "dep ", txt)
    txt <- gsub(" dept <S>", " dept ", txt)
    txt <- gsub("^dept <S>", "dept ", txt)
    txt <- gsub(" u <S> s( <S>)? ", "us ", txt)
    txt <- gsub("a <S> k <S> a ", "aka ", txt)
    txt <- gsub("v <S> i <S> p (<S>)?", "vip ", txt) #
    txt <- gsub("d <S> h <S>", "dh ", txt)
    txt <- gsub("p <S> s <S>", "ps ", txt)
    txt <- gsub("m <S> s <S>", "ms ", txt)
    txt <- gsub("r <S> i <S> p( <S>|$)?", "rip ", txt)
    txt <- gsub("<S> t <S> o <S> p ", " top ", txt)

    txt <- gsub("<T><N>(\\s|$)", "<N>", txt)
    txt <- gsub("<T><T>", "<T>", txt)
    txt <- gsub("<T><ON> ", "<N> st", txt)
    txt <- gsub("<S> ([a-z]) <S>", " \\1 ", txt)
    txt <- gsub("<S> ([a-z]+) <S>", " \\1 ", txt)
    txt <- gsub("^([a-z]+) <S>", "\\1 ", txt)
    txt <- gsub(">([a-z])", "> \\1", txt)
    txt <- gsub("([a-z])<", "\\1 ", txt)

    #q: ..  a: ...
    txt <- gsub("q <S>", " ", txt)
    txt <- gsub("<S> a <S>", "<S> ", txt)
    txt <- gsub("^a <S>", "", txt)
    txt <- gsub("^q <S>", "", txt)

    for (word in corpus::stopwords_en) {
        patt <- paste0('\\b', word, '\\b')
        repl <- paste(word, " ")
        txt <- gsub(patt, repl, txt)
    }

    #remove extra whitespaces
    txt <- stripWhitespace(txt)
    txt <- stri_trim_both(txt)
    print("clean_data_text end")
    txt
}


#' Given a sentence get the last word
#'
#'http://stringr.tidyverse.org/reference/word.html
str_get_last_word <- function(str) {
    sentences <- c(str)
    last <- word(sentences, -1)
    last
}


#' Given a sentence get the last words
str_get_words <- function(txt, total){
    array <- unlist(strsplit(txt, ' '))
    from <- (length(array) + 1) - total
    lst <- array[from:length(array)]
    stringi::stri_flatten(lst, collapse = " ")
}


guard <- function(func) {
    try_result = withCallingHandlers({
        func()
    }, warning = function(w) {
        flog.warn(w)
    }, error = function(e) {
        flog.error(e)
    }, finally = {

    })

    try_result
}