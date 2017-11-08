
#suppressMessages(rm(list = ls()))
#Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_151\\')
#options(encoding = "UTF-8")
##options(shiny.error = browser)
#options(mc.cores = 1)
#options(encoding = "UTF-8")
#options(stringsAsFactors = FALSE)

#source('c:/dev/r-course/10-capstone/include.r')
#suppressMessages(setwd("c:/dev/r-course/10-capstone"))
#using("ANLP")
#library(pacman)
#install_standard_packages()
#using("tm")
#using("rJava")
#using("RWeka")
#using("stringi")
#using("NLP")
#using("SnowballC")
#using("RColorBrewer")
#using("wordcloud")
#using("igraph")
#using("fpc")
#using("biclust")
#using("cluster")
#using("quanteda")
#using("Rgraphviz")
#using("Matrix")
#using("dplyr")
#using("SparseM")
#using("tm")
#using("rJava")
#using("RWeka")
#using("stringi")
#using("NLP")
#using("SnowballC")
#using("RColorBrewer")
#require(ngram)
#library(stringr)
#data.blogs <- (readTextFile("final/en_US/en_US.blogs.txt", encoding = "UTF-8"))
#data.news <- (readTextFile("final/en_US/en_US.news.txt", encoding = "UTF-8"))
#data.twitter <- (readTextFile("final/en_US/en_US.twitter.txt", encoding = "UTF-8"))

##data.all <- c(data.blogs, data.news, data.twitter)

#smaller <- function(x) {
    #x <- sample(x, length(x) * 0.0099999)
#}

#sample.blogs <- smaller(data.blogs)
#sample.news <- smaller(data.news)
#sample.twitter <- smaller(data.twitter)
#sample.all <- c(sample.blogs, sample.news, sample.twitter)

##sample.all <- cleanTextData(sample.all)
##rm(data.blogs)
##rm(data.news)
##rm(data.twitter)

#n1 <- buildNgramModel(1)
##n2 <- buildNgramModel(2)
##n3 <- buildNgramModel(3)


tdm2 <- generateTDM(sample.all, 2, T)
tdm3 <- generateTDM(n_all, 3, T)
##lst <- as.list(c(tdm2,tdm3))

##test <- cleanTextData( "If this isn't the cutest thing you've ever seen, then you must be")
##predict_Backoff(test,lst,T)

#Trim <- function(x) {
    ## http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
    #gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
#}


#create_ngrams <- function(sentence_splits, ngram_size = 2) {
    #ngrams <- c()
    #for (sentence in sentence_splits) {
        #sentence <- Trim(sentence)
        #if ((nchar(sentence) > 0) && (sapply(gregexpr("\\W+", sentence), length) >= ngram_size)) {
            #ngs <- ngram(sentence, n = ngram_size)
            #ngrams <- c(ngrams, get.ngrams(ngs))
        #}
    #}
    #return(ngrams)
#}

#clean_text <- function(text_blob) {
    ## swap all sentence ends with code 'ootoo'
    #text_blob <- gsub(pattern = ';|\\.|!|\\?', x = text_blob, replacement = 'ootoo')

    ## remove all non-alpha text (numbers etc)
    #text_blob <- gsub(pattern = "[^[:alpha:]]", x = text_blob, replacement = ' ')

    ## force all characters to lower case
    #text_blob <- tolower(text_blob)

    ## remove any small words {size} or {min,max}
    #text_blob <- gsub(pattern = "\\W*\\b\\w{1,2}\\b", x = text_blob, replacement = ' ')

    ## remove contiguous spaces
    #text_blob <- gsub(pattern = "\\s+", x = text_blob, replacement = ' ')

    ## split sentences by split code
    #sentence_vector <- unlist(strsplit(x = text_blob, split = 'ootoo', fixed = TRUE))
    #return(sentence_vector)
#}

#corpus_sentences <- clean_text(paste(sample.all, collapse = " "))

#n2 <- create_ngrams(corpus_sentences, ngram_size = 2)
#n3 <- create_ngrams(corpus_sentences, ngram_size = 3)

## consolidate all n-gram vectors into one
#n_all <- c(n2, n3)
 
##matches <- c()
##for (sentence in n_all) {
    ### find exact match with double backslash and escape
    ##if (grepl(word, sentence)) {
        ##print(sentence)
        ##matches <- c(matches, sentence)
    ##}
##}

### find highest probability word
##precision_match <- c()
##for (a_match in matches) {
    ### how many spaces in from of search word
    ##precision_match <- c(precision_match, nchar(strsplit(x = a_match, split = word)[[1]][[1]]))
##}

### use highest number and a random of highest for multiples
##best_matched_sentence <- sample(matches[precision_match == max(precision_match)], size = 1)

##print(best_matched_sentence)

word <- "which"


