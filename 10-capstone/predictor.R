#' Search ngrams with identical first two words. 
#' Calculate probabilities using modified Kneser - Ney smoothing. 
#' Use the word with the highest probability as prediction and the words with the highest probabilities as possible choices.
#' relies on n2,n3,n4,n5
#' @param sentence 
#'
#' @return
#' @export
#'
#' @examples
#' https://en.wikipedia.org/wiki/Katz%27s_back-off_model
predictor <- function(sentence) {
    
    if (is.na(sentence)) {
        warning("sentence NA or empty")
        stop()
    }
    sentence <- clean.text(sentence)

    flog.debug(paste("predictor --> sentence.cleaned =", sentence))
    dt.search.terms = as.data.table(str_split(sentence, " "), stringsAsFactors = FALSE)
    term_count <- nrow(dt.search.terms)

    if (term_count > 5) {
           term_count <- 4
    }

    dt.search.result <- colnames(c("ngram", "word", "freq", "length", "predicted"))
    
    counter <-1    
    while (counter < term_count) {
        ng_id <- term_count - (counter) + 2
        result <- search_ngram(dt.search.terms[counter:term_count], ng_id)
        if (nrow(result) > 0) {
            msg <- paste("predictor --> ngram ", ng_id, "found", nrow(result))
            flog.debug(msg)
            df.row <- sqldf(paste("select word, freq, length, word as predicted from result"))
            
            dt.search.result <- rbind(dt.search.result, data.frame(ngram = ng_id, word = df.row[1], freq = df.row[2], length = df.row[3], predicted = df.row[4]))
        }
        counter <- counter + 1
    }
    dt.search.result$predicted <- lapply(dt.search.result$predicted, function(x) str_get_last_word(x))
    
    dt.search.result
}



#' Cycle down ngram functions when no data found
#'
#' @param ngram
#'
#' @return
#' @export
#'
#' @examples
search_ngram <- function(search_terms, take) {
    
    #term_count <- 3
    flog.debug(paste("search_terms count", nrow(search_terms)))
    arg <- paste(search_terms$V1, sep = " ", collapse = " ")
    arg <- str_trim(arg)
    if (arg=="") stop("arg empty")
    sql <- paste0("select * from n", take, " where word like '", arg, "%' order by freq desc limit 10")
    flog.debug(paste("predictor --> search_ngram --> sql ngram =", sql))
    df.result <- sqldf(sql)
    df.result
}

search <- "a few"
search <- clean.text(search)
dt.search.terms = as.data.table(str_split(search, " "), stringsAsFactors = FALSE)
result <- predictor(search)

result