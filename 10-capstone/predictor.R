dt.result <- data.table()

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
    flog.info(paste("predictor input", sentence))
    
    if (is.na(sentence)) {
        warning("sentence NA or empty")
        stop()
    }

    sentence <- "a couple of weeks"
    flog.debug(paste("predictor --> sentence =", sentence))
    sentence <- clean.text(sentence)
    flog.debug(paste("predictor --> sentence.cleaned =", sentence))
    dt.words = as.data.table(str_split(sentence, " "),stringsAsFactors = FALSE)
    take <- nrow(dt.words)
   
    dt.result <- c("ngram", "word", "freq", "length")
    while (take > 1) {
        result <- search_ngram(words, take)
        print(result)
        if (nrow(result) > 0) {
            flog.debug(paste("predictor --> ngram ", take, "found", nrow(result)))
            df.row <- sqldf(paste("select word, freq, length from result"))
            print(df.row)
            dt.result <- rbind(dt.result, data.frame(ngram = take, word = df.row[1], freq = df.row[2], length = df.row[3]))
        }
        take <- take-1
    }

    stop("no results")
    
}



#' Cycle down ngram functions when no data found
#'
#' @param ngram
#' @param df.words
#'
#' @return
#' @export
#'
#' @examples
search_ngram <- function(df.words, take) {
    row_count <- nrow(df.words)
    if (row_count == 1) NULL
    if (take == 1) NULL

    flog.debug(paste("take",take))
    df.searchterms <- take_words(words, take-1)
    arg <- paste(df.searchterms$V1, sep = " ",collapse = " ")
        
    arg <- str_trim(arg)
    sql <- paste0("select * from n", take, " where word like '%", arg, "%' order by freq desc limit 10")
    flog.debug(paste("predictor --> search_ngram --> sql ngram =", sql))
    df.result <- sqldf(sql)
    df.result
}


result <- predictor("a couple of weeks")
result

