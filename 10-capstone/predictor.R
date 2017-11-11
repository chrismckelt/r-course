
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

    sentence <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

    flog.debug(paste("predictor --> sentence =", sentence))
    sentence <- clean.text(sentence)
    flog.debug(paste("predictor --> sentence.cleaned =", sentence))
    words = str_split(sentence, " ")
    words <- as.data.table(words, stringsAsFactors = FALSE)
   
    # 4 words given 
    if (nrow(words) >= 4)
    {
        flog.debug("predictor --> trying n5+")
        sqldf(try_next_ngram(5, words))
    }

    # 3 words given 
    if (nrow(words) == 3) {
        flog.debug("predictor --> trying n4")
        sqldf(try_next_ngram(4, words))
    }
    
    # 2 words given 
    if (nrow(words) == 2) {
        flog.debug("predictor --> trying n3")
        sqldf(try_next_ngram(3, words))
    }

    stop("no results")
    
}

take_words <- function(df.words, word_count_to_take) {
    to <- nrow(df.words)
    from <- to - word_count_to_take
    chopped <- df.words[from:to]
    flog.debug(chopped)
    chopped
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
try_next_ngram <- function(ngram, df.words) {
    #ngram <- 5
    #df.words <- words

    stopifnot(nrow(df.words) > 0)
    df.any <- create_sql(ngram, df.words)

    flog.debug(paste("predictor --> try_next_ngram --> no results found trying next ngram:",ngram-1))

    if (ngram > 1) {
        # no results found - try next ngram with 1 less word
        try_next_ngram(ngram - 1, df.words)
    }
    df.any
}

#' Dynamic SQL string to search a ngram source for N terms provided
#'
#' @param ngram_name
#' @param words
#'
#' @return
#' @export
#'
#' @examples
create_sql <- function(ngram, df.words) {
    if (ngram <= 1) NULL
    
    df.chopped <- take_words(df.words, ngram-1)
    arg <- ""

    for (i in 1:nrow(df.chopped)) {
       arg <- paste(arg, df.chopped$V1[i])
    }
    arg <- str_trim(arg)
    sql <- paste0("select * from n", ngram, " where word like '%", arg, "%' order by freq desc limit 10")
    flog.debug(paste("predictor --> create_sql --> sql ngram =", sql))
    sql
}

result <- predictor("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
result

