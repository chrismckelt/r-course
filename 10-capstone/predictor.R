
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
        sqldf(stupid_backoff(5, words))
    }

    # 3 words given 
    if (nrow(words) == 3) {
        flog.debug("predictor --> trying n4")
        sqldf(stupid_backoff(4, words))
    }
    
    # 2 words given 
    if (nrow(words) == 2) {
        flog.debug("predictor --> trying n3")
        sqldf(stupid_backoff(3, words))
    }

    
  
    
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
stupid_backoff <- function(ngram, df.words) {

    df.any <- create_sql(ngram, df.words)
    df.any <- as.data.table(df.any)
    if (nrow(df.any)) df.any

    flog.debug(paste("stupid_backoff --> no results found trying next ngram:",ngram-1))

    if (ngram <= 1) NULL

    # no results found - try next ngram with 1 less word
    sql <- paste("select * from [df.words] limit ", ngram - 1)
    flog.debug(paste("stupid_backoff --> sql words =", sql))
    df.words <- sqldf(sql)
    stupid_backoff(ngram - 1, df.words)

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

    #   ngram <- 5
    #   df.words <- words

    sql <- paste("select * from [df.words] limit ", ngram)
    flog.debug(paste("predictor --> sql words =", sql))
    df.words <- sqldf(sql)
    arg <- str_trim(convert.to.string(df.words))
    sql <- paste0("select * from n", ngram, " where word like '%", arg, "%' order by freq desc limit 10")
    flog.debug(paste("predictor --> sql =", sql))
    sql
}

result <- predictor("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
result

