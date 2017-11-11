
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
    flog.debug(paste("predictor --> sentence =", sentence))
    sentence <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
    sentence <- clean.text(sentence)
    flog.debug(paste("predictor --> sentence.cleaned =", sentence))
    words = str_split(sentence, " ")
    words <- (as.data.table(words, stringsAsFactors = FALSE))

    # 1 word given --> lookup to see the next in the list ordered by most frequesnt
    flog.debug("predictor --> trying n2")
    if (length(words) == 1) sqldf(create_sql(2, words))
    flog.debug("predictor --> trying n3")
    # 2 words given 
    if (length(words) == 2) sqldf(create_sql(3, words))
    flog.debug("predictor --> trying n4")
    # 3 words given 
    if (length(words) == 3) sqldf(create_sql(4, words))
    flog.debug("predictor --> trying n5+")
    # 4 words given 
    if (length(words) >= 4) sqldf(create_sql(5, words))
    
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
 
    sql <- paste("select * from [df.words] limit ", nrow(df.words) - ngram)
    flog.debug(paste("predictor --> sql words =", sql))
    df.words <- sqldf(sql)
    arg <- str_trim(convert.to.string(df.words))
    sql <- paste0("select * from n", ngram, " where word like '%", arg, "%' order by freq desc limit 10")
    flog.debug(paste("predictor --> sql =", sql))

    sql
}

result <- predictor("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
result

