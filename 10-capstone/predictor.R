
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
        #stop()
    }

    words = strsplit(sentence, " ")

    # 1 word given --> lookup to see the next in the list ordered by most frequesnt
    if (length(words) == 1) sqldf(create_sql("n2", words))
     
    # 2 words given 
    if (length(words) == 2) sqldf(create_sql("n3", words))

    # 3 words given 
    if (length(words) == 3) sqldf(create_sql("n4", words))

    # 4 words given 
    if (length(words) >= 4) sqldf(create_sql("n5", words))
    
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
create_sql <- function(ngram_name,words) {
    arg <- paste(words, collapse = '')
    flog.debug(paste("predictor sql", sql))
    sql <- paste0("select * from ",ngram," where word like '", word, "%' order by freq desc limit 10")
    sql
}

result <- predictor("soda")
result

