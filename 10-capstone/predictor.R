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
predictor <- function(text, hints = c()) {

    if (is.na(text)) {
        warning("text NA or empty")
        stop()
    }
    text <- clean_data_text(text)

    flog.debug(paste("predictor --> text.cleaned =", text))
    dt.search.result <- colnames(c("ngram", "word", "freq", "length", "predicted"))
    search_results_exist <- FALSE
    dt.search.terms = as.data.table(str_split(text, " "), stringsAsFactors = FALSE)
    term_count <- nrow(dt.search.terms)

    if (term_count > 5) {
           term_count <- 4
    }

    # input text given
    counter <- 1     
    while (counter < term_count) {
        ng_id <- term_count - (counter-1) + 1
        result <- search_ngram(text, ng_id)
        if (nrow(result) > 0) {
            sql <- paste("select word, freq, length, word as predicted from result")
            df.row <- sqldf(sql)
            dt.search.result <- rbind(dt.search.result, data.frame(ngram = ng_id, word = df.row[1], freq = df.row[2], length = df.row[3], predicted = df.row[4]))
            search_results_exist <- TRUE
        }
        counter <- counter + 1
    }
    dt.search.result$predicted <- lapply(dt.search.result$predicted, function(x) unlist(str_get_last_word(x)))
    dt.search.result <- as.data.table(dt.search.result)

    ### quiz hints --> choose the most frequest
    if (!is.null(hints) || length(hints) > 1) {
       
        if (search_results_exist) {
            sql <- paste0("select * from [dt.search.result] where predicted in ('", hints[1], "' , '", hints[2], "', '", hints[3], "', '", hints[4], "') order by freq desc")
            dt.search.result$predicted <- unlist(dt.search.result$predicted)
            flog.debug(paste("predictor --> hints sql ", sql))
            dt.found <- (sqldf(sql))
            if (nrow(dt.found) > 0) {
            return(dt.found)
            }
        }
 
        flog.warn("No results found -- looking up most frequest terms from hints")
        ### use lookup to find most frequent
        matched <- text_match(data.stringified, hints)
        sql <- paste0("select term, count(term) as total from matched group by term order by total desc")
        flog.debug(paste("predictor --> hints sql ", sql))
        return(sqldf(sql))
        stop()
    }
   
    as.data.table(dt.search.result)
}



#' Cycle down ngram functions when no data found
search_ngram <- function(text, ng_id) {
    
    search_sql <- str_get_words(text, (ng_id-1))
    if (search_sql == "") stop("search_search_sql empty")

    sql <- paste0("select * from n", ng_id)
    sql <- paste0(sql, " where word like '", search_sql, "%'")

    sql = paste(sql, " order by freq desc limit 10 ")

    flog.debug(paste("predictor --> search_ngram --> sql ngram =", sql))
    df.result <- sqldf(sql)
    df.result
}

