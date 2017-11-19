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

    if (length(trim(text)) == 0) {
        warning("text NA or empty")
        stop()
    }
    
    text <- clean_data_text(text)

    if (length(trim(text)) == 0) 

    flog.debug(paste("predictor --> text.cleaned =", text))
    dt.search.result <- colnames(c("ngram", "word", "freq", "predicted"))

    search_results_exist <- FALSE
    dt.search.terms = as.data.table(str_split(text, " "), stringsAsFactors = FALSE)
    term_count <- nrow(dt.search.terms)
    if (term_count == 0) return(NULL)

    ### tweak to optimise speed
    if (term_count >= 2) { 
           term_count <- 2
    }

    #search ngrams - cycle of word count and call function to search ngram table
    data_found <- TRUE
    if (term_count > 1) {
        counter <- 1
        search_results_exist <- FALSE
        while (all((counter < term_count + 1) && data_found)) {
            ng_id <- term_count - (counter - 1) + 1
            result <- search_ngram(text, ng_id)
            if (nrow(result) > 0) {
                sql <- paste("select word, freq, word as predicted from result")
                df.row <- sqldf(sql)
                if (nrow(df.row) > 0) {
                    dt.search.result <- rbind(dt.search.result, data.frame(ngram = 1, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                    search_results_exist <- TRUE
                }
            }
            counter <- counter + 1
        }

        not_indexed <- TRUE
        #if (nrow(dt.search.result)>0) {
            #ngram1_word <- str_get_last_word(text)
            #sql <- paste0("select word from [dt.search.result] where word = '", ngram1_word, "'")
            #exists <- nrow(sqldf(sql)>0)
        #}
        
        #search unigram
        result <- search_unigram(text)
        if (all(search_results_exist) && (not_indexed)) {
            sql <- paste("select word, freq, word as predicted from result")
            df.row <- sqldf(sql)
            if (nrow(df.row)> 0 ) dt.search.result <- rbind(dt.search.result, data.frame(ngram = 1, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
        }
        else{
          #flog.debug(paste("ngram1 exists",ngram1_word))
        }

        if (search_results_exist) {
            sqldf("create index idx_freq on [dt.search.result](freq)")
            sqldf("create index idx_word on [dt.search.result](word)")

            # update column to just show predicted
            dt.search.result$predicted <- lapply(dt.search.result$predicted, function(x) unlist(str_get_last_word(x)))
            out <- strategy_stupid_back_off(dt.search.result)
            out
        } else {
            default_words
        }
        
    }
 
    else {
        flog.warn(paste("no results found for ",text))
        default_words
    }

}

predictor.benchmark  <- function(text){
    predictor(text,c())
}


# stupid back off strategy
# https://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification
strategy_stupid_back_off <- function(pred) {
    pred$ngram <- as.numeric(pred$ngram)
    pred$freq <- as.numeric(pred$freq)
    pred$word <- as.character(pred$word)
    pred$predicted <- as.character(pred$predicted)
    freq_total_5 <- sqldf("select sum(freq) from pred where ngram = 5")
    freq_total_4 <- sqldf("select sum(freq) from pred where ngram = 4")
    freq_total_3 <- sqldf("select sum(freq) from pred where ngram = 3")
    freq_total_2 <- sqldf("select sum(freq) from pred where ngram = 2")
    freq_total_1 <- sqldf("select sum(freq) from pred where ngram = 1")

    i <- nrow(pred)
    score <- 1
    while (i > 0) {

        if (pred$ngram[i] == 5) {
            score <- pred$freq[i] / freq_total_4
        } else if (pred$ngram[i] == 4) {
            score <- 0.4 * pred$freq[i] / freq_total_3
        } else if (pred$ngram[i] == 3) {
            score <- 0.4 * 0.4 * pred$freq[i] / freq_total_2
        } else if (pred$ngram[i] == 2) {
            score <- 0.4 * 0.4 * 0.4 * pred$freq[i] / freq_total_1
        }
        else if (pred$ngram[i] == 1) {
            score <- 0
        }
        pred$score[i] <- score
        i <- i - 1
    }

    if (i > 0) c(sqldf("select score from pred where ngram <> 1 order by freq desc limit 3"))
    
    default_words

}

#' Cycle down ngram functions when no data found
search_ngram <- function(text, ng_id) {
    
    search_sql <- str_get_words(text, (ng_id-1))
    if (search_sql == "") stop("search_ngram input empty")

    sql <- paste0("select * from n", ng_id)
    sql <- paste0(sql, " where word like '", search_sql, "%'")
    sql = paste(sql, " order by freq desc limit 5 ")
    flog.debug(paste("predictor --> search_ngram --> ", sql))
    df.result <- sqldf(sql)
    flog.debug(paste("predictor --> search_ngram --> rows = ", nrow(df.result)))
    df.result
}

search_unigram <- function(text) {

    search_sql <- str_get_last_word(text)
    if (search_sql == "") stop("search_unigram input empty")

    sql <- paste0("select * from n1 where word = '", search_sql, "' order by freq desc limit 5 ")
    flog.debug(paste("predictor --> search_unigram --> ", sql))
    df.result <- sqldf(sql)
    df.result
}


default_words <- c('the', 'on', 'a')

