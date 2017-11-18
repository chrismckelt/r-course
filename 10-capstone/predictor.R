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
predictor <- function(sentence, hints = c()) {

    if (is.na(sentence)) {
        warning("sentence NA or empty")
        stop()
    }
    sentence <- clean.text(sentence)

    flog.debug(paste("predictor --> sentence.cleaned =", sentence))
    dt.search.terms = as.data.table(str_split(sentence, " "), stringsAsFactors = FALSE)
    term_count <- nrow(dt.search.terms)

    if (term_count > 5) {
           term_count <- 5
    }

    dt.search.result <- colnames(c("ngram", "word", "freq", "length", "predicted"))
    search_results_exist <- FALSE
    # input text given
    counter <-1    
    while (counter < term_count) {
        ng_id <- term_count - (counter)+1
        result <- search_ngram(dt.search.terms[counter:term_count], ng_id, hints)
        if (nrow(result) > 0) {
            flog.debug(paste("predictor --> ngram ", ng_id, "found", nrow(result)))
            sql <- paste("select word, freq, length, word as predicted from result")
           
            df.row <- sqldf(sql)
            
            dt.search.result <- rbind(dt.search.result, data.frame(ngram = ng_id, word = df.row[1], freq = df.row[2], length = df.row[3], predicted = df.row[4]))
            search_results_exist <- TRUE
        }
        counter <- counter + 1
    }
    dt.search.result$predicted <- lapply(dt.search.result$predicted, function(x) unlist(str_get_last_word(x)))

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
        sql <- paste0("select * from n1 where word in ('", hints[1], "' , '", hints[2], "', '", hints[3], "', '", hints[4], "') order by freq desc")
        flog.debug(paste("predictor --> hints sql ", sql))
        return(sqldf(sql))
        stop()
    }
   
    dt.search.result
}



#' Cycle down ngram functions when no data found
search_ngram <- function(search_terms, take, hints = c()) {
    
    #term_count <- 3
    flog.debug(paste("search_terms count", nrow(search_terms)))
    arg <- paste(search_terms$V1, sep = " ", collapse = " ")
    arg <- str_trim(arg)
    if (arg=="") stop("arg empty")
    sql <- paste0("select * from n", take)
    sql <- paste0(sql, " where word like '", arg, "%'")

    #if (!is.null(hints) && length(hints) > 1) {
        #sql = paste(sql, "or (word like ")
        #hints.length = length(hints)
        #while (hints.length > 0) {
            #sql = paste0(sql, "'%", hints[hints.length], "%'")
            #if (hints.length > 1) {
                #sql = paste(sql, "or word like ")
            #}
            #hints.length = hints.length - 1
        #}
        #sql = paste(sql, ") ")
    #}


    sql = paste(sql, " order by freq desc limit 10 ")

    flog.debug(paste("predictor --> search_ngram --> sql ngram =", sql))
    df.result <- sqldf(sql)
    df.result
}

search <- "a few"
search <- clean.text(search)
dt.search.terms = as.data.table(str_split(search, " "), stringsAsFactors = FALSE)
result <- predictor(search)

result

