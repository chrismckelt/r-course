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
   # text <- "asdf best thing asdf ddd asdad ddd "
    if (is.na(text)) {
        warning("text NA or empty")
        return (default_words)
    }

    if (length(trim(text)) == 0) {
        warning("text NA or empty")
        return (default_words)
    }
 
    text <- clean_data_text(text)

    if (length(trim(text)) == 0)  return (default_words)

    flog.debug(paste("predictor --> text.cleaned =", text))
    dt.search.result <- colnames(c("ngram", "word", "freq", "predicted"))
    dt.search.terms = as.data.table(str_split(text, " "), stringsAsFactors = FALSE)

    ### tweak to optimise speed
    term_count <- nrow(dt.search.terms)
    if (term_count >= 5) { 
           term_count <- 4
    }

    if (term_count == 0) return(NULL)

    search_results_exist <- FALSE
    #search ngrams - cycle of word count and call function to search ngram table
    search_complete <- FALSE
    if (term_count > 0) {
        counter <- 1
        search_results_exist <- FALSE

        if ((term_count >= 5) && (!search_complete)) {
            text.temp <- str_get_words(text, 4)
            sql <- paste0("select * from n5")
            sql <- paste0(sql, " where word like '", text.temp, "%'")
            sql = paste(sql, " order by freq desc limit 3 ")
            flog.debug(paste("predictor --> search_ngram --> ", sql))
            df.result <- sqldf(sql)
            flog.debug(paste("predictor --> search_ngram sql --> rows = ", nrow(df.result)))
            if (is_data_frame_valid(df.result)) {
                sql <- paste("select word, freq, word as predicted from [df.result] order by freq desc limit 3")
                df.row <- sqldf(sql)
                if (is_data_frame_valid(df.row)) {
                    dt.search.result <- rbind(dt.search.result, data.frame(ngram = 5, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                    if (!search_results_exist) {
                        sqldf("create index idx_freq on [dt.search.result](freq)")
                        sqldf("create index idx_word on [dt.search.result](word)")
                        search_results_exist <- TRUE
                    }
                }
            } else {
                print(paste("no results for ", text.temp))
            }
        }

        if ((term_count >= 4) && (!search_complete)) {
            text.temp <- str_get_words(text, 3)
            sql <- paste0("select * from n4")
            sql <- paste0(sql, " where word like '", text.temp, "%'")
            sql = paste(sql, " order by freq desc limit 3 ")
            flog.debug(paste("predictor --> search_ngram --> ", sql))
            df.result <- sqldf(sql)
            flog.debug(paste("predictor --> search_ngram sql --> rows = ", nrow(df.result)))
            if (is_data_frame_valid(df.result)) {
                sql <- paste("select word, freq, word as predicted from [df.result] order by freq desc limit 3")
                df.row <- sqldf(sql)
                if (is_data_frame_valid(df.row)) {
                    dt.search.result <- rbind(dt.search.result, data.frame(ngram = 4, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                    if (!search_results_exist) {
                        sqldf("create index idx_freq on [dt.search.result](freq)")
                        sqldf("create index idx_word on [dt.search.result](word)")
                        search_results_exist <- TRUE
                    }
                }
            } else {
                print(paste("no results for ", text.temp))
            }
        }

      #  if (all(is_data_frame_valid(dt.search.result) && nrow(dt.search.result > 2))) search_complete<-TRUE

        if ((term_count >= 3) && (!search_complete)) {
            text.temp <- str_get_words(text,2)
            sql <- paste0("select * from n3")
            sql <- paste0(sql, " where word like '", text.temp, "%'")
            sql = paste(sql, " order by freq desc limit 3 ")
            flog.debug(paste("predictor --> search_ngram --> ", sql))
            df.result <- sqldf(sql)
            flog.debug(paste("predictor --> search_ngram sql --> rows = ", nrow(df.result)))
            if (is_data_frame_valid(df.result)) {
                sql <- paste("select word, freq, word as predicted from [df.result] order by freq desc limit 3")
                df.row <- sqldf(sql)
                if (is_data_frame_valid(df.row)) {
                    dt.search.result <- rbind(dt.search.result, data.frame(ngram = 3, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                    if (!search_results_exist) {
                        sqldf("create index idx_freq on [dt.search.result](freq)")
                        sqldf("create index idx_word on [dt.search.result](word)")
                        search_results_exist <- TRUE
                    }
                }
            } else {
                print(paste("no results for ", text.temp))
            }
        }

        if (all(is_data_frame_valid(dt.search.result) && nrow(dt.search.result > 3))) search_complete <- TRUE

        if ((term_count >= 2) && (!search_complete)) {
            text.temp <- str_get_last_word(text)
            sql <- paste0("select * from n2")
            sql <- paste0(sql, " where word like '", text.temp, "%'")
            sql = paste(sql, " order by freq desc limit 3 ")
            flog.debug(paste("predictor --> search_ngram --> ", sql))
            df.result <- sqldf(sql)
            flog.debug(paste("predictor --> search_ngram sql --> rows = ", nrow(df.result)))
            if (is_data_frame_valid(df.result)) {
                sql <- paste("select word, freq, word as predicted from [df.result] order by freq desc limit 3")
                df.row <- sqldf(sql)
                if (is_data_frame_valid(df.row)) {
                    dt.search.result <- rbind(dt.search.result, data.frame(ngram = 2, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                    if (!search_results_exist) {
                        sqldf("create index idx_freq on [dt.search.result](freq)")
                        sqldf("create index idx_word on [dt.search.result](word)")
                        search_results_exist <- TRUE
                    }
                }
            } else {
                   print(paste("no results for ",text.temp))
            }
        }
        
        #search unigram
        if (all(search_results_exist)) {
            result <- search_unigram(text)
            sql <- paste("select word, freq, word as predicted from [df.result] order by freq desc limit 1")
            df.row <- sqldf(sql)
            if (nrow(df.row)>0) {
                dt.search.result <- rbind(dt.search.result, data.frame(ngram = 1, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
                #dt.fast.lookup <- rbind(dt.search.result, data.frame(ngram = 1, word = df.row[1], freq = df.row[2], predicted = df.row[3]))
            }
        }
        
        # update column to just show predicted
        dt.search.result$predicted <- lapply(dt.search.result$predicted, function(x) str_get_last_word(x))
        out <- strategy_stupid_back_off(dt.search.result)
      #  print("--------------------------------------------------------------------")
      #  print(out)
        out
    }
 
    else {
        flog.warn(paste("no results found for ",text))
        return (default_words)
        #c()
    }

}

predictor2 <- function(text, hints = c()) {
    
    if (is.na(text)) {
        warning("text NA or empty")
        stop()
    }

    if (length(trim(text)) == 0) {
        warning("text NA or empty")
        stop()
    }

    text <- clean_data_text(text)

    if (length(trim(text)) == 0) stop("no input text")

    # flog.debug(paste("predictor --> text.cleaned =", text))
    dt.search.terms = as.data.table(str_split(text, " "), stringsAsFactors = FALSE)
    dt.search.result <- colnames(c("ngram", "word", "freq", "predicted"))
    dt.search.result$ngram <- 0
    dt.search.result$word <- NA
    dt.search.result$freq <- NA
    dt.search.result$predicted <- NA

    ### tweak to optimise speed
    term_count <- nrow(dt.search.terms)
    if (term_count >= 5) {
        term_count <- 5
    }

    if (term_count == 0) return(NULL)

    if (term_count > 0) {

        dt.search.result <- get_ngram_data(1, text, dt.search.result)
        dt.search.result <- na.omit(dt.search.result)

        if (!is_data_frame_valid(dt.search.result)) default_words

        setkey(as.data.table(dt.search.result), predicted)
        
        if ((term_count >= 2)) {
            dt.search.result <- get_ngram_data(2, text, dt.search.result)
        }

        #if ((term_count >= 3)) {
            #dt.search.result <- get_ngram_data(3, text, dt.search.result)
        #}

        #if ((term_count >= 4)) {
            #dt.search.result <- get_ngram_data(4, text, dt.search.result)
        #}

        #if ((term_count >= 5)) {
            #dt.search.result <- get_ngram_data(5, text, dt.search.result)
        #}

        dt.search.result$predicted <- lapply(dt.search.result$word, function(x) str_get_last_word(x))

        if (length(dt.search.result) && is_data_frame_valid(dt.search.result) > 0) {
            # update column to just show predicted
            pred <- dt.search.result
            out <- strategy_stupid_back_off(pred)
            return (out)
        }
        flog.warn("FAILED")
        return (default_words)
    }

    else {
        flog.warn(paste("input invalid", text))
        
        return(default_words)
        #c()
    }

}


predictor.benchmark <- function(text) {
    predictor(text, c())
}

get_ngram_data <- function(ngramid, text, dt.search.result) {
    #flog.debug(paste("predictor --> get_ngram_data --> ngramid", ngramid, " text", text))

    if (ngramid == 1) {
        data <- (n1[word == str_get_last_word(text)])
        data$ngram <- 1
    }

    if (ngramid == 2){
        data <- (n2[word %like% str_get_last_word(text)])
        data$ngram <- 2
    }
     
    if (ngramid == 3) {
        data <- (n3[word %like% str_get_words(text, (ngramid - 1))])
        data$ngram <- 3
    }

    if (ngramid == 4) {
        data <- (n4[word %like% str_get_words(text, (ngramid - 1))])
        data$ngram <- 4
    }

    if (ngramid == 5) {
        data <- (n5[word %like% str_get_words(text, (ngramid - 1))])
        data$ngram <- 5
    }
    
    flog.debug(paste("predictor --> get_ngram_data --> ", ngramid, text <- ifelse(ngramid <= 2, str_get_last_word(text) , str_get_words(text, (ngramid - 1))), " found", nrow(data)))
    #print(data)
    if (is_data_frame_valid(data)) {
        dt.search.result <- na.omit(dt.search.result)
        
        dt.search.result <- head(sort(dt.search.result$freq, decreasing = TRUE), n = 3)
        df.row <- data[order(freq, decreasing = TRUE)]
        for (i in 1:nrow(df.row)) {
            dt.search.result <- rbind(dt.search.result, data.frame(ngram = df.row[i, 1], word = df.row[i, 2], freq = df.row[i, 3], predicted = ""))
        }

        dt.search.result <- dt.search.result[-1,]
        print("------------------------------------------------------------")
        
    } else {
        print(paste("no results for ", text))
    }
    
    return(dt.search.result)

}

# stupid back off strategy
# https://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification
strategy_stupid_back_off <- function(pred) {

    if (!is_data_frame_valid(pred)) {
        return (NULL)
    }
    #print(paste("strategy_stupid_back_off", pred))

    pred$ngram <- as.numeric(pred$ngram)
    pred$freq <- as.numeric(pred$freq)
    pred$word <- as.character(pred$word)
    pred$predicted <- as.character(pred$predicted)

    totals <- sqldf("select ngram, sum(freq) as total from pred group by ngram")
    
    #freq_total_5 <-  sqldf("select total from totals where ngram = 5")
    #freq_total_4 <-  sqldf("select total from totals where ngram = 4")
    freq_total_3 <-  sqldf("select total from totals where ngram = 3")
    freq_total_2 <-  sqldf("select total from totals where ngram = 2")
    freq_total_1 <-  sqldf("select total from totals where ngram = 1")
    
    i <- nrow(pred)
    score <- 1
    while (i > 0) {

        if (pred$ngram[i] == 5) {
            #score <- pred$freq[i] / freq_total_4
        } else if (pred$ngram[i] == 4) {
            #score <- 0.4 * pred$freq[i] / freq_total_3
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
    pred$score <- as.double(pred$score)
    aggregated <- sqldf("select predicted, sum(score) as total from pred group by predicted")
    result <- sqldf("select predicted from aggregated order by total desc limit 3")
    result
    #return (default_words)
   

}


#' Cycle down ngram functions when no data found
search_ngram <- function(text) {
    
    check <- strsplit(text, ' ')

    sql <- paste0("select * from n", (word_count(check)))
    if (trim(text) =='') return (NULL)
    sql <- paste0(sql, " where word like '", text, "%'")
    sql = paste(sql, " order by freq desc limit 3 ")
    flog.debug(paste("predictor --> search_ngram --> ", sql))
  
    df.result <- sqldf(sql)
    flog.debug(paste("predictor --> search_ngram sql --> rows = ", nrow(df.result)))
    df.result
}

search_unigram <- function(text) {
    sql <- paste0("select * from n1 where word = '", text, "' order by freq desc limit 3 ")
    flog.debug(paste("predictor --> search_unigram --> ", sql))
    df.result <- sqldf(sql)
    df.result
}


default_words <- c('the', 'on', 'a')
#default_words <- sqldf("select word from n1 order by freq desc limit 3")
