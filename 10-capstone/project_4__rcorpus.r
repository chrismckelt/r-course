
#--------------------------
# corpus  https://github.com/patperry/r-corpus
# winner of https://github.com/patperry/bench-term-matrix
#--------------------------
ngram_file_name <- "ngrams.RData"
ngram_file_path <- get_data_file_path(ngram_file_name)

if (!file.exists(ngram_file_path)) {

    get_word_stats <- function(text, ngram_size) {
        flog.info(paste("starting ngram size =", ngram_size))

        filt <- corpus::text_filter(stemmer = "en", drop_punct = TRUE,  drop_number = TRUE, drop = stop_words)
        ng <- term_stats(text, filt, ngrams = ngram_size, min_count = 10,,min_support = 1)

        ngram <- data.table(word = unlist(ng$term), freq = unlist(ng$count), length = unlist(ng$support))

        
        return(ngram)
    }

    n1 <- parallelize_task(get_word_stats, data.stringified, 1)

    n2 <- parallelize_task(get_word_stats, data.stringified, 2)

    n3 <- parallelize_task(get_word_stats, data.stringified, 3)

    n4 <- parallelize_task(get_word_stats, data.stringified, 4)

    n5 <- parallelize_task(get_word_stats, data.stringified, 5)

    save(n1, n2, n3, n4, n5, file = ngram_file_path)
     
    gc()
}

load(get_data_file_path(ngram_file_name))


xxx <- text_match(data.stringified, c("horticultural", "financial", "marital", "spiritual"))
yyy<- sqldf("select term, count(term) as total from xxx group by term")