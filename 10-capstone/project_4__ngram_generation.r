ngram_file_name <- "ngrams.RData"
ngram_file_path <- get_data_file_path(ngram_file_name)

if (!file.exists(ngram_file_path)) {

    create_ngram <- function(text, ngram_size) {
        flog.info(paste("starting ngram size =",ngram_size))
        ngram <- data.table()

        ng <- textcnt(text, method = "string", n = ngram_size,lower = 10L,verbose = TRUE) # freq must be 10 or greater

        ngram <- data.table(word = names(ng), freq = unclass(ng), length = nchar(names(ng)))
        
        return(ngram)
    }

    data.stringified <- paste(data.all, collapse = '')

    n1 <- parallelize_task(create_ngram, data.stringified, 1)

    n2 <- parallelize_task(create_ngram, data.stringified, 2)
 
    n3 <- parallelize_task(create_ngram, data.stringified, 3)

    n4 <- parallelize_task(create_ngram, data.stringified, 4)

    n5 <- parallelize_task(create_ngram, data.stringified, 5)

    save(n1,n2, n3, n4, n5, file = ngram_file_path)
    rm(data.stringified)
    gc()
}
 
load(get_data_file_path(ngram_file_name))