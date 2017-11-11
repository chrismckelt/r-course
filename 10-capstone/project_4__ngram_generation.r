ngram_file_name <- "ngrams.RData"
ngram_file_path <- get_data_file_path(ngram_file_name)

if (!file.exists(ngram_file_path)) {

    create_ngram <- function(text, ngram_size) {

        ngram <- data.table()

        ng <- textcnt(text, method = "string", n = ngram_size,lower = 10L) # freq must be 10 or greater

        if (ngram_size == 1) {
            ngram <- data.table(word = names(ng), freq = unclass(ng), length = nchar(names(ng)))
        }
        else {
            ngram <- data.table(word = names(ng), freq = unclass(ng), length = nchar(names(ng)))
        }
        
        return(ngram)
    }

    data.stringified <- paste(data.all, collapse = '')

    # setTimeLimit(360)
    # make a cluster with all possible threads (not cores)
    #cl <- makeCluster(detectCores())
    # register the number of parallel workers (here all CPUs)
    # registerDoParallel(cl)
    # return number of parallel workerss
    # flog.info(paste("registered workers: ", getDoParWorkers()))

    flog.info("starting ngram n2")
    n2 <- parallelize_task(create_ngram,data.stringified, 2)
    flog.info("starting ngram n3")
    n3 <- parallelize_task(create_ngram, data.stringified, 3)
    flog.info("starting ngram n4")
    #n4 <- create_ngram(data.stringified, 4)
    #flog.info("starting ngram n5")
    #n5 <- create_ngram(data.stringified, 5)

    #save(n2, n3, n4,n5,file = ngram_file_path)
    #unlink(ngram_file_name)
    gc()
}
 
source(get_data_file_path(ngram_file_name))