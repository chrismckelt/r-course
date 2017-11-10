ngram_file_name <- "ngrams.RData"
ngram_file_path <- get_data_file_path(ngram_file_name)
stop()
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

    n2 <- create_ngram(data.stringified, 2)
    n3 <- create_ngram(data.stringified, 3)
    n4 <- create_ngram(data.stringified, 4)
    n5 <- create_ngram(data.stringified, 5)

    save(n2, n3, n4,n5,file = ngram_file_path)
    #unlink(ngram_file_name)
}
 
source(get_data_file_path(ngram_file_name))