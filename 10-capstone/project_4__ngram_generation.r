tdm_file_name <- "tdm_ngrams.RData"
tdm_ngram_filepath <- get_data_file(tdm_file_name)

gram1Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
gram2Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
gram3Tokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }

tdm1 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram1Tokenizer))
tdm2 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram2Tokenizer))
tdm3 <- TermDocumentMatrix(corpus.data, control = list(tokenize = gram3Tokenizer))

gram1freq <- data.frame(word = tdm1$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm1$i, j = tdm1$j, x = tdm1$v)))
gram1freq <- as.data.frame(sqldf("select * from gram1freq order by freq desc limit 10"))

gram2freq <- data.frame(word = tdm2$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm2$i, j = tdm2$j, x = tdm2$v)))
gram2freq <- sqldf("select * from gram2freq order by freq desc limit 10")

gram3freq <- data.frame(word = tdm3$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm3$i, j = tdm3$j, x = tdm3$v)))
gram3freq <- sqldf("select * from gram3freq order by freq desc limit 10")

save_ngrams <- function() {
    save(tdm1, tdm2, tdm3, file = files.tdm_ngrams)
    unlink(tdm_file_name)
}

save_ngrams()
