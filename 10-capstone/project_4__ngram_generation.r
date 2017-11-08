tokenize.ngram <- function(x, min.gram, max.gram) {
    NGramTokenizer(x, Weka_control(min = min.gram, max = min.gram))
    }


tdm1 <- TermDocumentMatrix(corpus.data, control = list(tokenize = tokenize.ngram (1)))
tdm2 <- TermDocumentMatrix(corpus.data, control = list(tokenize = tokenize.ngram (2)))
tdm3 <- TermDocumentMatrix(corpus.data, control = list(tokenize =  tokenize.ngram(3)))

gram1freq <- data.frame(word = tdm1$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm1$i, j = tdm1$j, x = tdm1$v)))
gram1freq <- as.data.frame(sqldf("select * from gram1freq order by freq desc limit 10"))

gram2freq <- data.frame(word = tdm2$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm2$i, j = tdm2$j, x = tdm2$v)))
gram2freq <- sqldf("select * from gram2freq order by freq desc limit 10")

gram3freq <- data.frame(word = tdm3$dimnames$Terms, freq = rowSums(sparseMatrix(i = tdm3$i, j = tdm3$j, x = tdm3$v)))
gram3freq <- sqldf("select * from gram3freq order by freq desc limit 10")
