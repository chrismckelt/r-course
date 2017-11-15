#word2vec_file_name <- "word2vec.RData"
#word2vec_file_path <- get_data_file_path(word2vec_file_name)

#if (!file.exists(word2vec_file_path)) {
    #model = word2vec(train_file = "data/final/en_US/en_US.blogs.txt", output_file = "vec.bin", binary = 1)
#}
 
#load(get_data_file_path(word2vec_file_name))

#--------------------------
# skipgram or cbow methods
#--------------------------
suppressMessages(setwd("c:/dev/r-course/10-capstone/"))
require(pacman)
p_load("text2vec")

t1 <- Sys.time()

print(difftime(Sys.time(), t1, units = 'sec'))
data.stringified <- paste(data.all, collapse = '')

it <- itoken(data.stringified, preprocess_function = tolower, tokenizer = word_tokenizer, chunks_number = 10, progessbar = F)
vocab <- create_vocabulary(it, ngram = c(1L, 3L))

fh <- feature_hasher(hash_size = 2 ** 18, ngram = c(1L, 3L))

corpus <- create_hash_corpus(it, feature_hasher = fh)

print(difftime(Sys.time(), t1, units = 'sec'))