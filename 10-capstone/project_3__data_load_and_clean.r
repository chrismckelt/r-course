using("filehashSQLite")

database_name <- "sqldb_pcorpus_mydata"
database_file_path <- paste(getwd(), database_name)


if (!file.exists(database_file_path)) {
    group_texts <- list(sample.blogs, sample.news, sample.twitter)
    group_texts <- sent_detect_nlp(group_texts)

    group_texts <- tolower(group_texts)
    group_texts <- removeNumbers(group_texts)
    group_texts <- removePunctuation(group_texts, preserve_intra_word_dashes = TRUE)
    group_texts <- gsub("http[[:alnum:]]*", "", group_texts)
    group_texts <- stripWhitespace(group_texts)
    group_texts <- gsub("\u0092", "'", group_texts)
    group_texts <- gsub("\u0093|\u0094", "", group_texts)

   # dbCreate(database_name, "SQLite")
   # db <- dbInit(database_name, "SQLite")

    corpus.data <- PCorpus(VectorSource(group_texts), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite"))
    dbCreate(database_name, "SQLite")
    db <- dbInit(database_name, "SQLite")

    toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))
    corpus.data <- tm_map(corpus.data, toEmpty, "#\\w+")
    corpus.data <- tm_map(corpus.data, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")
    corpus.data <- tm_map(corpus.data, toEmpty, "@\\w+")
    corpus.data <- tm_map(corpus.data, toEmpty, "http[^[:space:]]*")
    corpus.data <- tm_map(corpus.data, toSpace, "/|@|\\|")

    save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
    bad_words <- readLines("./bad_word_list.txt")
    corpus.data <- tm_map(corpus.data, removeWords, bad_words)

    corpus.data <- tm_map(corpus.data, stemDocument)

    writeCorpus(corpus.data,path = ".", filenames = database_name)
} else {
    db <- dbLoad(db)
    db <- dbInit(database_name, "SQLite")
    corpus.data <- PCorpus(VectorSource(group_texts), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite"))
    }




dbFetch(db, "900")
