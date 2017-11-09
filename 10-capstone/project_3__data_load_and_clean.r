using("filehashSQLite")

database_name <- "sqldb_pcorpus_mydata"
database_file_path <- get_data_file(database_name, "")
db_exists <- ifelse(file.exists(database_file_path), TRUE, FALSE)
overwrite <- TRUE

if (overwrite && file.exists("sqldb_pcorpus_mydata")) {
    cat("Deleting SQL lite database")
    file.remove("sqldb_pcorpus_mydata")
}

if ((db_exists == FALSE))
{
    cat("database--> creation started...")
    data.all <- data.all
    data.all <- sent_detect_nlp(data.all)

    data.all <- tolower(data.all)
    data.all <- removeNumbers(data.all)
    data.all <- removePunctuation(data.all, preserve_intra_word_dashes = TRUE)
    data.all <- gsub("http[[:alnum:]]*", "", data.all)
    data.all <- stripWhitespace(data.all)
    data.all <- gsub("\u0092", "'", data.all)
    data.all <- gsub("\u0093|\u0094", "", data.all)
   
    dbCreate(database_name, "SQLite")
    dbInsert("data", data.all)
    corpus.data <- PCorpus(DataframeSource(data.all), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite"))
   # dbCreate(database_name, "SQLite")
    
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

    writeCorpus(corpus.data, path = ".", filenames = database_name)
    cat("database--> creation completed...")
}

db <- dbInit(database_name, "SQLite")

if (is.na(corpus.data)) {
    corpus.data <- PCorpus(DataframeSource(dbLoad(db)), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite"))
}

