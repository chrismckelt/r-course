using("filehashSQLite")

database_name <- "sqldb_pcorpus_mydata"
database_file_path <- get_data_file_path(database_name, "")
db_exists <- ifelse(file.exists(database_file_path), TRUE, FALSE)
overwrite <- FALSE

if (overwrite && file.exists("sqldb_pcorpus_mydata")) {
    cat("Deleting SQL lite database")
    file.remove("sqldb_pcorpus_mydata")
}

if ((db_exists == FALSE))
{

    save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
    bad_words <- readLines("./bad_word_list.txt")

    flog.info("database--> creation started...")
    data.all <- as.data.frame(data.all)
    data.all <- sent_detect(data.all) #Detect and split sentences on endmark boundaries.

    data.all <- tolower(data.all)
    data.all <- removeNumbers(data.all)
    data.all <- removePunctuation(data.all, preserve_intra_word_dashes = TRUE)
    data.all <- gsub("http[[:alnum:]]*", "", data.all)
    data.all <- stripWhitespace(data.all)
    data.all <- gsub("\u0092", "'", data.all)
    data.all <- gsub("\u0093|\u0094", "", data.all)

    suppressWarnings(corpus.original <- PCorpus(VectorSource(data.all), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite")))
   # dbCreate(database_name, "SQLite")
    
    toEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = TRUE))

    corpus.original <- tm_map(corpus.original, toEmpty, "#\\w+")
    corpus.original <- tm_map(corpus.original, toEmpty, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)")
    corpus.original <- tm_map(corpus.original, toEmpty, "@\\w+")
    corpus.original <- tm_map(corpus.original, toEmpty, "http[^[:space:]]*")
    corpus.original <- tm_map(corpus.original, toSpace, "/|@|\\|")
    corpus.original <- tm_map(corpus.original, removeWords, bad_words)
    corpus.original <- clean_text(corpus.original)
   
    corpus.result <- textreg(data.all, labeling = c(TRUE), banned = bad_words, verbosity = 1, token.type = )
    corpus.stemmed <- stem.corpus(convert.tm.to.character(corpus.original))
    
   # writeCorpus(corpus.original, path = ".", filenames = database_name)
 
   # save.corpus.to.files(corpus.original)
 
    flog.info("database--> creation completed...")
}

db <- dbInit(database_name, "SQLite")

if (is.na(corpus.original)) {
    flog.info("database--> loading from disk started")
    corpus.original <- suppressWarnings(PCorpus(VectorSource(c(dbLoad(db))), readerControl = list(language = "en"), dbControl = list(dbName = database_name, dbType = "SQLite")))
    corpus.stemmed <- stem.corpus(convert.tm.to.character(corpus.original))
    flog.info("database--> loading from disk complete")
}

dbDisconnect(db)