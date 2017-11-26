## bad words
if (!file.exists(get_data_file_path("bad_words.RData"))) {
    save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
    bad_words <- readLines("./bad_word_list.txt")
    save(bad_words, file = get_data_file_path("bad_words.RData"))
}

load(get_data_file_path("bad_words.RData"))

## clean data
if ((!file.exists(get_data_file_path("data.all.RData"))))
{
    flog.info("data cleaning...")
    
    data.all <- as.data.table(data.all, stringsAsFactors = FALSE) # stringsAsFactors = FALSE important for speed
   # data.all <- parallelize_task(sent_detect, data.all) #Detect and split sentences on endmark boundaries.

    data.all <- data.frame(lapply(data.all, function(x) (as.character(clean_data_text(x)))))
    
    flog.info("data cleaning complete...")

    save(data.all, file = get_data_file_path("data.all.RData"))
    flog.info("data cleaning file saved...")

}

load(get_data_file_path("data.all.RData"))


if ((!file.exists(get_data_file_path("data.stringified.RData")))) {
    data.stringified <- paste(data.all, collapse = '')
    data.stringified <- clean_data_text(data.stringified)
    save(data.stringified, file = get_data_file_path("data.stringified.RData"))
}

load(get_data_file_path("data.stringified.RData"))