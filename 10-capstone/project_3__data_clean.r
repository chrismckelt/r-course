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
    data.all <- parallelize_task(sent_detect, data.all) #Detect and split sentences on endmark boundaries.
    data.all <- clean.convert_to_and(data.all)
    data.all <- clean.convert_to_period(data.all)
    data.all <- clean.remove_symbols(data.all)
    data.all <- clean.reduce_periods(data.all)
    data.all <- clean.replace_numbers(data.all)

    data.all <- removePunctuation(data.all, preserve_intra_word_dashes = TRUE)

    data.all <- rm_non_words(data.all) # Remove Non-Words & N Character Words

    data.all <- stripWhitespace(data.all)
    data.all <- tolower(data.all)
    flog.info("data cleaning complete...")

    save(data.all, file = get_data_file_path("data.all.RData"))
    flog.info("data cleaning file saved...")

    rm(remove_symbols)
    rm(reduce_periods)
    rm(convert_to_and)
    rm(convert_to_period)
    rm(replace_numbers)
    gc()
}

load(get_data_file_path("data.all.RData"))