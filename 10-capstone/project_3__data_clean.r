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
    #Create clean functions
    remove_symbols <- function(corpus) gsub(perl = TRUE,
                                        pattern = '[\\]\\[\\(\\)-/+;:#%$^\\*=^~\\{\\}/"<>«»_\\\\“\\”⁰•‘’–]',
                                        replacement = "", corpus)

    convert_to_period <- function(corpus) gsub(pattern = "[\\!\\?…]",
                                           replacement = ".", corpus)
    reduce_periods <- function(corpus) gsub(pattern = "[\\.]{2,}",
                                        replacement = ".", corpus)
    convert_to_and <- function(corpus) gsub(pattern = "&", replacement = " and ", corpus)

    replace_numbers <- function(corpus) gsub(pattern = "[0-9]+",
                                         replacement = "", corpus)

    data.all <- as.data.table(data.all, stringsAsFactors = FALSE) # stringsAsFactors = FALSE important for speed
    data.all <- parallelize_task_chunked(sent_detect, data.all) #Detect and split sentences on endmark boundaries.
    data.all <- parallelize_task(convert_to_and, data.all)
    data.all <- parallelize_task(convert_to_period, data.all)
    data.all <- parallelize_task(remove_symbols, data.all)
    data.all <- parallelize_task(reduce_periods, data.all)
    data.all <- parallelize_task(replace_numbers, data.all)
    data.all <- parallelize_task(removePunctuation, data.all, preserve_intra_word_dashes = TRUE)

    data.all <- parallelize_task(gsub, data.all, "(ftp|http)(s?)://.*\\b", "") # urls
    data.all <- parallelize_task(gsub, data.all, "\\S+@\\S+", "") # emails 
    data.all <- parallelize_task(gsub, data.all, "[@][a - zA - Z0 - 9_]{1,15}", "") # twitter usernames 
    data.all <- parallelize_task(gsub, data.all, "RT |via", "") # twitter tags 

    data.all <- parallelize_task(stripWhitespace, data.all)
    data.all <- parallelize_task(tolower, data.all)
  
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