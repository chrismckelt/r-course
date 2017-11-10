## bad words
if (!file.exists(get_data_file_path("bad_words.RData"))) {
    save_file("https://goo.gl/To9w5B", "bad_word_list.txt")
    bad_words <- readLines("./bad_word_list.txt")
    save(bad_words, file = get_data_file_path("bad_words.RData"))
}

load(get_data_file_path("bad_words.RData"))

output_file <- function() {
    return (!file.exists(get_data_file_path("data.all.RData")))  # toggle true/false whilst debugging
}

## clean data
#if (output_file)
if (TRUE)
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

    #data.all <- as.data.frame(data.all)
    data.all <- sent_detect(data.all) #Detect and split sentences on endmark boundaries.

    data.all <- convert_to_and(data.all)
    data.all <- convert_to_period(data.all)
    data.all <- remove_symbols(data.all)

    data.all <- reduce_periods(data.all)
    data.all <- replace_numbers(data.all)

    data.all <- gsub("(ftp|http)(s?)://.*\\b", "", data.all) # urls
    data.all <- gsub("\\S+@\\S+", "", data.all) # emails 
    data.all <- gsub("[@][a - zA - Z0 - 9_]{1,15}", "", data.all) # twitter usernames 
    data.all <- gsub("RT |via", "", data.all) # twitter tags 
    
    data.all <- removePunctuation(data.all, preserve_intra_word_dashes = TRUE)
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

source(get_data_file_path("data.all.RData"))