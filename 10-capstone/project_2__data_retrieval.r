download_zip_files()

data.file.name <- "data.all.RData"

sample_mode <- TRUE

# We will use the Snowball English stop word list:
stop_words <- corpus::stopwords_en

if (!file.exists(data.file.name))
{
    data.blogs <- read_file("data/final/en_US/en_US.blogs.txt")
    data.news <- read_file("data/final/en_US/en_US.news.txt")
    data.twitter <- read_file("data/final/en_US/en_US.twitter.txt")
    data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
    data.news = iconv(data.news, "latin1", "ASCII", sub = "")
    data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")

    if (sample_mode) {
        sample.blogs <- small_sample(data.blogs, .1)
        sample.news <- small_sample(data.news, .1)
        sample.twitter <- small_sample(data.twitter, .1)
        data.all <- c(sample.blogs, sample.news, sample.twitter)
    }
    else {
        data.all <- c(data.blogs, data.news, data.twitter)
    }

    # data_summary <- data.frame( Blogs=c((length(nchar(data.blogs)))),News
    # =c((length(nchar(data.news)))), Twitter
    # =c((length(nchar(data.twitter))))) kable(data_summary)

    rm(sample.blogs)
    rm(sample.news)
    rm(sample.twitter)

    rm(data.blogs)
    rm(data.news)
    rm(data.twitter)

    save(data.all, file = get_data_file_path("data.all.RData"))
    gc()
}

if (length(data.all) == 0) {
    data.all <- source(get_data_file_path("data.all.RData"))
}

