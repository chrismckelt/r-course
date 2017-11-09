download_zip_files()

data.file.name <- "data.all.rds"
sample_mode <- TRUE

if (!file.exists(data.file.name))
{
    data.blogs <- read_file("data/final/en_US/en_US.blogs.txt")
    data.news <- read_file("data/final/en_US/en_US.news.txt")
    data.twitter <- read_file("data/final/en_US/en_US.twitter.txt")
    data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
    data.news = iconv(data.news, "latin1", "ASCII", sub = "")
    data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")

    if (sample_mode) {
        sample.blogs <- smaller(data.blogs)
        sample.news <- smaller(data.news)
        sample.twitter <- smaller(data.twitter)
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

    saveRDS(data.all, file = get_data_file("data.all.RData"))
    gc()
}

if (length(data.all) == 0) {
    data.all <- readRDS(get_data_file("data.all.RData"))
}