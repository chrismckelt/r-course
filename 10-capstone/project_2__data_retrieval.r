download_zip_files()

data.file.name <- "data.all.RData"

sample_mode <- TRUE

if (!file.exists(data.file.name))
    {
    data.blogs <- read_file("data/final/en_US/en_US.blogs.txt")
    data.twitter <- read_file("data/final/en_US/en_US.twitter.txt")
   # data.blogs <- read_file("data/final/en_US/blogs.txt")
  #  data.twitter <- read_file("data/final/en_US/tweets.txt")
    data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
    data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")

    if (sample_mode) {
        sample.blogs <- small_sample(data.blogs, .03)
        #sample.news <- small_sample(data.news, .1)
        sample.twitter <- small_sample(data.twitter, .01)
        data.all <- c(sample.blogs, sample.twitter)
    }
    else {
        data.all <- c(data.blogs, data.twitter)
    #    data.all <- c(data.blogs, data.twitter, data.blogs2, data.twitter2)
    }

    # data_summary <- data.frame( Blogs=c((length(nchar(data.blogs)))),News
    # =c((length(nchar(data.news)))), Twitter
    # =c((length(nchar(data.twitter))))) kable(data_summary)
    if (sample_mode) {
        rm(sample.blogs)
        #rm(sample.news)
        rm(sample.twitter)
    }

    rm(data.blogs)
    #rm(data.news)
    rm(data.twitter)

    save(data.all, file = get_data_file_path("data.all.RData"))
    gc()
}

if (length(data.all) == 0) {
    data.all <- source(get_data_file_path("data.all.RData"))
}

