

data.blogs <- read_file("final/en_US/en_US.blogs.txt")
data.news <- read_file("final/en_US/en_US.news.txt")
data.twitter <- read_file("final/en_US/en_US.twitter.txt")
data.blogs = iconv(data.blogs, "latin1", "ASCII", sub = "")
data.news = iconv(data.news, "latin1", "ASCII", sub = "")
data.twitter = iconv(data.twitter, "latin1", "ASCII", sub = "")


sample.blogs <- smaller(data.blogs)
sample.news <- smaller(data.news)
sample.twitter <- smaller(data.twitter)
sample.all <- c(sample.blogs, sample.news, sample.twitter)


data_summary <- data.frame(
Blogs=c((length(nchar(data.blogs)))),News =c((length(nchar(data.news)))),
Twitter =c((length(nchar(data.twitter)))))
kable(data_summary)
