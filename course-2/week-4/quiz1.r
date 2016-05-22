#install.packages("data.table")
#install.packages("quantmod")
packages <- c("data.table", "quantmod")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
setInternet2(TRUE)

# quiz start
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "ss06hid.csv")
download.file(url, f)
dt <- data.table(read.csv(f))
varNames <- names(dt)
varNamesSplit <- strsplit(varNames, "wgtp")
varNamesSplit[[123]]