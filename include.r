# use("sqldf")
use <- function(packageName) {
    if (!require(packageName)) {
        install.packages(dput(packageName))
        library(packageName, character.only = TRUE)
    }
}

install_standard_packages <- function() {
    ## setup - install missing packages and reference
    list.of.packages <- c("tidyverse", "knitr", "markdown", "moments", "e1071", "data.table", "sqldf", "downloader")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
    if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
    sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)
}

#' download and save file
save_file = function(url, name) {
    if (!file.exists(name)) {
        library(downloader)
        download(url, destfile = name)
    }
}