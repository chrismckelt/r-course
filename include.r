
using <- function(packageName) {
    if (!require(packageName, character.only = TRUE) && !(packageName %in% installed.packages())) {
        install.packages(dput(packageName), dependencies = TRUE, quiet = FALSE)
    }

   library(packageName, character.only = TRUE)
}


install_standard_packages <- function() {
    ## setup - install missing packages and reference
    default.packages <- c("tidyverse", "knitr", "markdown", "moments", "e1071", "data.table", "sqldf", "downloader", "magrittr", "readr", "foreach")
    #new.packages <- default.packages[!(default.packages %in% installed.packages()[, "Package"])]
    for (pkg in default.packages) {
           using(pkg)
    }
    #if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
    #sapply(sapply(default.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)
}

install_standard_packages()

#' download and save file
save_file = function(url, name) {
    if (!file.exists(name)) {
        library(downloader)
        download(url, destfile = name)
    }
}

string40 <- "ncnnccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
string80 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
string120 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
string160 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnc"
colString <- paste(string40, string80, string120, string160, sep = "")
