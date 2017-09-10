
using <- function(packageName) {
    if (!require(packageName, character.only = TRUE) && !(packageName %in% installed.packages())) {
        install.packages(dput(packageName), dependencies = TRUE, quiet = FALSE)
    }

   library(packageName, character.only = TRUE)
}

install_standard_packages <- function() {
    ## setup - install missing packages and reference
    default.packages <- c("tidyverse", "knitr", "markdown", "moments", "e1071", "data.table", "sqldf", "downloader", "magrittr")
    new.packages <- default.packages[!(default.packages %in% installed.packages()[, "Package"])]
    for (pkg in new.packages) {
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