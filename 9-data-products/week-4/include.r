

using <- function(packageName) {
    if (!require(packageName, character.only = TRUE) && !(packageName %in% installed.packages())) {
        install.packages(dput(packageName), dependencies = TRUE, quiet = FALSE)
    }

   library(packageName, character.only = TRUE)
}

require(pacman)

install_standard_packages <- function() {
    ## setup - install missing packages and reference
    require(tidyverse)
    require(knitr)
    require(markdown)
    require(moments)
    require(e1071)
    require(data.table)
    require(sqldf)
    require(downloader)
    require(magrittr)
    require(ggplot2)
    require(lubridate)
    require(foreach)   
}

#' download and save file
save_file = function(url, name) {
    if (!file.exists(name)) {
        library(downloader)
        download(url, destfile = name)
    }
}

colString <- function() {
    string40 <- "ncnnccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
    string80 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
    string120 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
    string160 <- "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnc"
    colString <- paste(string40, string80, string120, string160, sep = "")
}

