use <- function(packageName) {
    if (!require(packageName)) {
        install.packages(dput(packageName))
        library(packageName, character.only = TRUE)
    }
}

use("sqldf")