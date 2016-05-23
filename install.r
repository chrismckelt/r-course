setupPackage <- function(packageName) {
    if (!require(packageName)) {
        install.packages(dput(packageName))
        library(dput(packageName))
    }
}