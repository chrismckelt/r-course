switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/9-data-products/pitch")) },
#  Linux  = {suppressMessages(setwd("~/"))},
       Darwin = { print("I'm a Mac.") })

if (!file.exists("./data/lending-club-loan-data.zip")) {
    download.file("https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip", "./data/lending-club-loan-data.zip")
    unzip("./data/lending-club-loan-data.zip")
}
