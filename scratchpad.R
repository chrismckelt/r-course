switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/9-data-products/pitch")) },
#  Linux  = {suppressMessages(setwd("~/"))},
       Darwin = { print("I'm a Mac.") })

if (!file.exists("./data/lending-club-loan-data.zip")) {
    download.file("https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip", "./data/lending-club-loan-data.zip")
    unzip("./data/lending-club-loan-data.zip")
}


str <- "hell this is a test for r"
vect <- stringr::str_split(str,' ')
vect[0]
len <- nrow(vect)
vect <- unlist(vect[len-2:len])
vect
vect[1]
print(vect[1])
