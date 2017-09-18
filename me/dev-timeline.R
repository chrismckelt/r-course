suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/me"))
list.of.packages <- c("tidyverse", "knitr", "markdown", "moments", "data.table", "sqldf", "car", "timevis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = TRUE), require, character.only = TRUE, quietly = TRUE)

csv <- read.csv("C:/temp/stories.csv", header = TRUE)

add_days <- function(lbl) {
    
}

velocity_per_day <- 3

data <- sqldf("select WorkStream, sum(Points) as SummedPoints from csv where WorkStream <> 'BAU' and WorkStream <>'Bug' group by WorkStream ")
data$StartDate <- Sys.Date()
data$EndDate <- Sys.Date()

data <- data.frame(
  id = 1:nrow(data),
  content = data,
  start = data$StartDate,
  end = data$EndDate
)
timevis(data)