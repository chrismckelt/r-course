# Title: Fair Go Finance Velocity 
# Description: 

####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\me")

#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "downloader", "sqldf", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

df <- read.csv("time-series-data.csv", stringsAsFactors = FALSE, header = TRUE)
colnames(df)[1] <- 'Sprint_Date_string'
df$Sprint_Date <- as.Date(df$Sprint_Date_string, "%d/%m/%Y")

sprint_results <- sqldf::sqldf("select Sprint_Date, Velocity, Estimation_Inaccuracy as Estimation_Inaccuracy_Percentage,Remaining_Points from df")
last_3_sprints_start_date <- ymd(Sys.Date()) - 48
sprint_velocity <- subset(sprint_results, as.Date(Sprint_Date) > last_3_sprints_start_date)
total_velocity <- sum(sprint_velocity$Velocity)
velocity <- total_velocity / 3

ggplot(data = sprint_results, aes(Sprint_Date)) +
  geom_line(aes(y = Velocity, color = "Velocity")) +
  xlab("Sprint end date") + ylab("Velocity") + ggtitle("Dev team velocity over time")


### ------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#knit("PA1_template.Rmd", output = NULL)

rmd <- file.path(getwd(), "sprint-analysis.RMD")
knitr::opts_chunk$set(fig.width = 8, fig.height = 6, eval = TRUE, eval = TRUE, echo = TRUE, warning = FALSE, message = FALSE)
rmarkdown::render("sprint-analysis.rmd", c("html_document"))
