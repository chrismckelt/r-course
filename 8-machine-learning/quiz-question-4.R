setwd("C:/dev/r-course/8-machine-learning")
source("c:/dev/r-course/include.r")
using("caret")
using("lubridate")
using("pgmm")
using("rpart")
using("gbm")
using("forecast")
using("gbm")

save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", "gaData.csv")

library(lubridate) # For year() function below

dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# BATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
tsfit <- bats(tstrain)
tsforecast <- forecast(tsfit, h = nrow(testing))
results <- as.data.frame(tsforecast$lower[, 2])
results[, 2] <- as.data.frame(tsforecast$upper[, 2])
results[, 3] <- testing$visitsTumblr
sum((results[3] > results[1]) & (results[3] < results[2])) / nrow(results)