suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/8-machine-learning"))
source('c:/dev/r-course/include.r')
using("randomForest")

#readme --> https://www.coursera.org/learn/practical-machine-learning/supplement/PvInj/course-project-instructions-read-first

save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")

training <- read.csv("pml-training.csv", header = TRUE)
testing <- read.csv("pml-testing.csv", header = TRUE)

#You should create a report describing how you built your model, 
#how you used cross validation
#what you think the expected out of sample error is, 
#and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases.