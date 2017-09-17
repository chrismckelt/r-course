
# http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/8-machine-learning"))
source('c:/dev/r-course/include.r')
using("sqldf")
using("caret")
using("randomForest")
using("pROC")
using("xgboost")

#readme --> https://www.coursera.org/learn/practical-machine-learning/supplement/PvInj/course-project-instructions-read-first

save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
set.seed(333)

#explore
training <- read.csv("pml-training.csv", header = TRUE)
testing <- read.csv("pml-testing.csv", header = TRUE)
#training <- training[, order(names(training))]
#testing <- testing[, order(names(testing))]
dim(training)
dim(testing)
## “classe” variable is the one we are trying to predict
levels(training$classe)
training$classe = as.factor(training$classe)

# clean
## remove columns where ALL values are NA --> https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]
classe <- training$classe

# training 
## split the original training set because original test set does not have enough observations
data.include <- createDataPartition(training$classe, p = .70, list = FALSE)
data.train <- training[data.include,]
data.test <- training[-data.include,]

##todo remove highly correlated vars
model.rf <- train(classe ~ ., data = data.train, method = "rf", trControl = trainControl(method = "cv", number = 5), verbose = F, na.action = na.pass)

cat("a")
 

