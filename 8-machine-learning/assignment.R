
# http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/8-machine-learning"))
source('c:/dev/r-course/include.r')

using("sqldf")
using("readr")
using("caret")
using("randomForest")
using("ROCR")
using("pROC")
using("parallel")
using("doParallel")
using("dplyr")

#readme --> https://www.coursera.org/learn/practical-machine-learning/supplement/PvInj/course-project-instructions-read-first

save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
save_file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
set.seed(333)

#explore
data.training <- readr::read_csv("pml-training.csv", col_names = TRUE, col_types = colString)
data.testing <- readr::read_csv("pml-testing.csv", col_names = TRUE, col_types = colString)
data.training <- as.data.frame(data.training)
dim(data.training)
dim(data.testing)
## “classe” variable is the one we are trying to predict
levels(data.training$classe)


# clean
## remove columns lots ALL values are NA --> https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
data.training <- data.training[, colSums(is.na(data.training)) == 0]
data.testing <- data.testing[, colSums(is.na(data.testing)) == 0]


## split the original training set because original test set does not have enough observations
data.include <- createDataPartition(data.training$classe, p = .70, list = FALSE)
data.train <- data.training[data.include,]
data.test <- data.training[-data.include,]

##############################################################################################################################################################
# training - random forest
cat("random forest model started")
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
fitControl.rf <- trainControl(method = "cv",
                           number = 3,
                           allowParallel = TRUE)
timer.start <- Sys.time()
model.rf <- train(classe ~ ., data = data.train, method = "rf", trControl = fitControl.rf, verbose = FALSE, na.action = na.omit)
timer.end <- Sys.time()
stopCluster(cluster)
registerDoSEQ()
paste("random forest took: ", timer.end - timer.start, attr(timer.end - timer.start, "units"))

cat("random forest predictions")
prediction.rf <- predict(model.rf, data.test)
confusion_matrix.rf <- confusionMatrix(prediction.rf, data.test$classe)

##############################################################################################################################################################
# training - gradient boosted model
fitControl.gbm <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
cat("boosted model started")
timer.start <- Sys.time()
model.gbm <- train(classe ~ ., data = data.train, method = "gbm", trControl = fitControl.gbm, verbose = FALSE, na.action = na.omit)
timer.end <- Sys.time()
paste("boosted took: ", timer.end - timer.start, attr(timer.end - timer.start, "units"))

cat("boosted predictions")
prediction.gbm <- predict(model.gbm, data.test)
confusion_matrix.gbm <- confusionMatrix(prediction.gbm, data.test$classe)