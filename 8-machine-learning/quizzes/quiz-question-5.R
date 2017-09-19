setwd("C:/dev/r-course/8-machine-learning")
source("c:/dev/r-course/include.r")
using("caret")
using("lubridate")
using("pgmm")
using("rpart")
using("gbm")
using("forecast")
using("gbm")
using("e1071")

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3 / 4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

set.seed(3523)
svm.fit <- e1071::svm(CompressiveStrength ~ ., data = training)
svm.pred <- predict(svm.fit, testing)
mean((svm.pred - testing$CompressiveStrength) ^ 2) ^ .5