source("c:/dev/r-course/include.r")
using("caret")
using("lubridate")
using("pgmm")
using("rpart")
using("gbm")
using("forecast")
using("gbm")

set.seed(3433)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

#random forest
RFfit <- train(diagnosis ~ ., data = training, method = "rf")
RFpred <- predict(RFfit, testing)
accuracy.RF <- confusionMatrix(RFpred, testing$diagnosis)$overall['Accuracy']

# gbm
GBMfit <- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
GBMpred <- predict(GBMfit, testing)
accuracy.GBM <- confusionMatrix(GBMpred, testing$diagnosis)$overall['Accuracy']

# ldm
LDAfit <- train(diagnosis ~ ., data = training, method = "lda", verbose = FALSE)
LDApred <- predict(LDAfit, testing)
accuracy.LDA <- confusionMatrix(LDApred, testing$diagnosis)$overall['Accuracy']

#together
predDF <- data.frame(RFpred, LDApred, GBMpred, diagnosis = testing$diagnosis)
GAMFit <- train(diagnosis ~ ., data = predDF, method = "gam")
GAMPred <- predict(GAMFit, predDF)
accuracy.stacked <- confusionMatrix(GAMPred, testing$diagnosis)$overall['Accuracy']
