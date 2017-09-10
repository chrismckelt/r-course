source("c:/dev/r-course/include.r")
using("caret")
using("lubridate")
using("pgmm")
using("rpart")
using("gbm")
using("forecast")
using("gbm")
using("AppliedPredictiveModeling")
using("ElemStatLearn")

 
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modRF <- train(y ~ ., data = vowel.train, method = "rf") #, trControl=trainControl("cv"), number=3)
modBoost <- train(y ~ ., data = vowel.train, method = "gbm", verbose = FALSE)

predRF <- predict(modRF, vowel.test)
predBoost <- predict(modBoost, vowel.test)
agreedIndex <- predRF == predBoost

cfmRf <- confusionMatrix(vowel.test$y, predRF)
cfmBoost <- confusionMatrix(vowel.test$y, predBoost)
cfmAgreed <- confusionMatrix(vowel.test$y[agreedIndex], predRF[agreedIndex])

cfmRf$overall["Accuracy"]