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

library(ElemStatLearn)

data(vowel.train)
data(vowel.test)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

# random forest
random_forest_fit <- train(y ~ ., data = vowel.train, method = "rf")
random_forest_prediction <- predict(random_forest_fit, vowel.test)
random_forest_accuracy <- confusionMatrix(random_forest_prediction, vowel.test$y)$overall['Accuracy']

boosted_fit <- train(y ~ ., data = vowel.train, method = "gbm", verbose = FALSE)
boosted_predictor <- predict(boosted_fit, vowel.test)
boosted_accuracy <- confusionMatrix(boosted_predictor, vowel.test$y)$overall['Accuracy']

aa <- random_forest_prediction == boosted_predictor
bb <- vowel.test$y == random_forest_prediction
accuracy <- sum(aa * bb) / sum(aa)
 