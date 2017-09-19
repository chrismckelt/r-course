source("c:/dev/r-course/include.r")
using("caret")
using("lubridate")
using("pgmm")
using("rpart")
using("gbm")
using("forecast")
using("gbm")

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3 / 4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

#LASSO
lasso.training = concrete[inTrain,]
lasso.testing = concrete[-inTrain,]
set.seed(233)
lasso.fit <- enet(x = data.matrix(lasso.training[1:8]), y = lasso.training$CompressiveStrength, lambda = 1)
plot(LASSOfit, use.color = TRUE)