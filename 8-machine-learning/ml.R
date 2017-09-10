install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris