# Title: Regression Models Course Project
## Description: https://www.coursera.org/learn/regression-models/peer/nxntd/regression-models-course-project
## https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
## Motor Trend Car Road Tests: The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).

## Question
###Take the mtcars data set and write up an analysis to answer their question using regression models and exploratory data analyses.
###Your report must be:
###Written as a PDF printout of a compiled(using knitr) R markdown document.
###Brief. Roughly the equivalent of 2 pages or less for
###the main text. Supporting figures in an appendix can be included up to 5 total pages including the 2 for
###the main report. The appendix can only include figures.
###Include a first paragraph executive summary.
rm(list = ls()) 
setwd("C:/dev/r-course/6-logistic-regression")

## setup - install missing packages and reference
list.of.packages <- c("tidyverse", "knitr", "markdown", "moments", "data.table", "sqldf", "car")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages,dependencies = TRUE)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = TRUE), require, character.only = TRUE, quietly = TRUE)

# notes
# vs variable = V engine or a straight engine. 0 means a V-engine, and 1 straight engine. 
# am variable = automatic / manual 0 = automatic, 1 = manual
data(mtcars)

#tidy
data <- sqldf("select mpg, cyl as cylinders_number, disp as displacement_inches, hp as horsepower, drat as rear_axle_ratio, wt as weight_pounds, qsec as quarter_mile_time, vs as engine_type, am as transmission_type, gear as gears_number, carb as carburetors_number from mtcars")
data <- as_tibble(data)
data$engine_type <- factor(data$engine_type, levels = c(0, 1), labels = c('v_engine', 'straight_engine'))
data$transmission_type <- factor(data$transmission_type, levels = c(0, 1), labels = c('Automatic', 'Manual'))
data$cylinders_number <- factor(data$cylinders_number)
data$carburetors_number <- factor(data$carburetors_number)

# exploratory analysis
str(data)
##  H0 --> transmission_type does not affect miles per gallon
##  H1 --> transmission_type does affect miles per gallon
## as variance is not equal Automatic/Manual --> use a Welchs t-test to determine if transmission_type impacts mpg by inspecting the p-value ( alpha = 0.05)
test <- t.test(mpg ~ transmission_type, data = data, var.equal = FALSE, paired = FALSE, conf.level = .95)
test$p.value


# initial oberservations indicate manual transmissions achieve greater miles per gallon
boxplot(mpg ~ transmission_type, data = data,
        col = c("dark green", "light green"),
        xlab = "Transmission",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type")


pairs(data, panel = panel.smooth, main = "mtcars data")

#regression analysis
# multivarible regression 

model_all <- lm(mpg ~ ., data = data)
## detect collinearity -- find the confounding variables by calculating the variance inflation factors
## We can assess the collinearity by variance inflation factor(VIF) 
## Values for the VIF that are greater than 40 are considered large. 
## We should also pay attention to VIf values over 20
## At these point we might consider leaving only one of these variables in the model.
vif(model_all)

# use AIC to find the best fitting model -->AIC provides a means for model selection.From a set of models the one with minimum AIC value wins
model_best <- step(model_all, direction = "both")
summary(model_best)$coefficients
summary(model_best)$r.squared

