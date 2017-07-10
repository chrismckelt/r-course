#---
#title:"Regression Models Course Project - Is an automatic or manual transmission better for MPG?"
#author:"Chris McKelt"
#output:pdf_document
#- --

## Executive Summary

#Motor Trend, a popular automotive magazine, has expressed an interest in understanding the relationship between miles per gallon(`mpg`) and transmission type(`am`) .
#In order to conduct an analysis in this regard, we will leverage the 1974 Motor Trend Car Road Tests, which can be found[here](https: / / stat.ethz.ch / R - manual / R - devel / library / datasets / html / mtcars.html) .
#Ultimately, Motor Trend would like to answer two questions:

 #1. Is an automatic or manual transmission better for
    #MPG ?
 #2. Quantify the MPG difference between automatic and manual transmissions ?

#This analysis was conducted as part of the[Coursera Data Science Specialization](https: / / www.coursera.org / learn / regression - models / peer / nxntd / regression - models - course - project)

# 

#`` `{r, echo=FALSE, eval=FALSE, warning=FALSE, message=FALSE}
suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/6-logistic-regression"))
list.of.packages <- c("tidyverse", "knitr", "markdown", "moments", "data.table", "sqldf", "car")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = TRUE), require, character.only = TRUE, quietly = TRUE)
#` ``

## Data import and tidy

    #- vs variable = V engine or a straight engine. 0 means a V - engine, and 1 straight engine.
    #- am variable = automatic / manual 0 = automatic, 1 = manual
#`` `{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
data(mtcars)
sql1 <- 'select mpg, cyl as cylinders_number, disp as displacement_inches, hp as horsepower, drat as rear_axle_ratio, wt as weight_pounds, '
sql2 <- 'qsec as quarter_mile_time, vs as engine_type, am as transmission_type, gear as gears_number, carb as carburetors_number from mtcars'
data <- sqldf(paste(sql1, sql2))

data <- sqldf("select mpg, cyl as cylinders_number, disp as displacement_inches, hp as horsepower, drat as rear_axle_ratio, wt as weight_pounds, qsec as quarter_mile_time, vs as engine_type, am as transmission_type, gear as gears_number, carb as carburetors_number from mtcars")
data <- as_tibble(data)
data$engine_type <- factor(data$engine_type, levels = c(0, 1), labels = c('v_engine', 'straight_engine'))
data$transmission_type <- factor(data$transmission_type, levels = c(0, 1), labels = c('Automatic', 'Manual'))
data$cylinders_number <- factor(data$cylinders_number)
data$carburetors_number <- factor(data$carburetors_number)
#` ``


## Exploratory Analysis

### Conduct a hypothesis test
##### H0 --> transmission_type does not affect miles per gallon
##### H1 --> transmission_type does affect miles per gallon

#`` `{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
## as variance is not equal Automatic/Manual --> use a Welchs t-test to determine if transmission_type impacts mpg by inspecting the p-value ( alpha = 0.05)
test <- t.test(mpg ~ transmission_type, data = data, var.equal = FALSE, paired = FALSE, conf.level = .95)
test$p.value
#` ``
### 0.137%  --> since the p-value is less than alpha (0.05) we reject the null hypothesis and can determine there is a cause / effect between transmission_type and mile per gallon


#`` `{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
boxplot(mpg ~ transmission_type, data = data,
        col = c("dark green", "light green"),
        xlab = "Transmission",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type")
#` ``

## Regression Analysis

### The first model will look at the relationship between  `mpg` and `am` (transmission_type) using 1 predictor
model_1 <- lm(mpg ~ transmission_type, data = data)
summary(model_1)$coeff
summary(model_1)$r.squared
# Interpreting the coefficients , we see an increase of 7.24 in miles per gallon when using a manual transmission over an automated transmission.
# But the R squared value is 36% indicating that this does not explain alot of variability in the data (100% indicates that the model explains all the variability of the response data around its mean)
# It appear other confounding variables affect the mpg. To detect collinearity we first build a model for all variables
# Then we can assess the collinearity by variance inflation factor(VIF)
#`` `{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
model_all <- lm(mpg ~ ., data = data)
vif(model_all)
#` ``
# VIF values that are greater than 40 are considered large and polluted.
# Any value over 20 should be scrutinised. 
# To find the best model fit
# Strategy for model selection --> choose a model by AIC in a Stepwise Algorithm 
# --> AIC provides a means for model selection. The optimum model is the one with the smallest AIC
#`` `{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}
model_best <- step(model_all, direction = "both")

# Step:  AIC=61.65
# mpg ~ cylinders_number + horsepower + weight_pounds + transmission_type

summary(model_best)$coefficients
summary(model_best)$r.squared
#87%


###Residual and Diagnostic Plots
par(mfrow = c(2, 2))
plot(model_best)
#` ``

## Conclusion




## Appendix

#Data Table Names:

#`` `{r}
names(data)
#` ``

#Data Table Summary:

#`` `{r}
summary(data)
#` ``

#Model Variance Summary:

#`` `{r}
anova(model_1, model_all, model_best)
#` ``

#Pairs Visual:

#`` `{r message=FALSE, echo=FALSE}
pairs(data, panel = panel.smooth, main = "mtcars data")
#` ``
#`` `{r,echo=FALSE}
kable(data)
#` ``