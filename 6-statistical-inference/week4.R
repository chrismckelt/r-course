####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\5-statistical-inference")
# quiz 4

# q1  - A pharmaceutical company is interested in testing a potential blood pressure lowering medication.
# Their first examination considers only subjects that received the medication at baseline then two weeks later. 
#The data are as follows(SBP in mmHg)

#Subject Baseline Week 2
#1 140 132
#2 138 135
#3 150 151
#4 148 146
#5 135 130
subject <- c(1, 2, 3, 4, 5)
baseline <- c(140, 138, 150, 148, 135)
week2 <- c(132, 135, 151, 146, 130)
examinations <- data.frame(subject, baseline, week2)
examinations

test <- t.test(x = examinations$baseline, y = examinations$week2, alt = "two.sided", paired = TRUE)
pval <- test$p.value
round(pval, 3)

# q2 A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. 
# What is the complete set of values of ?0 that a test of H0:?=?0 would fail to reject the null hypothesis in a two sided 5% Students t-test?
n <- 9
m <- 1100
sigma <- 30
quantile = 0.975 # is 95% with 2.5% on both sides of the range
# gossets dist with n-1 degrees of freedom
# mean + c(-1,1) * qt(quantile, df=n-1) *sigma /sqrt(n)
confidenceInterval = m + c(-1, 1) * qt(quantile, df = n - 1) * sigma / sqrt(n)
confidenceInterval

#q3 Researchers conducted a blind taste test of Coke versus Pepsi.
# Each of four people was asked which of two blinded drinks given in random order that they preferred.
# The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative,
# report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

n <- 4
x <- 3
test <- binom.test(x = x, n = n, alt = "greater")
round(test$p.value, 2)