# Title: assignment.R
# Description: https://www.coursera.org/learn/statistical-inference/peer/3k8j5/statistical-inference-course-project

####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\5-statistical-inference")
#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "moments", "nortest", "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages))
    install.packages(new.packages)

sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

# Part 1 - Simulations

## simulation inputs
lambda <- 0.2 ## lambda = 0.2 for all of the simulations
size <- 40 ## 40 exponentials
simulations <- 1000 

# set a seed so we can reproduce the results
set.seed(881) # prime

# init a dataframe with column title 'mean' for recording sample mean results
samples <- data.frame(mean = numeric(size))

replicate(simulations, mean(rexp(size, lambda)))
# create a new sample for each simulation and get its mean and added to samples data frame
# iteration over vectorisation 
for (i in 1:simulations) {
    individual_sample <- rexp(size, lambda) # specific set simulation with 40 exponentials of lambda
    samples[i, 1] <- mean(individual_sample) 
}

# Show the sample mean and compare it to the theoretical mean of the distribution.
theoretical_mean <- 1 / lambda
data <- samples[, 1]
sample_mean <- mean(data)

### plot samples with theoretical mean vs sample mean
ggplot(samples, aes(x = mean)) +
    geom_histogram(bins = 20, boundary = -0.5, fill = NA, color = "black") +
    geom_density(alpha = .2, fill = "#FF6666",show.legend = FALSE) +
    geom_vline(aes(xintercept = sample_mean, color = "sample_mean", linetype = "sample_mean", show.legend = FALSE)) +
    geom_vline(aes(xintercept = theoretical_mean, color = "theoretical_mean", linetype = "theoretical_mean", show.legend = FALSE)) +
    scale_colour_manual(name = "Units", values = c(sample_mean = "red", theoretical_mean = "blue")) +
    scale_linetype_manual(name = "Units", values = c(sample_mean = "dashed", theoretical_mean = "dotted"), guide = FALSE) +
    labs(title = "Theoretical vs sample mean of 40 exponentials over 1000 samples") +
    labs(x = "Sample means", y = "Frequency") 

# Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
theoretical_sd <- (1 / lambda) / sqrt(size)
theoretical_variance <- theoretical_sd^2
sample_variance <- var(data)

# Show that the distribution is approximately normal.
# visually inspect bell curve
ggplot(samples, aes(x = mean)) +
    geom_histogram(aes(y = ..density..), bins = 20, boundary = -0.5, fill = NA, color = "black") +
    geom_density(alpha = .2, fill = "#FF6666", show.legend = FALSE) +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sqrt(sample_variance)), colour = "yellow", size = 2) +
    labs(title = "Approximation to Normality - visual inspection of bell curve") +
    labs(x = "Sample means", y = "Frequency")

# https://en.wikipedia.org/wiki/Normal_probability_plot
# nortest package to the rescue
# http://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed/52295

# Test 1 - skewness and kurtosis, they should be around (0,3)
skewness(data)
kurtosis(data)
# Test 2 - Shapiro-Wilks test
shapiro.test(data)
# Test 3 - Kolmogorov-Smirnov test
ks.test(data, "pnorm", mean(data), sqrt(var(data)))
# Test 4 - Anderson-Darling test
ad.test(data)
# Test 5 - qq-plot: you should observe a good fit of the straight line
qqnorm(data, ylab = "Sample Means of Exponentials (lambda 0.2)")
qqline(data)
# Test 6 - p-plot: you should observe a good fit of the straight line
probplot(data, qdist = qnorm)
# Test 7 - fitted normal density
f.den <- function(t) dnorm(t, mean(data), sqrt(var(data)))
curve(f.den, xlim = c(6, 14))
hist(data, prob = T, add = T)