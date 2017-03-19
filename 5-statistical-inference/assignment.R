# Title: assignment.R
# Description: https://www.coursera.org/learn/statistical-inference/peer/3k8j5/statistical-inference-course-project

####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\5-statistical-inference")
#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "downloader", "sqldf")
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
# Im old school & prefer iteration over vectorisation
for (i in 1:simulations) {
    individual_sample <- rexp(size, lambda) # specific set simulation with 40 exponentials of lambda
    samples[i, 1] <- mean(individual_sample) 
}

# compare theoretical mean vs sample mean
theoretical_mean <- 1 / lambda
sample_mean <- mean(samples[, 1])

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

