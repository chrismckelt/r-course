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

