# Title: Part 2: Basic Inferential Data Analysis
# Description: https://www.coursera.org/learn/statistical-inference/peer/3k8j5/statistical-inference-course-project
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
# ToothGrowth: The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. 
#Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, (orange juice or ascorbic acid (a form of vitamin C and coded as VC).


####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\5-statistical-inference")
#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "moments", "nortest", "e1071", "data.table","sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages))
    install.packages(new.packages)

sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

# Part 2 - Tooth Growth

# Load the ToothGrowth data and perform some basic exploratory data analyses
library(datasets)
data(ToothGrowth)

#cleanup
tbl <- data.table(ToothGrowth)
setnames(tbl, c('len', 'supp', 'dose'), c('tooth_length', 'supplement', 'dose'))

# Provide a basic summary of the data.
# Exploratory Data Analysis 
summary(tbl)
head(tbl)
table(tbl$supplement, tbl$dose)

team_oj <- sqldf("select tooth_length, supplement, dose from tbl where supplement = 'OJ'")
team_vc <- sqldf("select tooth_length, supplement, dose from tbl where supplement = 'VC'")

team_oj_total <- sqldf("select tooth_length, supplement, count(supplement) as total_supplement, sum(dose) as total_dose from team_oj group by tooth_length,supplement order by tooth_length")
team_vc_total <- sqldf("select tooth_length, supplement,count(supplement) as total_supplement, sum(dose) as total_dose from team_vc group by tooth_length,supplement order by tooth_length")

overview <- rbind(team_oj_total, team_vc_total)

ggplot(overview, aes(x = total_dose, y = tooth_length)) +
  geom_point() +

  geom_line(data = subset(overview, supplement == "OJ"), colour = "red", linetype = "solid", size = 1) +
  geom_line(data = subset(overview, supplement == "VC"), colour = "blue", linetype = "solid", size = 1) +
  labs(title = "Tooth growth by supplement dose") +
  labs(x = "Total dose", y = "Tooth growth")

# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
# (Only use the techniques from class, even if there's other approaches worth considering)

## visualise each set
#ggplot(overview, aes(x = tooth_length)) +
    #geom_histogram(aes(y = ..density..), bins = 20, boundary = -0.5, fill = NA, color = "black") +
    #geom_density(alpha = .2, fill = "#FF6666", show.legend = FALSE) +
    #stat_function(fun = dnorm, args = list(mean = mean(overview$tooth_length), sd = sqrt(sample_variance)), colour = "yellow", size = 2) +
    #labs(title = "Approximation to Normality - visual inspection of bell curve") +
    #labs(x = "Sample means", y = "Frequency")

