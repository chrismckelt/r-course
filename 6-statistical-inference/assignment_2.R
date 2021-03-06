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
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "moments", "nortest", "e1071", "data.table","sqldf", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages))
    install.packages(new.packages)

sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

## utils

test_reject <- function(reject_what, tresult) {
    alpha <- .05
    if (tresult$p.value > alpha) {
        #If the p-value is less than or equal to the chosen significance level (?), 
        #the test suggests that the observed data is inconsistent with the null hypothesis, 
        #so the null hypothesis must be rejected. However, that does not prove that the tested hypothesis is true
        msg <- paste("p.value is ", tresult$p.value, " which is greater than alpha (.05) so we reject H0")
        print(msg)
        print(paste("REJECT NULL HYPOTHESIS: ", reject_what))
    } else {
        print(paste("DO NOT REJECT NULL HYPOTHESIS: ", reject_what))
    }
}

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

# split the data into 2 groups of tests according to supplement type
team_oj <- sqldf("select tooth_length, supplement, dose from tbl where supplement = 'OJ'")
team_vc <- sqldf("select tooth_length, supplement, dose from tbl where supplement = 'VC'")

options(digits = 2)
overview <- rbind(team_oj, team_vc)
overview$dose <- as.numeric(overview$dose)

# visual to compare tooth growth by supplement type and dose
ggplot(overview, aes(x = dose, y = tooth_length)) +
  geom_smooth(data = subset(overview, supplement == "OJ"), aes(colour = "OJ"), linetype = "solid", size = 1) +
  geom_smooth(data = subset(overview, supplement == "VC"), aes(colour = "VC"), linetype = "solid", size = 1) +
  scale_color_manual("Legend", values = c("red", "blue")) +
  labs(title = "Tooth growth by supplement dose") +
  labs(x = "dose", y = "Tooth growth")

# output stats
stat.desc(team_oj)
stat.desc(team_vc)

# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
# (Only use the techniques from class, even if there's other approaches worth considering)


# Supplements
### H0 supplements have no affect on tooth growth
### HA supplements will affect tooth growth
tresult_supplement <- t.test(tooth_length ~ supplement, data = overview, var.equal = FALSE, paired = FALSE, conf.level = .95)
test_reject("H0 supplements have no affect on tooth growth", tresult_supplement)

# Dose
# need to break up the dose into 3 groups
dose_small <- subset(overview, dose == 0.5)
dose_medium <- subset(overview, dose == 1.0)
dose_high <- subset(overview, dose == 2.0)

### small dose
### H0 small supplemented dose .05 mg has no affect on tooth growth
### HA small supplemented dose .05 mg will affect tooth growth
tresult_dose_small <- t.test(tooth_length ~ supplement, data = dose_small, var.equal = FALSE, paired = FALSE, conf.level = .95)
test_reject("H0 supplemented small dose of .05 mg has no affect on tooth growth", tresult_dose_small)
tresult_dose_small$conf.int[1:2]

### medium dose
### H0 medium supplemented dose of 1.0 mg has no affect on tooth growth
### HA medium supplemented dose of 1.0 mg will affect tooth growth

tresult_dose_medium <- t.test(tooth_length ~ supplement, data = dose_medium, var.equal = FALSE, paired = FALSE, conf.level = .95)
test_reject("H0 medium supplemented  dose of 1.0 mg has no affect on tooth growth", tresult_dose_medium)

### high  dose
### H0 high  supplemented dose of 2.0 mg has no affect on tooth growth
### HA high supplemented dose of 2.0 mg will affect tooth growth

tresult_dose_high <- t.test(tooth_length ~ supplement, data = dose_high, var.equal = FALSE, paired = FALSE, conf.level = .95)
test_reject("H0 high supplemented dose of 2.0 mg has no affect on tooth growth", tresult_dose_high)



