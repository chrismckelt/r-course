####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\5-statistical-inference")
# quiz 3

# q1  - In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T confidence interval for the mean brain volume in this new population?
n=9
sd=30
m=1100
confidenceInterval <- m + c(-1, 1) * qt(p = .95 + (1 - .95) / 2, df = n - 1) * sd / sqrt(n)
confidenceInterval

#q2 - A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) is -2 pounds. What would the standard deviation of the difference in weight have to be for the upper endpoint of the 95% T confidence interval to touch 0?
n <- 9
averageDifference <- -2
quantile = 0.975 # is 95% with 2.5% on both sides of the range
ci_up = 0
sigma = (ci_up - averageDifference * sqrt(n)) / qt(quantile, df = n - 1)
round(sigma, 2)

#q3 -In an effort to improve running performance, 5 runners were either given a protein supplement or placebo. Then, after a suitable washout period, they were given the opposite treatment. Their mile times were recorded under both the treatment and placebo, yielding 10 measurements with 2 per subject. The researchers intend to use a T test and interval to investigate the treatment. Should they use a paired or independent group T test and interval?
# A paired interval

#q4 - In a study of emergency room waiting times, investigators consider a new and the standard triage systems. To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).
n_x <- 10
n_y <- 10
mwt_x <- 5
mwt_y <- 3
v_x <- .68
v_y <- .6

numerator <- ((n_x - 1) * v_x + (n_y - 1) * v_y)
denominator <- (n_x + n_y - 2)
pooled_sigma <- sqrt(numerator / denominator)
pooled_sigma

confidenceInterval <- (mwt_y - mwt_x) + c(-1, 1) * qt(p = .975, df = (n_x + n_y - 2)) * pooled_sigma * sqrt(1 / n_x + 1 / n_y)
confidenceInterval

#q5 - Suppose that you create a 95% T confidence interval. You then create a 90% interval using the same data. What can be said about the 90% interval with respect to the 95% interval?
# narrower

#q6 - To further test the hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment.
# What does the 95 % independent group confidence interval with unequal variances suggest vis a vis this hypothesis ? (Because there 's so many observations per group, just use the Z quantile instead of the T.)

nnew <- 100
nold <- 100
mnew <- 4
mold <- 6
snew <- 0.5
sold <- 2
SE <- sqrt(snew ^ 2 / 100 + sold ^ 2 / 100)
ci <- (mold - mnew) + c(-1, 1) * qnorm(0.975) * SE
ci

#q7 - Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was ?3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the four week period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, calculate the relevant *90%* t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.

quantile <- 0.95 # is 95% with 2.5% on both tails
control <- 9 #size of control group
treatment <- 9 #size of treatment group
meantreatment <- -3 #average difference from follow-up to the baseline (followup - baseline) treatment
meancontrol <- 1 #average difference from follow-up to the baseline (followup - baseline) control
SDtreatment <- 1.5
SDcontrol <- 1.8
tscore <- qt(quantile, (control + treatment - 2)) #T score quantile (.95) and df
# calculate pooled standard deviation
sp <- sqrt(((treatment - 1) * SDtreatment ^ 2 + (control - 1) * SDcontrol ^ 2) / (control + treatment - 2))
#sqrt gets from variance to sd. Then you pool the variance divided by their sample size minus one for each sample size which gives us a waited avarage.
confidenceInterval <- round(meantreatment - meancontrol + c(-1, 1) * tscore * sp * (1 / control + 1 / treatment) ^ 0.5, 3)
#round to two numbers first. The put the higher number first and subtract the lower number from it. then add and subtract. Then multiple by the t-score times the pooled standard deviation time 1 divided by sample size of each group raised to the half power. 
confidenceInterval