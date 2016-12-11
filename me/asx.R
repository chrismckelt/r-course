#https://www.r-bloggers.com/flipsider-support-for-asx-option-chain-data/
rm(list = ls()) # clear vars
library(devtools)
#install_github('DataWookie/flipsideR')
#install.packages("quantmod")
library(flipsideR)

WBC = getOptionChain("WBC", "ASX")
head(WBC)
plot(WBC)