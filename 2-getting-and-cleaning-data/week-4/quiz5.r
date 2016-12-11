library(quantmod)
amzn = getSymbols("AMZN", auto.assign = FALSE)
sampleTimes = index(amzn)
addmargins(table(year(sampleTimes), weekdays(sampleTimes)))



