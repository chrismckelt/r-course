#QUESTION4
#What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group? 

data2[, mean(rankingGDP, na.rm = TRUE), by = Income.Group]

#QUESTION5
#Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. 
#How many countries are Lower middle income but among the 38 nations with highest GDP?

breaks <- quantile(data2$rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)
data2$quantileGDP <- cut(data2$rankingGDP, breaks = breaks)
data2[Income.Group == "Lower middle income", .N, by = c("Income.Group", "quantileGDP")]