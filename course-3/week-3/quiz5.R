breaks <- quantile(dt$rankingGDP, probs=seq(0, 1, 0.2), na.rm=TRUE)
dt$quantileGDP <- cut(dt$rankingGDP, breaks=breaks)
dt[Income.Group == "Lower middle income", .N, by=c("Income.Group", "quantileGDP")]