# Title: plot2.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland(fips == "24510") from 1999 to 2008 ? 
#Use the base plotting system to make a plot answering this question.

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI.24510 <- (NEI) %>% filter(fips == "24510")

baltimore <- NEI.24510 %>% group_by(year, type) %>% summarise(Emissions = sum(as.numeric(as.character(Emissions))))

png("plot2.png")
data <- with(baltimore, aggregate(Emissions, by = list(year), sum))
plot(data, type = "o", ylab = expression("Total Emissions, PM"[2.5]), xlab = "Year", main = "Total Emissions for Baltimore County", xlim = c(1999, 2008))
dev.off()