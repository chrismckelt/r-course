# Title: plot1.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008. 
# Upload a PNG file containing your plot addressing this question.

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png("plot1.png")
data <- with(NEI, aggregate(Emissions, by = list(year), sum))
plot(data, type = "o", ylab = expression("Total Emissions, PM"[2.5]), xlab = "Year", main = "Total Emissions in the United States")
polygon(data, col = "green", border = "blue")
dev.off()