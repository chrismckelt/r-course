# Title: plot6.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI.24510 <- (NEI) %>% filter(fips == "24510")

baltimore <- NEI.24510 %>% group_by(year, type) %>% summarise(Emissions = sum(as.numeric(as.character(Emissions))))

g <- qplot(year, Emissions, data = baltimore, group = baltimore$type, color = baltimore$type,
    geom = c("point", "line"), ylab = expression("Total Emissions, PM"[2.5]),
    xlab = "Year", main = "Total Emissions in U.S. by Type of Pollutant")

ggsave("plot3.png", plot = g)