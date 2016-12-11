# Title: plot6.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds") #source code classification

# filter motor
SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor,]

# filter for LA and Balitmore
# Los Angeles County, California (fips == "06037")
# Balitmore  (fips == "24510")

NEI.filtered <- tbl_df(NEI) %>%
                filter(grepl("24510|06037", fips))

NEI.filtered$fips[NEI.filtered$fips == "24510"] <- "Baltimore"
NEI.filtered$fips[NEI.filtered$fips == "06037"] <- "LA"

yearly.motor.emissions <- NEI.filtered %>%
        filter(NEI.filtered$SCC %in% SCC.motor$SCC) %>%
        group_by(year, fips) %>%
        summarise(Emissions = sum(as.numeric(as.character(Emissions))))

city <- yearly.motor.emissions$fips

g <- qplot(year, Emissions, data = yearly.motor.emissions, group = city, color = city,
    geom = c("point", "line"), ylab = expression("Total Emissions, PM"[2.5]),
    xlab = "Year", main = "LA vs Baltimore annual motor emissions" )

ggsave("plot6.png", plot = g)

