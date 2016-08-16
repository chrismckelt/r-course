# Title: plot4.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC.coal <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
SCC.coal <- SCC[SCC.coal,]

coal.yearly.emissions <- tbl_df(NEI) %>%
        filter(NEI$SCC %in% SCC.coal$SCC) %>%
        group_by(year) %>%
        summarise(Emissions = sum(as.numeric(as.character(Emissions))))

png("plot4.png")
plot(coal.yearly.emissions, ylab = expression("Total Emissions, PM"[2.5]),
    xlab = "Year", main = "Annual Coal Combustion Emissions for the USA",
    xlim = c(1999, 2008))
polygon(coal.yearly.emissions, col = "red", border = "red")
dev.off()