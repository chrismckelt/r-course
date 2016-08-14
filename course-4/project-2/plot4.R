# Title: plot4.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

coal_related <- grepl("coal", SCC$Short.Name, ignore.case = TRUE) |
grepl("coal", SCC$EI.Sector, ignore.case = T)

coal_related <- SCC[coal_related,]

aggregate.coal <- tbl_df(NEI) %>%
        filter(SCC %in% coal_related$SCC) %>%
        group_by(year) %>%
        summarise(Emissions = sum(as.numeric(as.character(Emissions))))

aggregate.coal$year = as.factor(aggregate.coal$year)

png("plot4.png")
plot(aggregate.coal, type = "o", ylab = expression("Total Emissions, PM"[2.5]),
    xlab = "Year", main = "Emissions and Total Coal Combustion for the United States",
    xlim = c(1999, 2008))
polygon(aggregate.coal, col = "red", border = "red")
dev.off()