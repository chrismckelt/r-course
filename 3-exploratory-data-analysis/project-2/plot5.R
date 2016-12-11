# Title: plot5.R
# Description: https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\course-4\\project-2")

library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor,]

baltimore.yearly.motor.emissions <- tbl_df(NEI) %>%
        filter(NEI$SCC %in% SCC.motor$SCC & NEI$fips=="24510") %>% # motor vehicles in baltimore
        group_by(year) %>%
        summarise(Emissions = sum(as.numeric(as.character(Emissions))))

png("plot5.png")
plot(baltimore.yearly.motor.emissions, type="o", ylab = expression("Total Emissions, PM"[2.5]), xlab = "Year", main = "Baltimore annual motor emissions", xlim = c(1999, 2008))

dev.off()