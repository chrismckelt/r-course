# Title: analysis.R
# Description: https://www.coursera.org/learn/reproducible-research/peer/OMZ37/course-project-2

####setup
rm(list = ls()) # clear vars
setwd("C:\\dev\\r-course\\4-reproducible-research\\project-2")

#------
####install missing packages and reference
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "knitr", "markdown", "downloader", "sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages)
sapply(sapply(list.of.packages, library, character.only = TRUE, quietly = FALSE), require, character.only = TRUE, quietly = FALSE)

#load and read the data
if (!file.exists("StormData.csv.bz2")) {
    library(downloader)
    download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "StormData.csv.bz2")
}

df <- read.csv("Stormdata.csv.bz2", stringsAsFactors = FALSE, header = TRUE)

###Question: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

# select the data related with "population health" and "economic consequences",
df.indicators <- sqldf(" select STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP from df")
df.indicators$EVTYPE <- as.factor(df.indicators$EVTYPE)
#df.indicators$BGN_DATE <- as.POSIXct(df.indicators$BGN_DATE, format = "%Y-%m-%d %H:%M:%S")
df.indicators$PROPDMGEXP <- as.factor(df.indicators$PROPDMGEXP)
df.indicators$CROPDMGEXP <- as.factor(df.indicators$CROPDMGEXP)

# Display the top 10 fatal events types 
df.event_fatalities_summary <- sqldf("select EVTYPE as Event_Type, sum(FATALITIES) as Total_Fatalities from [df.indicators] group by EVTYPE order by Total_Fatalities desc limit 10")

ggplot(df.event_fatalities_summary, aes(x = Event_Type, y = Total_Fatalities)) +
    geom_bar(stat = "identity", fill = "blue", position = "stack") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Event_Type") + ylab("Total_Injuries") + ggtitle("Number of fatalities by top 10 Weather Events")

# To calculate the property and crop costs - use the relative column to calculate the cost
unique.property.exp <- unique(df.indicators$PROPDMGEXP)
unique.crop.exp <- unique(df.indicators$CROPDMGEXP)

# For property damage - convert H, K, M, B units  
df.indicators$PROPDMGCALC = 0
df.indicators[df.indicators$PROPDMGEXP == "H",]$PROPDMGCALC = df.indicators[df.indicators$PROPDMGEXP == "H",]$PROPDMG * 10 ^ 2
df.indicators[df.indicators$PROPDMGEXP == "K",]$PROPDMGCALC = df.indicators[df.indicators$PROPDMGEXP == "K",]$PROPDMG * 10 ^ 3
df.indicators[df.indicators$PROPDMGEXP == "M",]$PROPDMGCALC = df.indicators[df.indicators$PROPDMGEXP == "M",]$PROPDMG * 10 ^ 6
df.indicators[df.indicators$PROPDMGEXP == "B",]$PROPDMGCALC = df.indicators[df.indicators$PROPDMGEXP == "B",]$PROPDMG * 10 ^ 9

# For crop damage - convert H, K, M, B units to calculate Crop Damage
df.indicators$CROPDMGCALC = 0
df.indicators[df.indicators$CROPDMGEXP == "H",]$CROPDMGCALC = df.indicators[df.indicators$CROPDMGEXP == "H",]$CROPDMG * 10 ^ 2
df.indicators[df.indicators$CROPDMGEXP == "K",]$CROPDMGCALC = df.indicators[df.indicators$CROPDMGEXP == "K",]$CROPDMG * 10 ^ 3
df.indicators[df.indicators$CROPDMGEXP == "M",]$CROPDMGCALC = df.indicators[df.indicators$CROPDMGEXP == "M",]$CROPDMG * 10 ^ 6
df.indicators[df.indicators$CROPDMGEXP == "B",]$CROPDMGCALC = df.indicators[df.indicators$CROPDMGEXP == "B",]$CROPDMG * 10 ^ 9

# Sum the property and crop total calculated cost into a new Total Damage Cost field and display the top 10
df.total_damage_costs <- sqldf("select EvType as Event_Type, sum(PROPDMGCALC) + sum(CROPDMGCALC) as Total_Damage_Cost from [df.indicators] group by Event_Type order by Total_Damage_Cost desc limit 10")
ggplot(df.total_damage_costs, aes(x = Event_Type, y = Total_Damage_Cost)) +
    geom_bar(stat = "identity", fill = "red", position = "stack") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
xlab("Event Type") + ylab("Total Damage Cost (USD $)") + ggtitle("Total cost of property and crop damage by top 10 Weather Events")


 
