## app.R ##
suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/9-data-products/week-4"))
source("c:/dev/r-course/include.r")
install_standard_packages()
using("base64enc")
#devtools::install_github("rstudio/shiny")
using("shiny")
using("choroplethr")
using("choroplethrMaps")
using("DescTools")
using("readxl")
using("devtools")
using("ggplot2")
using("plotly")
using("BH")
using("rCharts")
using("DT")

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(scipen = 999)
set.seed(3333)

loanbook <- read.csv("./data/loan.csv")

## load the state names
data(state.regions)
# merge the loan book with the state names
loanbook <- merge(loanbook, state.regions, by.x = "addr_state", by.y = "abb")

## clean
dataDictionary <- read_excel("./data/LCDataDictionary.xlsx")
dataDictionary <- sqldf("select * from dataDictionary where LoanStatNew != 'NA'")
### fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))
### fields available in the loan book
loanbook_names <- names(loanbook)
### show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)
### fix dates
loanbook$issue_d <- as.Date(gsub("^", "01-", loanbook$issue_d), format = "%d-%b-%Y")
loanbook$grade <- as.factor(loanbook$grade)

### show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)

# snapshot data
data.amount <- loanbook %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(Amount = sum(loan_amnt))

data.loan_amount_by_grade <- sqldf("select sum(loan_amnt) as total_loan_amount, grade from [loanbook] group by grade")
#data.loan_amount_by_month_year <- sqldf("select sum(loan_amnt) as total_loan_amount, issue_d from [loanbook] group by issue_d")