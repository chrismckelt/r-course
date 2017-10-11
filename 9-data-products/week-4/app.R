## SETUP ##
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

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(scipen = 999)
set.seed(3333)

loanbook <- read.csv("./data/loan.csv")
#loanbook <- read.csv.sql(file, sql = "select top 100 * from file", header = TRUE, sep = ",")
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



## SHINY START

server <- function(input, output, session) {
   
    output$chartLoanAmount <- renderPlotly({
        p <- plot_ly(data.loan_amount_by_grade, x = ~grade, y = ~total_loan_amount, colors = TRUE, type = 'bar',
        marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
        layout(title = "Loan amounts by credit grade quality",
              autosize = F, 
              width = 500, 
              height = 350,
              xaxis = list(title = "Credit grade quality"),
              yaxis = list(title = "Total loan amount")
            )
        return(p)
        })
    
    
    observeEvent(input$do, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
    })

}

ui = htmlTemplate("www/index.html",
                    aboutTag = includeMarkdown("about.md"),
                    footerTag = h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
                    chartLoanAmount = plotlyOutput("chartLoanAmount")
                )

shinyApp(ui, server)