## SETUP ##
suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/9-data-products/week-4"))
source("c:/dev/r-course/include.r")
install_standard_packages()
using("base64enc")
#devtools::install_github("rstudio/shiny")
using("RSQLite")
using("shiny")
using("choroplethr")
using("choroplethrMaps")
using("DescTools")
using("readxl")
using("devtools")
using("ggplot2")
using("plotly")
devtools::install_github('rstudio/DT')
library(DT)

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(scipen = 999)
set.seed(3333)


if (!file.exists("./data/lending-club-loan-data.zip")) {
    download.file("https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip", "./data/lending-club-loan-data.zip")
    unzip("./lending-club-loan-data.zip")
}

connection_string <- data1 <- paste(getwd(), '/data/database.sqlite', sep = '')
db <- dbConnect(SQLite(), dbname = connection_string, loadable.extensions = TRUE, cache_size = NULL, synchronous = "off", flags = SQLITE_RWC, vfs = NULL)

data.tables = dbListTables(db)
data.loanbook <- sqldf("select * from loan LIMIT 100", connection = db)
dbDisconnect(db)
#loanbook <- read.csv.sql(file, sql = "select top 100 * from file", header = TRUE, sep = ",")
## load the state names
data(state.regions)
# merge the loan book with the state names
data.loanbook <- merge(data.loanbook, state.regions, by.x = "addr_state", by.y = "abb")

## clean
excel_file <- paste0(getwd(), "/data/LCdatadictionary.xlsx")
data.dictionary <- read_excel(excel_file)
data.dictionary <- sqldf("select * from [data.dictionary] where LoanStatNew != 'NA'")

### fix dates
data.loanbook$issue_d <- as.Date(gsub("^", "01-", data.loanbook$issue_d), format = "%d-%b-%Y")
data.loanbook$grade <- as.factor(data.loanbook$grade)

# snapshot data  tag_data_dictionary
view.amount <- data.loanbook %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(Amount = sum(loan_amnt))

view.loan_amount_by_grade <- sqldf("select sum(loan_amnt) as total_loan_amount, grade from [data.loanbook] group by grade")
#data.loan_amount_by_month_year <- sqldf("select sum(loan_amnt) as total_loan_amount, issue_d from [loanbook] group by issue_d")

# SHINY S

#' SERVER
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples 
server <- function(input, output, session) {

    output$data_dictionary <- renderPrint(data.dictionary)
   # session$selected.names <- renderDataTable(data_dictionary)

    output$chartLoanAmount <- renderPlotly({
    p <- plot_ly(view.loan_amount_by_grade, x = ~grade, y = ~total_loan_amount, colors = TRUE, type = 'bar',
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
      session$sendCustomMessage(type = 'getColNames',
                                message = renderPrint(data_dictionary))
    })

    #observe({
      #session$sendCustomMessage(type = 'testmessage',
                                #message = list(a = 1, b = 'text',
                                               #controller = input$controller))

     #on.exit(dbDisconnect(db), add = TRUE)
    #})
    
}


#' UI
#'
ui = htmlTemplate("www/index.html",
                    tag_about = includeMarkdown("about.md"),
                    tag_footer = h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
                    chart_loan_amount = plotlyOutput("chartLoanAmount"),
                    tag_data_dictionary = verbatimTextOutput("data_dictionary")
                )

shinyApp(ui, server)

