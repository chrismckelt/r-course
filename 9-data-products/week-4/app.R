source("c:/dev/r-course/include.r")
install_standard_packages()
source("./global.R")
using("RSQLite")
using("shiny")
using("shinyjs")
using("choroplethr")
using("choroplethrMaps")
using("DescTools")
using("readxl")
using("devtools")
using("ggplot2")
using("plotly")
using("DT")

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
options(scipen = 999)
set.seed(3333)


if (!file.exists("./data/lending-club-loan-data.zip")) {
    download.file("https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip", "./data/lending-club-loan-data.zip")
    unzip("./lending-club-loan-data.zip")
}

connection_string <- paste(getwd(), '/data/database.sqlite', sep = '')
db <- RSQLite::dbConnect(SQLite(), dbname = connection_string, loadable.extensions = TRUE, cache_size = NULL, synchronous = "off", flags = SQLITE_RWC, vfs = NULL)

data.tables = dbListTables(db)
data_loanbook <- sqldf("select * from loan LIMIT 100", connection = db)

#loanbook <- read.csv.sql(file, sql = "select top 100 * from file", header = TRUE, sep = ",")
## load the state names
data(state.regions)
# merge the loan book with the state names
data_loanbook <- merge(data_loanbook, state.regions, by.x = "addr_state", by.y = "abb")

## clean
excel_file <- paste0(getwd(), "/data/LCdatadictionary.xlsx")
data_dictionary <- read_excel(excel_file)
data_dictionary <- sqldf("select * from [data_dictionary] where LoanStatNew != 'NA'")

### fix dates
data_loanbook$issue_d <- as.Date(gsub("^", "01-", data_loanbook$issue_d), format = "%d-%b-%Y")
data_loanbook$grade <- as.factor(data_loanbook$grade)

selected_query_by <- "grade"
query_for <- function(query_by) {
    sql <- paste("select sum(loan_amnt) as total_loan_amount, ", query_by, " from [data_loanbook] group by ", query_by)
    query <- sqldf(sql)
}

#stop("stopping")

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

    #on.exit(dbDisconnect(db), add = TRUE)

    query_choosen <- reactiveValues()
    query_choosen <- selected_query_by

    output$data_dictionary <- renderDataTable(data_dictionary, options = list(pageLength = 25, server = TRUE, selection = list(mode = "single", target = "cell")))
    observe({
        print(input$data_dictionary_rows_selected)
        query_choosen <- "emp_length"# data_dictionary[input$data_dictionary_rows_selected[0]]
    })


    #output$chartLoanAmount <- reactive({
        #renderPlotly({
            #p <- plot_ly(query_for(selected_query_by), x = ~grade, y = ~total_loan_amount, colors = TRUE, type = 'bar',
                        #marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        #layout(title = paste("Loan amounts by ", selected_query_by),
                              #autosize = F,
                              #width = 500,
                              #height = 350,
                              #xaxis = list(title = "Credit grade quality"),
                              #yaxis = list(title = "Total loan amount")
                            #)
            #return(p)
        #})
    #})

    output$chartLoanAmount <-  renderPlotly({
                    p <- plot_ly(query_for(query_choosen), x = ~grade, y = ~total_loan_amount, colors = TRUE, type = 'bar',
                                    marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                                    layout(title = paste("Loan amounts by ", query_choosen),
                                            autosize = F,
                                            width = 500,
                                            height = 350,
                                            xaxis = list(title = "Credit grade quality"),
                                            yaxis = list(title = "Total loan amount")
                                        )
                    return(p)
                })

    observeEvent(input$stop, {
        shiny::stopApp()
        #RSQLite::dbDisconnect()
    })

    observeEvent(input$do, {
        session$sendCustomMessage(type = 'getColNames',
                                message = renderPrint(data_dictionary))
    })

    values <- reactiveValues();
    values$lastAction <- NULL;
    observe({
        values$lastAction <- input$submit;
    })

    observeEvent(input$divChanged, {
        cat("divChanged")
        session$sendCustomMessage(type = 'divChanged', shiny:::flushReact())
    })


    outputOptions(output, 'chartLoanAmount', suspendWhenHidden = FALSE)
    outputOptions(output, 'data_dictionary', suspendWhenHidden = FALSE)
}

#' UI
#'
ui = htmlTemplate("www/index.html",
                    tag_about = includeMarkdown("about.md"),
                    tag_footer = h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
                    chart_loan_amount = plotlyOutput("chartLoanAmount", height = "auto"),
                    tag_data_dictionary = DT::dataTableOutput("data_dictionary")
                )

shinyApp(ui, server)

#shiny::stopApp()
