suppressMessages(rm(list = ls()))
options(encoding = "UTF-8")
#options(shiny.error = browser)
options(shiny.reactlog = TRUE)

switch(Sys.info()[['sysname']],
       Windows= {suppressMessages(setwd("C:/dev/r-course/9-data-products/week-4"))},
     #  Linux  = {suppressMessages(setwd("~/srv/connect/apps/loan_book_analyser"))},
       Darwin = {print("I'm a Mac.")})

library(pacman)
library(tidyverse)
library(knitr)
library(markdown)
library(data.table)
library(sqldf)
library(ggplot2)
library(lubridate)
library(foreach)
library(RSQLite)
library(shiny)
library(shinyjs)
library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(readxl)
library(devtools)
library(ggplot2)
library(plotly)
library(DT)

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
options(scipen = 999)
set.seed(3333)

if (!file.exists("./data/lending-club-loan-data.zip")) {
  ### original file = "https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip"
  ### requires authentication
  ### for this exercise most to server with no Auth for download
  save_file("http://www.mckelt.com/lending-club-loan-data.zip", "/data/lending-club-loan-data.zip")
  outdir <- paste0(trimws(getwd()), "/data")
  outdir
  zippedFile <- paste0(trimws(outdir), "/lending-club-loan-data.zip")
  zippedFile
  unzip(zipfile=zippedFile,exdir = outdir)
}


connection_string <- paste(getwd(), '/data/database.sqlite', sep = '')
db <- RSQLite::dbConnect(SQLite(), dbname = connection_string, loadable.extensions = TRUE, cache_size = NULL, synchronous = "off", flags = SQLITE_RWC, vfs = NULL)

data.tables = dbListTables(db)
data_loanbook <- sqldf("select * from loan", connection = db)

## clean
#excel_file <- paste0(getwd(), "/data/LCdatadictionary.xlsx")
data_dictionary <- read_excel("data/LCDataDictionary.xlsx")
data_dictionary <- sqldf("select LoanStatNew as Variable_name, description from [data_dictionary] where LoanStatNew != 'NA'")
data_dictionary$Id <- seq.int(nrow(data_dictionary))

### keep only columns in data dictionary
cols_to_keep <- data_dictionary[[1]]
data_loanbook <- data_loanbook[, which(names(data_loanbook) %in% cols_to_keep)]

### fix dates
data_loanbook$issue_d <- as.Date(gsub("^", "01-", data_loanbook$issue_d), format = "%d-%b-%Y")
data_loanbook$grade <- as.factor(data_loanbook$grade)

## load the state names
data(state.regions)
# merge the loan book with the state names
data_loanbook <- merge(data_loanbook, state.regions, by.x = "addr_state", by.y = "abb")

#' takes a variable name to filter and group the data (vs total loan amount)
#'
#' @param by
#'
#' @return
#' @export
#'
#' @examples
query.data <- function(by) {
    if (length(by) < 1) {
        by <- "grade"
    }
    sql <- paste("select sum(loan_amnt) as total_loan_amount, ", by, " from [data_loanbook] group by ", by)
    print(sql)
    return(sqldf(sql))
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
    query_by = "grade"
    query_desc = "credit grade"

    observe({

        index_choosen <- as.numeric(input$data_dictionary_rows_selected[[1]])
        print(paste("index_choosen", index_choosen))
        if (length(index_choosen) > 0) {
            index_choosen <- as.numeric(length(input$data_dictionary_rows_selected)) # issue with multiple selects on UI grid - just get last item selected
            sql <- paste("select * from data_dictionary where Id =", index_choosen)
        
            selected_item <- sqldf(sql)
            query2_by = unlist(selected_item[1])
            query2_desc = unlist(selected_item[2])

            output$chartLoanAmount <- renderPlotly({
                p <- plot_ly(query.data(query2_by), x = ~get(query2_by), y = ~total_loan_amount, colors = TRUE, type = 'bar',
                                marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)'))) %>%
                                layout(title = paste("Loan amounts by ", ~ get(query2_desc)),
                                        autosize = F,
                                        width = 600,
                                        height = 400,
                                        xaxis = list(title = ~get(query2_by)),
                                        yaxis = list(title = "Total loan amount")
                                    )
                return(p)
            })
        }
    })

    # initial load vals are null
    if (query_by == "") {
        query_by <- "grade"
        query_desc <- "credit grade"
    }

    dataset <- reactive({ query.data(query_by) })
     
    output$data_dictionary <- renderDataTable(data_dictionary, options = list(pageLength = 25, server = FALSE, selection = list(mode = "single", target = "cell")))

    output$chartLoanAmount <- renderPlotly({
        p <- plot_ly(dataset(), x = ~get(query_by), y = ~total_loan_amount, colors = TRUE, type = 'bar', environment = environment(),
                                    marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)'))) %>%
                                    layout(title = paste("Loan amounts by ", ~ get(query_by)),
                                            autosize = F,
                                            width = 600,
                                            height = 400,
                                            xaxis = list(title = ~get(query_by)),
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

    #observeEvent(input$divChanged, {
    #cat("divChanged")
    #session$sendCustomMessage(type = 'divChanged', shiny:::flushReact())
    #})


    outputOptions(output, 'chartLoanAmount', suspendWhenHidden = FALSE)
    outputOptions(output, 'data_dictionary', suspendWhenHidden = FALSE)
}

#' UI
#'
ui = htmlTemplate("www/index.html",
                    tag_about = includeMarkdown("about.md"),
                    tag_footer = h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
                    chart_loan_amount = plotlyOutput("chartLoanAmount"),
                    tag_data_dictionary = DT::dataTableOutput("data_dictionary")
     
                )

shinyApp(ui, server)

#shiny::stopApp()
