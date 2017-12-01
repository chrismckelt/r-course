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

source('c:/dev/r-course/10-capstone/project_0_run.r')
 
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

    observe({
        p <- predictor(input$search)
        return(p)
    })

    observeEvent(input$stop, {
        shiny::stopApp()
        #RSQLite::dbDisconnect()
    })


}

#' UI
#'
ui = htmlTemplate("www/index.html")

shinyApp(ui, server)

#shiny::stopApp()
