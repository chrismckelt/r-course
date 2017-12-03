suppressMessages(rm(list = ls()))
Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
options(shiny.port = 7775)
options(encoding = "UTF-8")
options(shiny.error = browser)
options(shiny.reactlog = TRUE)

suppressMessages(rm(list = ls()))
set.seed(2017)
switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/10-capstone")) },
       #Linux = { suppressMessages(setwd("~/srv/connect/apps/datascience-captstone")) },
       Darwin = { print("I'm a Mac.") })
source(paste0(getwd(),'/project_0_run.r'))

# SHINY 

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
    useShinyjs(html = TRUE)


    observeEvent(input$search, {
      if (input$search != "")
      {
        flog.info(input$search)
        results <- predictor(input$search)
        flog.debug(results)
        if (length(results) > 0)        
        {
          print(paste("results found", nrow(results)))
          session$sendCustomMessage(type = 'pred_1', message = results$predicted[1])
          session$sendCustomMessage(type = 'pred_2', message = results$predicted[2])
          session$sendCustomMessage(type = 'pred_3', message = results$predicted[3])
        } 
      }
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
