suppressMessages(rm(list = ls()))
Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
options(shiny.port = 7775)
options(encoding = "UTF-8")
options(shiny.error = browser)
options(shiny.reactlog = TRUE)

source('c:/dev/r-course/10-capstone/project_0_run.r')
suppressMessages(setwd("C:/dev/r-course/10-capstone"))
#stop("stopping")

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
  
    reactive({
      output$search_result_1 <- reactiveVal("")
      output$search_result_2 <- reactiveVal("")
      output$search_result_3 <- reactiveVal("")
      
    })

    observeEvent(input$search, {
      flog.info(input$search)
      results <- predictor(input$search)
      flog.debug(results)
      if (length(results) > 0)        
      {
        output$search_result_1 <- renderText(results$predicted[1])
        output$search_result_2 <- renderText(results$predicted[2])
        output$search_result_3 <- renderText(results$predicted[3])
      }
    })
    
    
    observeEvent(input$stop, {
        shiny::stopApp()
        #RSQLite::dbDisconnect()
    })
    
    #outputOptions(output$search_result_1, 'search_result_1', suspendWhenHidden = FALSE)
    #outputOptions(output$search_result_2, 'search_result_2', suspendWhenHidden = FALSE)
    #outputOptions(output$search_result_3, 'search_result_3', suspendWhenHidden = FALSE)
}

#' UI
#'

#ui = htmlTemplate("www/index.html", button_search_result_1 =  actionButton("r1", 'search_result_1'))
#                 button_search_result_2 =  actionButton("r1",output$search_result_2),
#                 button_search_result_3 =  actionButton("r1",output$search_result_3)
#                 )

ui = htmlTemplate("www/index.html")


shinyApp(ui, server)

#shiny::stopApp()
