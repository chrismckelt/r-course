## SETUP ##
suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/10-capstone/"))

#shiny::runApp('C:/dev/r-course/9-data-products/week-4', port = 9191, display.mode = "showcase")
#shiny::stopApp()

install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name = 'chrismckelt',
                           token = 'AD514A6E98A5ECF80DC8B4D39E879F9D',
                           secret = 'ys4H/4hnLulYPoq5/wITCwXAiWoSktKzH/HP3PCC')


 rsconnect::deployApp(appTitle = "Capstone", appDir = "C:/dev/r-course/10-capstone/")


