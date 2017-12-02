 library(shiny)
 runApp(appDir = getwd(), port = NULL,
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"), workerId = "",
  quiet = FALSE, display.mode = c("auto", "normal", "showcase"))