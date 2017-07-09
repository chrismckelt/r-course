knitr::opts_chunk$set(echo = FALSE)
#knit("PA1_template.Rmd", output = NULL)

rmd <- file.path(getwd(), "assignment.RMD")
knitr::opts_chunk$set(fig.width = 8, fig.height = 6, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE)
rmarkdown::render("assignment.rmd", c("html_document"))