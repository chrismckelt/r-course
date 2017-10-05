# Peer-graded Assignment: R Markdown Presentation & Plotly
# https://www.coursera.org/learn/data-products/peer/a1Uy9/r-markdown-presentation-plotly
# Create a web page presentation using R Markdown that features a plot created with Plotly

# Host your webpage on either GitHub Pages, RPubs, or NeoCities.

# Your webpage must contain the date that you created the document, and it must contain a plot created with Plotly. We would love to see you show off your creativity!

suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/9-data-products/week-3"))

source("c:/dev/r-course/include.r")
using("dplyr")
using("devtools")
devtools::install_github("ropensci/plotly")

library(plotly)

Sys.setenv("plotly_username" = "chrismckelt")
Sys.setenv("plotly_api_key" = "M6S961nyr6MaEAwtNAM0")
#p <- plotly(username = "chrismckelt", key = "M6S961nyr6MaEAwtNAM0")

 
###rmarkdown::render('C:/dev/r-course/9-data-products/week-3/assignment-2.Rmd')