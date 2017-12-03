#Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_151\\')
switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/10-capstone")) },
 #      Linux = { suppressMessages(setwd("~/srv/connect/apps/datascience-captstone")) },
       Darwin = { print("I'm a Mac.") })

source(paste0(getwd(), '/include.r'))

require(devtools)
require(tidyverse)
require(data.table)
require(downloader)
require(magrittr)
require(ggplot2)
require(magrittr)
require(foreach)
require(stringr)
require(stringi)
require(lubridate)
require(foreach)
require(devtools)
require(ggplot2)
require(plotly)
require(DT)
require(knitr)
require(markdown)

require("futile.logger")
require(tidyverse)
require(ggplot2)

require(doParallel)
#require(Rmpi)
require(NLP)
require(openNLP)
#require(RColorBrewer)
#require(biclust)
#require(cluster)
require(Matrix)
require(dplyr)
#require(SparseM)

require(parallel)
require(DBI)

require(foreach)

require(corpus)
require(SnowballC)
require(tm)
require(tau)

require(shiny)
require(shinyjs)

require(qdapRegex)
require(qdapDictionaries)
require(qdapTools)
require(qdap)

#install_github("ggrothendieck/sqldf")
#install_github("trinker/qdapRegex")
#install_github("trinker/qdapDictionaries")
#install_github("trinker/qdapRegex")
#install_github("trinker/qdapTools")
#install_github("trinker/qdap")

options(mc.cores = 1)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
options(save.defaults = list(ascii = TRUE, safe = FALSE))

flog.threshold(DEBUG, name = 'logger')

 