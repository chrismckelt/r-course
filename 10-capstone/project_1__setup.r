#Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_151\\')
source('c:/dev/r-course/10-capstone/include.r')

install_standard_packages()
using("pacman")
p_load("futile.logger")
flog.threshold(DEBUG)
p_load("tidyverse")
p_load("data.table")
p_load("tm")
p_load("doParallel")
p_load("Rmpi")
p_load("stringr")
p_load("stringi")
p_load("NLP")
p_load("openNLP")
p_load("RColorBrewer")
p_load("biclust")
p_load("cluster")
p_load("Matrix")
p_load("dplyr")
p_load("SparseM")
p_load("qdap")
p_load("parallel")
using("textreg") #https://cran.r-project.org/web/packages/textreg/textreg.pdf
using("tau")
p_load("foreach")
pacman::p_load_gh("trinker/qdapRegex") # https://github.com/trinker/qdapRegex
 
options(mc.cores = 1)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
options(save.defaults = list(ascii = TRUE, safe = FALSE))


flog.threshold(DEBUG, name = 'logger')



 