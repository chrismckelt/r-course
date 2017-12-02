## ----setup, include=FALSE------------------------------------------------
suppressMessages(rm(list = ls()))
set.seed(2017)

switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/10-capstone")) },
       Linux = { suppressMessages(setwd("~/srv/connect/apps/loan_book_analyser")) },
       Darwin = { print("I'm a Mac.") })


source('c:/dev/r-course/10-capstone/project_1__setup.r')
 
program_result = withCallingHandlers({
    source('c:/dev/r-course/10-capstone/project_2__data_retrieval.r')
    source('c:/dev/r-course/10-capstone/project_3__data_clean.r')
    source('c:/dev/r-course/10-capstone/project_4__ngram_generation.r')
    #source('c:/dev/r-course/10-capstone/project_4__tokenization.r')
    #source('c:/dev/r-course/10-capstone/project_4__rcorpus.r')
    source('c:/dev/r-course/10-capstone/predictor.r')
    #source('c:/dev/r-course/10-capstone/predictor.tests.r')
    #source('c:/dev/r-course/10-capstone/dsci-benchmark-master/benchmark.r')

}, warning = function(w) {    
    flog.warn(w)
}, error = function(e) {    
    flog.error(e)
}, finally = {
  
})

program_result

