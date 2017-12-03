switch(Sys.info()[['sysname']],
       Windows = { suppressMessages(setwd("C:/dev/r-course/10-capstone")) },
    #   Linux = { suppressMessages(setwd("~/srv/connect/apps/datascience-captstone")) },
       Darwin = { print("I'm a Mac.") })

source(paste0(getwd(), '/project_1__setup.r'))

program_result = withCallingHandlers({
    source(paste0(getwd(), '/project_2__data_retrieval.r'))
    source(paste0(getwd(), '/project_3__data_clean.r'))
    source(paste0(getwd(), '/project_4__ngram_generation.r'))
    source(paste0(getwd(), '/predictor.R'))
    #source(paste0(getwd(), '/sci-benchmark-master/benchmark.r')))

    #source('project_4__tokenization.r')
    #source('project_4__rcorpus.r')
    #source('predictor.tests.r')

}, warning = function(w) {    
    flog.warn(w)
}, error = function(e) { 
    print(e)
    flog.error(e)
}, finally = {
  
})

program_result

