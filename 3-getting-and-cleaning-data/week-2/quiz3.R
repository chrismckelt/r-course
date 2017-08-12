library(sqldf)
sqldf("select distinct AGEP from acs")
length(unique(acs$AGEP))