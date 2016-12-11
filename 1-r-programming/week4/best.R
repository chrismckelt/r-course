best <- function(state, outcome) {
  
  ## Reads outcome data
  fileContent <- read.csv("outcome-of-care-measures.csv", sep = ",")
  
  ## Checks that state and outcome are valid
  isValidState(state)
  isValidOutcome(outcome)
  
  ## Returns hospital name in that state with lowest 30-day death
  data <- fileContent[fileContent$State == state,]
  headerName <- switch(outcome, 
                        "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                        "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                        stop("switch case not implemented")
                        )

  mortalityRate <- data[,headerName]
  mortalityRate <- mortalityRate[!mortalityRate == "Not Available"]
  mortalityRate <- as.numeric(as.character(mortalityRate))
  minRate <- min(mortalityRate)
  bestHospitals <- data[data[,headerName] == minRate,]
  hospitalNames <- sort(bestHospitals[,"Hospital.Name"])
  return(as.character(hospitalNames[1]))
}

isValidState <- function(arg){
    validstates <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
    if (!is.element(arg, validstates)) {
      stop("invalid state")
    }
}

isValidOutcome <- function(arg){
  validoutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(arg, validoutcomes)) {
    stop("invalid outcome")
  }
}


 

