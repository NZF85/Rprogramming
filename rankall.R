rankall <- function(outcome, num = "best") {
  
  ## Reads outcome data
  file_data <- read.csv("outcome-of-care-measures.csv", sep = ",")
  
  ## Checks that state and outcome are valid
  valid_states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
  
  header_name <- NULL
  if (outcome == "heart attack") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  hosps <- c()
  states <- c()
  
  ## For each state, finds the hospital of the given rank
  for (state in valid_states) {
    
    ranked_hosp <- c()
    ##looks at a state
    data <- file_data[file_data$State == state,]
    ##first it gives each of the hospital an order number,
    ##arranges the data according to the order number
    ##arranges it according to hospital name alphabetical order
    ##, then pulls the values
    ##for the header_name plus the hospital name into sorted_data
    sorted_data <- data[order(as.numeric(as.character(data[,header_name])), as.character(data[,"Hospital.Name"])),]
    ##since the coercion of data didnt happen in the data set, it still contains the "not available" status
    sorted_data <- sorted_data[!sorted_data[,header_name] == "Not Available",]
    if (num == "best") {
      ranked_hosp <- best(state, outcome)
    } else if (num == "worst") {
      ranked_hosp <- as.character(tail(sorted_data[,"Hospital.Name"]))
    } else {
      ranked_hosp <- as.character(sorted_data[,"Hospital.Name"][num])
    }
    hosps <- c(hosps, ranked_hosp)
  }
  result <- data.frame(hosps, valid_states)
  colnames(result) <- c("hospital", "state")
  return(result)
}
