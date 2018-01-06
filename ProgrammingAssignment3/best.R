best <- function(state, outcome){
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(identical(state.abb[state.abb == state], character(0))){ ##Tests to see if state.abb does not contain the state. If true, throws error
    stop("invalid state")   ##STATE ARGUMENT DOESN'T MAKE SENSE
  }
  
  ##Column w/ 30-day mortality for heart attack is 11
  if(outcome == "heart attack"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 11] <- as.numeric(states_data[, 11]) ##Sets column 11 to be numeric instead of character
    states_data <- states_data[order(states_data[,11],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
    states_data[1,2] ##Returns hospital name at top of dataframe
  }
  
  ##Column w/ 30-day mortality for heart failure is 17
  else if(outcome == "heart failure"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 17] <- as.numeric(states_data[, 17]) ##Sets column 11 to be numeric instead of character
    states_data <- states_data[order(states_data[,17],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 17), for ties sorts hospital name (col 2)
    states_data[1,2] ##Returns hospital name at top of dataframe
  }
  
  ##Column w/ 30-day mortality for pneumonia is 23
  else if(outcome == "pneumonia"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 23] <- as.numeric(states_data[, 23]) ##Sets column 11 to be numeric instead of character
    states_data <- states_data[order(states_data[,23],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 23), for ties sorts hospital name (col 2)
    states_data[1,2] ##Returns hospital name at top of dataframe
  }
  else{
    stop("invalid outcome")   ##OUTCOME ARGUMENT DOESN'T MAKE SENSE
  }
}