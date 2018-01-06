##Same as best() function, but it can give whatever rank, not just "best"

rankhospital <- function(state, outcome, num = "best") {
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(identical(state.abb[state.abb == state], character(0))){ ##Tests to see if state.abb does not contain the state. If true, throws error
    stop("invalid state")   ##STATE ARGUMENT DOESN'T MAKE SENSE
  }
  
  ##HEART ATTACK
  ##Column w/ 30-day mortality for heart attack is 11
  if(outcome == "heart attack"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 11] <- as.numeric(states_data[, 11]) ##Sets column 11 to be numeric instead of character
    good_states_data <- complete.cases(states_data[,11])  ##Finds rows containing NA in col 11
    states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
    states_data <- states_data[order(states_data[,11],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
    length <- nrow(states_data) ##Stores length (# of rows) of data frame for if statements below
    if(num == "best"){
      states_data[1,2] ##Returns hospital name at top of dataframe
    }
    else if(num == "worst"){
      states_data[length,2] ##Returns hospital at bottom of dataframe
    }
    else if(num > length){
      return(NA) ##There is no specified ranking that large
    }
    else{
      states_data[num,2] ##Returns hospital name at top of dataframe
    }
  }
  
  ##HEART FAILURE
  ##Column w/ 30-day mortality for heart failure is 17
  else if(outcome == "heart failure"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 17] <- as.numeric(states_data[, 17]) ##Sets column 11 to be numeric instead of character
    good_states_data <- complete.cases(states_data[,17])  ##Finds rows containing NA in col 11
    states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
    states_data <- states_data[order(states_data[,17],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
    length <- nrow(states_data) ##Stores length (# of rows) of data frame for if statements below
    if(num == "best"){
      states_data[1,2] ##Returns hospital name at top of dataframe
    }
    else if(num == "worst"){
      states_data[length,2] ##Returns hospital at bottom of dataframe
    }
    else if(num > length){
      return(NA) ##There is no specified ranking that large
    }
    else{
      states_data[num,2] ##Returns hospital name at top of dataframe
    }
  }
  
  ##PNEUMONIA
  ##Column w/ 30-day mortality for pneumonia is 23
  else if(outcome == "pneumonia"){
    states_data <- all_data[all_data$State == state,] ##Extracts rows of hospitals in specified state
    states_data[, 23] <- as.numeric(states_data[, 23]) ##Sets column 11 to be numeric instead of character
    good_states_data <- complete.cases(states_data[,23])  ##Finds rows containing NA in col 11
    states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
    states_data <- states_data[order(states_data[,23],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
    length <- nrow(states_data) ##Stores length (# of rows) of data frame for if statements below
    if(num == "best"){
      states_data[1,2] ##Returns hospital name at top of dataframe
    }
    else if(num == "worst"){
      states_data[length,2] ##Returns hospital at bottom of dataframe
    }
    else if(num > length){
      return(NA) ##There is no specified ranking that large
    }
    else{
      states_data[num,2] ##Returns hospital name at top of dataframe
    }
  }
  else{
    stop("invalid outcome")   ##OUTCOME ARGUMENT DOESN'T MAKE SENSE
  }
}