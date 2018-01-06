##Same as best() function, but it can give whatever rank, not just "best"
  ##Reads in data
  ##Makes sure state & outcome are valid
  ##Then for each outcome:
    ##Extracts outcome data from original data that is from that state
    ##Removes NA data
    ##Sorts state data and uses the num to return either the best, worst, or whatever ranking specified

findRankFromSortedStatesData <- function(ds,num){
  length <- nrow(ds) ##Stores length (# of rows) of data frame for if statements below
  
  if(num == "best"){
    ds[1,2] ##Returns hospital name at top of dataframe
  }
  else if(num == "worst"){
    ds[length,2] ##Returns hospital at bottom of dataframe
  }
  else if(num > length){
    return(NA) ##There is no specified ranking that large
  }
  else{
    ds[num,2] ##Returns hospital name at top of dataframe
  }
}

prepareData <- function(ds, colnum, st){
  states_data <- ds[ds$State == st,] ##Extracts rows of hospitals in specified state
  states_data[, colnum] <- as.numeric(states_data[,colnum]) ##Sets column 11 to be numeric instead of character
  good_states_data <- complete.cases(states_data[,colnum])  ##Finds rows containing NA in col 11
  states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
  states_data <- states_data[order(states_data[,colnum],states_data[,2]),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
  states_data
}

rankhospital <- function(state, outcome, num = "best") {
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(identical(state.abb[state.abb == state], character(0))){ ##Tests to see if state.abb does not contain the state. If true, throws error
    stop("invalid state")   ##STATE ARGUMENT DOESN'T MAKE SENSE
  }
  
  ##HEART ATTACK
  ##Column w/ 30-day mortality for heart attack is 11
  if(outcome == "heart attack"){
    states_data <- prepareData(all_data,11,state)
    
    findRankFromSortedStatesData(states_data,num)
  }
  
  ##HEART FAILURE
  ##Column w/ 30-day mortality for heart failure is 17
  else if(outcome == "heart failure"){
    states_data <- prepareData(all_data,17,state)
    
    findRankFromSortedStatesData(states_data,num)
  }
  
  ##PNEUMONIA
  ##Column w/ 30-day mortality for pneumonia is 23
  else if(outcome == "pneumonia"){
    states_data <- prepareData(all_data,23,state)
    
    findRankFromSortedStatesData(states_data,num)
  }
  
  else{
    stop("invalid outcome")   ##OUTCOME ARGUMENT DOESN'T MAKE SENSE
  }
}