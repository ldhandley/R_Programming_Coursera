prepareData <- function(ds, colnum){
  hospital_name <- ds[, 2]
  state <- ds[, 7]
  outcome_results <- ds[, colnum]
  specific_data <- data.frame(cbind(hospital_name,state,outcome_results)) ##specific_data is smaller dataframe w/ just data we need
  specific_data[, 3] <- as.numeric(levels(specific_data[, 3])[specific_data[, 3]]) ##Converts outcome data from factor to numeric, if you just set as numeric, it uses factor levels instead of values
  specific_data[, 2] <- as.character(levels(specific_data[, 2])[specific_data[, 2]]) ##Converts state data from factor to character, if you don't do this, factors throw a wrench in 
  specific_data
}

cleanAndOrder <- function(states_data){
  good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
  states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
  ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name
}

processFinalData <- function(ranked_outcome){
  ranked_outcome <- ranked_outcome[order(ranked_outcome$state),] ##reorder states alphabetically
  rownames(ranked_outcome) <- ranked_outcome[,2] ##label the row names by the state
  colnames(ranked_outcome) <- c("hospital","state") ##label the row names by the state
  ranked_outcome <- ranked_outcome[,1:2] ##Remove outcome data column
}

sortAndGrabRankings <- function(){
  
}

rankall <- function(outcome, num = "best") {
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ranked_outcome <-  as.data.frame(matrix(ncol = 3, nrow = 0)) ##Set up dataframe w/ 3 columns we'll use to return final
  ranked_outcome[,1] <- as.character(ranked_outcome[,1])
  ranked_outcome[,2] <- as.character(ranked_outcome[,2])
  ranked_outcome[,3] <- as.numeric(ranked_outcome[,3])
  
  list_of_states <- sort(unique(all_data$State))
  
  ##HEART ATTACK
  if(outcome == "heart attack"){
  specific_data <- prepareData(all_data,11)
    
      ##BEST
      if(num == "best"){
        for(st in list_of_states){
          states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
          ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
          ranked_outcome <- rbind(ranked_outcome,ordered_states_data[1,])
        }
      }
      
      ##WORST
      else if(num=="worst"){
        for(st in list_of_states){
          states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
          ordered_states_data <- cleanAndOrder(states_data)
          length <- nrow(ordered_states_data)
          ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
        }
      }
    
      else{
        for(st in list_of_states){
          states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
          ordered_states_data <- cleanAndOrder(states_data)
          if(is.na(ordered_states_data[num,3])){ ##If there is not ranking for this outcome for this state (i.e. its NA)...
            ranked_outcome <- rbind(ranked_outcome, data.frame(  ##Creates a special data frame row that will have NA, the state's name, and NA, w/ column names labeled
                                                      hospital_name = ordered_states_data[num,1],
                                                      state         = st, 
                                                      outcome_results = ordered_states_data[num,3])
                                    )
          }
          else{
            ranked_outcome <- rbind(ranked_outcome,ordered_states_data[num,]) ##If ranking exists, tacks row onto bottom of ranked_outcome
          }
        }
      }
    
      ranked_outcome <- processFinalData(ranked_outcome)
  }
  
  ##HEART FAILURE
  else if(outcome == "heart failure"){
    specific_data <- prepareData(all_data,17)
    
    ##BEST
    if(num == "best"){
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[1,])
      }
    }
    
    ##WORST
    else if(num=="worst"){
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- cleanAndOrder(states_data)
        length <- nrow(ordered_states_data)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
      }
    }
    
    else{
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- cleanAndOrder(states_data)
        if(is.na(ordered_states_data[num,3])){ ##If there is not ranking for this outcome for this state (i.e. its NA)...
          ranked_outcome <- rbind(ranked_outcome, data.frame(  ##Creates a special data frame row that will have NA, the state's name, and NA, w/ column names labeled
            hospital_name = ordered_states_data[num,1],
            state         = st, 
            outcome_results = ordered_states_data[num,3])
          )
        }
        else{
          ranked_outcome <- rbind(ranked_outcome,ordered_states_data[num,]) ##If ranking exists, tacks row onto bottom of ranked_outcome
        }
      }
    }
    
    ranked_outcome <- processFinalData(ranked_outcome)
  }
  
  ##PNEUMONIA
  else if(outcome == "pneumonia"){
    specific_data <- prepareData(all_data,23)
    
    ##BEST
    if(num == "best"){
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data (col 11), for ties sorts hospital name (col 2)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[1,])
      }
    }
    
    ##WORST
    else if(num=="worst"){
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- cleanAndOrder(states_data)
        length <- nrow(ordered_states_data)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
      }
    }
    
    else{
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        ordered_states_data <- cleanAndOrder(states_data)
        if(is.na(ordered_states_data[num,3])){ ##If there is not ranking for this outcome for this state (i.e. its NA)...
          ranked_outcome <- rbind(ranked_outcome, data.frame(  ##Creates a special data frame row that will have NA, the state's name, and NA, w/ column names labeled
            hospital_name = ordered_states_data[num,1],
            state         = st, 
            outcome_results = ordered_states_data[num,3])
          )
        }
        else{
          ranked_outcome <- rbind(ranked_outcome,ordered_states_data[num,]) ##If ranking exists, tacks row onto bottom of ranked_outcome
        }
      }
    }
    
    ranked_outcome <- processFinalData(ranked_outcome)
  }
  else{
    stop("invalid outcome")   ##OUTCOME ARGUMENT DOESN'T MAKE SENSE
  }
}