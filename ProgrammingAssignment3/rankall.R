rankall <- function(outcome, num = "best") {
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ranked_outcome <-  as.data.frame(matrix(ncol = 3, nrow = 0)) ##Set up dataframe w/ 3 columns we'll use to return final
  ranked_outcome[,1] <- as.character(ranked_outcome[,1])
  ranked_outcome[,2] <- as.character(ranked_outcome[,2])
  ranked_outcome[,3] <- as.numeric(ranked_outcome[,3])
  list_of_states <- sort(unique(specific_data$state))
  
  ##HEART ATTACK
  ##For each state, one at a time, 
      ##sort the new dataframe 
      ##and grab the hospital name & state from the top and 
      ##add it to bottom of "best" dataframe that we'll return at end
  ##Return best data frame at end
  if(outcome == "heart attack"){
    hospital_name <- all_data[, 2]
    state <- all_data[, 7]
    outcome_results <- all_data[, 11]
    specific_data <- data.frame(cbind(hospital_name,state,outcome_results)) ##specific_data is smaller dataframe w/ just data we need
    specific_data[, 3] <- as.numeric(levels(specific_data[, 3])[specific_data[, 3]]) ##Converts outcome data from factor to numeric, if you just set as numeric, it uses factor levels instead of values
    specific_data[, 2] <- as.character(levels(specific_data[, 2])[specific_data[, 2]]) ##Converts state data from factor to character, if you don't do this, factors throw a wrench in 
    
    
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
          good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
          states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
          ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name
          length <- nrow(ordered_states_data)
          ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
        }
      }
    
      else{
        for(st in list_of_states){
          states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
          good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
          states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
          ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name          
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
    
    ranked_outcome <- ranked_outcome[order(ranked_outcome$state),] ##reorder states alphabetically
    rownames(ranked_outcome) <- ranked_outcome[,2] ##label the row names by the state
    colnames(ranked_outcome) <- c("hospital","state") ##label the row names by the state
    ranked_outcome <- ranked_outcome[,1:2] ##Remove outcome data column
  }
  
  ##HEART FAILURE
  ##Column w/ 30-day mortality for heart failure is 17
  else if(outcome == "heart failure"){
    hospital_name <- all_data[, 2]
    state <- all_data[, 7]
    outcome_results <- all_data[, 17]
    specific_data <- data.frame(cbind(hospital_name,state,outcome_results)) ##specific_data is smaller dataframe w/ just data we need
    specific_data[, 3] <- as.numeric(levels(specific_data[, 3])[specific_data[, 3]]) ##Converts outcome data from factor to numeric, if you just set as numeric, it uses factor levels instead of values
    specific_data[, 2] <- as.character(levels(specific_data[, 2])[specific_data[, 2]]) ##Converts state data from factor to character, if you don't do this, factors throw a wrench in 
    
    
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
        good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
        states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name
        length <- nrow(ordered_states_data)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
      }
    }
    
    else{
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
        states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name          
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
    
    ranked_outcome <- ranked_outcome[order(ranked_outcome$state),] ##reorder states alphabetically
    rownames(ranked_outcome) <- ranked_outcome[,2] ##label the row names by the state
    colnames(ranked_outcome) <- c("hospital","state") ##label the row names by the state
    ranked_outcome <- ranked_outcome[,1:2] ##Remove outcome data column
  }
  
  ##PNEUMONIA
  ##Column w/ 30-day mortality for pneumonia is 23
  else if(outcome == "pneumonia"){
    hospital_name <- all_data[, 2]
    state <- all_data[, 7]
    outcome_results <- all_data[, 23]
    specific_data <- data.frame(cbind(hospital_name,state,outcome_results)) ##specific_data is smaller dataframe w/ just data we need
    specific_data[, 3] <- as.numeric(levels(specific_data[, 3])[specific_data[, 3]]) ##Converts outcome data from factor to numeric, if you just set as numeric, it uses factor levels instead of values
    specific_data[, 2] <- as.character(levels(specific_data[, 2])[specific_data[, 2]]) ##Converts state data from factor to character, if you don't do this, factors throw a wrench in 
    
    
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
        good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
        states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name
        length <- nrow(ordered_states_data)
        ranked_outcome <- rbind(ranked_outcome,ordered_states_data[length,])
      }
    }
    
    else{
      for(st in list_of_states){
        states_data <- specific_data[specific_data$state == st,] ##temporarily stores states_data for this state
        good_states_data <- complete.cases(states_data[,3])  ##Finds rows containing NA in col 3
        states_data <- states_data[good_states_data,] ##Updates states_data to remove NA rows
        ordered_states_data <- states_data[order(states_data$outcome_results,states_data$hospital_name),] ##Sorts lowest -> highest all 30-day mortality data, for ties sorts hospital name          
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
    
    ranked_outcome <- ranked_outcome[order(ranked_outcome$state),] ##reorder states alphabetically
    rownames(ranked_outcome) <- ranked_outcome[,2] ##label the row names by the state
    colnames(ranked_outcome) <- c("hospital","state") ##label the row names by the state
    ranked_outcome <- ranked_outcome[,1:2] ##Remove outcome data column
  }
  else{
    stop("invalid outcome")   ##OUTCOME ARGUMENT DOESN'T MAKE SENSE
  }
}