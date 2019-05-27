# Question -3

## The function reads the outcome-of-care-measures.csv ???le and 
## returns a character vector with the name of the hospital that has the ranking specified 
## by the num argument. For example, the call


rankhospital <- function(state, outcome, num = "best") { 
  
  ## Set the current working directory to point to the data files  
  getwd()
  setwd('C:\\Users\\Ishwa\\Desktop\\DataScience\\R\\Course_2_Assignment_3')
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## Valid outcomes and states
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  valid_states <- unique(outcome_data[,7])
  
  ## Check that state and outcome are valid
  if (!outcome %in% valid_outcomes){
    stop('invalid outcome')
  }  
  if (!state %in% valid_states){
    stop('invalid state')
  }
  
  ## Filter the data for the given state
  st_outcome <- outcome_data[outcome_data$State==state,]
  
  ## Format the column lower mortality column name for the given outcome
  if (outcome == 'heart failure'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  }
  else if (outcome == 'heart attack'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  }
  else 
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  
  ## Rank the hospital for the given mortality
  best_st <- st_outcome[order(as.numeric(st_outcome[[req_col_name]]),st_outcome$Hospital.Name),]
  non_na_count <- sum(!is.na(best_st[req_col_name]))
  if (num=='worst'){
    num <- non_na_count
  }  
  else if (num=='best'){
    num <- 1
  }
  else if(num > non_na_count){
    return (NA)
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  best_st[num,'Hospital.Name']
}

# Sample outputs
print(rankhospital("MD", "heart failure", 5))
print(rankhospital("TX", "heart failure", 4))
print(rankhospital("MD", "heart attack", "worst"))
rankhospital("MN", "heart attack", 5000)

# Quiz assessment questions
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

