## Question -1

## Set the current working directory to point to the data files  
getwd()
setwd('C:\\Users\\Ishwa\\Desktop\\DataScience\\R\\Course_2_Assignment_3')


## Explore the data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)

## Plot the 30-day mortality rates for heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## Question -2

## Finding the best hospital in a state for the given outcome

best <- function(state, outcome) {
  
  ## Set the current working directory to point to the data files  
  getwd()
  setwd('C:\\Users\\Ishwa\\Desktop\\DataScience\\R\\Course_2_Assignment_3')
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  ## Format the column Hospital mortality column name for the given outcome
  if (outcome == 'heart failure'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  }
  else if (outcome == 'heart attack'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  }
  else 
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  
  ## Sort the lower mortality column 
  best_st <- st_outcome[order(as.numeric(st_outcome[[req_col_name]]),st_outcome$Hospital.Name),]
  
  ## Return hospital name in that state with lowest 30-day death rate for the given outcome
  best_st[1,'Hospital.Name']
  
}

# Sample outputs
print(best("TX", "heart failure"))
print(best("TX", "heart attack"))
print(best("MD", "heart attack"))
print(best("MD", "pneumonia"))
print(best("BB", "heart attack"))
print(best("NY", "hert attack"))

# Quiz assessment questions
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
