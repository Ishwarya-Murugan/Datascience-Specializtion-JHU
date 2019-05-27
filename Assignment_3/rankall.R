## Question -4

## Ranking hospitals in all states

## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv ???le and returns a 2-column data frame containing the hospital
## in each state that has the ranking speci???ed in num. For example the function call rankall("heart attack", "best") 
## would return a data frame containing the names of the hospitals that are the best in their respective states for 
## 30-day heart attack death rates. The function should return a value for every state (some may be NA). The ???rst 
## column in the data frame is named hospital, which contains the hospital name, and the second column is named state, 
## which contains the 2-character abbreviation for the state name. Hospitals that do not have data on a particular 
## outcome should be excluded from the set of hospitals when deciding the rankings.

rankall <- function(outcome, num = "best") { 
  
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
  
  ## Format the column lower mortality column name for the given outcome
  if (outcome == 'heart failure'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  }
  else if (outcome == 'heart attack'){
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  }
  else 
    req_col_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  
  
  ## For each state, find the hospital of the given rank
  out <- sapply(split(outcome_data,outcome_data$State),
                function(outcome_by_st, num,req_col_name) {
                  non_na_count <- sum(!is.na(outcome_by_st[req_col_name]))
                  if (num=='worst'){
                    num <- non_na_count
                  }  
                  else if (num=='best'){
                    num <- 1
                  }
                  else if(num > non_na_count){
                    return (NA)
                  }
                  ranking <- outcome_by_st[order(as.numeric(outcome_by_st[[req_col_name]]),outcome_by_st$Hospital.Name),]
                  ranking[num,'Hospital.Name']
                },num,req_col_name)
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  out <- stack(out)
  names(out) <- c("hospital", "state")
  out
}

# Sample outputs
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

# Quiz assessment questions
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
