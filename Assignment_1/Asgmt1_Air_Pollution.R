## Part 1

## Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a 
## specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory
## specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring 
## any missing values coded as NA.

library(stringr)
pollutantmean <- function(directory, pollutant, id=1:332){
  
  ## 'directory' is a character vector of length 1 indicating the location of the CSv file
  
  ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate 
  ## the mean; either 'sulfate' or 'nitrate'
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant across all the monitors list in the 'id' vector (ignoring NA value) 
  ## NOTE: Do not round the result!
  
  file_ids <- str_pad(id, width=3, side="left", pad="0")
  print(file_ids)
  data <- data.frame()
  
  for (file in file_ids){

    file_name <- paste(directory,"\\",file,".csv",sep="")
    data <- rbind(data, read.csv(file_name))
  }

  data_rm_na = data[!is.na(data[pollutant]),]
  pol_sum = sum(data_rm_na[pollutant])
  pol_count = dim(data_rm_na)[1]
  pol_sum/pol_count
}

# Sample outputs

directory = "C:\\Users\\Ishwa\\Desktop\\rprog_data_specdata\\specdata"

pollutantmean(directory,"sulfate", id=1:10)
pollutantmean(directory,"nitrate", id=70:72)
pollutantmean(directory,"nitrate")
pollutantmean(directory, "sulfate", 34)

## ------------------------------------------------------------------------------------------

## Part 2

## Write a function that reads a directory full of files and reports the number of completely observed cases in each 
## data file. The function should return a data frame where the first column is the name of the file and the second 
## column is the number of complete cases

complete <- function(directory, id=1:332) {
  
  ## 'directory' is a character vector of length 1 indicating the location of CSV file
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
  
  file_ids <- str_pad(id, width=3, side="left", pad="0")

  data <- data.frame()
  cc_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cc_df) <-  c("id", "nobs")
  
  for (file in file_ids){
    
    file_name <- paste(directory,"\\",file,".csv",sep="")
    data <- read.csv(file_name)
    file_id = as.integer(file)
    file_nobs = dim(data[complete.cases(data),])[1]

    cc_df <- rbind(cc_df, data.frame("id"=file_id,"nobs"=file_nobs))
  }
  cc_df
}

# Sample outputs

complete(directory, id=1)
complete(directory, c(6, 10, 20, 34, 100, 200, 310))
complete(directory, 30:25)
set.seed(42)
cc<- complete(directory,332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

## ------------------------------------------------------------------------------------------

## Part 3

## Write a function that takes a directory of data files and a threshold for complete cases and calculates the 
## correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
## (on all variables) is greater than the threshold. The function should return a vector of correlations for the 
## monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function 
## should return a numeric vector of length 0.

corr <- function(directory, threshold=332:10) {
  
  ## 'directory' is a character vector of length 1 indicating the location of CSV file
  ## 'threshold is a numeric vector of length 1 indicating the number of completely observed
  ## observations (on all variables) required to compute the correlation between nitrate and
  ## sulfate; the default is 0
  
  ## Return a numeric vector of correlations 
  ## NOTE: Do not round the result!
  
  file_list <- list.files(directory)
  data <- data.frame()
  cor_vect <- c()
  
  for (file in file_list){
    file_name <- paste(directory,"\\",file,sep="")
    data <- read.csv(file_name, header=TRUE)
    data_rm_na <- data[complete.cases(data),]
    cc_count <- dim(data_rm_na)[1]

    if(cc_count > threshold ){
      cor_value <- cor(data_rm_na[,"sulfate"],data_rm_na[,"nitrate"])
      cor_vect <- c(cor_vect, cor_value)
    }
  }
  cor_vect
  
}

# Sample outputs

summary(corr(directory, 150))
summary(corr(directory, 400))
summary(corr(directory, 5000))
summary(corr(directory))

cr <- corr(directory)                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr(directory, 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
