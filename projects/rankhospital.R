rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  rawData <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  outcomeData <- rawData[ ,c(2, 7, 11, 17, 23)]
  colnames(outcomeData) <- c('Hospital', 'State', 'heart attack', 'heart failure', 'pneumonia')
  
  ## Check that state and outcome are valid
  if (state %in% outcomeData[, 2] == FALSE) {
    #if state is not in the state list, stop the function and print error
    stop('invalid state')
  }
  if (outcome %in% colnames(outcomeData)[3:5] == FALSE) {
    #if outcome is not in the outcome list, stop the function and print error
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with the given rank
  selectData <- outcomeData[outcomeData$State == state, c('Hospital', 'State', outcome)]
  #select only the query outcome in the state
  selectData <- selectData[order(selectData$Hospital, decreasing = F), ]
  #order hospitals alphabetically
  selectData[, 3] <- suppressWarnings(as.numeric(selectData[, 3]))
  #change the mortality column into numeric
  #warnings for coercing into NA - suppress this warning in the output
  selectData <- selectData[complete.cases(selectData), ]
  #remove the NAs
  selectData <- selectData[order(selectData[, 3], decreasing = F), ]
  #order the hospitals by smallest mortality on top (rank == 1)
  #this helps breaks the tie
  
  #Modify the num variable to rank numbers and check numbers
  if (num == 'best') { 
    #best finds the top-ranked hospital
    rank <- 1
  } else if (num == 'worst') { 
    #worst finds the hospital ranked the last place
    rank <- nrow(selectData)
  } else if (num > nrow(selectData)) { 
    #if number larger than the hospitals, function returns NA
    return(NA)
  } else {
    #else, rank == number
    rank <- num
  }
  
  ## 30-day death rate
  return(selectData[rank, 1])
  #first column is the hospital name
  #row number is the rank
}