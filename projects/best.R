best <- function(state, outcome) {
  
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
  
  ## Return hospital name in that state with lowest 30-day death
  selectData <- outcomeData[outcomeData$State == state, c('Hospital', 'State', outcome)]
  #select only the query outcome in the state
  selectData <- selectData[order(selectData$Hospital, decreasing = F), ]
  #order hospitals alphabetically
  selectData[, 3] <- suppressWarnings(as.numeric(selectData[, 3]))
  #change the mortality column into numeric
  #warnings for coercing into NA - suppress this warning in the output
  
  ## rate
  return(selectData[which.min(selectData[, 3]), 1]) 
}