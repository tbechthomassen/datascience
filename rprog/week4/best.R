best <- function(state, outcome) {
      outData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if(state %in% outData$State & outcome %in% outData$){
            
      }
      
      ## Return hospital name in that state with lowest 30-day death rate
}

## set variables
inputState <- "AL" # two-letter abbreviation of State
outcome <- "heart failure" # corresponds to a value in the outcomeList


## get outcome data
alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outData <- alldata[, c(2, 7, 11, 17, 23)]
colnames(outData) <- c("Hospital.name", "state", "heart.attack.30mort", "heart.failure.30mort", "pneumonia.30mort")
outcomeList <- c("heart failure", "heart attack", "pneumonia")


## check if variables are in dataset

if(inputState %in% outData$state & outcome %in% outcomeList){
      
      if (outcome == "heart failure"){
            stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.failure.30mort"))
      } else if (outcome == "heart attack"){
            stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.attack.30mort"))
      } else if (outcome == "pneumonia"){
            stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "pneumonia.30mort"))
      } else {stop("'ALT ER GALT -- IGEN!'")}
      #       
      #       sorted <- with(stateData, order("pneumonia"))
      
} else if(!is.element(inputState, outData$state)){
      stop("'invalid state'")
} else if(!is.element(outcome, outcomeList)){
      stop("'invalid outcome'")
} else{stop('"ALT ER GALT!"')}


