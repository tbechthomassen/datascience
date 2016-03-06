best <- function(inputState, outcome) {
     
     ## get outcome data
     alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     outData <- alldata[, c(2, 7, 11, 17, 23)]
     colnames(outData) <- c("Hospital.name", "state", "heart.attack", "heart.failure", "pneumonia")
     outcomeList <- c("heart failure", "heart attack", "pneumonia")
     
     
     ## check if variables are in dataset
     
     if(inputState %in% outData$state & outcome %in% outcomeList){
          
          if (outcome == "heart failure"){
               stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.failure"))
               attach(stateData)
               sorted <- stateData[order(heart.failure, Hospital.name),]
               sorted[1,"Hospital.name"]
               # detach(stateData)
               # rm(stateData)
          } else if (outcome == "heart attack"){
               stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.attack"))
               attach(stateData)
               sorted <- stateData[order(heart.attack, Hospital.name),]
               sorted[1,"Hospital.name"]
               # detach(stateData)
               # rm(stateData)
          } else if (outcome == "pneumonia"){
               stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "pneumonia"))
               attach(stateData)
               sorted <- with(stateData, stateData[order(pneumonia, Hospital.name),])
               sorted[1,"Hospital.name"]
               detach(stateData)
               rm(stateData)
          } else {stop("'ALT ER GALT -- IGEN!'")}
          
     } else if(!is.element(inputState, outData$state)){
          stop("'invalid state'")
          
     } else if(!is.element(outcome, outcomeList)){
          stop("'invalid outcome'")
          
     } else{stop('"ALT ER GALT!"')}
}

# ## set variables
# inputState <- "AL" # two-letter abbreviation of State
# outcome <- "heart failure" # corresponds to a value in the outcomeList


# ## get outcome data
# alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# outData <- alldata[, c(2, 7, 11, 17, 23)]
# colnames(outData) <- c("Hospital.name", "state", "heart.attack", "heart.failure", "pneumonia")
# outcomeList <- c("heart failure", "heart attack", "pneumonia")
# 
# 
# ## check if variables are in dataset
# 
# if(inputState %in% outData$state & outcome %in% outcomeList){
#      
#      if (outcome == "heart failure"){
#           stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.failure"))
#           attach(stateData)
#           sorted <- stateData[order(heart.failure, Hospital.name),]
#           sorted[1,"Hospital.name"]
#      } else if (outcome == "heart attack"){
#           stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "heart.attack"))
#           attach(stateData)
#           sorted <- stateData[order(heart.attack, Hospital.name),]
#           sorted[1,"Hospital.name"]
#           
#      } else if (outcome == "pneumonia"){
#           stateData <- subset(outData, subset = state == inputState, select = c("Hospital.name", "state", "pneumonia"))
#           attach(stateData)
#           sorted <- stateData[order(pneumonia, Hospital.name),]
#           sorted[1,"Hospital.name"]
#      } else {stop("'ALT ER GALT -- IGEN!'")}
#      
# } else if(!is.element(inputState, outData$state)){
#      stop("'invalid state'")
#      
# } else if(!is.element(outcome, outcomeList)){
#      stop("'invalid outcome'")
#      
# } else{stop('"ALT ER GALT!"')}
# 
# 
