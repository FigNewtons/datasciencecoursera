##      Programming in R
##          Week 4
##

# Given a two character state abbreviation (i.e. "MD") and a condition 
# ("heart attack", "heart failure", pneumonia"), the function returns the 
# name of the hospital in that state with the lowest 30-day mortality rate 
# for that condition; in the event of a tie, the function returns the first 
# name occurring alphabetically 

library(R.utils)

best <- function(stateAbbrev, condition){
    
    # ------ Load data -----
    setwd("~/github/datasciencecoursera/assignments/hospital")
    dataSet <- "outcome-of-care-measures.csv"
    outcome <- read.csv(dataSet)
    
    # ------ Outcome/condition constants ------
    possCondition <- c("heart attack", "heart failure", "pneumonia")
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    # ----- Error Handles for bad user input -----
    if(!any(stateAbbrev == outcome$State)){
        stop("invalid state")
    }
    
    if(!any(condition == possCondition)){
        stop("invalid outcome")
    }
    
    # ----- Query data and output hospital name -----

    # Format end part of column name ("heart attack" --> "Heart.Attack")
    end <- strsplit(condition, " ")[[1]]
    end <- paste(capitalize(end), collapse=".")
 
    colName <- paste(colName, end, sep="")
    
    stateOutcome <- 
        outcome[outcome$State == stateAbbrev, c("Hospital.Name", colName)]
    
    # Shortcut for column of condition
    column <- stateOutcome[, colName]
    
    # Convert from factor to numeric to apply min 
    stateOutcome[, colName] <- 
        suppressWarnings(as.numeric(levels(column))[column])

    bestValue <- min(stateOutcome[colName], na.rm = TRUE)
    
    index <- which(stateOutcome[colName] == bestValue)
    
    # Drop factor levels and retrieve name(s)
    best <- droplevels(stateOutcome[index, "Hospital.Name"])
    
    # If tie, get the first name alphabetically 
    min(as.character(best))

}