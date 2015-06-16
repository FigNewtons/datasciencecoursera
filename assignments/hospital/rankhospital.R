##      Programming in R
##          Week 4
##

# Given a two character state abbreviation (i.e. "MD"), a condition 
# ("heart attack", "heart failure", pneumonia"), and a rank (either an integer,
# "best", or "worst"), the function returns the name of the hospital with that
# rank based on the specified condition (out of all the hospitals in that state)

library(R.utils)

rankhospital <- function(state, condition, rank = "best"){
    
    # ------ Load data -----
    setwd("~/github/datasciencecoursera/assignments/hospital")
    dataSet <- "outcome-of-care-measures.csv"
    outcome <- read.csv(dataSet)
    
    # ------ Outcome/condition constants ------
    possCondition <- c("heart attack", "heart failure", "pneumonia")
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    # ----- Error Handles for bad user input -----
    if(!any(state == outcome$State)){
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
        outcome[outcome$State == state, c("Hospital.Name", colName)]
    
    # Shortcut for column of condition
    column <- stateOutcome[, colName]
    
    # Convert from factor to numeric to sort
    stateOutcome[, colName] <- 
        suppressWarnings(as.numeric(levels(column))[column])
    
    # Remove hospitals without value for mortality rate
    stateOutcome <- stateOutcome[complete.cases(stateOutcome), ]
    
    # Sort by mortality rate and alphabetical order of hospital name
    sorted <-
        stateOutcome[order(stateOutcome[colName], stateOutcome$Hospital.Name), ]
    
    # Get hospital name
    name <- if(rank == "best"){
                sorted[1, "Hospital.Name"]            
    }else if(rank == "worst"){
                sorted[nrow(sorted), "Hospital.Name"]
    }else if(rank > nrow(sorted)){
                NA
    }else{
                sorted[rank, "Hospital.Name"]
    }
    
    if(!is.na(name)){
        name <- as.character(droplevels(name))
    }
    
    name
}