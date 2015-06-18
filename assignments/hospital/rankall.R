##      Programming in R
##          Week 4
##

# Given a condition and a rank, this function returns a data frame whose
# rows are the state and hospital of the rank (compared to others in that state)

library(R.utils)

rankall <- function(condition, rank = "best"){
    
    # ------ Load data -----
    setwd("~/github/datasciencecoursera/assignments/hospital")
    dataSet <- "outcome-of-care-measures.csv"
    outcome <- read.csv(dataSet)
    
    # ------ Outcome/condition constants ------
    possCondition <- c("heart attack", "heart failure", "pneumonia")
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    # ----- Error Handles for bad user input -----
    if(!any(condition == possCondition)){
        stop("invalid outcome")
    }
 
    # ----- Query data and output hospital name -----
    
    # Format end part of column name ("heart attack" --> "Heart.Attack")
    end <- strsplit(condition, " ")[[1]]
    end <- paste(capitalize(end), collapse=".")
    
    colName <- paste(colName, end, sep="")
    
    # Focus only on necessary columns and prepare data for ordering
    subOutcome <- outcome[, c("Hospital.Name", colName, "State")]
    
    # Rename columns
    names(subOutcome) <- c("hospital", "rate", "state")
    
    # Convert columns from factors
    rateCol <-  subOutcome[, "rate"]
    hosCol <- subOutcome[, "hospital"]
    stateCol <- subOutcome[, "state"]
    
    subOutcome[ , "rate"] <-
            suppressWarnings(as.numeric(levels(rateCol))[rateCol])
    subOutcome[ , "hospital"] <-
            suppressWarnings(as.character(levels(hosCol))[hosCol])
    subOutcome[, "state"] <-
            suppressWarnings(as.character(levels(stateCol))[stateCol])
    
    # Remove NAs 
    df <- subOutcome[complete.cases(subOutcome), ]
    
    # Order 
    df <- df[with(df, order(state, rate, hospital)), c("hospital", "state") ]
    
    # Factor by state
    fac <- factor(df[, "state"])
    
    # Returns a matrix -- splits df by state and gets hospital of that rank
    smat <- if(rank == "best"){
            sapply(split(df, fac), function(x){x[1, ]})
    }else if (rank == "worst"){
            sapply(split(df, fac), function(x){x[nrow(x), ]})
    }else{
            sapply(split(df, fac), function(x){x[rank, ]})
    }
    
    # Rebind as data frame     
    state <- colnames(smat)
    hospital <- smat[1, ]
    
    data.frame(cbind(hospital, state))
}
