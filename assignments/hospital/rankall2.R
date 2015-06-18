##      Programming in R
##          Week 4
##

# Given a condition and a rank, this function returns a data frame whose
# rows are the state and hospital of the rank (compared to others in that state)

# This version uses dplyr instead -- the better choice

library(R.utils)
library(dplyr)
library(plyr)

rankall2 <- function(condition, rank = "best"){
    
    # ------ Load data -----
    setwd("~/github/datasciencecoursera/assignments/hospital")
    dataSet <- "outcome-of-care-measures.csv"
    outcome <- read.csv(dataSet)
    
    outcome <- tbl_df(outcome)
    
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
    
    # Select and rename necessary columns
    tbl_out <- select(outcome, hospital = Hospital.Name, state = State, 
                      rate = starts_with(colName))
    
    # Remove rows without ratings
    tbl_out <- filter(tbl_out, rate != "Not Available")
    
    # Sort by state, then rating, then hospital name
    tbl_out <- arrange(tbl_out, state, rate, hospital)

    # Get hospital of specified rank from each state
    res <- if(rank == "best"){
        ddply(tbl_out, .(state), function(x){ x[1, ]})
    }else if(rank == "worst"){
        ddply(tbl_out, .(state), function(x){ x[nrow(x), ]})  
    }else{
        ddply(tbl_out, .(state), function(x){ x[rank, ]}) 
    }

    # Prepare Final Form 
    states <- select(tbl_out, state) %>% distinct(state)
    
    final <- cbind(select(res, hospital), states)
    rownames(final) <- final$state
 
    final
}
