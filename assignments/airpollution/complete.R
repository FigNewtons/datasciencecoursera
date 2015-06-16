##      Programming in R
##          Week 2
##

# Given the directory containing the csv file and a vector of integers,
# the function outputs the total number of complete observations for each
# monitor (labelled by id)

complete <- function(directory, id = 1:332){
    
    observed <- data.frame()
    
    for(i in seq_along(id)){ 
        
        filename <- sprintf("%03d.csv", id[i])
        path <- paste(directory, "/", filename, sep="")
        data <- read.csv(path)

        observed[i, "id"] <- id[i]
        observed[i, "nobs"] <- sum(complete.cases(data))
    }
    observed
}