##      Programming in R
##          Week 2
##

# Given the directory containing the csv file, and a threshold number of 
# complete observations, the function computes the correlation between 
# nitrate and sulfate levels across all monitors with complete records
# above the threshold (returning the correlations as a vector)

corr <- function(directory, threshold = 0){
    
    vec <- vector("numeric")
    
    for(i in 1:332){
        
        filename <- sprintf("%03d.csv", i)
        path <- paste(directory, "/", filename, sep="")
        data <- read.csv(path)
        
        if(sum(complete.cases(data)) > threshold){
            
            nitrate <- data["nitrate"]
            sulfate <- data["sulfate"]
            vec <- c(vec, cor(nitrate, sulfate, use ="complete.obs"))
        }
    }

    vec
}