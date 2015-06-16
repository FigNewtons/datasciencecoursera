##      Programming in R
##          Week 2
##

# Given the directory containing the csv file, the pollutant ("nitrate" or 
# "sulfate"), and a vector of integers, the function returns the mean of
# that pollutant over the id-marked monitors

pollutantmean <- function(directory, pollutant, id=1:332){
    
    total <- 0
    count <- 0
    
    for(i in id){
        
        # Filename is in form 'xxx.csv' where xxx is a padded integer
        filename <- sprintf("%03d.csv", i)
        
        path <- paste(directory, "/", filename, sep="")
       
        data <- read.csv(path)
        
        observations <- data[[pollutant]]
        
        total <- total + sum(observations, na.rm = TRUE)
        count <- count + sum(!is.na(observations))
    }
    total / count
}